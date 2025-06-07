const std = @import("std");
const InternPool = @import("InternPool.zig");

const Allocator = std.mem.Allocator;
const Item = InternPool.Item;
const Index = InternPool.Index;
const asBytes = std.mem.asBytes;

pub const Type = union(enum) {
    // hardware bit vector
    bits: u32,
    // hardware unsigned integer vector
    uint: u32,
    // hardware signed integer vector
    sint: u32,
    // hardware boolean
    bool: void,
    // compile time integer, no width and not synthesizable
    int: void,
    // hardware bundle of nets
    bundle: struct {
        field_names: []const Index,
        field_types: []const Index,
    },
    // hardware module
    module: struct {
        input_names: []const Index,
        input_types: []const Index,
        output_type: Index,
    },

    pub const Tag = std.meta.Tag(Type);

    // converts a Type into an InternPool.Item for efficient
    // storage. Extra serialization storage is available via
    // the .extra and .wide arrays owned by the pool.
    pub fn serialize(ty: Type, pool: *InternPool) !Item {
        return switch (ty) {
            .bits => |width| .{ .tag = .bits_ty, .payload = .{ .width = width } },
            .uint => |width| .{ .tag = .uint_ty, .payload = .{ .width = width } },
            .sint => |width| .{ .tag = .sint_ty, .payload = .{ .width = width } },
            .bool => .{ .tag = .bool_ty, .payload = .{ .placeholder = {} } },
            .int => .{ .tag = .int_ty, .payload = .{ .placeholder = {} } },
            .bundle => |bundle| bundle: {
                const extra = try pool.addExtra(Item.Bundle{
                    .field_names = try pool.addSlice(@ptrCast(bundle.field_names)),
                    .field_types = try pool.addSlice(@ptrCast(bundle.field_types)),
                });

                break :bundle .{
                    .tag = .bundle_ty,
                    .payload = .{ .extra = extra },
                };
            },
            .module => |module| module: {
                const extra = try pool.addExtra(Item.Module{
                    .input_names = try pool.addSlice(@ptrCast(module.input_names)),
                    .input_types = try pool.addSlice(@ptrCast(module.input_types)),
                    .output_type = module.output_type,
                });

                break :module .{
                    .tag = .module_ty,
                    .payload = .{ .extra = extra },
                };
            },
        };
    }

    // converts an InternPool.Item back into a Type. It is invalid
    // to call this function on an Item that is not a Type.
    pub fn deserialize(item: Item, pool: *const InternPool) Type {
        return switch (item.tag) {
            .bits_ty => .{ .bits = item.payload.width },
            .uint_ty => .{ .uint = item.payload.width },
            .sint_ty => .{ .sint = item.payload.width },
            .bool_ty => .{ .bool = {} },
            .int_ty => .{ .int = {} },
            .bundle_ty => type: {
                const bundle = pool.extraData(item.payload.extra, Item.Bundle);
                const field_names: []const Index = pool.extraSlice(bundle.field_names);
                const field_types: []const Index = pool.extraSlice(bundle.field_types);

                break :type .{
                    .bundle = .{
                        .field_names = field_names,
                        .field_types = field_types,
                    },
                };
            },
            .module_ty => type: {
                const module = pool.extraData(item.payload.extra, Item.Module);
                const input_names: []const Index = pool.extraSlice(module.input_names);
                const input_types: []const Index = pool.extraSlice(module.input_types);

                break :type .{
                    .module = .{
                        .input_names = input_names,
                        .input_types = input_types,
                        .output_type = module.output_type,
                    },
                };
            },
            else => unreachable,
        };
    }

    pub fn hash64(ty: Type) u64 {
        const Hash = std.hash.Wyhash;
        const seed = @intFromEnum(ty);
        var hasher: Hash = .init(seed);
        switch (ty) {
            .bits, .uint, .sint => |width| hasher.update(asBytes(&width)),
            .bool, .int => {},
            .bundle => |bundle| {
                for (bundle.field_names) |*field| hasher.update(asBytes(field));
                for (bundle.field_types) |*field| hasher.update(asBytes(field));
            },
            .module => |module| {
                for (module.input_names) |*field| hasher.update(asBytes(field));
                for (module.input_types) |*field| hasher.update(asBytes(field));
                hasher.update(asBytes(&module.output_type));
            },
        }

        return hasher.final();
    }

    pub fn eql(a: Type, b: Type) bool {
        const a_tag = @as(Tag, a);
        const b_tag = @as(Tag, b);
        if (a_tag != b_tag) return false;

        return switch (a_tag) {
            .bundle => {
                if (!std.mem.eql(Index, a.bundle.field_names, b.bundle.field_names)) return false;
                if (!std.mem.eql(Index, a.bundle.field_types, b.bundle.field_types)) return false;
                return true;
            },
            inline else => std.meta.eql(a, b),
        };
    }

    pub const common = struct {
        pub const b1: Type = .{ .bits = 1 };
        pub const @"u1": Type = .{ .uint = 1 };
        pub const @"i1": Type = .{ .sint = 1 };
        pub const @"bool": Type = .{ .bool = {} };
        pub const int: Type = .{ .int = {} };
    };
};

// test "union types" {
//     const unionTypes = Type.unionTypes;
//
//     var pool = try InternPool.init(std.testing.allocator);
//     defer pool.deinit();
//     var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
//     defer arena_allocator.deinit();
//     const arena = arena_allocator.allocator();
//     const failing = std.testing.failing_allocator;
//
//     const int_type: Type = .{ .int = {} };
//     const float_type: Type = .{ .float = {} };
//     const bool_type: Type = .{ .bool = {} };
//     try std.testing.expectEqual(int_type, try unionTypes(failing, &pool, int_type, int_type));
//     try std.testing.expectEqual(float_type, try unionTypes(failing, &pool, float_type, float_type));
//     try std.testing.expectEqual(bool_type, try unionTypes(failing, &pool, bool_type, bool_type));
//
//     // note: these union literals must be ordered per the definitions in the
//     // InternPool.Index enum. This is not recommended usage for unions, prefer
//     // unionTypes() over explicit union types.
//     const int_float: Type = .{ .@"union" = &.{ .int, .float } };
//     // union of two primitives
//     try std.testing.expect(int_float.eql(try unionTypes(arena, &pool, int_type, float_type)));
//     try std.testing.expect(int_float.eql(try unionTypes(arena, &pool, float_type, int_type)));
//     // equality of unions
//     try std.testing.expect(int_float.eql(try unionTypes(arena, &pool, int_float, int_float)));
//
//     const int_float_bool: Type = .{ .@"union" = &.{ .int, .float, .bool } };
//     // union | primitive
//     try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, int_float, bool_type)));
//     try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, bool_type, int_float)));
//     // fully overlapping
//     try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, int_float, int_float_bool)));
//     try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, int_float_bool, int_float)));
//
//     const float_bool: Type = .{ .@"union" = &.{ .float, .bool } };
//     // partially overlapping
//     try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, int_float, float_bool)));
//     try std.testing.expect(int_float_bool.eql(try unionTypes(arena, &pool, float_bool, int_float)));
// }
