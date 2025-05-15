const std = @import("std");
const InternPool = @import("InternPool.zig");

const Allocator = std.mem.Allocator;
const Item = InternPool.Item;
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
    // list: InternPool.Index,
    // @"union": []const InternPool.Index,
    // any: void,

    pub const Tag = std.meta.Tag(Type);

    // converts a Type into an InternPool.Item for efficient
    // storage. Extra serialization storage is available via
    // the .extra and .wide arrays owned by the pool.
    pub fn serialize(ty: Type, pool: *InternPool) !Item {
        _ = pool;
        return switch (ty) {
            .bits => |width| .{ .tag = .bits_ty, .payload = .{ .width = width } },
            .uint => |width| .{ .tag = .uint_ty, .payload = .{ .width = width } },
            .sint => |width| .{ .tag = .sint_ty, .payload = .{ .width = width } },
            .bool => .{ .tag = .bool_ty, .payload = .{ .placeholder = {} } },
            .int => .{ .tag = .int_ty, .payload = .{ .placeholder = {} } },
            // .list => |ip| .{ .tag = .list_ty, .payload = .{ .ip = ip } },
            // .@"union" => |types| {
            //     const slice = try pool.addSlice(@ptrCast(types));
            //     return .{ .tag = .union_ty, .payload = .{ .extra = slice } };
            // },
            // .object => |fields| {
            //     const ptr = asBytes(fields.ptr);
            //     comptime std.debug.assert(@alignOf(InternPool.Index) == @alignOf(Attribute));
            //     const ips: [*]const InternPool.Index = @ptrCast(ptr);
            //     // std.debug.print("{any}\n", .{ips[0 .. fields.len * 2]});
            //     const slice = try pool.addSlice(@ptrCast(ips[0 .. fields.len * 2]));
            //     return .{ .tag = .object_ty, .payload = .{ .extra = slice } };
            // },
            // .any => .{ .tag = .any_ty, .payload = .{ .placeholder = {} } },
        };
    }

    // converts an InternPool.Item back into a Type. It is invalid
    // to call this function on an Item that is not a Type.
    pub fn deserialize(item: Item, pool: *const InternPool) Type {
        _ = pool;
        return switch (item.tag) {
            .bits_ty => .{ .bits = item.payload.width },
            .uint_ty => .{ .uint = item.payload.width },
            .sint_ty => .{ .sint = item.payload.width },
            .bool_ty => .{ .bool = {} },
            .int_ty => .{ .int = {} },
            // .union_ty => {
            //     const slice = pool.extraData(item.payload.extra, Item.ExtraSlice);
            //     return .{ .@"union" = @ptrCast(pool.extraSlice(slice)) };
            // },
            // .object_ty => {
            //     const slice = pool.extraData(item.payload.extra, Item.ExtraSlice);
            //     const ips: []const InternPool.Index = @ptrCast(pool.extraSlice(slice));
            //     const ptr = asBytes(ips.ptr);
            //     const fields: [*]const Attribute = @ptrCast(ptr);
            //     // std.debug.print("{any}\n", .{fields[0 .. ips.len / 2]});
            //     return .{ .object = @ptrCast(fields[0 .. ips.len / 2]) };
            // },
            // .any_ty => .{ .any = {} },
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
            // inline .@"union", .object => |items| {
            //     for (items) |*item| hasher.update(asBytes(item));
            // },
        }

        return hasher.final();
    }

    pub fn eql(a: Type, b: Type) bool {
        const a_tag = @as(Tag, a);
        const b_tag = @as(Tag, b);
        if (a_tag != b_tag) return false;

        return switch (a_tag) {
            // .@"union" => std.mem.eql(InternPool.Index, a.@"union", b.@"union"),
            // .object => {
            //     if (a.object.len != b.object.len) return false;
            //     for (a.object, b.object) |a_attr, b_attr| {
            //         if (!std.meta.eql(a_attr, b_attr)) return false;
            //     }
            //     return true;
            // },
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
