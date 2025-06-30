const std = @import("std");
const InternPool = @import("InternPool.zig");

const Allocator = std.mem.Allocator;
const Item = InternPool.Item;

pub const Value = union(enum) {
    int: i64,
    bool: bool,

    pub const Tag = std.meta.Tag(Value);
};

pub const TypedValue = struct {
    ty: InternPool.Index,
    val: Value,

    // converts a Value into an InternPool.Item for efficient
    // storage. Extra serialization storage is available via
    // the .extra and .wide arrays owned by the pool.
    pub fn serialize(tv: TypedValue, pool: *InternPool) !Item {
        const val = tv.val;

        return switch (val) {
            .int => |int| item: {
                const wide_top: u32 = @intCast(pool.wide.items.len);
                try pool.wide.append(pool.gpa, int);
                const extra = try pool.addExtra(Item.Int{
                    .ty = tv.ty,
                    .wide = @enumFromInt(wide_top),
                });

                break :item .{
                    .tag = .int_tv,
                    .payload = .{ .extra = extra },
                };
            },
            .bool => |b| .{ .tag = .bool_tv, .payload = .{ .bool = b } },
        };
    }

    // converts an InternPool.Item back into a TypedValue. It is invalid
    // to call this function on an Item that is not a TypedValue.
    pub fn deserialize(item: Item, pool: *const InternPool) TypedValue {
        const tag = item.tag;
        const payload = item.payload;

        return switch (tag) {
            .int_tv => {
                const int = pool.extraData(payload.extra, Item.Int);
                const val = pool.wide.items[@intFromEnum(int.wide)];
                return .{ .ty = int.ty, .val = .{ .int = val } };
            },
            .bool_tv => .{ .ty = .bool, .val = .{ .bool = payload.bool } },
            else => unreachable,
        };
    }

    pub fn hash64(tv: TypedValue) u64 {
        const Hash = std.hash.Wyhash;
        const asBytes = std.mem.asBytes;

        const seed = @intFromEnum(tv.val);
        var hasher = Hash.init(seed);
        hasher.update(asBytes(&tv.ty));

        switch (tv.val) {
            inline else => |val| hasher.update(asBytes(&val)),
        }

        return hasher.final();
    }

    pub const common = struct {
        pub const izero: TypedValue = .{ .ty = .int, .val = .{ .int = 0 } };
        pub const ione: TypedValue = .{ .ty = .int, .val = .{ .int = 1 } };
        pub const @"true": TypedValue = .{ .ty = .bool, .val = .{ .bool = true } };
        pub const @"false": TypedValue = .{ .ty = .bool, .val = .{ .bool = false } };
    };
};
