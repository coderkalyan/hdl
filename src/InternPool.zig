const std = @import("std");
const builtin = @import("builtin");
const Type = @import("type.zig").Type;
const TypedValue = @import("value.zig").TypedValue;
const Air = @import("Air.zig");
const Cst = @import("Cst.zig");
const Package = @import("Package.zig");

const InternPool = @This();
const Allocator = std.mem.Allocator;
const Decl = Package.Decl;

gpa: Allocator,
/// Used to map keys to indices in `items` using standard
/// hashing and probing.
map: std.AutoArrayHashMapUnmanaged(void, void),
/// The primary backing store of the pool - stores a
/// enum tag (representing the kind of object) and a single u32.
items: std.MultiArrayList(Item),
/// Extra u32 store for objects.
extra: std.ArrayListUnmanaged(u32),
/// Extra i64 store for objects, primarily used for 64 bit constants.
wide: std.ArrayListUnmanaged(i64),
/// A list of all generated module Airs.
airs: std.SegmentedList(Air, 1),
/// A list of all package Decls.
decls: std.SegmentedList(Decl, 1),
// TODO: probably move this to somewhere else
// Index  into the decls list by name.
decls_map: std.AutoHashMapUnmanaged(InternPool.Index, DeclIndex),
/// Backing store for string interning.
bytes: std.ArrayListUnmanaged(u8),
/// Probing table for interning strings in the `bytes` backing store.
string_table: std.HashMapUnmanaged(StringIndex, void, IndexContext, std.hash_map.default_max_load_percentage),

pub const ExtraIndex = enum(u32) { _ };
pub const WideIndex = enum(u32) { _ };
pub const StringIndex = enum(u32) { _ };
pub const AirIndex = enum(u32) { _ };
pub const DeclIndex = enum(u32) { _ };

pub const Key = union(enum) {
    placeholder,
    ty: Type,
    tv: TypedValue,
    str: []const u8,
    // air: AirIndex,
    // decl: DeclIndex,

    const Tag = std.meta.Tag(Key);

    const Adapter = struct {
        pool: *InternPool,

        pub fn eql(adapter: Adapter, a: Key, b_void: void, b_map_index: usize) bool {
            _ = b_void;
            const b = adapter.pool.get(@enumFromInt(b_map_index));
            return b.eql(a, adapter.pool);
        }

        pub fn hash(adapter: Adapter, key: Key) u32 {
            _ = adapter;
            return key.hash32();
        }
    };

    fn hash32(key: Key) u32 {
        return @truncate(key.hash64());
    }

    fn hash64(key: Key) u64 {
        const Hash = std.hash.Wyhash;
        // const asBytes = std.mem.asBytes;
        const seed = @intFromEnum(key);

        return switch (key) {
            .placeholder => seed,
            .ty => |ty| ty.hash64(),
            .tv => |tv| tv.hash64(),
            .str => |str| Hash.hash(seed, str),
            // inline .air,
            // .decl,
            // => |index| Hash.hash(seed, asBytes(&index)),
        };
    }

    fn eql(a: Key, b: Key, pool: *const InternPool) bool {
        _ = pool;

        const a_tag = @as(Key.Tag, a);
        const b_tag = @as(Key.Tag, b);
        if (a_tag != b_tag) return false;

        switch (a_tag) {
            .placeholder => return true,
            .ty => return a.ty.eql(b.ty),
            inline .tv => |tag| {
                const a_data = @field(a, @tagName(tag));
                const b_data = @field(b, @tagName(tag));
                return std.meta.eql(a_data, b_data);
            },
            .str => return std.mem.eql(u8, a.str, b.str),
        }
    }
};

pub const Item = struct {
    tag: Tag,
    payload: Payload,

    pub const Tag = enum(u8) {
        placeholder,
        bits_ty,
        uint_ty,
        sint_ty,
        bool_ty,
        int_ty,
        bundle_ty,
        array_ty,
        signature_ty,
        module_ty,
        int_tv,
        bool_tv,
        str,
        // air,
        // decl,
    };

    pub const Payload = union {
        /// Used when tag enum is enough to encode the entire item.
        placeholder: void,
        /// Used to encode bit widths of vector types.
        width: u32,
        /// Bools are the only constant small enough to encode inline.
        bool: bool,
        /// Index into item array (reference to another item).
        ip: Index,
        /// Index into extra data array (32 bit store).
        extra: ExtraIndex,
        /// Index into wide data array (64 bit store).
        wide: WideIndex,
        /// Index into string table.
        str: StringIndex,
        // Index into airs list.
        // air: AirIndex,
        // Index into the decls list.
        // decl: DeclIndex,

        comptime {
            if (builtin.mode == .ReleaseFast) {
                std.debug.assert(@sizeOf(Payload) <= 4);
            }
        }
    };

    /// Unpacks to `Type.Bundle`.
    pub const Bundle = struct {
        cst_index: Cst.Index,
        name: Index,
        field_names: Index.Slice,
        field_types: Index.Slice,
    };

    /// Unpacks to `Type.Array`.
    pub const Array = struct {
        element_type: Index,
        len: u32,
    };

    /// Unpacks to `Type.Signature`.
    pub const Signature = struct {
        input_names: Index.Slice,
        input_types: Index.Slice,
        output_type: Index,
    };

    /// Unpacks to `Type.Module`.
    pub const Module = struct {
        cst_index: Cst.Index,
        name: Index,
        signature: Index,
        air: AirIndex,
    };

    /// Unpacks to `TypedValue.Int`
    pub const Int = struct {
        ty: Index,
        wide: WideIndex,
    };

    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };
};

/// Indices are 31 bits so the upper bit can be used to discriminate
/// between runtime (air indices) and compile time (intern pool indices).
/// Common items are created at init time and given reserved indices.
pub const Index = enum(u31) {
    /// Placeholder for representing null index.
    null,
    // Common types.
    int,
    bool,
    b1,
    /// Common values.
    izero,
    ione,
    btrue,
    bfalse,
    /// Common strings.
    builtin_out,
    builtin_bool,

    _,

    pub const Slice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };
};

const IndexContext = struct {
    bytes: *std.ArrayListUnmanaged(u8),

    pub fn eql(self: IndexContext, a: StringIndex, b: StringIndex) bool {
        _ = self;
        return a == b;
    }

    pub fn hash(self: IndexContext, index: StringIndex) u64 {
        const x: u32 = @intFromEnum(index);
        const str = std.mem.span(@as([*:0]const u8, @ptrCast(self.bytes.items.ptr)) + x);
        return std.hash_map.hashString(str);
    }
};

const SliceAdapter = struct {
    bytes: *std.ArrayListUnmanaged(u8),

    pub fn eql(self: SliceAdapter, a_str: []const u8, b: u32) bool {
        const b_str = std.mem.span(@as([*:0]const u8, @ptrCast(self.bytes.items.ptr)) + b);
        return std.mem.eql(u8, a_str, b_str);
    }

    pub fn hash(self: SliceAdapter, str: []const u8) u64 {
        _ = self;
        return std.hash_map.hashString(str);
    }
};

const static_keys = [_]Key{
    .{ .placeholder = {} },
    .{ .ty = Type.common.int },
    .{ .ty = Type.common.bool },
    .{ .ty = Type.common.b1 },
    .{ .tv = TypedValue.common.izero },
    .{ .tv = TypedValue.common.ione },
    .{ .tv = TypedValue.common.true },
    .{ .tv = TypedValue.common.false },
    .{ .str = "out" },
    .{ .str = "bool" },
};

pub fn init(gpa: Allocator) !InternPool {
    var pool: InternPool = .{
        .gpa = gpa,
        .map = .empty,
        .items = .empty,
        .extra = .empty,
        .wide = .empty,
        .airs = .{},
        .decls = .{},
        .decls_map = .empty,
        .bytes = .empty,
        .string_table = .empty,
    };

    for (static_keys) |key| _ = try pool.put(key);
    return pool;
}

pub fn deinit(pool: *InternPool) void {
    const gpa = pool.gpa;
    pool.map.deinit(gpa);
    pool.items.deinit(gpa);
    pool.extra.deinit(gpa);
    pool.wide.deinit(gpa);
    pool.decls.deinit(gpa);
    pool.decls_map.deinit(gpa);
    pool.bytes.deinit(gpa);
    pool.string_table.deinit(gpa);

    var it = pool.airs.iterator(0);
    while (it.next()) |air| air.deinit(gpa);
    pool.airs.deinit(gpa);
}

pub fn addExtra(pool: *InternPool, extra: anytype) !ExtraIndex {
    std.debug.assert(@alignOf(@TypeOf(extra)) >= 4);

    const words: []const u32 = @ptrCast(std.mem.asBytes(&extra));
    const len: u32 = @intCast(pool.extra.items.len);
    try pool.extra.appendSlice(pool.gpa, words);
    return @enumFromInt(len);
}

pub fn extraData(pool: *const InternPool, index: ExtraIndex, comptime T: type) T {
    std.debug.assert(@alignOf(T) >= 4);

    var result: T = undefined;
    const dst: []u32 = @ptrCast(std.mem.asBytes(&result));
    const start: u32 = @intFromEnum(index);
    const size = @sizeOf(T) / @sizeOf(u32);
    const src: []const u32 = pool.extra.items[start .. start + size];
    @memcpy(dst, src);

    return result;
}

pub fn addSlice(pool: *InternPool, sl: []const u32) !Index.Slice {
    const start: u32 = @intCast(pool.extra.items.len);
    try pool.extra.appendSlice(pool.gpa, sl);
    const end: u32 = @intCast(pool.extra.items.len);

    return .{
        .start = @enumFromInt(start),
        .end = @enumFromInt(end),
    };
}

pub fn extraSlice(pool: *const InternPool, sl: Index.Slice) []const Index {
    const start: u32 = @intFromEnum(sl.start);
    const end: u32 = @intFromEnum(sl.end);
    return @ptrCast(pool.extra.items[start..end]);
}

pub fn put(pool: *InternPool, key: Key) !Index {
    const adapter: Key.Adapter = .{ .pool = pool };
    const gop = try pool.map.getOrPutAdapted(pool.gpa, key, adapter);
    if (gop.found_existing) return @enumFromInt(gop.index);

    try pool.items.append(pool.gpa, switch (key) {
        .placeholder => .{
            .tag = .placeholder,
            .payload = .{ .placeholder = {} },
        },
        .ty => |ty| try ty.serialize(pool),
        .tv => |tv| try tv.serialize(pool),
        .str => |str| try pool.putString(str),
        // .air => |air| .{
        //     .tag = .air,
        //     .payload = .{ .air = air },
        // },
        // .decl => |decl| .{
        //     .tag = .decl,
        //     .payload = .{ .decl = decl },
        // },
    });
    return @enumFromInt(pool.items.len - 1);
}

pub fn get(pool: *const InternPool, _index: Index) Key {
    const index: u64 = @intFromEnum(_index);
    std.debug.assert(index < pool.items.len);

    const item = pool.items.get(index);
    return switch (item.tag) {
        .placeholder => .{ .placeholder = {} },
        .bits_ty,
        .uint_ty,
        .sint_ty,
        .bool_ty,
        .int_ty,
        .bundle_ty,
        .array_ty,
        .signature_ty,
        .module_ty,
        => .{ .ty = Type.deserialize(item, pool) },
        .int_tv,
        .bool_tv,
        => .{ .tv = TypedValue.deserialize(item, pool) },
        .str => .{ .str = pool.getString(item) },
        // .air => .{ .air = item.payload.air },
        // .decl => .{ .decl = item.payload.decl },
    };
}

fn putString(pool: *InternPool, str: []const u8) !Item {
    const index: StringIndex = @enumFromInt(@as(u32, @intCast(pool.bytes.items.len)));

    try pool.bytes.ensureUnusedCapacity(pool.gpa, str.len + 1);
    pool.bytes.appendSliceAssumeCapacity(str);
    pool.bytes.appendAssumeCapacity('\x00');

    const index_context: IndexContext = .{ .bytes = &pool.bytes };
    try pool.string_table.putContext(pool.gpa, index, {}, index_context);

    return .{ .tag = .str, .payload = .{ .str = index } };
}

fn getString(pool: *const InternPool, item: Item) []const u8 {
    const offset: u32 = @intFromEnum(item.payload.str);
    return std.mem.span(@as([*:0]const u8, @ptrCast(pool.bytes.items.ptr)) + offset);
}

pub fn createAir(pool: *InternPool, air: Air) !AirIndex {
    const index: u32 = @intCast(pool.airs.count());
    try pool.airs.append(pool.gpa, air);
    return @enumFromInt(index);
}

pub fn airPtr(pool: *InternPool, index: AirIndex) *Air {
    return pool.airs.at(@intFromEnum(index));
}

pub fn airPtrConst(pool: *const InternPool, index: AirIndex) *const Air {
    return pool.airs.at(@intFromEnum(index));
}

pub fn createDecl(pool: *InternPool, decl: Decl) !DeclIndex {
    const index: u32 = @intCast(pool.decls.count());
    try pool.decls.append(pool.gpa, decl);
    return @enumFromInt(index);
}

pub fn declPtr(pool: *InternPool, index: DeclIndex) *Decl {
    return pool.decls.at(@intFromEnum(index));
}

pub fn print(pool: *const InternPool, writer: anytype, ip: Index) !void {
    const key = pool.get(ip);
    switch (key) {
        .ty => |ty| switch (ty) {
            inline else => try writer.print("{s}", .{@tagName(ty)}),
        },
        .tv => |tv| {
            try pool.print(writer, tv.ty);
            try writer.print("(", .{});
            switch (tv.val) {
                .none => try writer.print("none", .{}),
                inline .int, .float, .bool => |val| try writer.print("{}", .{val}),
            }
            try writer.print(")", .{});
        },
        .str => |str| try writer.print("\"{s}\"", .{str}),
        .air => |air| try writer.print("air{}", .{@intFromEnum(air)}),
    }
}

fn testRoundtrip(pool: *InternPool, key: Key) !void {
    // put the key into the pool and record the index
    const index = try pool.put(key);
    // put it in two more times and make sure we get the same index
    try std.testing.expectEqual(index, try pool.put(key));
    try std.testing.expectEqual(index, try pool.put(key));

    // index -> key -> index roundtrip
    var key_rt = pool.get(index);
    try std.testing.expect(key.eql(key_rt, pool));
    try std.testing.expectEqual(index, try pool.put(key_rt));

    // key -> index -> key roundtrip
    const index_rt = try pool.put(key);
    try std.testing.expectEqual(index, index_rt);
    key_rt = pool.get(index_rt);
    try std.testing.expect(key.eql(key_rt, pool));
}

fn testIndex(pool: *InternPool, key: Key, index: Index) !void {
    try std.testing.expectEqual(index, try pool.put(key));
    try std.testing.expect(key.eql(pool.get(index), pool));
}

test "basic type intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try testRoundtrip(&pool, .{ .ty = .{ .bits = {} } });
    try testRoundtrip(&pool, .{ .ty = .{ .uint = {} } });
    try testRoundtrip(&pool, .{ .ty = .{ .sint = {} } });
    try testRoundtrip(&pool, .{ .ty = .{ .bool = {} } });
    try testRoundtrip(&pool, .{ .ty = .{ .int = {} } });
}

test "bool intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .bool, .val = .{ .bool = true } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.true });
    try testIndex(&pool, .{ .tv = .{ .ty = .bool, .val = .{ .bool = true } } }, .true);
    try testIndex(&pool, .{ .tv = TypedValue.common.true }, .true);

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .bool, .val = .{ .bool = false } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.false });
    try testIndex(&pool, .{ .tv = .{ .ty = .bool, .val = .{ .bool = false } } }, .false);
    try testIndex(&pool, .{ .tv = TypedValue.common.false }, .false);
}

test "int intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = 0 } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.izero });
    try testIndex(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = 0 } } }, .izero);
    try testIndex(&pool, .{ .tv = TypedValue.common.izero }, .izero);

    try testRoundtrip(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = 1 } } });
    try testRoundtrip(&pool, .{ .tv = TypedValue.common.ione });
    try testIndex(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = 1 } } }, .ione);
    try testIndex(&pool, .{ .tv = TypedValue.common.ione }, .ione);

    var rng = std.Random.Xoshiro256.init(0);
    var dedup = std.ArrayList(struct { val: u64, index: Index }).init(std.testing.allocator);
    defer dedup.deinit();

    outer: for (0..100) |_| {
        const val = rng.next();
        for (dedup.items) |item| if (item.val == val) continue :outer;

        try testRoundtrip(&pool, .{ .tv = .{ .ty = .int, .val = .{ .int = @bitCast(val) } } });
        const index = try pool.put(.{ .tv = .{ .ty = .int, .val = .{ .int = @bitCast(val) } } });
        for (dedup.items) |item| try std.testing.expect(index != item.index);
    }
}

test "string intern" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // add three distinct strings
    const apple = try pool.put(.{ .str = "apple" });
    const banana = try pool.put(.{ .str = "banana" });
    const cherry = try pool.put(.{ .str = "cherry" });
    try std.testing.expect(std.mem.eql(u8, "apple", pool.get(apple).str));
    try std.testing.expect(std.mem.eql(u8, "banana", pool.get(banana).str));
    try std.testing.expect(std.mem.eql(u8, "cherry", pool.get(cherry).str));

    // adding the same string again should return the original id
    try std.testing.expectEqual(apple, try pool.put(.{ .str = "apple" }));

    // partly overlapping string is a unique string
    const apfel = try pool.put(.{ .str = "apfel" });
    try std.testing.expect(std.mem.eql(u8, "apfel", pool.get(apfel).str));
    try std.testing.expect(apple != apfel);
    try std.testing.expect(!std.mem.eql(u8, "apple", pool.get(apfel).str));

    // existing strings should not be modified
    try std.testing.expect(std.mem.eql(u8, "apple", pool.get(apple).str));
    try std.testing.expect(std.mem.eql(u8, "banana", pool.get(banana).str));
    try std.testing.expect(std.mem.eql(u8, "cherry", pool.get(cherry).str));
}
