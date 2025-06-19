const std = @import("std");
const Air = @import("Air.zig");
const InternPool = @import("InternPool.zig");

const Allocator = std.mem.Allocator;
const GenericWriter = std.io.GenericWriter;

const Node = Air.Node;
const Index = Air.Index;
const Value = Air.Value;

pub fn generate(gpa: Allocator, writer: anytype, pool: *const InternPool, air: *const Air) !void {
    var arena: std.heap.ArenaAllocator = .init(gpa);
    defer arena.deinit();

    var codegen: CodeGen(@TypeOf(writer)) = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .writer = writer,
        .pool = pool,
        .air = air,
        .scratch = .empty,
        .bytes = .empty,
    };

    try codegen.block(air.body);
}

fn CodeGen(WriterType: type) type {
    return struct {
        gpa: Allocator,
        arena: Allocator,
        writer: WriterType,
        air: *const Air,
        pool: *const InternPool,
        scratch: std.ArrayListUnmanaged(u32),
        bytes: std.ArrayListUnmanaged(u8),

        const Self = @This();
        const Error = WriterType.Error || Allocator.Error;

        pub fn block(self: *Self, index: Index) !void {
            const data = self.air.get(index).block;
            const slice = self.air.indices(data);
            for (slice) |idx| {
                try self.statement(idx);
                try self.writer.print("\n", .{});
            }
        }

        fn statement(self: *Self, index: Index) !void {
            const tag = self.air.tag(index);
            switch (tag) {
                .null => unreachable,
                .def => try self.def(index),
                .yield => try self.yield(index),
                // TODO: unimplemented
                else => unreachable,
            }
        }

        fn def(self: *Self, index: Index) !void {
            const data = self.air.get(index).def;
            const signal = self.air.extraData(data.signal, Node.Signal);
            const str = self.pool.get(signal.name).str;

            try self.writer.print("wire {s} = ", .{str});

            // when evaluating the expression and target, the rendered
            // strings are written to bytes, and their extents are recorded
            // in the scratch buffer. expressions that process aggregates
            // add multiple entries to the scratch buffer, rest are blind
            const scratch_top = self.scratch.items.len;
            defer self.scratch.shrinkRetainingCapacity(scratch_top);
            defer self.bytes.clearRetainingCapacity();

            const values = values: {
                const top = self.scratch.items.len;
                try self.scratch.append(self.arena, @intCast(self.bytes.items.len));
                try self.expression(data.value);
                try self.scratch.append(self.arena, @intCast(self.bytes.items.len));
                break :values self.scratch.items[top..];
            };

            for (0..values.len - 1) |i| {
                const start = values[i];
                const end = values[i + 1];
                try self.writer.print("{s}", .{self.bytes.items[start..end]});
            }
            try self.writer.print(";", .{});
        }

        fn yield(self: *Self, index: Index) !void {
            const data = self.air.get(index).yield;

            // FIXME: this is such a hack
            try self.writer.print("assign out = ", .{});
            try self.expression(data);
            try self.writer.print(";", .{});
        }

        fn @"type"(self: *Self, ip: InternPool.Index) Error!void {
            const ty = self.pool.get(ip).ty;
            switch (ty) {
                .bits, .uint => |width| try self.bytes.appendSlice(self.arena, width),
                .uint => try self.bytes.appendSlice(self.arena, "uint"),
                .bits => try self.bytes.appendSlice(self.arena, "bits"),
                .bool => try self.bytes.appendSlice(self.arena, "bool"),
                .bundle => try self.bytes.appendSlice(self.arena, "bundle"),
                else => unreachable,
            }
        }

        fn expression(self: *Self, value: Value) Error!void {
            // TODO: implement interned values
            std.debug.assert(value.tag == .index);
            const index = value.payload.index;

            const tag = self.air.tag(index);
            switch (self.air.get(index)) {
                .null => unreachable,
                .ident => try self.ident(index),
                .bundle_literal => try self.bundleLiteral(index),
                .ineg => try self.ineg(index),
                .bnot => try self.bnot(index),
                .lnot => try self.lnot(index),
                .paren => try self.paren(index),
                .band => try self.band(index),
                .bor => try self.bor(index),
                .bxor => try self.bxor(index),
                .iadd,
                .isub,
                .land,
                .lor,
                => |pl| try self.binary(tag, pl),
                .lxor,
                .def,
                .decl,
                .yield,
                .block,
                .toplevel,
                => unreachable,
                // TODO: unimplemented
                else => unreachable,
            }
        }

        fn ident(self: *Self, index: Index) !void {
            const data = self.air.get(index).ident;
            const str = self.pool.get(data.name).str;
            try self.bytes.appendSlice(self.arena, str);
        }

        fn bundleLiteral(self: *Self, index: Index) !void {
            const ty = self.air.typeOf(Value.index(index));
            std.debug.assert(self.pool.get(ty).ty == .bundle);

            const data = self.air.get(index).bundle_literal;
            const inits = self.air.values(data.inits);
            for (inits, 0..) |init, i| {
                try self.expression(init);
                if (i < inits.len - 1) try self.scratch.append(self.arena, @intCast(self.bytes.items.len));
            }
        }

        fn ineg(self: *Self, index: Index) !void {
            const ty = self.air.typeOf(Value.index(index));
            std.debug.assert(self.pool.get(ty).ty == .sint);

            const data = self.air.get(index).ineg;
            try self.bytes.appendSlice(self.arena, "-");
            try self.expression(data);
        }

        fn bnot(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.air.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .sint) or (ty == .uint) or (ty == .bits));

            const data = self.air.get(index).bnot;
            try self.bytes.appendSlice(self.arena, "~");
            try self.expression(data);
        }

        fn lnot(self: *Self, index: Index) !void {
            const ty = self.air.typeOf(Value.index(index));
            std.debug.assert(self.pool.get(ty).ty == .bool);

            const data = self.air.get(index).lnot;
            try self.bytes.appendSlice(self.arena, "!");
            try self.expression(data);
        }

        fn paren(self: *Self, index: Index) !void {
            const data = self.air.get(index).paren;
            try self.bytes.appendSlice(self.arena, "(");
            try self.expression(data);
            try self.bytes.appendSlice(self.arena, ")");
        }

        fn band(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.air.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .sint) or (ty == .uint) or (ty == .bits));

            const data = self.air.get(index).band;
            try self.expression(data.l);
            try self.bytes.appendSlice(self.arena, " & ");
            try self.expression(data.r);
        }

        fn bor(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.air.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .sint) or (ty == .uint) or (ty == .bits));

            const data = self.air.get(index).bor;
            try self.expression(data.l);
            try self.bytes.appendSlice(self.arena, " | ");
            try self.expression(data.r);
        }

        fn bxor(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.air.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .sint) or (ty == .uint) or (ty == .bits));

            const data = self.air.get(index).bxor;
            try self.expression(data.l);
            try self.bytes.appendSlice(self.arena, " ^ ");
            try self.expression(data.r);
        }

        fn binary(self: *Self, tag: Node.Tag, data: Node.Binary) !void {
            // FIXME: split this up so we can assert types
            const op = switch (tag) {
                .iadd => "+",
                .isub => "-",
                // NOTE: this can also be the logical operators &&, ||, and !=
                // which is just a stylistic choice that should be exposed
                .land => "&",
                .lor => "|",
                .lxor => "^",
                else => unreachable,
            };

            try self.writer.print(" {s} ", .{op});
            try self.expression(data.r);
        }

        // NOTE: this is not meant to be used in release builds
        fn unimplemented() noreturn {
            std.debug.print("unimplemented!\n", .{});
            unreachable;
        }
    };
}
