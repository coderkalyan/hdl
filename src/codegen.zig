const std = @import("std");
const Air = @import("Air.zig");
const InternPool = @import("InternPool.zig");

const Allocator = std.mem.Allocator;
const GenericWriter = std.io.GenericWriter;

const Node = Air.Node;
const Index = Air.Index;
const Value = Air.Value;

pub fn generate(gpa: Allocator, writer: anytype, pool: *const InternPool, air: *const Air) !void {
    var codegen: CodeGen(@TypeOf(writer)) = .{
        .gpa = gpa,
        .writer = writer,
        .pool = pool,
        .air = air,
    };

    try codegen.block(air.body);
}

fn CodeGen(WriterType: type) type {
    return struct {
        gpa: Allocator,
        writer: WriterType,
        pool: *const InternPool,
        air: *const Air,

        const Self = @This();

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

        fn expression(self: *Self, value: Value) WriterType.Error!void {
            // TODO: implement interned values
            std.debug.assert(value.tag == .index);
            const index = value.payload.index;
            const data = self.air.get(index);
            switch (data) {
                .null => unreachable,
                .ident => try self.ident(index),
                .iadd,
                .isub,
                .band,
                .bor,
                .bxor,
                .land,
                .lor,
                => |pl| try self.binary(self.air.tag(index), pl),
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
            const str = self.pool.get(data).str;
            try self.writer.print("{s}", .{str});
        }

        fn binary(self: *Self, tag: Node.Tag, data: Node.Binary) !void {
            const op = switch (tag) {
                .iadd => "+",
                .isub => "-",
                .band => "&",
                .bor => "|",
                .bxor => "^",
                // NOTE: this can also be the logical operators &&, ||, and !=
                // which is just a stylistic choice that should be exposed
                .land => "&",
                .lor => "|",
                .lxor => "^",
                else => unreachable,
            };

            try self.expression(data.l);
            try self.writer.print(" {s} ", .{op});
            try self.expression(data.r);
        }

        fn def(self: *Self, index: Index) !void {
            const data = self.air.get(index).def;
            const signal = self.air.extraData(data.signal, Node.Signal);
            const str = self.pool.get(signal.name).str;

            try self.writer.print("wire {s} = ", .{str});
            try self.expression(data.value);
            try self.writer.print(";", .{});
        }

        fn yield(self: *Self, index: Index) !void {
            const data = self.air.get(index).yield;

            // FIXME: this is such a hack
            try self.writer.print("assign out = ", .{});
            try self.expression(data);
            try self.writer.print(";", .{});
        }
    };
}
