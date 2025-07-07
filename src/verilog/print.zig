const std = @import("std");
const Mir = @import("Mir.zig");
const InternPool = @import("../InternPool.zig");
const indenting_writer = @import("../indenting_writer.zig");

const Allocator = std.mem.Allocator;
const GenericWriter = std.io.GenericWriter;

const Node = Mir.Node;
const Index = Mir.Index;
const IndentingWriter = indenting_writer.IndentingWriter;
const indentingWriter = indenting_writer.indentingWriter;

pub fn Formatter(WriterType: type) type {
    return struct {
        writer: IndentingWriter(4, WriterType),
        pool: *const InternPool,
        mir: *const Mir,

        const Self = @This();
        const Error = WriterType.Error || Allocator.Error;

        pub fn init(writer: anytype, pool: *const InternPool, mir: *const Mir) Self {
            return .{
                .writer = indentingWriter(4, writer),
                .pool = pool,
                .mir = mir,
            };
        }

        pub fn format(self: *Self, node: Mir.Index) Error!void {
            switch (self.mir.tag(node)) {
                .null => unreachable,
                .ident => try self.mirIdent(node),
                .unsized_literal,
                .bin_literal,
                .oct_literal,
                .dec_literal,
                .hex_literal,
                => try self.mirLiteral(node),
                .net_wire_unsigned,
                .net_wire_signed,
                .net_reg_unsigned,
                .net_reg_signed,
                => try self.mirNetType(node),
                .unary_ineg,
                .unary_bnot,
                .unary_lnot,
                => try self.mirUnary(node),
                .paren => try self.mirParen(node),
                .bin_iadd,
                .bin_isub,
                .bin_bor,
                .bin_band,
                .bin_bxor,
                .bin_lor,
                .bin_land,
                .bin_eq,
                .bin_ne,
                => try self.mirBinary(node),
                .bit_select => try self.mirBitSelect(node),
                .part_select => try self.mirPartSelect(node),
                .net => try self.mirNet(node),
                .net_decl => unimplemented(), // try self.mirNetDecl(node),
                .net_def => try self.mirNetDef(node),
                .net_assign => try self.mirNetAssign(node),
                .port_input,
                .port_output,
                => try self.mirPort(node),
                .block => try self.mirBlock(node),
                .module => try self.mirModule(node),
            }
        }

        fn mirIdent(self: *Self, node: Mir.Index) !void {
            const ip = self.mir.payload(node).ip;
            const str = self.pool.get(ip).str;
            try self.writer.print("{s}", .{str});
        }

        fn mirLiteral(self: *Self, node: Mir.Index) !void {
            const tag = self.mir.tag(node);
            const ip = self.mir.payload(node).ip;
            const tv = self.pool.get(ip).tv;
            const ty = self.pool.get(tv.ty).ty;
            const size = switch (ty) {
                .int => 0,
                .bits, .uint, .sint => |size| size,
                else => unreachable,
            };

            switch (tag) {
                .unsized_literal => try self.writer.print("{d}", .{tv.val.int}),
                .bin_literal => try self.writer.print("{}'b{b}", .{ size, tv.val.int }),
                .oct_literal => try self.writer.print("{}'o{o}", .{ size, tv.val.int }),
                .dec_literal => try self.writer.print("{}'d{d}", .{ size, tv.val.int }),
                .hex_literal => try self.writer.print("{}'x{x}", .{ size, tv.val.int }),
                else => unreachable,
            }
        }

        fn mirNetType(self: *Self, node: Mir.Index) !void {
            const tag = self.mir.tag(node);
            switch (tag) {
                .net_wire_unsigned => try self.writer.print("wire", .{}),
                .net_wire_signed => try self.writer.print("wire signed", .{}),
                .net_reg_unsigned => try self.writer.print("reg", .{}),
                .net_reg_signed => try self.writer.print("reg signed", .{}),
                else => unreachable,
            }

            const size = self.mir.payload(node).size;
            if (size > 1) {
                try self.writer.print(" [{}:0]", .{size - 1});
            }
        }

        fn mirUnary(self: *Self, node: Mir.Index) !void {
            const tag = self.mir.tag(node);
            switch (tag) {
                .unary_ineg => try self.writer.print("-", .{}),
                .unary_bnot => try self.writer.print("~", .{}),
                .unary_lnot => try self.writer.print("!", .{}),
                else => unreachable,
            }

            const unary = self.mir.payload(node).unary;
            try self.format(unary);
        }

        fn mirParen(self: *Self, node: Mir.Index) !void {
            const unary = self.mir.payload(node).unary;
            try self.writer.print("(", .{});
            try self.format(unary);
            try self.writer.print(")", .{});
        }

        fn mirBinary(self: *Self, node: Mir.Index) !void {
            const tag = self.mir.tag(node);
            const binary = self.mir.payload(node).binary;

            try self.format(binary.l);
            switch (tag) {
                .bin_iadd => try self.writer.print(" + ", .{}),
                .bin_isub => try self.writer.print(" - ", .{}),
                .bin_bor => try self.writer.print(" | ", .{}),
                .bin_band => try self.writer.print(" & ", .{}),
                .bin_bxor => try self.writer.print(" ^ ", .{}),
                .bin_lor => try self.writer.print(" || ", .{}),
                .bin_land => try self.writer.print(" && ", .{}),
                .bin_eq => try self.writer.print(" == ", .{}),
                .bin_ne => try self.writer.print(" != ", .{}),
                else => unreachable,
            }
            try self.format(binary.r);
        }

        fn mirBitSelect(self: *Self, node: Mir.Index) !void {
            const binary = self.mir.payload(node).binary;

            try self.format(binary.l);
            try self.writer.print("[", .{});
            try self.format(binary.r);
            try self.writer.print("]", .{});
        }

        fn mirPartSelect(self: *Self, node: Mir.Index) !void {
            const op_extra = self.mir.payload(node).op_extra;
            const range = self.mir.extraData(op_extra.extra, Mir.Node.Range);

            try self.format(op_extra.op);
            try self.writer.print("[", .{});
            try self.format(range.upper);
            try self.writer.print(":", .{});
            try self.format(range.lower);
            try self.writer.print("]", .{});
        }

        fn mirNet(self: *Self, node: Mir.Index) !void {
            const binary = self.mir.payload(node).binary;
            try self.format(binary.l);
            try self.writer.print(" ", .{});
            try self.format(binary.r);
        }

        fn mirNetDef(self: *Self, node: Mir.Index) !void {
            const binary = self.mir.payload(node).binary;
            try self.format(binary.l);
            try self.writer.print(" = ", .{});
            try self.format(binary.r);
            try self.writer.print(";", .{});
            try self.writer.newline();
        }

        fn mirNetAssign(self: *Self, node: Mir.Index) !void {
            const binary = self.mir.payload(node).binary;
            try self.writer.print("assign ", .{});
            try self.format(binary.l);
            try self.writer.print(" = ", .{});
            try self.format(binary.r);
            try self.writer.print(";", .{});
            try self.writer.newline();
        }

        fn mirPort(self: *Self, node: Mir.Index) !void {
            const tag = self.mir.tag(node);
            switch (tag) {
                .port_input => try self.writer.print("input ", .{}),
                .port_output => try self.writer.print("output ", .{}),
                else => unreachable,
            }

            const unary = self.mir.payload(node).unary;
            try self.format(unary);
        }

        fn mirBlock(self: *Self, block_node: Mir.Index) !void {
            const indices = self.mir.payload(block_node).indices;
            const nodes = self.mir.indices(indices);
            for (nodes) |node| {
                try self.format(node);
            }
        }

        fn mirModule(self: *Self, node: Mir.Index) !void {
            const extra = self.mir.payload(node).extra;
            const module = self.mir.extraData(extra, Mir.Node.Module);

            // Format the preamble - module header and ports.
            const name = self.pool.get(module.name).str;
            const ports = self.mir.indices(module.ports);

            try self.writer.print("module {s} (", .{name});
            self.writer.indent();
            try self.writer.newline();
            for (ports, 0..) |port, i| {
                try self.format(port);
                if (i < ports.len - 1) try self.writer.print(",", .{});
                try self.writer.newline();
            }
            self.writer.dedent();
            try self.writer.print(");", .{});
            try self.writer.newline();

            // Recursively print the block, without `begin/end` keywords.
            self.writer.indent();
            try self.mirBlock(module.body);
            self.writer.dedent();

            // Format the postamble - just the `endmodule`
            try self.writer.print("endmodule", .{});
            try self.writer.newline();
        }

        // NOTE: this is not meant to be used in release builds
        fn unimplemented() noreturn {
            std.debug.print("unimplemented!\n", .{});
            unreachable;
        }
    };
}
