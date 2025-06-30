const std = @import("std");
const Air = @import("Air.zig");
const InternPool = @import("InternPool.zig");
const Type = @import("type.zig").Type;
const indenting_writer = @import("indenting_writer.zig");

const Allocator = std.mem.Allocator;
const GenericWriter = std.io.GenericWriter;

const Node = Air.Node;
const Index = Air.Index;
const Value = Air.Value;
const IndentingWriter = indenting_writer.IndentingWriter;
const indentingWriter = indenting_writer.indentingWriter;

pub fn generate(gpa: Allocator, writer: anytype, pool: *const InternPool, module: Type.Module) !void {
    var arena: std.heap.ArenaAllocator = .init(gpa);
    defer arena.deinit();

    const air = pool.airPtrConst(module.air);
    var codegen: CodeGen(@TypeOf(writer)) = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .writer = indentingWriter(4, writer),
        .pool = pool,
        .module = module,
        .air = air,
        .scratch = .empty,
        .bytes = .empty,
    };

    try codegen.preamble();
    try codegen.block(air.body);
    try codegen.postamble();
}

fn CodeGen(WriterType: type) type {
    return struct {
        gpa: Allocator,
        arena: Allocator,
        writer: IndentingWriter(4, WriterType),
        module: Type.Module,
        air: *const Air,
        pool: *const InternPool,
        scratch: std.ArrayListUnmanaged(u32),
        bytes: std.ArrayListUnmanaged(u8),

        const Self = @This();
        const Error = WriterType.Error || Allocator.Error;

        const ExprIterator = struct {
            arena: Allocator,
            air: *const Air,
            stack: std.ArrayListUnmanaged(Frame),

            pub const Frame = struct {
                value: Value,
                ptr: u32,
            };

            pub fn init(arena: Allocator, air: *const Air, root: Value) !ExprIterator {
                var stack: std.ArrayListUnmanaged(Frame) = .empty;
                try stack.append(arena, .{
                    .value = root,
                    .ptr = 0,
                });

                return .{
                    .arena = arena,
                    .air = air,
                    .stack = stack,
                };
            }

            pub fn next(self: *ExprIterator) !?Value {
                const stack = self.stack.items;
                if (stack.len == 0) return null;

                const frame = &stack[stack.len - 1];
                if (frame.value.tag == .ip) {
                    // FIXME: this will completely break for interned
                    // bundle literals when those are added
                    if (frame.ptr == 0) {
                        frame.ptr += 1;
                        return frame.value;
                    } else {
                        return null;
                    }
                }

                const index = frame.value.payload.index;
                const items = switch (self.air.get(index)) {
                    .bundle_literal => |bundle| self.air.values(bundle.inits),
                    else => &.{frame.value},
                };

                if (frame.ptr < items.len) {
                    const item = items[frame.ptr];
                    frame.ptr += 1;
                    return item;
                } else {
                    _ = self.stack.pop();
                    return self.next();
                }
            }
        };

        const TargetIterator = struct {
            cg: *Super,
            // base: []const u8,
            stack: std.ArrayListUnmanaged(Frame),

            // NOTE: The naming is very confusing but Self is not
            // defined here and the parent defines it to itself.
            const Super = Self;

            const Frame = struct {
                type: InternPool.Index,
                name: InternPool.Index,
                ptr: u32,
            };

            const Target = struct {
                name: []const u8,
                type: InternPool.Index,
            };

            pub fn init(cg: *Super, base: InternPool.Index, root: InternPool.Index) !TargetIterator {
                var stack: std.ArrayListUnmanaged(Frame) = .empty;
                try stack.append(cg.arena, .{
                    .type = root,
                    .name = base,
                    .ptr = 0,
                });

                return .{
                    .cg = cg,
                    // .base = base,
                    .stack = stack,
                };
            }

            pub fn next(self: *TargetIterator) !?Target {
                const stack = self.stack.items;
                if (stack.len == 0) return null;

                const bytes = &self.cg.bytes;
                const bytes_top = bytes.items.len;

                for (stack, 0..) |*frame, i| {
                    const name = self.cg.pool.get(frame.name).str;
                    try bytes.ensureUnusedCapacity(self.cg.arena, name.len + 1);
                    try bytes.appendSlice(self.cg.arena, name);
                    // bytes.appendSliceAssumeCapacity(name);
                    // if (i < stack.len - 1) bytes.appendAssumeCapacity('_');
                    if (i < stack.len - 1) try bytes.append(self.cg.arena, '_');
                }

                const frame = &stack[stack.len - 1];
                const ty = self.cg.pool.get(frame.type).ty;
                switch (ty) {
                    .bundle => |bundle| {
                        std.debug.assert(bundle.field_names.len == bundle.field_types.len);
                        if (frame.ptr < bundle.field_names.len) {
                            const name = bundle.field_names[frame.ptr];
                            const frame_ty = bundle.field_types[frame.ptr];
                            frame.ptr += 1;

                            const str = self.cg.pool.get(name).str;
                            try bytes.ensureUnusedCapacity(self.cg.arena, str.len + 1);
                            // bytes.appendAssumeCapacity('_');
                            // bytes.appendSliceAssumeCapacity(str);
                            try bytes.append(self.cg.arena, '_');
                            try bytes.appendSlice(self.cg.arena, str);

                            return .{
                                .name = bytes.items[bytes_top..],
                                .type = frame_ty,
                            };
                        } else {
                            _ = self.stack.pop();
                            return self.next();
                        }
                    },
                    .module => unreachable,
                    else => {
                        // std.debug.print("name: '{s}'\n", .{bytes.items[bytes_top..]});
                        _ = self.stack.pop();
                        return .{
                            .name = bytes.items[bytes_top..],
                            .type = frame.type,
                        };
                    },
                }
            }
        };

        fn typeOf(self: *Self, value: Value) InternPool.Index {
            return self.air.typeOf(self.pool, value);
        }

        pub fn block(self: *Self, index: Index) !void {
            const data = self.air.get(index).block;
            const slice = self.air.indices(data);
            for (slice) |idx| {
                try self.statement(idx);
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
            std.debug.assert(signal.type == self.typeOf(data.value));

            var targets = try self.iterateTarget(signal.name, signal.type);
            var exprs = try self.iterateExpression(data.value);
            while (true) {
                const target = try targets.next();
                const value = try exprs.next();
                if (target == null) {
                    std.debug.assert(value == null);
                    break;
                }

                // std.debug.print("target: '{s}'\n", .{target.?.name});
                const datatype_tmp = try self.type(target.?.type);
                const datatype = try self.arena.alloc(u8, datatype_tmp.len);
                @memcpy(datatype, datatype_tmp);
                const name_tmp = target.?.name;
                const name = try self.arena.alloc(u8, name_tmp.len);
                @memcpy(name, name_tmp);
                const expr_tmp = try self.formatExpression(value.?);
                const expr = try self.arena.alloc(u8, expr_tmp.len);
                @memcpy(expr, expr_tmp);
                try self.writer.print("wire", .{});
                if (datatype.len > 0) {
                    try self.writer.print(" {s}", .{datatype});
                }
                try self.writer.print(" {s}", .{name});
                if (signal.salt > 0) {
                    try self.writer.print("${}", .{signal.salt});
                }
                try self.writer.print(" = {s};", .{expr});
                try self.writer.newline();
            }
            // std.debug.print("\n", .{});
        }

        fn yield(self: *Self, index: Index) !void {
            const data = self.air.get(index).yield;
            const ty = self.typeOf(data);
            var targets = try self.iterateTarget(.builtin_out, ty);
            var exprs = try self.iterateExpression(data);
            while (true) {
                const target = try targets.next();
                const value = try exprs.next();
                if (target == null) {
                    std.debug.assert(value == null);
                    break;
                }

                const name = target.?.name;
                const expr = try self.formatExpression(value.?);
                try self.writer.print("assign {s} = {s};\n", .{ name, expr });
            }
        }

        fn @"type"(self: *Self, ip: InternPool.Index) Error![]const u8 {
            const ty = self.pool.get(ip).ty;
            switch (ty) {
                .bits, .uint, .sint => |width| {
                    if (width == 1) return "";
                    const bytes_top = self.bytes.items.len;
                    const writer = self.bytes.writer(self.arena);
                    try writer.print("[{}:0]", .{width - 1});
                    return self.bytes.items[bytes_top..];
                },
                .bool => return "",
                else => unreachable,
            }
        }

        fn iterateExpression(self: *Self, root: Value) !ExprIterator {
            return .init(self.arena, self.air, root);
        }

        fn iterateTarget(self: *Self, base: InternPool.Index, root: InternPool.Index) !TargetIterator {
            return .init(self, base, root);
        }

        fn formatExpression(self: *Self, value: Value) Error![]const u8 {
            const start = self.bytes.items.len;
            try self.expression(value);
            return self.bytes.items[start..];
        }

        fn expression(self: *Self, value: Value) Error!void {
            return switch (value.tag) {
                .index => self.expressionIndex(value.payload.index),
                .ip => self.expressionInterned(value.payload.ip),
            };
        }

        fn expressionIndex(self: *Self, index: Index) Error!void {
            const tag = self.air.tag(index);
            switch (tag) {
                .null => unreachable,
                .ident => try self.ident(index),
                // do not call expression() directly on aggregate
                // types, use iterate() first to flatten the tree
                .bundle_literal => unreachable,
                .ineg => try self.ineg(index),
                .bnot => try self.bnot(index),
                .lnot => try self.lnot(index),
                .paren => try self.paren(index),
                .band => try self.band(index),
                .bor => try self.bor(index),
                .bxor => try self.bxor(index),
                .iadd => try self.iadd(index),
                .isub => try self.isub(index),
                .land => try self.land(index),
                .lor => try self.lor(index),
                .lxor => try self.lxor(index),
                .limplies => try self.limplies(index),
                .bit => try self.bit(index),
                .bitslice => try self.bitslice(index),
                .def,
                .decl,
                .yield,
                .block,
                .toplevel,
                => unreachable,
                else => unimplemented(),
            }
        }

        fn expressionInterned(self: *Self, index: InternPool.Index) !void {
            const tv = self.pool.get(index).tv;
            const ty = self.pool.get(tv.ty).ty;
            switch (ty) {
                .bool => try self.bool(index),
                .uint, .sint, .int => try self.int(index),
                else => unimplemented(),
            }
        }

        fn @"bool"(self: *Self, index: InternPool.Index) !void {
            const tv = self.pool.get(index).tv;
            std.debug.assert(tv.ty == .bool);

            switch (tv.val.bool) {
                true => try self.bytes.appendSlice(self.arena, "1'b1"),
                false => try self.bytes.appendSlice(self.arena, "1'b0"),
            }
        }

        fn int(self: *Self, index: InternPool.Index) !void {
            const tv = self.pool.get(index).tv;
            const ty = self.pool.get(tv.ty).ty;

            const writer = self.bytes.writer(self.arena);
            switch (ty) {
                .int => try writer.print("{d}", .{tv.val.int}),
                // TODO: choose the base for the format intelligently
                .uint => try writer.print("{}'b{b}", .{ ty.uint, tv.val.int }),
                else => unreachable,
            }
        }

        fn ident(self: *Self, index: Index) !void {
            const data = self.air.get(index).ident;
            const name, const salt = switch (self.air.get(data.signal)) {
                .def => |pl| name: {
                    const extra = self.air.extraData(pl.signal, Node.Signal);
                    break :name .{ extra.name, extra.salt };
                },
                .decl => |signal| name: {
                    const extra = self.air.extraData(signal, Node.Signal);
                    break :name .{ extra.name, extra.salt };
                },
                .param => |param| name: {
                    const signature = self.pool.get(self.module.signature).ty.signature;
                    break :name .{ signature.input_names[param.index], 0 };
                },
                else => unreachable,
            };

            const str = self.pool.get(name).str;
            try self.bytes.appendSlice(self.arena, str);
            if (salt > 0) {
                try self.bytes.writer(self.arena).print("${}", .{salt});
            }
        }

        fn bundleLiteral(self: *Self, index: Index) !void {
            const ty = self.typeOf(Value.index(index));
            std.debug.assert(self.pool.get(ty).ty == .bundle);

            const data = self.air.get(index).bundle_literal;
            const inits = self.air.values(data.inits);
            for (inits, 0..) |init, i| {
                try self.expression(init);
                if (i < inits.len - 1) try self.scratch.append(self.arena, @intCast(self.bytes.items.len));
            }
        }

        fn ineg(self: *Self, index: Index) !void {
            const ty = self.typeOf(Value.index(index));
            std.debug.assert(self.pool.get(ty).ty == .sint);

            const data = self.air.get(index).ineg;
            try self.bytes.appendSlice(self.arena, "-");
            try self.expression(data);
        }

        fn bnot(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .sint) or (ty == .uint) or (ty == .bits));

            const data = self.air.get(index).bnot;
            try self.bytes.appendSlice(self.arena, "~");
            try self.expression(data);
        }

        fn lnot(self: *Self, index: Index) !void {
            const ty = self.typeOf(Value.index(index));
            std.debug.assert(self.pool.get(ty).ty == .bool);

            const data = self.air.get(index).lnot;
            try self.bytes.appendSlice(self.arena, "~");
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
                const ip = self.typeOf(Value.index(index));
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
                const ip = self.typeOf(Value.index(index));
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
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .sint) or (ty == .uint) or (ty == .bits));

            const data = self.air.get(index).bxor;
            try self.expression(data.l);
            try self.bytes.appendSlice(self.arena, " ^ ");
            try self.expression(data.r);
        }

        fn land(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert(ty == .bool);

            const data = self.air.get(index).land;
            try self.expression(data.l);
            try self.bytes.appendSlice(self.arena, " & ");
            try self.expression(data.r);
        }

        fn lor(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert(ty == .bool);

            const data = self.air.get(index).lor;
            try self.expression(data.l);
            try self.bytes.appendSlice(self.arena, " | ");
            try self.expression(data.r);
        }

        fn lxor(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert(ty == .bool);

            const data = self.air.get(index).lxor;
            try self.expression(data.l);
            try self.bytes.appendSlice(self.arena, " ^ ");
            try self.expression(data.r);
        }

        fn limplies(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert(ty == .bool);

            const data = self.air.get(index).limplies;
            try self.bytes.appendSlice(self.arena, "(~");
            try self.expression(data.l);
            try self.bytes.appendSlice(self.arena, " | ");
            try self.expression(data.r);
            try self.bytes.appendSlice(self.arena, ")");
        }

        fn iadd(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .sint) or (ty == .uint));

            const data = self.air.get(index).iadd;
            const bin = self.air.extraData(data.bin, Node.Binary);
            try self.expression(bin.l);
            try self.bytes.appendSlice(self.arena, " + ");
            try self.expression(bin.r);
        }

        fn isub(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .sint) or (ty == .uint));

            const data = self.air.get(index).isub;
            const bin = self.air.extraData(data.bin, Node.Binary);
            try self.expression(bin.l);
            try self.bytes.appendSlice(self.arena, " - ");
            try self.expression(bin.r);
        }

        fn bit(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .bits) or (ty == .uint) or (ty == .sint));

            const data = self.air.get(index).bit;
            try self.expression(data.operand);
            try self.bytes.appendSlice(self.arena, "[");
            try self.expression(data.index);
            try self.bytes.appendSlice(self.arena, "]");
        }

        fn bitslice(self: *Self, index: Index) !void {
            const ty = type: {
                const ip = self.typeOf(Value.index(index));
                break :type self.pool.get(ip).ty;
            };
            std.debug.assert((ty == .bits) or (ty == .uint) or (ty == .sint));

            const data = self.air.get(index).bitslice;
            const extra = self.air.extraData(data.bitslice, Node.BitSlice);
            try self.expression(extra.operand);
            const upper = self.pool.get(extra.upper).tv.val.int;
            const lower = self.pool.get(extra.lower).tv.val.int;
            try self.bytes.writer(self.arena).print("[{}:{}]", .{ upper - 1, lower });
        }

        fn preamble(self: *Self) !void {
            const module_name = self.pool.get(self.module.name).str;
            const signature = self.pool.get(self.module.signature).ty.signature;

            try self.writer.print("module {s} (", .{module_name});
            self.writer.indent();
            try self.writer.newline();
            for (signature.input_names, signature.input_types) |name, ty| {
                var targets = try self.iterateTarget(name, ty);
                while (try targets.next()) |target| {
                    const ts = try self.type(target.type);
                    try self.writer.print("input wire", .{});
                    if (ts.len > 0) try self.writer.print(" {s}", .{ts});
                    try self.writer.print(" {s}", .{target.name});
                    try self.writer.print(",", .{});
                    try self.writer.newline();
                }
            }
            var targets = try self.iterateTarget(.builtin_out, signature.output_type);
            var first = true;
            while (try targets.next()) |target| {
                if (!first) {
                    try self.writer.print(",", .{});
                    try self.writer.newline();
                }
                first = false;
                const ts = try self.type(target.type);
                try self.writer.print("output wire", .{});
                if (ts.len > 0) try self.writer.print(" {s}", .{ts});
                try self.writer.print(" {s}", .{target.name});
            }

            self.writer.dedent();
            try self.writer.newline();
            try self.writer.print(");", .{});
            self.writer.indent();
            try self.writer.newline();
        }

        fn postamble(self: *Self) !void {
            self.writer.dedent();
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
