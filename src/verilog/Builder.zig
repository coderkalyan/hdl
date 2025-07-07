const std = @import("std");
const Air = @import("../Air.zig");
const Mir = @import("Mir.zig");
const InternPool = @import("../InternPool.zig");
const Type = @import("../type.zig").Type;
const TypedValue = @import("../value.zig").TypedValue;

const Allocator = std.mem.Allocator;
const Builder = @This();

gpa: Allocator,
arena: Allocator,
// The intern pool is used to look up strings, types, and literals,
// and to intern new identifiers.
pool: *InternPool,
/// Read only copy of the Air being translated.
air: *const Air,
// FIXME: why do we need this?
module: Type.Module,
/// List of Mir nodes, which are stored flat and referenced by index.
nodes: std.MultiArrayList(Mir.Node),
/// Extra data for nodes that don't fit into the node struct.
extra: std.ArrayListUnmanaged(u32),
/// Scratch data for constructing expressions.
scratch: std.ArrayListUnmanaged(u32),
/// Scratch data for constructing identifiers and strings.
bytes: std.ArrayListUnmanaged(u8),

const Context = struct {
    next: ?*Context,
    payload: Payload,

    const Payload = union(enum) {
        /// Toplevel terminator expression.
        head,
        /// Elaborate a single field in a bundle by index.
        field: u32,
        // Elaborate a single element in an array by index.
        element: u32,
    };

    pub const head: Context = .{
        .next = null,
        .payload = .{ .head = {} },
    };

    pub fn field(index: u32) Context {
        return .{
            .next = null,
            .payload = .{ .field = index },
        };
    }

    pub fn element(index: u32) Context {
        return .{
            .next = null,
            .payload = .{ .element = index },
        };
    }
};

const Scratch = std.ArrayListUnmanaged(Mir.Index);
const Indices = union(enum) {
    single: Mir.Index,
    multi: Scratch.Slice,
};

pub fn build(gpa: Allocator, pool: *InternPool, module: Type.Module) !Mir {
    var arena: std.heap.ArenaAllocator = .init(gpa);
    defer arena.deinit();

    var builder: Builder = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .pool = pool,
        .air = pool.airPtrConst(module.air),
        .module = module,
        .nodes = .empty,
        .extra = .empty,
        .scratch = .empty,
        .bytes = .empty,
    };

    const toplevel = try builder.airModule();
    return .{
        .nodes = builder.nodes.toOwnedSlice(),
        .extra = try builder.extra.toOwnedSlice(gpa),
        .toplevel = toplevel,
    };
}

fn addNode(b: *Builder, tag: Mir.Node.Tag, payload: Mir.Node.Payload) !Mir.Index {
    const len: u32 = @intCast(b.nodes.len);
    try b.nodes.append(b.gpa, .{ .tag = tag, .payload = payload });
    return @enumFromInt(len);
}

fn addExtra(b: *Builder, extra: anytype) !Mir.ExtraIndex {
    const len: u32 = @intCast(b.extra.items.len);
    const struct_fields = std.meta.fields(@TypeOf(extra));
    try b.extra.ensureUnusedCapacity(b.gpa, struct_fields.len);
    inline for (struct_fields) |struct_field| {
        switch (struct_field.type) {
            u32 => {
                const num = @field(extra, struct_field.name);
                b.extra.appendAssumeCapacity(num);
            },
            inline else => {
                const num = @intFromEnum(@field(extra, struct_field.name));
                b.extra.appendAssumeCapacity(num);
            },
        }
    }
    return @enumFromInt(len);
}

fn addIndices(b: *Builder, ids: []const Mir.Index) !Mir.Indices {
    const start: u32 = @intCast(b.extra.items.len);
    try b.extra.appendSlice(b.gpa, @ptrCast(ids));
    const end: u32 = @intCast(b.extra.items.len);

    return b.addExtra(Mir.ExtraSlice{
        .start = @enumFromInt(start),
        .end = @enumFromInt(end),
    });
}

fn typeOf(b: *Builder, value: Air.Value) InternPool.Index {
    return b.air.typeOf(b.pool, value);
}

fn typeOfIndex(b: *Builder, node: Air.Index) InternPool.Index {
    return b.air.typeOfIndex(b.pool, node);
}

fn airModule(b: *Builder) !Mir.Index {
    const module_name = b.module.name;
    const signature = b.pool.get(b.module.signature).ty.signature;

    // There are any number of inputs, each with a recursive elaboration.
    var scratch: Scratch = .empty;
    for (signature.input_names, signature.input_types) |name, ty| {
        const inputs = try b.airNet(name, ty);
        switch (inputs) {
            .single => |net| {
                const port = try b.addNode(.port_input, .{ .unary = net });
                try scratch.append(b.arena, port);
            },
            .multi => |nets| {
                try scratch.ensureUnusedCapacity(b.arena, nets.len);
                for (nets) |net| {
                    const port = try b.addNode(.port_input, .{ .unary = net });
                    scratch.appendAssumeCapacity(port);
                }
            },
        }
    }
    // There is a single output with a hardcoded base name, but it also
    // has to be recursively elaborated.
    const outputs = try b.airNet(.builtin_out, signature.output_type);
    switch (outputs) {
        .single => |net| {
            const port = try b.addNode(.port_output, .{ .unary = net });
            try scratch.append(b.arena, port);
        },
        .multi => |nets| {
            try scratch.ensureUnusedCapacity(b.arena, nets.len);
            for (nets) |net| {
                const port = try b.addNode(.port_output, .{ .unary = net });
                scratch.appendAssumeCapacity(port);
            }
        },
    }

    const ports = try b.addIndices(scratch.items);
    const block = try b.airBlock(b.air.body);
    const module = try b.addExtra(Mir.Node.Module{
        .name = module_name,
        .ports = ports,
        .body = block,
    });

    return b.addNode(.module, .{ .extra = module });
}

fn airBlock(b: *Builder, block_node: Air.Index) !Mir.Index {
    const block = b.air.get(block_node).block;
    var scratch: Scratch = .empty;

    const indices = b.air.indices(block);
    for (indices) |node| {
        const tag = b.air.tag(node);
        const statements = switch (tag) {
            .def => try b.airDef(node),
            .yield => try b.airYield(node),
            else => unreachable,
        };

        switch (statements) {
            .single => |single| try scratch.append(b.arena, single),
            .multi => |multi| try scratch.appendSlice(b.arena, multi),
        }
    }

    const extra = try b.addIndices(scratch.items);
    return b.addNode(.block, .{ .indices = extra });
}

fn airDef(b: *Builder, node: Air.Index) !Indices {
    const def = b.air.get(node).def;
    const signal = b.air.extraData(def.signal, Air.Node.Signal);
    std.debug.assert(signal.type == b.typeOf(def.value));

    const net_indices = try b.airNet(signal.name, signal.type);
    const val_indices = try b.airExpression(def.value);

    const indices: Indices = if (net_indices == .single) def: {
        const net = net_indices.single;
        const val = val_indices.single;
        const mir_node = try b.addNode(.net_def, .{
            .binary = .{ .l = net, .r = val },
        });
        break :def .{ .single = mir_node };
    } else def: {
        const nets = net_indices.multi;
        const vals = val_indices.multi;
        var scratch = try b.arena.alloc(Mir.Index, nets.len);
        for (nets, vals, 0..) |net, val, i| {
            scratch[i] = try b.addNode(.net_def, .{
                .binary = .{ .l = net, .r = val },
            });
        }

        break :def .{ .multi = scratch };
    };

    return indices;
}

fn airYield(b: *Builder, node: Air.Index) !Indices {
    const yield = b.air.get(node).yield;

    const ty = b.typeOf(yield);
    const net_indices = try b.airNetRef(.builtin_out, ty);
    const val_indices = try b.airExpression(yield);

    const indices: Indices = if (net_indices == .single) def: {
        const net = net_indices.single;
        const val = val_indices.single;
        const mir_node = try b.addNode(.net_assign, .{
            .binary = .{ .l = net, .r = val },
        });
        break :def .{ .single = mir_node };
    } else def: {
        const nets = net_indices.multi;
        const vals = val_indices.multi;
        var scratch = try b.arena.alloc(Mir.Index, nets.len);
        for (nets, vals, 0..) |net, val, i| {
            scratch[i] = try b.addNode(.net_assign, .{
                .binary = .{ .l = net, .r = val },
            });
        }

        break :def .{ .multi = scratch };
    };

    return indices;
}

fn airNet(b: *Builder, name: InternPool.Index, ty: InternPool.Index) !Indices {
    // Signals represent only the base name of an elaborated set of
    // verilog nets. In the case of aggregates, the field names or
    // indices are recursively appended to the base name to elaborate.
    defer b.bytes.clearRetainingCapacity();
    const base = b.pool.get(name).str;
    try b.bytes.appendSlice(b.arena, base);

    var scratch: Scratch = .empty;
    try b.airNetInner(&scratch, null, ty);

    std.debug.assert(scratch.items.len > 0);
    if (scratch.items.len > 1) {
        return .{ .multi = try scratch.toOwnedSlice(b.arena) };
    } else {
        return .{ .single = scratch.items[0] };
    }
}

/// Evaluates airIdent behind the scenes but provides an airNet interface.
/// Used for evaluating assignments that don't declare the full net type.
fn airNetRef(b: *Builder, name: InternPool.Index, ty: InternPool.Index) !Indices {
    // Signals represent only the base name of an elaborated set of
    // verilog nets. In the case of aggregates, the field names or
    // indices are recursively appended to the base name to elaborate.
    defer b.bytes.clearRetainingCapacity();
    const base = b.pool.get(name).str;
    try b.bytes.appendSlice(b.arena, base);

    var scratch: Scratch = .empty;
    try b.airIdentInner(&scratch, null, ty);

    std.debug.assert(scratch.items.len > 0);
    if (scratch.items.len > 1) {
        return .{ .multi = try scratch.toOwnedSlice(b.arena) };
    } else {
        return .{ .single = scratch.items[0] };
    }
}

fn airNetInner(b: *Builder, scratch: *Scratch, optional_ctx: ?*Context, ty: InternPool.Index) !void {
    if (optional_ctx) |ctx| {
        // Context was provided, so extract the necessary subset
        // of the aggregate type and elaborate recursively.
        const field = ctx.payload.field;
        const bundle = b.pool.get(ty).ty.bundle;
        const field_name = bundle.field_names[field];

        const str = b.pool.get(field_name).str;
        const bytes_top = b.bytes.items.len;
        defer b.bytes.shrinkRetainingCapacity(bytes_top);
        try b.bytes.ensureUnusedCapacity(b.arena, str.len + 1);
        b.bytes.appendAssumeCapacity('_');
        b.bytes.appendSliceAssumeCapacity(str);

        const field_type = bundle.field_types[field];
        try b.airNetInner(scratch, ctx.next, field_type);
    } else {
        // No context provided, so elaborate all aggregate subfields
        // recursively.
        switch (b.pool.get(ty).ty) {
            .bits, .uint, .sint, .bool => {
                // Leaf nodes of the expression tree. No more field
                // suffixes to tack on, so intern the final string
                // and emit a net node.
                const name = try b.pool.put(.{ .str = b.bytes.items });
                const ident = try b.addNode(.ident, .{ .ip = name });
                const net_type = try b.airType(ty);
                const net = try b.addNode(.net, .{ .binary = .{ .l = net_type, .r = ident } });
                try scratch.append(b.arena, net);
            },
            .int, .signature => unreachable,
            .bundle => {
                const bundle = b.pool.get(ty).ty.bundle;
                for (bundle.field_names, bundle.field_types) |field_name, field_type| {
                    const str = b.pool.get(field_name).str;
                    const bytes_top = b.bytes.items.len;
                    defer b.bytes.shrinkRetainingCapacity(bytes_top);
                    try b.bytes.ensureUnusedCapacity(b.arena, str.len + 1);
                    b.bytes.appendAssumeCapacity('_');
                    b.bytes.appendSliceAssumeCapacity(str);

                    try b.airNetInner(scratch, null, field_type);
                }
            },
            .array => {
                const array = b.pool.get(ty).ty.array;
                for (0..array.len) |i| {
                    const bytes_top = b.bytes.items.len;
                    defer b.bytes.shrinkRetainingCapacity(bytes_top);
                    const writer = b.bytes.writer(b.arena);
                    try writer.print("_{}", .{i});

                    try b.airNetInner(scratch, null, array.element_type);
                }
            },
            .module => unimplemented(),
        }
    }
}

fn airType(b: *Builder, ip: InternPool.Index) !Mir.Index {
    const ty = b.pool.get(ip).ty;
    return switch (ty) {
        .bits, .uint => |width| try b.addNode(.net_wire_unsigned, .{ .size = width }),
        .sint => |width| try b.addNode(.net_wire_signed, .{ .size = width }),
        .bool => try b.addNode(.net_wire_unsigned, .{ .size = 1 }),
        .int, .signature, .module => unreachable,
        .bundle, .array => unreachable,
    };
}

fn airExpression(b: *Builder, value: Air.Value) !Indices {
    var ctx: Context = .head;
    return b.airExpressionInner(&ctx, &ctx, value);
}

fn airExpressionInner(b: *Builder, top: *Context, ctx: *Context, value: Air.Value) Allocator.Error!Indices {
    switch (value.tagged()) {
        .index => |node| {
            const tag = b.air.tag(node);
            return switch (tag) {
                .null => unreachable,
                .ident => b.airIdent(top, ctx, node),
                .bundle_literal => b.airBundleLiteral(top, ctx, node),
                .field_init => unreachable,
                .array_literal => b.airArrayLiteral(top, ctx, node),
                .module_literal => unimplemented(),
                .input_init, .param => unreachable,
                .ineg,
                .bnot,
                .lnot,
                => b.airUnary(top, ctx, node),
                .paren => b.airParen(top, ctx, node),
                .band,
                .bor,
                .bxor,
                .iadd,
                .isub,
                .land,
                .lor,
                .lxor,
                .eq,
                .ne,
                => b.airBinary(top, ctx, node),
                // .limplies => b.airImplies(top, ctx, node),
                .limplies => b.airImplies(top, ctx, node),
                .bit => b.airBit(top, ctx, node),
                .bitslice => b.airBitSlice(top, ctx, node),
                .subscript => b.airSubscript(top, ctx, node),
                .field => b.airField(top, ctx, node),
                .def, .decl, .yield, .block, .toplevel => unreachable,
            };
        },
        .ip => |ip| return b.ipExpressionInner(ctx, ctx, ip),
    }
}

fn ipExpressionInner(b: *Builder, _: *Context, _: *Context, index: InternPool.Index) !Indices {
    const tv = b.pool.get(index).tv;
    const ty = b.pool.get(tv.ty).ty;
    return switch (ty) {
        .bool => b.ipBool(index),
        .bits, .uint, .sint, .int => b.ipInt(index),
        .bundle => unimplemented(),
        else => unreachable,
    };
}

fn ipBool(b: *Builder, index: InternPool.Index) !Indices {
    const tv = b.pool.get(index).tv;
    std.debug.assert(tv.ty == .bool);

    const bool_tv: TypedValue = .{
        .ty = .b1,
        .val = .{ .int = @intFromBool(tv.val.bool) },
    };

    const ip = try b.pool.put(.{ .tv = bool_tv });
    const node = try b.addNode(.bin_literal, .{ .ip = ip });
    return .{ .single = node };
}

fn ipInt(b: *Builder, index: InternPool.Index) !Indices {
    const tv = b.pool.get(index).tv;
    const ty = b.pool.get(tv.ty).ty;
    const node = switch (ty) {
        .int => try b.addNode(.unsized_literal, .{ .ip = index }),
        .bits, .uint, .sint => try b.addNode(.dec_literal, .{ .ip = index }),
        else => unreachable,
    };

    return .{ .single = node };
}

fn airIdent(b: *Builder, top: *Context, _: *Context, node: Air.Index) !Indices {
    const ident = b.air.get(node).ident;
    const net = b.air.get(ident.signal);
    const name = switch (net) {
        .def => |*def| name: {
            const signal = b.air.extraData(def.signal, Air.Node.Signal);
            break :name signal.name;
        },
        .decl => |extra| name: {
            const signal = b.air.extraData(extra, Air.Node.Signal);
            break :name signal.name;
        },
        .param => |*param| name: {
            const signature = b.pool.get(b.module.signature).ty.signature;
            break :name signature.input_names[param.index];
        },
        else => unreachable,
    };

    // Identifiers represent only the base name of an elaborated expression.
    // In the case of aggregates, the field names or indices are recursively
    // appended to the base name to elaborate.
    defer b.bytes.clearRetainingCapacity();
    const base = b.pool.get(name).str;
    try b.bytes.appendSlice(b.arena, base);

    const ty = b.typeOfIndex(node);
    var scratch: Scratch = .empty;
    try b.airIdentInner(&scratch, top.next, ty);

    std.debug.assert(scratch.items.len > 0);
    if (scratch.items.len > 1) {
        return .{ .multi = try scratch.toOwnedSlice(b.arena) };
    } else {
        return .{ .single = scratch.items[0] };
    }
}

fn airIdentInner(b: *Builder, scratch: *Scratch, optional_ctx: ?*Context, ty: InternPool.Index) !void {
    if (optional_ctx) |ctx| {
        // Context was provided, so extract the necessary subset
        // of the aggregate type and elaborate recursively.
        switch (ctx.payload) {
            .head => unreachable,
            .field => |field| {
                const bundle = b.pool.get(ty).ty.bundle;
                const field_name = bundle.field_names[field];

                const str = b.pool.get(field_name).str;
                const bytes_top = b.bytes.items.len;
                defer b.bytes.shrinkRetainingCapacity(bytes_top);
                try b.bytes.ensureUnusedCapacity(b.arena, str.len + 1);
                b.bytes.appendAssumeCapacity('_');
                b.bytes.appendSliceAssumeCapacity(str);

                const field_type = bundle.field_types[field];
                try b.airIdentInner(scratch, ctx.next, field_type);
            },
            .element => |element| {
                const array = b.pool.get(ty).ty.array;

                const bytes_top = b.bytes.items.len;
                defer b.bytes.shrinkRetainingCapacity(bytes_top);
                const writer = b.bytes.writer(b.arena);
                try writer.print("_{}", .{element});

                const element_type = array.element_type;
                try b.airIdentInner(scratch, ctx.next, element_type);
            },
        }
    } else {
        // No context provided, so elaborate all aggregate subfields
        // recursively.
        switch (b.pool.get(ty).ty) {
            .bits, .uint, .sint, .bool => {
                // Leaf nodes of the expression tree. No more field
                // suffixes to tack on, so intern the final string
                // and emit an identifier node.
                const ident = try b.pool.put(.{ .str = b.bytes.items });
                const mir_node = try b.addNode(.ident, .{ .ip = ident });
                try scratch.append(b.arena, mir_node);
            },
            .int, .signature => unreachable,
            .bundle => {
                const bundle = b.pool.get(ty).ty.bundle;
                for (bundle.field_names, bundle.field_types) |field_name, field_type| {
                    const str = b.pool.get(field_name).str;
                    const bytes_top = b.bytes.items.len;
                    defer b.bytes.shrinkRetainingCapacity(bytes_top);
                    try b.bytes.ensureUnusedCapacity(b.arena, str.len + 1);
                    b.bytes.appendAssumeCapacity('_');
                    b.bytes.appendSliceAssumeCapacity(str);

                    try b.airIdentInner(scratch, null, field_type);
                }
            },
            .array => {
                const array = b.pool.get(ty).ty.array;
                for (0..array.len) |i| {
                    const bytes_top = b.bytes.items.len;
                    defer b.bytes.shrinkRetainingCapacity(bytes_top);
                    const writer = b.bytes.writer(b.arena);
                    try writer.print("_{}", .{i});

                    try b.airIdentInner(scratch, null, array.element_type);
                }
            },
            .module => unimplemented(),
        }
    }
}

fn airBundleLiteral(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const bundle_literal = b.air.get(node).bundle_literal;
    const ty = b.typeOfIndex(node);
    std.debug.assert(b.pool.get(ty).ty == .bundle);

    // FIXME: doesn't elaborate context
    //
    // Each field initializer in a bundle literal is its own expression
    // and is elaborated recursively.
    var scratch: Scratch = .empty;
    const inits = b.air.values(bundle_literal.inits);
    for (inits) |init| {
        const inner = try b.airExpressionInner(top, ctx, init);
        switch (inner) {
            .single => |index| try scratch.append(b.arena, index),
            .multi => |indices| try scratch.appendSlice(b.arena, indices),
        }
    }

    return .{ .multi = try scratch.toOwnedSlice(b.arena) };
}

fn airArrayLiteral(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const array_literal = b.air.get(node).array_literal;
    const ty = b.typeOfIndex(node);
    std.debug.assert(b.pool.get(ty).ty == .array);

    // FIXME: doesn't elaborate context
    //
    // Each field initializer in a bundle literal is its own expression
    // and is elaborated recursively.
    var scratch: Scratch = .empty;
    const inits = b.air.values(array_literal.inits);
    for (inits) |init| {
        const inner = try b.airExpressionInner(top, ctx, init);
        switch (inner) {
            .single => |index| try scratch.append(b.arena, index),
            .multi => |indices| try scratch.appendSlice(b.arena, indices),
        }
    }

    return .{ .multi = try scratch.toOwnedSlice(b.arena) };
}

fn airUnary(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const air_tag = b.air.tag(node);
    const unary = switch (b.air.get(node)) {
        inline .ineg, .bnot, .lnot => |unary| unary,
        else => unreachable,
    };

    std.debug.assert(unary.tag == .index);
    const inner = try b.airExpressionInner(top, ctx, unary);

    // The Mir node is a 1:1 translation for all the current unary operators.
    const mir_tag: Mir.Node.Tag = switch (air_tag) {
        .ineg => .unary_ineg,
        .bnot => .unary_bnot,
        .lnot => .unary_lnot,
        else => unreachable,
    };

    // Currently, there are no unary operators defined on aggregate types.
    return .{ .single = try b.addNode(mir_tag, .{ .unary = inner.single }) };
}

fn airParen(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const paren = b.air.get(node).paren;

    std.debug.assert(paren.tag == .index);
    const inner = try b.airExpressionInner(top, ctx, paren);

    return switch (inner) {
        .single => |index| .{ .single = try b.addNode(.paren, .{ .unary = index }) },
        // Currently, parentheses around aggregate types are elided during
        // elaboration since they don't translate clearly to the verilog.
        .multi => |indices| .{ .multi = indices },
    };
}

fn airBinary(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const air_tag = b.air.tag(node);
    const binary = switch (b.air.get(node)) {
        // FIXME: is inline needed here? probably not
        inline .bor, .band, .bxor => |binary| binary,
        inline .lor, .land, .lxor => |binary| binary,
        inline .eq, .ne => |binary| binary,
        .limplies => unimplemented(),
        inline .iadd, .isub => |*pl| b.air.extraData(pl.bin, Air.Node.Binary),
        else => unreachable,
    };

    const l = try b.airExpressionInner(top, ctx, binary.l);
    const r = try b.airExpressionInner(top, ctx, binary.r);

    // The Mir node is a 1:1 translation for all the current binary operators.
    const mir_tag: Mir.Node.Tag = switch (air_tag) {
        .bor => .bin_bor,
        .band => .bin_band,
        .bxor => .bin_bxor,
        .lor => .bin_lor,
        .land => .bin_land,
        .lxor => .bin_bxor,
        .iadd => .bin_iadd,
        .isub => .bin_isub,
        .eq => .bin_eq,
        .ne => .bin_ne,
        else => unreachable,
    };

    // Currently, there are no binary operators defined on aggregate types.
    return .{
        .single = try b.addNode(
            mir_tag,
            .{ .binary = .{
                .l = l.single,
                .r = r.single,
            } },
        ),
    };
}

fn airBit(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const bit = b.air.get(node).bit;
    const operand = try b.airExpressionInner(top, ctx, bit.operand);
    const index = try b.airExpressionInner(top, ctx, bit.index);

    // The bit select operator is not defined on aggregate types.
    return .{
        .single = try b.addNode(
            .bit_select,
            .{ .binary = .{
                .l = operand.single,
                .r = index.single,
            } },
        ),
    };
}

fn airBitSlice(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const data = b.air.get(node).bitslice;
    const bitslice = b.air.extraData(data.bitslice, Air.Node.BitSlice);

    // The bit select operator is not defined on aggregate types.
    const operand = try b.airExpressionInner(top, ctx, bitslice.operand);
    const exclusive = b.pool.get(bitslice.upper).tv.val.int;
    const inclusive = try b.pool.put(.{ .tv = .{ .ty = .int, .val = .{ .int = exclusive - 1 } } });
    const upper = try b.ipExpressionInner(top, ctx, inclusive);
    const lower = try b.ipExpressionInner(top, ctx, bitslice.lower);
    const extra = try b.addExtra(Mir.Node.Range{
        .upper = upper.single,
        .lower = lower.single,
    });

    return .{
        .single = try b.addNode(
            .part_select,
            .{ .op_extra = .{
                .op = operand.single,
                .extra = extra,
            } },
        ),
    };
}

fn airImplies(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const limplies = b.air.get(node).limplies;

    const l = try b.airExpressionInner(top, ctx, limplies.l);
    const r = try b.airExpressionInner(top, ctx, limplies.r);

    // Logical implication is not natively available in verilog so
    // we reduce it to an equivalent expression with not/or.
    const lnot = try b.addNode(.unary_lnot, .{ .unary = l.single });
    const lor = try b.addNode(.bin_lor, .{ .binary = .{ .l = lnot, .r = r.single } });
    const paren = try b.addNode(.paren, .{ .unary = lor });

    // Currently, there are no binary operators defined on aggregate types.
    return .{ .single = paren };
}

fn airSubscript(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const subscript = b.air.get(node).subscript;

    // Because aggregates are recursively elaborated, no need to emit
    // a node here. Just annotate the context with the field index
    // and elaborate the operand.
    // TODO: support non-constant array index
    const val = subscript.index.asInterned().?;
    var inner: Context = .element(@intCast(b.pool.get(val).tv.val.int));
    inner.next = top.next;
    top.next = &inner;
    defer top.next = inner.next;

    return try b.airExpressionInner(top, ctx, subscript.operand);
}

fn airField(b: *Builder, top: *Context, ctx: *Context, node: Air.Index) !Indices {
    const field = b.air.get(node).field;

    // Because aggregates are recursively elaborated, no need to emit
    // a node here. Just annotate the context with the field index
    // and elaborate the operand.
    var inner: Context = .field(field.index);
    inner.next = top.next;
    top.next = &inner;
    defer top.next = inner.next;

    return try b.airExpressionInner(top, ctx, field.operand);
}

// NOTE: this is not meant to be used in release builds
fn unimplemented() noreturn {
    std.debug.print("unimplemented!\n", .{});
    unreachable;
}
