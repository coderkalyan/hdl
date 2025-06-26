const std = @import("std");
const Cst = @import("Cst.zig");
const Air = @import("Air.zig");
const InternPool = @import("InternPool.zig");
const Type = @import("type.zig").Type;
const Package = @import("Package.zig");
const error_handler = @import("error_handler.zig");

const Allocator = std.mem.Allocator;
const Index = Air.Index;
const Node = Air.Node;
const Value = Air.Value;
const Decl = Package.Decl;
const SourceError = error_handler.SourceError;

const Error = Allocator.Error || std.fmt.ParseIntError || std.posix.WriteError || error{SourceError};

pub fn analyze(gpa: Allocator, pool: *InternPool, cst: *Cst) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    // NOTE: this prevents a memory leak if the parser fails, but ownership should be
    // changed if we eventually want to print something using the source on failure
    // errdefer cst.deinit(gpa);

    var sema = try Sema.init(gpa, arena.allocator(), pool, cst, undefined, .root);
    defer sema.deinit();

    // toplevel analysis does not generate any Air, but populates the InternPool
    // and automatically runs analysis on each module body
    // TODO: eventually we want to multithread this which will require a slightly different setup
    try sema.root();
}

// NOTE: in Zig 0.15 std.SinglyLinkedList is an intrusive list
// so we can migrate to it
// const Scope = struct {
//     kind: Kind,
//     // FIXME: because this map only contains named signals,
//     // which are always defined (not anonymous expressions)
//     // we should use Index here and not Value
//     map: std.AutoHashMapUnmanaged(InternPool.Index, Value),
//     parent: ?*Scope,
//
//     const Kind = enum {
//         toplevel,
//         body,
//     };
// };

const Scope = struct {
    parent: ?*Scope,
    payload: Payload,

    pub const Payload = union(enum) {
        /// Toplevel scope, does not contain anything.
        toplevel,
        /// Namespace (for example toplevel package) with signal
        /// and type definitions. Names are unique within the
        /// namespace (no shadowing) and unordered.
        namespace: std.AutoHashMapUnmanaged(InternPool.Index, Index),
        /// A signal definition with name and value node.
        def: struct {
            name: InternPool.Index,
            index: Index,
        },
        /// A signal declaration with no value.
        decl: InternPool.Index,
        /// A type definition with name and interned type.
        type: struct {
            name: InternPool.Index,
            type: InternPool.Index,
        },
    };

    pub fn resolve(scope: *Scope, name: InternPool.Index) ?*Scope {
        var s = scope;
        while (true) {
            switch (s.payload) {
                .toplevel => return null,
                inline .def, .type => |*pl| if (pl.name == name) return s,
                .decl => |pl| if (pl == name) return s,
                .namespace => |*map| {
                    if (map.contains(name)) return s;
                },
            }

            s = s.parent.?;
        }
    }

    pub fn count(scope: *Scope, name: InternPool.Index) u32 {
        var i: u32 = 0;
        var s = scope;
        while (true) {
            if (s.resolve(name)) |next| {
                s = next.parent.?;
                i += 1;
            } else break;
        }

        return i;
    }
};

// TODO: pull this out into the file struct itself
const Sema = struct {
    gpa: Allocator,
    arena: Allocator,

    // read only copy of the CST, used as source for semantic analysis into Air
    cst: *const Cst,
    // intern pool is used for types and identifiers
    pool: *InternPool,
    // NOTE: currently unused but kept around in case we need to make decisions
    // on how to analyze something based on the context
    scope: ScopeKind,
    // FIXME: this is unsafe because it is only used in the body context
    // and undefined in the root context
    // however the whole context situation needs to be revisited
    signature: InternPool.Index,

    nodes: std.MultiArrayList(Node),
    extra: std.ArrayListUnmanaged(u32),
    scratch: std.ArrayListUnmanaged(u32),
    errors: std.ArrayListUnmanaged(SourceError),

    pub const ScopeKind = enum {
        // dispatched once at the root of the CST
        // the only valid syntax element here right now is a type
        // implicit namespace without need for forward declaration
        root,
        // dispatched for each module body
        // all synthesizable and comptime syntax elements are valid here
        // linear scoping rules so explicit forward declaration required
        body,
    };

    pub const Context = struct {
        /// The expression being evaluated should attempt to coerce
        /// into this type, or error otherwise. If no type hint is
        /// specified, this is `null` and the expression should infer
        /// the type.
        type: InternPool.Index,
        /// The declaration (type or function) being evaluated should
        /// adopt this name. If no name hint is specified, this is
        /// `null` and the declaration is anonymous.
        name: InternPool.Index,
        /// The Decl that this expression is being assigned to.
        // decl: ?*Decl = null,

        pub const anon_type: Context = .{ .type = .null, .name = .null };
    };

    pub fn init(gpa: Allocator, arena: Allocator, pool: *InternPool, cst: *const Cst, signature: InternPool.Index, scope: ScopeKind) !Sema {
        var nodes: std.MultiArrayList(Node) = .empty;
        errdefer nodes.deinit(gpa);
        try nodes.append(gpa, .{ .null = {} });

        return .{
            .gpa = gpa,
            .arena = arena,
            .pool = pool,
            .cst = cst,
            .scope = scope,
            .signature = signature,
            .nodes = nodes,
            .extra = .empty,
            .scratch = .empty,
            .errors = .empty,
        };
    }

    pub fn deinit(self: *Sema) void {
        self.nodes.deinit(self.gpa);
        self.extra.deinit(self.gpa);
        self.scratch.deinit(self.arena);
        self.errors.deinit(self.arena);
    }

    fn addNode(self: *Sema, node: Node) !Index {
        const len: u32 = @intCast(self.nodes.len);
        try self.nodes.append(self.gpa, node);
        return @enumFromInt(len);
    }

    fn addExtra(self: *Sema, extra: anytype) !Air.ExtraIndex {
        const len: u32 = @intCast(self.extra.items.len);
        const struct_fields = std.meta.fields(@TypeOf(extra));
        try self.extra.ensureUnusedCapacity(self.gpa, struct_fields.len);
        inline for (struct_fields) |struct_field| {
            switch (struct_field.type) {
                u32 => {
                    const num = @field(extra, struct_field.name);
                    self.extra.appendAssumeCapacity(num);
                },
                Air.Value => {
                    const value = @field(extra, struct_field.name);
                    const num: u32 = @bitCast(value);
                    self.extra.appendAssumeCapacity(num);
                },
                inline else => {
                    const num = @intFromEnum(@field(extra, struct_field.name));
                    self.extra.appendAssumeCapacity(num);
                },
            }
        }
        return @enumFromInt(len);
    }

    fn allocScratchSlice(self: *Sema, comptime T: type, len: usize) ![]T {
        std.debug.assert(@sizeOf(T) == @sizeOf(u32));
        std.debug.assert(@alignOf(T) == @alignOf(u32));

        const ptr = try self.scratch.addManyAsSlice(self.arena, len);
        return @ptrCast(ptr);
    }

    fn internToken(self: *Sema, token: Cst.TokenIndex) !InternPool.Index {
        const str = self.cst.tokenString(token);
        return self.pool.put(.{ .str = str });
    }

    pub fn addIndices(self: *Sema, ids: []const Index) !Air.Indices {
        const start: u32 = @intCast(self.extra.items.len);
        try self.extra.appendSlice(self.gpa, @ptrCast(ids));
        const end: u32 = @intCast(self.extra.items.len);

        return self.addExtra(Cst.ExtraSlice{
            .start = @enumFromInt(start),
            .end = @enumFromInt(end),
        });
    }

    pub fn addValues(self: *Sema, ids: []const Value) !Air.Values {
        const start: u32 = @intCast(self.extra.items.len);
        try self.extra.appendSlice(self.gpa, @ptrCast(ids));
        const end: u32 = @intCast(self.extra.items.len);

        return self.addExtra(Cst.ExtraSlice{
            .start = @enumFromInt(start),
            .end = @enumFromInt(end),
        });
    }

    pub fn putType(self: *Sema, ty: Type) !InternPool.Index {
        return self.pool.put(.{ .ty = ty });
    }

    pub fn addError(self: *Sema, tag: SourceError.Tag, token: Cst.TokenIndex) !void {
        try self.errors.append(self.arena, .{ .tag = tag, .token = token });
    }

    // no Airs are generated here, but the InternPool is populated with
    // types and identifiers, and a new analysis is called on each module body
    pub fn root(self: *Sema) !void {
        // the root forms a namespace with implicit forward declaration,
        // so a two pass approach is used
        const payload = self.cst.payload(self.cst.root);
        const slice = self.cst.indices(payload.root);
        for (slice) |cst_index| {
            const decl = try self.typeDef(cst_index);
            _ = decl;
            // const ptr = self.pool.declPtr(decl);
            // const name = self.pool.get(ptr.name).str;
            // std.debug.print("{s}: {}\n", .{ name, ptr.* });
        }
    }

    pub fn body(self: *Sema, s: *Scope, index: Cst.Index) !Air {
        const ai = try self.block(s, index);

        return .{
            .nodes = self.nodes.toOwnedSlice(),
            .extra = try self.extra.toOwnedSlice(self.gpa),
            .body = ai,
        };
    }

    fn block(self: *Sema, scope: *Scope, index: Cst.Index) !Index {
        const data = self.cst.payload(index).block;
        const slice = self.cst.indices(data);
        var s = scope;

        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        try self.scratch.ensureUnusedCapacity(self.arena, slice.len);
        for (slice) |i| {
            const ai, s = try self.statement(s, i);
            self.scratch.appendAssumeCapacity(@intFromEnum(ai));
        }

        const items: []const Index = @ptrCast(self.scratch.items[scratch_top..]);
        const nodes = try self.addIndices(items);
        return self.addNode(.{ .block = nodes });
    }

    fn statement(self: *Sema, s: *Scope, index: Cst.Index) !struct { Index, *Scope } {
        const data = self.cst.payload(index);

        return switch (data) {
            .def => self.def(s, index),
            .yield => .{ try self.yield(s, index), s },
            else => unimplemented(),
        };
    }

    fn def(self: *Sema, s: *Scope, index: Cst.Index) !struct { Index, *Scope } {
        const data = self.cst.payload(index).def;
        const ident_token = self.cst.mainToken(index).advance(1);
        const name = try self.internToken(ident_token);

        // Name shadowing is allowed within a body, but with restrictions.
        const resolved = s.resolve(name);
        if (resolved) |f| {
            // Signal names cannot shadow existing type names.
            if (f.payload == .type) {
                try self.addError(.shadow_signal_type, ident_token);
                return error.SourceError;
            }
        }

        // Record the salt information to de-duplicate name shadowing.
        const salt = s.count(name);

        const ty, const value = if (data.type == .null) tv: {
            // Type inference, so first evaluate the value, and then
            // determine its type and define the signal to that type.
            const ctx: Context = .{ .type = .null, .name = name };
            const value = try self.expression(s, data.value, &ctx);
            const ty = self.tempAir().typeOf(value);
            break :tv .{ ty, value };
        } else tv: {
            // Otherwise, first evaluate the type, and then use it in the
            // result context for the value (i.e. to evaluate anonymous
            // literals).
            const ty = try self.type(data.type, &.anon_type);
            const ctx: Context = .{ .type = ty, .name = name };
            const value = try self.expression(s, data.value, &ctx);
            break :tv .{ ty, value };
        };

        const signal: Node.Signal = .{ .name = name, .type = ty, .salt = salt };
        const node = try self.addNode(.{
            .def = .{
                .signal = try self.addExtra(signal),
                .value = value,
            },
        });

        const scope = try self.arena.create(Scope);
        scope.* = .{
            .parent = s,
            .payload = .{
                .def = .{
                    .name = signal.name,
                    .index = node,
                },
            },
        };

        return .{ node, scope };
    }

    fn yield(self: *Sema, s: *Scope, index: Cst.Index) !Index {
        const data = self.cst.payload(index).yield;

        const signature = self.pool.get(self.signature).ty.signature;
        const ctx: Context = .{ .type = signature.output_type, .name = .null };
        const value = try self.expression(s, data, &ctx);
        return self.addNode(.{ .yield = value });
    }

    fn typeDef(self: *Sema, index: Cst.Index) !InternPool.DeclIndex {
        const data = self.cst.payload(index).type;
        const ident_token = self.cst.mainToken(index).advance(1);
        const name = try self.internToken(ident_token);

        const ctx: Context = .{ .type = .null, .name = name };
        const ty = try self.type(data, &ctx);

        const decl = try self.pool.createDecl(.{
            .kind = .type,
            .name = name,
            .type = ty,
        });

        try self.pool.decls_map.put(self.gpa, name, decl);
        return decl;
    }

    fn @"type"(self: *Sema, index: Cst.Index, ctx: *const Context) Error!InternPool.Index {
        const data = self.cst.payload(index);

        return switch (data) {
            .ident => self.identType(index),
            .bundle => self.bundle(index, ctx),
            .module => self.module(index, ctx),
            else => unimplemented(),
        };
    }

    fn identType(self: *Sema, index: Cst.Index) !InternPool.Index {
        const token = self.cst.mainToken(index);
        const str = self.cst.tokenString(token);

        if (std.mem.eql(u8, str, "bool")) return .bool;

        const builtin: ?InternPool.Index = switch (str[0]) {
            'b' => try self.bits(str),
            'u' => try self.uint(str),
            'i' => try self.sint(str),
            else => null,
        };

        // builtin types do not need to be looked up in scope
        if (builtin) |ip| return ip;

        // const ident = try self.pool.put(.{ .str = str });
        // if ()

        unimplemented();
    }

    fn bits(self: *Sema, str: []const u8) !InternPool.Index {
        std.debug.assert(str.len > 1);
        std.debug.assert(str[0] == 'b');

        const width = try std.fmt.parseInt(u32, str[1..], 10);
        const ty: Type = .{ .bits = width };
        return self.pool.put(.{ .ty = ty });
    }

    fn uint(self: *Sema, str: []const u8) !InternPool.Index {
        std.debug.assert(str.len > 1);
        std.debug.assert(str[0] == 'u');

        const width = try std.fmt.parseInt(u32, str[1..], 10);
        const ty: Type = .{ .uint = width };
        return self.pool.put(.{ .ty = ty });
    }

    fn sint(self: *Sema, str: []const u8) !InternPool.Index {
        std.debug.assert(str.len > 1);
        std.debug.assert(str[0] == 'i');

        const width = try std.fmt.parseInt(u32, str[1..], 10);
        const ty: Type = .{ .sint = width };
        return self.pool.put(.{ .ty = ty });
    }

    fn bundle(self: *Sema, index: Cst.Index, ctx: *const Context) !InternPool.Index {
        const data = self.cst.payload(index).bundle;
        const items = self.cst.indices(data);

        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        const names = try self.allocScratchSlice(InternPool.Index, items.len);
        const types = try self.allocScratchSlice(InternPool.Index, items.len);

        for (items, names, types) |idx, *name, *ty| {
            const field = self.cst.payload(idx).field;
            const token = self.cst.mainToken(idx);
            name.* = try self.internToken(token);
            ty.* = try self.type(field, &.anon_type);
        }

        return self.pool.put(.{
            .ty = .{
                .bundle = .{
                    .cst_index = index,
                    .name = ctx.name,
                    .field_names = names,
                    .field_types = types,
                },
            },
        });
    }

    fn expression(self: *Sema, s: *Scope, index: Cst.Index, ctx: *const Context) Error!Value {
        const data = self.cst.payload(index);

        return switch (data) {
            .ident => self.identExpression(s, ctx, index),
            .unary => self.unary(s, ctx, index),
            .binary => self.binary(s, ctx, index),
            .bundle_literal => self.bundleLiteral(s, ctx, index),
            else => unimplemented(),
        };
    }

    fn identExpression(self: *Sema, s: *Scope, ctx: *const Context, index: Cst.Index) !Value {
        const token = self.cst.mainToken(index);
        const ident = try self.internToken(token);

        // Resolve the identifer in the current scope to validate
        // its existence. If valid, the signal is recorded and the
        // identifier is re-emitted, not inlined (because this is
        // a source to source translation).
        const resolved = s.resolve(ident);
        const signal = if (resolved) |rs| v: {
            break :v rs.payload.def.index;
        } else {
            try self.addError(.unknown_identifier, token);
            return error.SourceError;
        };

        // If a type hint is provided in the context, validate it
        // against the type of the signal that was resolved.
        const ty = self.tempAir().typeOf(Value.index(signal));
        if (ctx.type != .null and ctx.type != ty) {
            try self.addError(.type_coerce_fail, token);
            return error.SourceError;
        }

        const node = try self.addNode(.{
            .ident = .{
                .signal = signal,
                .type = ty,
            },
        });
        return Value.index(node);
    }

    fn unary(self: *Sema, s: *Scope, ctx: *const Context, index: Cst.Index) !Value {
        const data = self.cst.payload(index).unary;
        const main_token = self.cst.mainToken(index);
        const operator = self.cst.tokenTag(main_token);

        const inner = try self.expression(s, data, ctx);

        // check that the expression type is compatible with the operator
        const ty = self.tempAir().typeOf(inner);
        const dty = self.pool.get(ty).ty;
        const valid = switch (operator) {
            // TODO: is this correct, what does negating unsigned mean?
            .minus => dty == .uint or dty == .sint,
            .tilde => dty == .uint or dty == .sint or dty == .bits,
            .k_not => dty == .bool,
            .l_paren => true,
            else => self.unexpectedToken(main_token),
        };
        if (!valid) {
            try self.addError(.type_unary_invalid, main_token);
            return error.SourceError;
        }

        const node: Node = switch (operator) {
            .minus => .{ .ineg = inner },
            .tilde => .{ .bnot = inner },
            .k_not => .{ .lnot = inner },
            .l_paren => .{ .paren = inner },
            else => self.unexpectedToken(main_token),
        };

        return Value.index(try self.addNode(node));
    }

    fn binary(self: *Sema, s: *Scope, ctx: *const Context, index: Cst.Index) !Value {
        const data = self.cst.payload(index).binary;
        const main_token = self.cst.mainToken(index);
        const operator = self.cst.tokenTag(main_token);

        const inner_ctx: *const Context = switch (operator) {
            .k_and, .k_or, .k_xor, .k_implies => &.{ .type = .bool, .name = ctx.name },
            else => &.{ .type = .null, .name = ctx.name },
        };

        const l = try self.expression(s, data.l, inner_ctx);
        const r = try self.expression(s, data.r, inner_ctx);

        // check that both expressions have the same type
        const lty = self.tempAir().typeOf(l);
        const rty = self.tempAir().typeOf(r);
        if (lty != rty) {
            try self.addError(.type_binary_diff, main_token);
            return error.SourceError;
        }

        // check that the expression type is compatible with the operator
        const ty = self.pool.get(lty).ty;
        const valid = switch (operator) {
            .plus,
            .minus,
            => ty == .uint or ty == .sint,
            .ampersand,
            .pipe,
            .caret,
            => ty == .bits or ty == .uint or ty == .sint,
            .k_and,
            .k_or,
            .k_xor,
            .k_implies,
            => ty == .bool,
            else => self.unexpectedToken(main_token),
        };
        if (!valid) {
            try self.addError(.type_binary_invalid, main_token);
            return error.SourceError;
        }

        const dty = switch (operator) {
            .plus => ty: {
                const src = self.pool.get(lty).ty;
                const dst: Type = switch (src) {
                    .uint => |width| .{ .uint = width + 1 },
                    .sint => unimplemented(),
                    else => unreachable,
                };

                break :ty try self.pool.put(.{ .ty = dst });
            },
            .minus => unimplemented(),
            else => lty,
        };

        // If a type hint is provided in the context, validate it
        // against the type of the signal that was resolved.
        if (ctx.type != .null and ctx.type != dty) {
            try self.addError(.type_coerce_fail, main_token);
            return error.SourceError;
        }

        const bin: Node.Binary = .{ .l = l, .r = r };
        const node: Node = switch (operator) {
            .ampersand => .{ .band = bin },
            .pipe => .{ .bor = bin },
            .caret => .{ .bxor = bin },
            .plus => node: {
                const extra = try self.addExtra(bin);
                break :node .{ .iadd = .{ .bin = extra, .type = dty } };
            },
            .minus => node: {
                const extra = try self.addExtra(bin);
                break :node .{ .isub = .{ .bin = extra, .type = dty } };
            },
            .k_and => .{ .land = bin },
            .k_or => .{ .lor = bin },
            .k_xor => .{ .lxor = bin },
            .k_implies => .{ .limplies = bin },
            else => self.unexpectedToken(main_token),
        };

        return Value.index(try self.addNode(node));
    }

    fn bundleLiteral(self: *Sema, s: *Scope, ctx: *const Context, index: Cst.Index) !Value {
        const data = self.cst.payload(index).bundle_literal;
        const inits = self.cst.indices(data.inits);

        const bundle_type = if (data.type != .null) type: {
            // if explicitly specified, no need to infer from context
            const ty = try self.type(data.type, &.anon_type);
            break :type ty;
        } else type: {
            // otherwise use the context
            // FIXME: if this is null it is a compiler error
            const inferred = ctx.type;
            std.debug.assert(inferred != .null);
            break :type inferred;
        };

        const ty = self.pool.get(bundle_type).ty.bundle;

        // TODO: actually validate the struct
        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        const values = try self.allocScratchSlice(Value, inits.len);
        for (inits) |idx| {
            const field_init = self.cst.payload(idx).field_init;
            const main_token = self.cst.mainToken(idx);
            const init_name = try self.internToken(main_token);
            const value = try self.expression(s, field_init, undefined);
            const i = for (ty.field_names, 0..) |name, i| {
                if (name == init_name) break i;
            } else unreachable;
            values[i] = value;
        }

        const node = try self.addNode(.{
            .bundle_literal = .{
                .type = bundle_type,
                .inits = try self.addValues(values),
            },
        });
        return Value.index(node);
    }

    fn module(self: *Sema, index: Cst.Index, ctx: *const Context) !InternPool.Index {
        const data = self.cst.payload(index).module;
        const ports = self.cst.payload(data.ports).ports;
        const inputs = self.cst.indices(ports.inputs);

        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        const input_names = try self.allocScratchSlice(InternPool.Index, inputs.len);
        const input_types = try self.allocScratchSlice(InternPool.Index, inputs.len);

        for (inputs, 0..) |idx, i| {
            const input = self.cst.payload(idx).input;
            const token = self.cst.mainToken(idx);
            const name = try self.internToken(token);
            const ty = try self.type(input, &.anon_type);

            input_names[i] = name;
            input_types[i] = ty;
        }

        const output_type = try self.type(ports.output, &.anon_type);
        const signature = try self.putType(.{
            .signature = .{
                .input_names = input_names,
                .input_types = input_types,
                .output_type = output_type,
            },
        });

        // const signature: Type = .{
        //     .module = .{
        //         .cst_index = index,
        //         .name = ctx.name,
        //         .input_names = input_names,
        //         .input_types = input_types,
        //         .output_type = output_type,
        //     },
        // };

        var sema = try Sema.init(self.gpa, self.arena, self.pool, self.cst, signature, .body);
        errdefer sema.deinit();

        // the parameter nodes should actually exist in the inner scope
        var toplevel: Scope = .{
            .parent = null,
            .payload = .{ .toplevel = {} },
        };

        var scope = &toplevel;
        const scopes = try self.arena.alloc(Scope, inputs.len);
        for (input_names, input_types, 0..) |name, ty, i| {
            const param = try sema.addNode(.{
                .param = .{
                    .index = @intCast(i),
                    .type = ty,
                },
            });

            scopes[i] = .{
                .parent = scope,
                .payload = .{
                    .def = .{
                        .name = name,
                        .index = param,
                    },
                },
            };
            scope = &scopes[i];
        }

        const air = sema.body(scope, data.body) catch |err| {
            if (err == error.SourceError) {
                const stderr = std.io.getStdErr().writer();
                const errors = try error_handler.LocatedSourceError.locateErrors(self.gpa, self.cst, sema.errors.items);
                defer self.gpa.free(errors);
                var render: error_handler.CompileErrorRenderer(4, @TypeOf(stderr)) = .init(stderr, self.arena, self.cst, "main.hdl", errors);
                try render.render();
            }

            return err;
        };

        const air_index = try self.pool.createAir(air);
        const module_type = try self.putType(.{
            .module = .{
                .cst_index = index,
                .name = ctx.name,
                .signature = signature,
                .air = air_index,
            },
        });

        return module_type;
    }

    fn tempAir(self: *Sema) Air {
        return .{
            .nodes = self.nodes.slice(),
            .extra = self.extra.items,
            // NOTE: dereferencing this will explode
            .body = .null,
        };
    }

    // NOTE: this is not meant to be used in release builds
    fn unimplemented() noreturn {
        std.debug.print("unimplemented!\n", .{});
        unreachable;
    }

    // NOTE: this is not meant to be used in release builds
    fn unexpectedToken(self: *Sema, token: Cst.TokenIndex) noreturn {
        const tag = self.cst.tokenTag(token);
        const str = self.cst.tokenString(token);
        std.debug.print("unexpected token: {} '{s}'\n", .{ tag, str });
        unreachable;
    }
};
