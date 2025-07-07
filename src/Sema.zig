const std = @import("std");
const Cst = @import("Cst.zig");
const Air = @import("Air.zig");
const InternPool = @import("InternPool.zig");
const Type = @import("type.zig").Type;
const TypedValue = @import("value.zig").TypedValue;
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
const Scope = struct {
    parent: ?*Scope,
    payload: Payload,

    pub const Payload = union(enum) {
        /// Toplevel scope, does not contain anything.
        toplevel,
        /// Namespace (for example toplevel package) with const
        /// and type definitions. Names are unique within the
        /// namespace (no shadowing) and unordered.
        namespace: std.AutoHashMapUnmanaged(InternPool.Index, InternPool.Index),
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
        /// A constant definition with name and interned value.
        @"const": struct {
            name: InternPool.Index,
            value: InternPool.Index,
        },
    };

    pub const toplevel: Scope = .{
        .parent = null,
        .payload = .{ .toplevel = {} },
    };

    pub fn namespace(parent: *Scope) Scope {
        return .{
            .parent = parent,
            .payload = .{ .namespace = .empty },
        };
    }

    pub fn resolve(scope: *Scope, name: InternPool.Index) ?*Scope {
        var s = scope;
        while (true) {
            // std.debug.print("{any}\n", .{s.*});
            switch (s.payload) {
                .toplevel => return null,
                inline .def,
                .type,
                .@"const",
                => |*pl| if (pl.name == name) return s,
                .decl => |pl| if (pl == name) return s,
                .namespace => |*ns| {
                    if (ns.contains(name)) return s;
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

    pub fn find(scope: *Scope, tag: std.meta.Tag(Payload)) ?*Scope {
        var s: ?*Scope = scope;
        while (s) |next| {
            if (next.payload == tag) return next;
            s = next;
        }

        return null;
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

    pub const ResultInfo = struct {
        /// Defines the context in which an expression is being evaluated.
        /// This includes information about type coercion, compile time vs
        /// elaboration time semantics, and whether or not to discard.
        ctx: Context,
        /// The expression (type or value) being evaluated should
        /// adopt this name. If no name hint is specified, this is
        /// `null` and the expression is anonymous and may autogenerate.
        /// a name if one is necessary as a salt (i.e. bundle types).
        name: InternPool.Index = .null,

        pub const Context = union(enum) {
            /// The expression will be discarded. Only side effects
            /// need be elaborated.
            discard,
            /// The expression will be elaborated into a signal of
            /// inferred type.
            def_inferred,
            /// The expression will be elaborated into a signal of
            /// declared type, and must coerce into that type or error.
            def_type: InternPool.Index,
            /// The expression will be evaluated as a type.
            type,
            /// The expression must be eagerly evaluated in a compile time
            /// context and may use compile time only types. The type of
            /// the expression is inferred.
            const_inferred,
            /// The expression must be eagerly evaluated in a compile time
            /// context and may use compile time only types. The expression
            /// must coerce into the specified type or error.
            const_type: InternPool.Index,
        };

        pub const discard: ResultInfo = .{
            .ctx = .{ .discard = {} },
            .name = .null,
        };

        pub fn ty(name: InternPool.Index) ResultInfo {
            return .{
                .ctx = .{ .type = {} },
                .name = name,
            };
        }

        pub fn val(type_hint: ?InternPool.Index) ResultInfo {
            if (type_hint) |t| return .{ .ctx = .{ .def_type = t }, .name = .null };
            return .{ .ctx = .{ .def_inferred = {} }, .name = .null };
        }

        pub fn cst(type_hint: ?InternPool.Index) ResultInfo {
            if (type_hint) |t| return .{ .ctx = .{ .const_type = t }, .name = .null };
            return .{ .ctx = .{ .const_inferred = {} }, .name = .null };
        }
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

    fn addIndices(self: *Sema, ids: []const Index) !Air.Indices {
        const start: u32 = @intCast(self.extra.items.len);
        try self.extra.appendSlice(self.gpa, @ptrCast(ids));
        const end: u32 = @intCast(self.extra.items.len);

        return self.addExtra(Cst.ExtraSlice{
            .start = @enumFromInt(start),
            .end = @enumFromInt(end),
        });
    }

    fn addValues(self: *Sema, ids: []const Value) !Air.Values {
        const start: u32 = @intCast(self.extra.items.len);
        try self.extra.appendSlice(self.gpa, @ptrCast(ids));
        const end: u32 = @intCast(self.extra.items.len);

        return self.addExtra(Cst.ExtraSlice{
            .start = @enumFromInt(start),
            .end = @enumFromInt(end),
        });
    }

    fn putType(self: *Sema, ty: Type) !InternPool.Index {
        return self.pool.put(.{ .ty = ty });
    }

    fn addError(self: *Sema, tag: SourceError.Tag, token: Cst.TokenIndex) !void {
        try self.errors.append(self.arena, .{ .tag = tag, .token = token });
    }

    fn typeOf(self: *Sema, value: Value) InternPool.Index {
        return self.tempAir().typeOf(self.pool, value);
    }

    fn typeOfIndex(self: *Sema, index: Index) InternPool.Index {
        return self.tempAir().typeOfIndex(self.pool, index);
    }

    fn typeOfInterned(self: *Sema, index: InternPool.Index) InternPool.Index {
        return self.tempAir().typeOfInterned(self.pool, index);
    }

    // no Airs are generated here, but the InternPool is populated with
    // types and identifiers, and a new analysis is called on each module body
    pub fn root(self: *Sema) !void {
        // the root forms a namespace with implicit forward declaration,
        // so a two pass approach is used
        const payload = self.cst.payload(self.cst.root);
        const sl = self.cst.indices(payload.root);

        var toplevel: Scope = .toplevel;
        var namespace: Scope = .namespace(&toplevel);
        var s = &namespace;

        for (sl) |cst_index| {
            const decl, s = try self.typeDef(s, cst_index);
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
        const sl = self.cst.indices(data);
        var s = scope;

        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        try self.scratch.ensureUnusedCapacity(self.arena, sl.len);
        for (sl) |i| {
            const ai, s = try self.statement(s, i);
            if (ai) |node| self.scratch.appendAssumeCapacity(@intFromEnum(node));
        }

        const items: []const Index = @ptrCast(self.scratch.items[scratch_top..]);
        const nodes = try self.addIndices(items);
        return self.addNode(.{ .block = nodes });
    }

    fn statement(self: *Sema, s: *Scope, index: Cst.Index) !struct { ?Index, *Scope } {
        const data = self.cst.payload(index);

        return switch (data) {
            .def => out: {
                const i, const scope = try self.def(s, index);
                break :out .{ i, scope };
            },
            .type => out: {
                _, const scope = try self.typeDef(s, index);
                break :out .{ null, scope };
            },
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
            const ri: ResultInfo = .val(null);
            const value = try self.expression(s, ri, data.value);
            const ty = self.tempAir().typeOf(self.pool, value);
            break :tv .{ ty, value };
        } else tv: {
            // Otherwise, first evaluate the type, and then use it in the
            // result context for the value (i.e. to evaluate anonymous
            // literals).
            const ty = try self.type(s, .ty(.null), data.type);
            const ri: ResultInfo = .{
                .ctx = .{ .def_type = ty },
                .name = name,
            };
            const value = try self.expression(s, ri, data.value);
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
        const ri: ResultInfo = .val(signature.output_type);
        const value = try self.expression(s, ri, data);
        return self.addNode(.{ .yield = value });
    }

    fn typeDef(self: *Sema, s: *Scope, index: Cst.Index) !struct { InternPool.DeclIndex, *Scope } {
        const data = self.cst.payload(index).type;
        const ident_token = self.cst.mainToken(index).advance(1);
        const name = try self.internToken(ident_token);

        const ty = try self.type(s, .ty(name), data);
        const decl = try self.pool.createDecl(.{
            .kind = .type,
            .name = name,
            .type = ty,
        });

        try self.pool.decls_map.put(self.gpa, name, decl);

        if (self.scope == .root) {
            const scope = s.find(.namespace).?;
            try scope.payload.namespace.put(self.arena, name, ty);
            return .{ decl, s };
        } else {
            const scope = try self.arena.create(Scope);
            scope.* = .{
                .parent = s,
                .payload = .{ .type = .{
                    .name = name,
                    .type = ty,
                } },
            };

            return .{ decl, scope };
        }
    }

    fn @"type"(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) Error!InternPool.Index {
        const data = self.cst.payload(index);

        return switch (data) {
            .ident => self.identType(s, index),
            .bundle => self.bundle(s, ri, index),
            .array => self.array(s, ri, index),
            .module => self.module(s, ri, index),
            else => unimplemented(),
        };
    }

    fn identValue(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
        const token = self.cst.mainToken(index);
        const ident = try self.internToken(token);

        // Resolve the identifier in the current scope to validate
        // its existence. Then based on the result info, either:
        // (1) re-emit the identifier (source to source translation)
        // (2) inline the value (and possibly intern it)
        const resolved = s.resolve(ident);
        const rs = if (resolved) |rs| scope: {
            break :scope rs;
        } else {
            try self.addError(.unknown_identifier, token);
            return error.SourceError;
        };

        const value = switch (ri.ctx) {
            .discard => unimplemented(),
            // Type hint is not provided, so just resolve the type of the
            // underlying definition and record it here (scope information
            // is not preserved after semantic analysis so type is needed).
            .def_inferred => value: {
                const signal = rs.payload.def.index;
                const ty = self.typeOfIndex(signal);
                const node = try self.addNode(.{ .ident = .{ .signal = signal, .type = ty } });
                break :value Value.index(node);
            },
            // Type hint is provided, so valid it against the type of the
            // signal that was resolved.
            .def_type => |hint| value: {
                const signal = rs.payload.def.index;
                const ty = self.typeOfIndex(signal);
                std.debug.assert(hint != .null);
                if (ty != hint) {
                    try self.addError(.type_coerce_fail, token);
                    return error.SourceError;
                }
                const node = try self.addNode(.{ .ident = .{ .signal = signal, .type = ty } });
                break :value Value.index(node);
            },
            .type => unreachable,
            // The const contexts are similar to the elaboration contexts
            // `.def_inferred` and `.def_type` but validate that the resulting
            // expression is compile time known and inline the result instead
            // of re-emitting a node.
            .const_inferred => value: {
                const value = rs.payload.@"const".value;
                break :value Value.ip(value);
            },
            .const_type => |hint| value: {
                const value = rs.payload.@"const".value;

                const ty = self.typeOfInterned(value);
                std.debug.assert(hint != .null);
                if (ty != hint) {
                    try self.addError(.type_coerce_fail, token);
                    return error.SourceError;
                }
                break :value Value.ip(value);
            },
        };

        return value;
    }

    fn identType(self: *Sema, s: *Scope, index: Cst.Index) !InternPool.Index {
        const token = self.cst.mainToken(index);
        const str = self.cst.tokenString(token);
        const ident = try self.pool.put(.{ .str = str });

        // TODO: check reserved types in declaration
        if (ident == .builtin_bool) {
            return .bool;
        }

        // First check the identifier against the builtin types.
        // Because the bit vector types (bits, uint, sint) can
        // be of any width, they cannot be exhaustively interned
        // and are instead checked and lazily interned when used.
        const builtin: ?InternPool.Index = switch (str[0]) {
            // FIXME: this will incorrectly silence an allocator error
            'b' => self.bits(str) catch null,
            'u' => self.uint(str) catch null,
            'i' => self.sint(str) catch null,
            else => null,
        };

        // If there is a match, no need to resolve the identifier.
        if (builtin) |ip| {
            return ip;
        }

        // Not builtin, so resolve the identifier in the current scope.
        const resolved = s.resolve(ident);
        // std.debug.print("{any}\n", .{resolved});
        if (resolved) |rs| {
            switch (rs.payload) {
                .toplevel => unreachable,
                .def, .@"const", .decl => {
                    try self.addError(.shadow_type_signal, token);
                    return error.SourceError;
                },
                .type => |*ty| {
                    return ty.type;
                },
                .namespace => |*ns| {
                    return ns.get(ident).?;
                },
            }
        }

        try self.addError(.unknown_identifier, token);
        return error.SourceError;
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

    fn bundle(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !InternPool.Index {
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
            ty.* = try self.type(s, .ty(.null), field);
        }

        return self.pool.put(.{
            .ty = .{
                .bundle = .{
                    .cst_index = index,
                    .name = ri.name,
                    .field_names = names,
                    .field_types = types,
                },
            },
        });
    }

    fn array(self: *Sema, s: *Scope, _: ResultInfo, index: Cst.Index) !InternPool.Index {
        const data = self.cst.payload(index).array;

        const len_expression = try self.expression(s, .cst(.int), data.len);
        const len = self.pool.get(len_expression.asInterned().?).tv.val.int;
        const element_type = try self.type(s, .ty(.null), data.child);

        return self.pool.put(.{
            .ty = .{
                .array = .{
                    .element_type = element_type,
                    .len = @intCast(len),
                },
            },
        });
    }

    fn expression(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) Error!Value {
        const data = self.cst.payload(index);

        return switch (data) {
            .bool => self.bool(s, ri, index),
            .integer => self.integer(s, ri, index),
            .ident => self.identValue(s, ri, index),
            .unary => self.unary(s, ri, index),
            .binary => self.binary(s, ri, index),
            .bundle_literal => self.bundleLiteral(s, ri, index),
            .array_literal => self.arrayLiteral(s, ri, index),
            .subscript => self.subscript(s, ri, index),
            .field_access => self.fieldAccess(s, ri, index),
            .slice => self.slice(s, ri, index),
            else => unimplemented(),
        };
    }

    fn @"bool"(self: *Sema, _: *Scope, _: ResultInfo, index: Cst.Index) !Value {
        const token = self.cst.mainToken(index);
        const tag = self.cst.tokenTag(token);
        return switch (tag) {
            .k_true => Value.ip(.btrue),
            .k_false => Value.ip(.bfalse),
            else => unreachable,
        };
    }

    fn integer(self: *Sema, _: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
        const token = self.cst.mainToken(index);
        const str = self.cst.tokenString(token);
        std.debug.assert(str.len > 0);

        const value = try std.fmt.parseInt(i64, str, 0);

        return switch (ri.ctx) {
            .discard => unimplemented(),
            // If the expression does not have a type hint in an elaboration
            // context, it is a compiler error.
            // NOTE: While it would be trivial to implement semantics that
            // guess a "safe" bitwidth and type to elaborate the literal, this
            // would be of limited use because the resulting type does not
            // easily coerce for use in an expression downstream.
            // FIXME: compiler error
            .def_inferred => unimplemented(),
            // If the expression has a type hint, try to coerce the integer
            // to that type by checking that the value can be represented
            // losslessly.
            // NOTE: Integers should never be of `.int` (compile time int)
            // type in this context, compile time expressions use the
            // `.const` context.
            .def_type => |hint| val: {
                const ty = self.pool.get(hint).ty;
                std.debug.assert((ty == .uint) or (ty == .sint) or (ty == .bits));
                // FIXME: actually check if coercion is legal properly
                if (ty == .uint) std.debug.assert(value >= 0);

                const tv: TypedValue = .{ .ty = hint, .val = .{ .int = value } };
                const ip = try self.pool.put(.{ .tv = tv });
                break :val Value.ip(ip);
            },
            .type => unreachable,
            // When inferring the type in a comptime context, the integer
            // is not coerced into any concrete type.
            .const_inferred => val: {
                const tv: TypedValue = .{ .ty = .int, .val = .{ .int = value } };
                const ip = try self.pool.put(.{ .tv = tv });
                break :val Value.ip(ip);
            },
            // When a type hint is provided in a comptime context, try to
            // coerce the integer to that type similar to `.def_type`
            .const_type => |hint| val: {
                const ty = self.pool.get(hint).ty;
                // TODO: implement comptime manipulation of runtime types
                std.debug.assert(ty == .int);
                const tv: TypedValue = .{ .ty = hint, .val = .{ .int = value } };
                const ip = try self.pool.put(.{ .tv = tv });
                break :val Value.ip(ip);
            },
        };
    }

    // fn identExpression(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
    //     const token = self.cst.mainToken(index);
    //     const ident = try self.internToken(token);
    //
    //     // Resolve the identifer in the current scope to validate
    //     // its existence. If valid, the signal is recorded and the
    //     // identifier is re-emitted, not inlined (because this is
    //     // a source to source translation).
    //     const resolved = s.resolve(ident);
    //     const signal = if (resolved) |rs| v: {
    //         break :v rs.payload.def.index;
    //     } else {
    //         try self.addError(.unknown_identifier, token);
    //         return error.SourceError;
    //     };
    //
    //     // If a type hint is provided in the context, validate it
    //     // against the type of the signal that was resolved.
    //     const ty = self.typeOf(Value.index(signal));
    //     if (ri.ctx == .def_type and ri.ctx.def_type != ty) {
    //         try self.addError(.type_coerce_fail, token);
    //         return error.SourceError;
    //     }
    //
    //     const node = try self.addNode(.{
    //         .ident = .{
    //             .signal = signal,
    //             .type = ty,
    //         },
    //     });
    //     return Value.index(node);
    // }

    fn unary(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
        const data = self.cst.payload(index).unary;
        const main_token = self.cst.mainToken(index);
        const operator = self.cst.tokenTag(main_token);

        const inner = try self.expression(s, ri, data);

        // check that the expression type is compatible with the operator
        const ty = self.typeOf(inner);
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

    fn binary(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
        const data = self.cst.payload(index).binary;
        const main_token = self.cst.mainToken(index);
        const operator = self.cst.tokenTag(main_token);

        const l = try self.expression(s, .val(null), data.l);
        const r = try self.expression(s, .val(null), data.r);

        // check that both expressions have the same type
        const lty = self.typeOf(l);
        const rty = self.typeOf(r);
        if (lty != rty) {
            std.debug.print("{} {}\n", .{ self.pool.get(lty).ty, self.pool.get(rty).ty });
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
            // TODO: implement equality for aggregate types
            .equal_equal,
            .bang_equal,
            => ty == .bits or ty == .uint or ty == .sint or ty == .bool,
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
        if (ri.ctx == .def_type and ri.ctx.def_type != dty) {
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
            .equal_equal => .{ .eq = bin },
            .bang_equal => .{ .ne = bin },
            else => self.unexpectedToken(main_token),
        };

        return Value.index(try self.addNode(node));
    }

    fn bundleLiteral(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
        const data = self.cst.payload(index).bundle_literal;
        const inits = self.cst.indices(data.inits);

        const bundle_type = if (data.type != .null) type: {
            // if explicitly specified, no need to infer from context
            const ty = try self.type(s, .ty(.null), data.type);
            break :type ty;
        } else type: {
            // otherwise use the context
            // TODO: add a comptime compound literal type and add support
            // for the `.const_inferred` type here
            const inferred = ri.ctx.def_type;
            break :type inferred;
        };

        const ty = bundle: {
            const ty = self.pool.get(bundle_type).ty;
            switch (ty) {
                .bundle => |val| break :bundle val,
                else => {
                    const main_token = self.cst.mainToken(index);
                    try self.addError(.type_coerce_fail, main_token);
                    return error.SourceError;
                },
            }
        };

        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        const values = try self.allocScratchSlice(Value, inits.len);
        @memset(values, .index(.null));

        for (inits) |idx| {
            const field_init = self.cst.payload(idx).field_init;
            const main_token = self.cst.mainToken(idx);
            const init_name = try self.internToken(main_token);
            // Attempt to find the initializer name in the bundle fields,
            // since it may be declared out of order. The underlying type
            // defines the canonical order.
            const names = ty.field_names;
            const i = for (names, 0..) |name, i| {
                if (name == init_name) {
                    const v = values[i].asIndex();
                    // If the slot has already been specified, this is a
                    // duplicate field init, which is an error.
                    if (v != .null) {
                        try self.addError(.field_duplicate, main_token);
                        return error.SourceError;
                    } else {
                        break i;
                    }
                }
            } else {
                // The field name was not found in the underlying type.
                try self.addError(.field_unknown, main_token);
                return error.SourceError;
            };

            const field_type = ty.field_types[i];
            const field_ri: ResultInfo = .val(field_type);
            values[i] = try self.expression(s, field_ri, field_init);
        }

        const node = try self.addNode(.{
            .bundle_literal = .{
                .type = bundle_type,
                .inits = try self.addValues(values),
            },
        });
        return Value.index(node);
    }

    fn arrayLiteral(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
        const data = self.cst.payload(index).array_literal;
        const inits = self.cst.indices(data);

        // TODO: add a comptime compound literal type and add support
        // for the `.const_inferred` type here
        // TODO: some kind of type inference?
        const array_type = ri.ctx.def_type;

        const ty = array: {
            const ty = self.pool.get(array_type).ty;
            switch (ty) {
                .array => |val| break :array val,
                else => {
                    const main_token = self.cst.mainToken(index);
                    try self.addError(.type_coerce_fail, main_token);
                    return error.SourceError;
                },
            }
        };

        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        const values = try self.allocScratchSlice(Value, inits.len);
        @memset(values, .index(.null));

        // FIXME: this should be an error
        std.debug.assert(inits.len == ty.len);

        for (inits, 0..) |idx, i| {
            const element_ri: ResultInfo = .val(ty.element_type);
            values[i] = try self.expression(s, element_ri, idx);
        }

        const node = try self.addNode(.{
            .array_literal = .{
                .type = array_type,
                .inits = try self.addValues(values),
            },
        });
        return Value.index(node);
    }

    fn subscript(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
        const data = self.cst.payload(index).subscript;
        const operand = try self.expression(s, ri, data.operand);
        const ty = self.pool.get(self.typeOf(operand)).ty;

        const idx_ri: ResultInfo = .{ .ctx = .{ .const_type = .int } };
        const idx = try self.expression(s, idx_ri, data.index);

        const node = switch (ty) {
            .bits, .uint, .sint => try self.addNode(.{
                .bit = .{
                    .operand = operand,
                    .index = idx,
                },
            }),
            .array => try self.addNode(.{
                .subscript = .{
                    .operand = operand,
                    .index = idx,
                },
            }),
            // FIXME: error
            else => unreachable,
        };

        return Value.index(node);
    }

    fn slice(self: *Sema, s: *Scope, _: ResultInfo, index: Cst.Index) !Value {
        const data = self.cst.payload(index).slice;
        const bounds = self.cst.extraData(data.bounds, Cst.Node.Bounds);
        const operand = try self.expression(s, .val(null), data.operand);
        const oty = self.pool.get(self.typeOf(operand)).ty;
        std.debug.assert((oty == .bits) or (oty == .uint) or (oty == .sint));

        const idx_ri: ResultInfo = .{ .ctx = .{ .const_type = .int } };
        const upper = try self.expression(s, idx_ri, bounds.upper);
        const lower = try self.expression(s, idx_ri, bounds.lower);

        std.debug.assert(upper.tag == .ip);
        std.debug.assert(lower.tag == .ip);
        const dty = ty: {
            const end: u32 = @intCast(self.pool.get(upper.payload.ip).tv.val.int);
            const start: u32 = @intCast(self.pool.get(lower.payload.ip).tv.val.int);
            std.debug.assert(end >= start);
            const len = end - start;
            break :ty try self.pool.put(.{ .ty = .{ .bits = len } });
        };

        const extra = try self.addExtra(Node.BitSlice{
            .operand = operand,
            .upper = upper.payload.ip,
            .lower = lower.payload.ip,
        });

        const node = try self.addNode(.{
            .bitslice = .{
                .bitslice = extra,
                .type = dty,
            },
        });
        return Value.index(node);
    }

    fn fieldAccess(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !Value {
        const field = self.cst.payload(index).field_access;
        const ident_token = self.cst.mainToken(index);
        const name = try self.internToken(ident_token);
        const operand = try self.expression(s, .val(null), field);

        const bundle_type = bundle: {
            const ip = self.typeOf(operand);
            const ty = self.pool.get(ip).ty;
            switch (ty) {
                .bundle => |val| break :bundle val,
                else => {
                    const main_token = self.cst.mainToken(index);
                    try self.addError(.type_coerce_fail, main_token);
                    return error.SourceError;
                },
            }
        };

        const i = for (bundle_type.field_names, 0..) |field_name, i| {
            if (name == field_name) {
                break i;
            }
        } else {
            // The field name was not found in the underlying type.
            try self.addError(.field_unknown, ident_token);
            return error.SourceError;
        };

        // If a type hint is provided in the context, validate it
        // against the type of the signal that was resolved.
        const dty = bundle_type.field_types[i];
        if (ri.ctx == .def_type and ri.ctx.def_type != dty) {
            try self.addError(.type_coerce_fail, ident_token);
            return error.SourceError;
        }

        const node = try self.addNode(.{
            .field = .{
                .operand = operand,
                .index = @intCast(i),
            },
        });
        return Value.index(node);
    }

    fn module(self: *Sema, s: *Scope, ri: ResultInfo, index: Cst.Index) !InternPool.Index {
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
            const ty = try self.type(s, .ty(.null), input);

            input_names[i] = name;
            input_types[i] = ty;
        }

        const output_type = try self.type(s, .ty(.null), ports.output);
        const signature = try self.putType(.{
            .signature = .{
                .input_names = input_names,
                .input_types = input_types,
                .output_type = output_type,
            },
        });

        var sema = try Sema.init(self.gpa, self.arena, self.pool, self.cst, signature, .body);
        errdefer sema.deinit();

        // the parameter nodes should actually exist in the inner scope
        var scope = s;
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
                .name = ri.name,
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
