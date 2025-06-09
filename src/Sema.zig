const std = @import("std");
const Cst = @import("Cst.zig");
const Air = @import("Air.zig");
const InternPool = @import("InternPool.zig");
const Type = @import("type.zig").Type;

const Allocator = std.mem.Allocator;
const Index = Air.Index;
const Node = Air.Node;
const Value = Air.Value;

const Error = Allocator.Error || std.fmt.ParseIntError;

pub fn analyze(gpa: Allocator, pool: *InternPool, cst: *Cst) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    // NOTE: this prevents a memory leak if the parser fails, but ownership should be
    // changed if we eventually want to print something using the source on failure
    errdefer cst.deinit(gpa);

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
    kind: Kind,
    // FIXME: because this map only contains named signals,
    // which are always defined (not anonymous expressions)
    // we should use Index here and not Value
    map: std.AutoHashMapUnmanaged(InternPool.Index, Value),
    parent: ?*Scope,

    const Kind = enum {
        toplevel,
        body,
    };
};

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
    module_type: InternPool.Index,

    nodes: std.MultiArrayList(Node),
    extra: std.ArrayListUnmanaged(u32),
    scratch: std.ArrayListUnmanaged(u32),

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
        // when not null, the expression being evaluated should
        // attempt to coerce into this type
        type: ?InternPool.Index,
    };

    pub fn init(gpa: Allocator, arena: Allocator, pool: *InternPool, cst: *const Cst, module_type: InternPool.Index, scope: ScopeKind) !Sema {
        return .{
            .gpa = gpa,
            .arena = arena,
            .pool = pool,
            .cst = cst,
            .scope = scope,
            .module_type = module_type,
            .nodes = .empty,
            .extra = .empty,
            .scratch = .empty,
        };
    }

    pub fn deinit(self: *Sema) void {
        self.nodes.deinit(self.gpa);
        self.extra.deinit(self.gpa);
        self.scratch.deinit(self.arena);
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

    // no Airs are generated here, but the InternPool is populated with
    // types and identifiers, and a new analysis is called on each module body
    pub fn root(self: *Sema) !void {
        // the root forms a namespace with implicit forward declaration,
        // so a two pass approach is used
        const payload = self.cst.payload(self.cst.root);
        const slice = self.cst.indices(payload.root);
        for (slice) |cst_index| {
            const index = try self.typeStatement(cst_index);
            _ = index;
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

    fn block(self: *Sema, s: *Scope, index: Cst.Index) !Index {
        const data = self.cst.payload(index).block;
        const slice = self.cst.indices(data);

        const len = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(len);
        try self.scratch.ensureUnusedCapacity(self.arena, slice.len);
        for (slice) |i| {
            const ai = try self.statement(s, i);
            self.scratch.appendAssumeCapacity(@intFromEnum(ai));
        }

        return .null;
    }

    fn statement(self: *Sema, s: *Scope, index: Cst.Index) !Index {
        const data = self.cst.payload(index);

        return switch (data) {
            .def => self.def(s, index),
            .yield => self.yield(s, index),
            else => unimplemented(),
        };
    }

    fn def(self: *Sema, s: *Scope, index: Cst.Index) !Index {
        const data = self.cst.payload(index).def;
        const ident_token = self.cst.mainToken(index).advance(1);
        const str = self.cst.tokenString(ident_token);
        const signal: Node.Signal = .{
            .name = try self.pool.put(.{ .str = str }),
            // FIXME: implement this
            .type = .int,
        };

        // FIXME: implement type context
        const value = try self.expression(s, data.value, .{ .type = null });
        const node = try self.addNode(.{
            .def = .{
                .signal = try self.addExtra(signal),
                .value = value,
            },
        });

        try s.map.put(self.arena, signal.name, Value.index(node));
        return node;
    }

    fn yield(self: *Sema, s: *Scope, index: Cst.Index) !Index {
        const data = self.cst.payload(index).yield;

        const module_type = self.pool.get(self.module_type).ty.module;
        const context: Context = .{ .type = module_type.output_type };
        const value = try self.expression(s, data, context);
        _ = value;
        return .null;
    }

    fn typeStatement(self: *Sema, index: Cst.Index) !Index {
        const data = self.cst.payload(index).type;
        _ = try self.type(data);

        return .null;
    }

    fn @"type"(self: *Sema, index: Cst.Index) Error!InternPool.Index {
        const data = self.cst.payload(index);

        return switch (data) {
            .ident => self.identType(index),
            .bundle => self.bundle(index),
            .module => self.module(index),
            else => unimplemented(),
        };
    }

    fn identType(self: *Sema, index: Cst.Index) !InternPool.Index {
        const token = self.cst.mainToken(index);
        const str = self.cst.tokenString(token);

        const builtin: ?InternPool.Index = switch (str[0]) {
            'b' => try self.bits(str),
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

    fn bundle(self: *Sema, index: Cst.Index) !InternPool.Index {
        const data = self.cst.payload(index).bundle;
        const indices = self.cst.indices(data);

        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        const names = try self.allocScratchSlice(InternPool.Index, indices.len);
        const types = try self.allocScratchSlice(InternPool.Index, indices.len);

        for (indices, names, types) |idx, *name, *ty| {
            const field = self.cst.payload(idx).field;
            const token = self.cst.mainToken(idx);
            name.* = try self.internToken(token);
            ty.* = try self.type(field);
        }

        return self.pool.put(.{
            .ty = .{
                .bundle = .{
                    .field_names = names,
                    .field_types = types,
                },
            },
        });
    }

    fn expression(self: *Sema, s: *Scope, index: Cst.Index, ctx: Context) Error!Value {
        const data = self.cst.payload(index);

        return switch (data) {
            .ident => self.identExpression(s, ctx, index),
            .unary => self.unary(s, ctx, index),
            .binary => self.binary(s, ctx, index),
            .bundle_literal => self.bundleLiteral(s, ctx, index),
            else => unimplemented(),
        };
    }

    fn identExpression(self: *Sema, s: *Scope, _: Context, index: Cst.Index) !Value {
        const token = self.cst.mainToken(index);
        const ident = try self.internToken(token);

        // FIXME: implement traversal across the scope list
        return s.map.get(ident).?;
    }

    fn unary(self: *Sema, s: *Scope, ctx: Context, index: Cst.Index) !Value {
        const data = self.cst.payload(index).unary;
        const main_token = self.cst.mainToken(index);
        const operator = self.cst.tokenTag(main_token);

        const inner = try self.expression(s, data, ctx);
        const node: Node = switch (operator) {
            .minus => .{ .ineg = inner },
            .tilde => .{ .bnot = inner },
            .k_not => .{ .lnot = inner },
            .l_paren => .{ .paren = inner },
            else => self.unexpectedToken(main_token),
        };

        return Value.index(try self.addNode(node));
    }

    fn binary(self: *Sema, s: *Scope, ctx: Context, index: Cst.Index) !Value {
        const data = self.cst.payload(index).binary;
        const main_token = self.cst.mainToken(index);
        const operator = self.cst.tokenTag(main_token);

        const l = try self.expression(s, data.l, ctx);
        const r = try self.expression(s, data.r, ctx);
        const bin: Node.Binary = .{ .l = l, .r = r };
        const node: Node = switch (operator) {
            .plus => .{ .iadd = bin },
            .minus => .{ .isub = bin },
            .ampersand => .{ .band = bin },
            .pipe => .{ .bor = bin },
            .caret => .{ .bxor = bin },
            .k_and => .{ .land = bin },
            .k_or => .{ .lor = bin },
            .k_xor => .{ .lxor = bin },
            .k_implies => .{ .limplies = bin },
            else => self.unexpectedToken(main_token),
        };

        return Value.index(try self.addNode(node));
    }

    fn bundleLiteral(self: *Sema, s: *Scope, ctx: Context, index: Cst.Index) !Value {
        const data = self.cst.payload(index).bundle_literal;
        const inits = self.cst.indices(data.inits);

        const bundle_type = if (data.type != .null) type: {
            // if explicitly specified, no need to infer from context
            const ty = try self.type(data.type);
            break :type ty;
        } else type: {
            // otherwise use the context
            // FIXME: if this is null it is a compiler error
            const inferred = ctx.type.?;
            break :type inferred;
        };

        const ty = self.pool.get(bundle_type).ty.bundle;

        // TODO: actually validate the struct
        const values = try self.allocScratchSlice(Value, inits.len);
        for (inits) |idx| {
            const field_init = self.cst.payload(idx).field_init;
            const main_token = self.cst.mainToken(idx);
            const init_name = try self.internToken(main_token);
            const value = try self.expression(s, field_init, undefined);
            const i = for (ty.field_names, 0..) |name, i| {
                // const S = self.pool.get(name).str;
                // const T = self.pool.get(init_name).str;
                // std.debug.print("{}: {s} {}: {s}\n", .{ name, S, init_name, T });
                if (name == init_name) break i;
            } else unreachable;
            values[i] = value;
        }

        for (values) |value| std.debug.print("{}\n", .{value.payload.index});
        // std.debug.print("{any}\n", .{values});
        return Value.index(.null);
    }

    fn module(self: *Sema, index: Cst.Index) !InternPool.Index {
        const data = self.cst.payload(index).module;
        const ports = self.cst.payload(data.ports).ports;
        const inputs = self.cst.indices(ports.inputs);

        const scratch_top = self.scratch.items.len;
        defer self.scratch.shrinkRetainingCapacity(scratch_top);
        const input_names = try self.allocScratchSlice(InternPool.Index, inputs.len);
        const input_types = try self.allocScratchSlice(InternPool.Index, inputs.len);

        var scope: Scope = .{
            .kind = .body,
            .map = .empty,
            .parent = null,
        };
        const map = &scope.map;
        try map.ensureUnusedCapacity(self.arena, @intCast(inputs.len));

        for (inputs, 0..) |idx, i| {
            const input = self.cst.payload(idx).input;
            const token = self.cst.mainToken(idx);
            const name = try self.internToken(token);
            const ty = try self.type(input);
            const param = try self.addNode(.{
                .param = .{
                    .index = @intCast(i),
                    .type = ty,
                },
            });

            input_names[i] = name;
            input_types[i] = ty;
            map.putAssumeCapacity(name, Value.index(param));
        }

        const output_type = try self.type(ports.output);
        const signature: Type = .{
            .module = .{
                .input_names = input_names,
                .input_types = input_types,
                .output_type = output_type,
            },
        };
        const module_type = try self.pool.put(.{ .ty = signature });

        var sema = try Sema.init(self.gpa, self.arena, self.pool, self.cst, module_type, .body);
        errdefer sema.deinit();
        var air = try sema.body(&scope, data.body);
        defer air.deinit(self.gpa);

        return .int;
    }

    fn getTempAir(self: *Sema) Air {
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
