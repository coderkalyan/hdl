const std = @import("std");
const lex = @import("lex.zig");
const Cst = @import("Cst.zig");
const Lexer = lex.Lexer;

const Allocator = std.mem.Allocator;
const Token = lex.Token;
const Node = Cst.Node;
const TokenIndex = Cst.TokenIndex;
const Index = Cst.Index;
const Item = Cst.Item;

pub const Error = error{UnexpectedToken} || Allocator.Error;

// parses a string of source characters into a concrete syntax tree
// gpa: allocator for tree data that outlives this function call
pub fn parse(gpa: Allocator, source: [:0]const u8) Error!Cst {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    // NOTE: this prevents a memory leak if the parser fails, but ownership should be
    // changed if we eventually want to print something using the source on failure
    errdefer gpa.free(source);

    // lex entire source file into token list
    var tokens: Cst.TokenList = .empty;
    errdefer tokens.deinit(gpa);
    var lexer: lex.Lexer = .init(source);
    while (true) {
        const token = lexer.next();
        try tokens.append(gpa, .{ .tag = token.tag, .start = token.loc.start });
        if (token.tag == .eof) break;
    }

    // initialize parser
    var parser = try Parser.init(gpa, arena.allocator(), source, tokens.slice());
    defer parser.deinit();

    // for (parser.tokens.items(.tag)) |tag| {
    //     std.debug.print("{} ", .{tag});
    // }
    // std.debug.print("\n", .{});

    _ = try parser.toplevel();

    // copy parser results into an abstract syntax tree
    // that owns the source, token list, node list, and node extra data
    return .{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .items = parser.items.toOwnedSlice(),
        .extra = try parser.extra.toOwnedSlice(gpa),
    };
}

pub const Parser = struct {
    gpa: Allocator,
    arena: Allocator,

    // original source string, stored since tokens and ast do not keep
    // any owned copies of strings/spans
    source: [:0]const u8,
    tokens: Cst.TokenList.Slice,
    // current index into the token list
    index: TokenIndex,

    items: std.MultiArrayList(Item),
    extra: std.ArrayListUnmanaged(u32),
    scratch: std.ArrayListUnmanaged(u32),

    const ExpressionContext = enum {
        value,
        type,
    };

    pub fn init(gpa: Allocator, arena: Allocator, source: [:0]const u8, tokens: Cst.TokenList.Slice) !Parser {
        var parser: Parser = .{
            .gpa = gpa,
            .arena = arena,
            .source = source,
            .tokens = tokens,
            .index = @enumFromInt(0),
            .items = .empty,
            .extra = .empty,
            .scratch = .empty,
        };

        // initialize the parser with a single default node that "fills up" the
        // null slot
        const null_node = try parser.addNode(.{
            .main_token = .null,
            .payload = .{ .null = {} },
        });
        std.debug.assert(null_node == .null);

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.items.deinit(self.gpa);
        self.extra.deinit(self.gpa);
        self.scratch.deinit(self.arena);
    }

    fn addNode(p: *Parser, node: Node) !Index {
        const len: u32 = @intCast(p.items.len);
        const item = try node.serialize(p);
        try p.items.append(p.gpa, item);
        return @enumFromInt(len);
    }

    // eats the current token (whatever it is) and returns the index
    fn eatCurrent(p: *Parser) TokenIndex {
        const index = p.index;
        p.index = @enumFromInt(@intFromEnum(p.index) + 1);
        return index;
    }

    // eats the current token if it matches a tag, and returns null otherwise
    fn eat(p: *Parser, tag: Token.Tag) ?TokenIndex {
        if (p.tokens.items(.tag)[@intFromEnum(p.index)] == tag) {
            return p.eatCurrent();
        } else {
            return null;
        }
    }

    // east the current token if it matches a tag, and errors otherwise
    fn expect(p: *Parser, tag: Token.Tag) Error!TokenIndex {
        if (p.eat(tag)) |token| {
            return token;
        } else {
            return error.UnexpectedToken;
        }
    }

    inline fn current(p: *Parser) Token.Tag {
        return p.tokens.items(.tag)[@intFromEnum(p.index)];
    }

    // this can quite easily go out of bounds, so it should only be used
    // after checking for the guard token (eof)
    inline fn next(p: *Parser, offset: u32) Token.Tag {
        return p.token_tags[p.index + offset];
    }

    fn addExtra(p: *Parser, extra: anytype) Allocator.Error!Cst.ExtraIndex {
        const len: u32 = @intCast(p.extra.items.len);
        const struct_fields = std.meta.fields(@TypeOf(extra));
        try p.extra.ensureUnusedCapacity(p.gpa, struct_fields.len);
        inline for (struct_fields) |struct_field| {
            switch (struct_field.type) {
                inline else => {
                    const num = @intFromEnum(@field(extra, struct_field.name));
                    p.extra.appendAssumeCapacity(num);
                },
            }
        }
        return @enumFromInt(len);
    }

    pub fn tokenString(p: *Parser, index: TokenIndex) []const u8 {
        const tokens = p.tokens;
        const token_start = tokens.items(.start)[@intFromEnum(index)];
        var lexer = Lexer.initIndex(p.source, token_start);
        const token = lexer.next();

        return p.source[token.loc.start..token.loc.end];
    }

    pub fn extraSlice(p: *Parser, sl: Cst.ExtraSlice) []const u32 {
        const start: u32 = @intFromEnum(sl.start);
        const end: u32 = @intFromEnum(sl.end);
        return p.extra.items[start..end];
    }

    pub fn addSlice(p: *Parser, sl: []const u32) !Cst.ExtraIndex {
        const start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(p.gpa, sl);
        const end: u32 = @intCast(p.extra.items.len);
        return p.addExtra(Cst.ExtraSlice{
            .start = @enumFromInt(start),
            .end = @enumFromInt(end),
        });
    }

    fn parseList(p: *Parser, comptime element: anytype, surround: struct { open: Token.Tag, close: Token.Tag }) ![]const Cst.Index {
        _ = try p.expect(surround.open);

        const scratch_top = p.scratch.items.len;
        // defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eat(surround.close)) |_| break;
            const element_node = try element(p);
            try p.scratch.append(p.arena, @intFromEnum(element_node));

            if (p.current() == .comma) {
                _ = p.eatCurrent();
            } else if (p.current() != surround.close) {
                return error.UnexpectedToken;
            }
        }

        const elements = p.scratch.items[scratch_top..];
        return @ptrCast(elements);
    }

    const min_precedence: i32 = 0;

    inline fn precedence(tag: Token.Tag) i32 {
        return switch (tag) {
            .k_or => 10,
            .k_and => 11,
            .equal_equal => 13,
            .bang_equal => 13,
            .l_angle => 14,
            .r_angle => 14,
            .plus => 20,
            .minus => 20,
            .pipe => 30,
            .ampersand => 31,
            .caret => 32,
            .r_angle_r_angle => 33,
            .l_angle_l_angle => 33,
            else => -1,
        };
    }

    pub fn toplevel(p: *Parser) !Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const node = switch (p.current()) {
                .eof => break,
                .k_module => try p.module(),
                else => try p.statement(),
            };

            try p.scratch.append(p.arena, @intFromEnum(node));
        }

        const stmts: []const Index = @ptrCast(p.scratch.items[scratch_top..]);
        return p.addNode(.{
            .main_token = .null,
            .payload = .{ .toplevel = stmts },
        });
    }

    pub fn block(p: *Parser) !Index {
        const block_token = try p.expect(.l_brace);

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eat(.r_brace)) |_| break;
            const node = try p.statement();
            try p.scratch.append(p.arena, @intFromEnum(node));
        }

        const stmts: []const Index = @ptrCast(p.scratch.items[scratch_top..]);
        return p.addNode(.{
            .main_token = block_token,
            .payload = .{ .block = stmts },
        });
    }

    fn expression(p: *Parser, comptime context: ExpressionContext) Error!Cst.Index {
        switch (context) {
            .value => {
                const expr = try p.unary(true);
                return p.associateBinary(expr, min_precedence);
            },
            .type => return switch (p.current()) {
                .ident => p.identifier(),
                .l_paren => p.paren(.type),
                .k_module => p.module(),
                .l_bracket => p.array(),
                .k_bundle => p.bundle(),
                .k_enum => p.@"enum"(),
                // .k_union => p.unionType(),
                else => unreachable,
            },
        }
    }

    fn primary(p: *Parser, accept_l_brace: bool) Error!Cst.Index {
        _ = accept_l_brace;
        return switch (p.current()) {
            .l_paren => p.paren(.value),
            .ident => p.identifier(),
            .period => p.structLiteral(),
            .int => p.addNode(.{
                .main_token = p.eatCurrent(),
                .payload = .{ .integer = {} },
            }),
            .k_true, .k_false => p.addNode(.{
                .main_token = p.eatCurrent(),
                .payload = .{ .bool = {} },
            }),
            else => {
                std.debug.print("{}\n", .{p.current()});
                return Error.UnexpectedToken;
            },
        };
    }

    fn associateBinary(p: *Parser, l: Cst.Index, expr_precedence: i32) !Cst.Index {
        // tries to associate an existing "left side" node with a right side
        // in one or more binary expressions - operator precedence parsing
        var l_node = l;
        while (true) {
            const prec = precedence(p.current());
            if (prec < expr_precedence) {
                return l_node;
            }

            const op_token = p.eatCurrent();
            var r_node = try p.unary(false);

            const next_prec = precedence(p.current());
            if (prec < next_prec) {
                r_node = try p.associateBinary(r_node, prec + 1);
            }

            l_node = try p.addNode(.{
                .main_token = op_token,
                .payload = .{
                    .binary = .{
                        .l = l_node,
                        .r = r_node,
                    },
                },
            });
        }
    }

    fn postfix(p: *Parser, accept_l_brace: bool) Error!Cst.Index {
        var expr = try p.primary(accept_l_brace);

        while (true) {
            expr = switch (p.current()) {
                // handles subscript and slice, since we can't
                // yet look far enough to know which it is
                // .l_bracket => try p.subscript(expr),
                // .period => try p.attribute(expr),
                // .l_paren => try p.call(expr),
                else => return expr,
            };
        }
    }

    fn paren(p: *Parser, comptime context: ExpressionContext) !Cst.Index {
        const l_paren_token = try p.expect(.l_paren);
        const inner_node = try p.expression(context);
        _ = try p.expect(.r_paren);

        return p.addNode(.{
            .main_token = l_paren_token,
            .payload = .{ .unary = inner_node },
        });
    }

    // identifier used as an expression (like a variable or type name)
    // expressions that need an identifier, like a decl or struct init,
    // just use main_token
    fn identifier(p: *Parser) !Cst.Index {
        const ident_token = try p.expect(.ident);
        if (p.current() == .l_paren) return p.instance(ident_token);

        return p.addNode(.{
            .main_token = ident_token,
            .payload = .{ .ident = {} },
        });
    }

    fn instance(p: *Parser, ident_token: TokenIndex) !Cst.Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        const assignments = try p.parseList(portAssign, .{ .open = .l_paren, .close = .r_paren });

        return p.addNode(.{
            .main_token = ident_token,
            .payload = .{ .instance = @ptrCast(assignments) },
        });
    }

    fn portAssign(p: *Parser) !Cst.Index {
        const ident_token = try p.expect(.ident);
        _ = try p.expect(.equal);
        const value = try p.expression(.value);

        return p.addNode(.{
            .main_token = ident_token,
            .payload = .{ .port_assign = value },
        });
    }

    fn unary(p: *Parser, accept_l_brace: bool) Error!Cst.Index {
        switch (p.current()) {
            .tilde,
            .k_not,
            .minus,
            => {
                const unary_token = p.eatCurrent();
                const expr = try p.unary(accept_l_brace);
                return p.addNode(.{
                    .main_token = unary_token,
                    .payload = .{ .unary = expr },
                });
            },
            else => return p.postfix(accept_l_brace),
        }
    }

    fn structLiteral(p: *Parser) !Cst.Index {
        var type_node: Cst.Index = .null;
        if (p.current() == .period) {
            _ = p.eatCurrent();
        } else {
            type_node = try p.expression(.type);
        }

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        const l_brace_token = p.index;
        const fields = try p.parseList(fieldInitializer, .{ .open = .l_brace, .close = .r_brace });

        return p.addNode(.{
            .main_token = l_brace_token,
            .payload = .{
                .struct_literal = .{
                    .type = type_node,
                    .fields = @ptrCast(fields),
                },
            },
        });
    }

    fn fieldInitializer(p: *Parser) !Cst.Index {
        const dot_token = try p.expect(.period);
        _ = try p.expect(.ident);
        _ = try p.expect(.equal);
        const value_node = try p.expression(.value);

        return p.addNode(.{
            .main_token = dot_token,
            .payload = .{ .field_init = value_node },
        });
    }

    fn listLiteral(p: *Parser) !Node.Index {
        const l_bracket_token: TokenIndex = @enumFromInt(p.index);
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        const elements = try p.parseList(expression, .{ .open = .l_bracket, .close = .r_bracket });

        return p.addNode(.{
            .main_token = l_bracket_token,
            .data = .{ .list_literal = .{ .elements = elements } },
        });
    }

    fn tupleLiteral(p: *Parser) !Node.Index {
        const l_paren_token = p.index;
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        const elements = try p.parseList(expression, .{ .open = .l_paren, .close = .r_paren });

        return p.addNode(.{
            .main_token = l_paren_token,
            .data = .{ .tuple_literal = .{ .elements = elements } },
        });
    }

    // operand.attribute
    fn attribute(p: *Parser, operand: Node.Index) Error!Node.Index {
        const dot_token = try p.expect(.period);
        _ = try p.expect(.ident);

        return p.addNode(.{
            .main_token = dot_token,
            .data = .{ .attribute = operand },
        });
    }

    fn module(p: *Parser) !Index {
        const module_token = try p.expect(.k_module);
        // _ = try p.expect(.ident);

        // NOTE: we can probably pull this out to help when parsing module
        // signature types without the body
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        const l_paren_token = p.index;
        const inputs = try p.parseList(port, .{ .open = .l_paren, .close = .r_paren });
        _ = try p.expect(.minus_r_angle);
        const outputs = try p.parseList(port, .{ .open = .l_paren, .close = .r_paren });

        const ports = try p.addNode(.{
            .main_token = l_paren_token,
            .payload = .{
                .ports = .{
                    .inputs = @ptrCast(inputs),
                    .outputs = @ptrCast(outputs),
                },
            },
        });

        const body = try p.block();
        return p.addNode(.{
            .main_token = module_token,
            .payload = .{
                .module = .{
                    .ports = ports,
                    .body = body,
                },
            },
        });
    }

    fn port(p: *Parser) !Cst.Index {
        const ident_token = try p.expect(.ident);
        _ = try p.expect(.colon);
        const type_node = try p.expression(.type);

        return p.addNode(.{
            .main_token = ident_token,
            .payload = .{ .port = type_node },
        });
    }

    fn statement(p: *Parser) Error!Cst.Index {
        const node = switch (p.current()) {
            .k_module => return p.module(),
            .k_let => p.signal(),
            .k_type => p.typedef(),
            .k_decl => p.decl(),
            .k_yield => p.yield(),
            else => {
                std.debug.print("unexpected token: {} {s}\n", .{ p.current(), p.tokenString(p.index) });
                unreachable;
            },
        };

        _ = try p.expect(.semi);
        return node;
    }

    fn signal(p: *Parser) !Cst.Index {
        const let_token = try p.expect(.k_let);
        _ = try p.expect(.ident);

        var type_node: Cst.Index = .null;
        if (p.current() == .colon) {
            _ = p.eatCurrent();
            type_node = try p.expression(.type);
        }

        _ = try p.expect(.equal);
        const value_node = try p.expression(.value);

        return p.addNode(.{
            .main_token = let_token,
            .payload = .{
                .signal = .{
                    .type = type_node,
                    .value = value_node,
                },
            },
        });
    }

    fn decl(p: *Parser) !Cst.Index {
        const decl_token = try p.expect(.k_decl);
        _ = try p.expect(.ident);
        _ = try p.expect(.colon);
        const type_node = try p.expression(.type);

        return p.addNode(.{
            .main_token = decl_token,
            .payload = .{
                .decl = type_node,
            },
        });
    }

    fn bundle(p: *Parser) !Cst.Index {
        const bundle_token = try p.expect(.k_bundle);
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        const fields = try p.parseList(field, .{ .open = .l_brace, .close = .r_brace });

        return p.addNode(.{
            .main_token = bundle_token,
            .payload = .{ .bundle = @ptrCast(fields) },
        });
    }

    fn array(p: *Parser) !Cst.Index {
        const l_bracket_token = try p.expect(.l_bracket);
        const count = try p.expression(.value);
        _ = try p.expect(.r_bracket);
        const element_type = try p.expression(.type);

        return p.addNode(.{
            .main_token = l_bracket_token,
            .payload = .{
                .array = .{
                    .count = count,
                    .element_type = element_type,
                },
            },
        });
    }

    fn @"enum"(p: *Parser) Error!Cst.Index {
        const enum_token = try p.expect(.k_enum);
        var backing_type_identifier: Cst.Index = .null;

        if (p.current() == .l_paren) {
            _ = try p.expect(.l_paren);
            backing_type_identifier = try p.expression(.type);
            _ = try p.expect(.r_paren);
        }

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        const named_states = try p.parseList(identifier, .{ .open = .l_brace, .close = .r_brace });

        return p.addNode(.{
            .main_token = enum_token,
            .payload = .{
                .@"enum" = .{ .type = backing_type_identifier, .variants = named_states },
            },
        });
    }

    fn field(p: *Parser) !Cst.Index {
        const ident_token = try p.expect(.ident);
        _ = try p.expect(.colon);
        const type_node = try p.expression(.type);

        return p.addNode(.{
            .main_token = ident_token,
            .payload = .{ .field = type_node },
        });
    }

    fn typedef(p: *Parser) !Cst.Index {
        const type_token = try p.expect(.k_type);
        _ = try p.expect(.ident);
        _ = try p.expect(.equal);
        const ty = try p.expression(.type);

        return p.addNode(.{
            .main_token = type_token,
            .payload = .{ .typedef = ty },
        });
    }

    fn yield(p: *Parser) !Cst.Index {
        const yield_token = try p.expect(.k_yield);
        const yield_value = try p.expression(.value);
        return p.addNode(.{
            .main_token = yield_token,
            .payload = .{ .yield = yield_value },
        });
    }
};
