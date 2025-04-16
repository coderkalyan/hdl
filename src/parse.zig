const std = @import("std");
const lex = @import("lex.zig");
const Cst = @import("Cst.zig");

const Allocator = std.mem.Allocator;
const Token = lex.Token;
const Node = Cst.Node;
const TokenIndex = Cst.TokenIndex;

pub const Error = error{UnexpectedToken} || Allocator.Error;

// parses a string of source characters into a concrete syntax tree
// gpa: allocator for tree data that outlives this function call
pub fn parse(gpa: Allocator, source: [:0]const u8) Error!Cst {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    // lex entire source file into token list
    var tokens: Cst.TokenList = .{};
    errdefer tokens.deinit(gpa);
    var lexer: lex.Lexer = .init(source, arena.allocator());
    while (true) {
        const token = try lexer.next(arena.allocator());
        try tokens.append(gpa, token);
        if (token.tag == .eof) break;
    }

    // initialize parser
    var parser = Parser.init(gpa, source, &tokens);
    defer parser.deinit();

    const null_node = try parser.addNode(.{
        .main_token = .unused,
        .data = .{ .placeholder = {} },
    });
    std.debug.assert(null_node == .null);

    _ = try parser.module();

    // for (parser.nodes.items(.data), parser.nodes.items(.main_token)) |data, tok| {
    //     std.debug.print("{} {}\n", .{ data, tokens.items(.tag)[tok] });
    // }

    // copy parser results into an abstract syntax tree
    // that owns the source, token list, node list, and node extra data
    return Cst{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra = try parser.extra.toOwnedSlice(gpa),
    };
}

const Parser = struct {
    gpa: Allocator,
    source: []const u8,

    token_tags: []const Token.Tag,
    index: u32,

    nodes: std.MultiArrayList(Node),
    extra: std.ArrayListUnmanaged(u32),
    scratch: std.ArrayList(u32),
    attributes: std.ArrayListUnmanaged(Node.Index),

    pub fn init(gpa: Allocator, source: []const u8, tokens: *Cst.TokenList) Parser {
        return .{
            .source = source,
            .gpa = gpa,
            .token_tags = tokens.items(.tag),
            .index = 0,
            .nodes = .{},
            .extra = .{},
            .scratch = std.ArrayList(u32).init(gpa),
            .attributes = .{},
        };
    }

    pub fn deinit(self: *Parser) void {
        self.nodes.deinit(self.gpa);
        self.extra.deinit(self.gpa);
    }

    fn addNode(p: *Parser, node: Node) !Node.Index {
        const result: u32 = @intCast(p.nodes.len);
        try p.nodes.append(p.gpa, node);
        return @enumFromInt(result);
    }

    fn setNode(p: *Parser, i: usize, node: Node) Node.Index {
        p.nodes.set(i, node);
        return @intCast(i);
    }

    fn reserveNode(p: *Parser, tag: Cst.Node.Tag) !usize {
        try p.nodes.resize(p.gpa, p.nodes.len + 1);
        p.nodes.items(.tag)[p.nodes.len - 1] = tag;
        return p.nodes.len - 1;
    }

    // eats the current token (whatever it is) and returns the index
    fn eatCurrent(p: *Parser) TokenIndex {
        p.index += 1;
        return @enumFromInt(p.index - 1);
    }

    // eats the current token if it matches a tag, and returns null otherwise
    fn eat(p: *Parser, tag: Token.Tag) ?TokenIndex {
        if (p.token_tags[p.index] == tag) {
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
        return p.token_tags[p.index];
    }

    // this can quite easily go out of bounds, so it should only be used
    // after checking for the guard token (eof)
    inline fn next(p: *Parser, offset: u32) Token.Tag {
        return p.token_tags[p.index + offset];
    }

    fn addExtra(p: *Parser, extra: anytype) Allocator.Error!Node.ExtraIndex {
        const len: u32 = @intCast(p.extra.items.len);
        const fields = std.meta.fields(@TypeOf(extra));
        try p.extra.ensureUnusedCapacity(p.gpa, fields.len);
        inline for (fields) |field| {
            switch (field.type) {
                inline else => {
                    const num = @intFromEnum(@field(extra, field.name));
                    p.extra.appendAssumeCapacity(num);
                },
            }
        }
        return @enumFromInt(len);
    }

    pub fn extraSlice(p: *Parser, sl: Cst.Node.ExtraSlice) []const u32 {
        const start: u32 = @intFromEnum(sl.start);
        const end: u32 = @intFromEnum(sl.end);
        return p.extra.items[start..end];
    }

    pub fn addSlice(p: *Parser, sl: []const u32) !Cst.Node.ExtraIndex {
        const start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(p.gpa, sl);
        const end: u32 = @intCast(p.extra.items.len);
        return p.addExtra(Cst.Node.ExtraSlice{
            .start = @enumFromInt(start),
            .end = @enumFromInt(end),
        });
    }

    fn parseList(p: *Parser, comptime element: anytype, surround: struct { open: Token.Tag, close: Token.Tag }) !Node.ExtraIndex {
        _ = try p.expect(surround.open);

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eat(surround.close)) |_| break;
            const element_node = try @call(.auto, element, .{p});
            try p.scratch.append(@intFromEnum(element_node));

            if (p.current() == .comma) {
                _ = p.eatCurrent();
            } else if (p.current() != surround.close) {
                return error.UnexpectedToken;
            }
        }

        const elements = p.scratch.items[scratch_top..];
        return p.addSlice(elements);
    }

    // TODO:(Max) refactor this into two seperate methods
    // really small perf impact, but why not just make it better =)
    fn consumeUntilValidTLD(p: *Parser, report_unmatched: bool) Allocator.Error!void {
        // consume until we're in a valid state e.g. next let
        var open_parenths: i32 = 0;
        var open_braces: i32 = 0;
        // var open_quotes: u32 = 0;
        // var open_square: u32 = 0;

        var last_open_parenth: u32 = 0;
        var last_close_parenth: u32 = 0;
        var last_open_brace: u32 = 0;
        var last_close_brace: u32 = 0;
        // var last_open_quote: u32 = 0;
        // var last_open_square: u32 = 0;

        while (true) {
            switch (p.token_tags[p.index]) {
                .eof => break,
                .l_paren => {
                    last_open_parenth = p.index;
                    open_parenths += 1;
                },
                .r_paren => {
                    last_close_parenth = p.index;
                    open_parenths -= 1;
                },
                .l_brace => {
                    last_open_brace = p.index;
                    open_braces += 1;
                },
                .r_brace => {
                    last_close_brace = p.index;
                    open_braces -= 1;
                },
                .k_let => {
                    // have to make sure this isn't in some smaller block
                    if (open_parenths == 0 and open_braces == 0) break;
                },
                else => {},
            }
            p.index += 1;
        }

        if (report_unmatched) {
            if (open_parenths > 0) {
                try p.errors.append(.{ .tag = .unmatched_parenth, .token = last_open_parenth });
            }

            if (open_braces > 0) {
                try p.errors.append(.{ .tag = .unmatched_brace, .token = last_open_brace });
            }

            if (open_parenths < 0) {
                try p.errors.append(.{ .tag = .unmatched_parenth, .token = last_close_parenth });
            }

            if (open_braces < 0) {
                try p.errors.append(.{ .tag = .unmatched_brace, .token = last_close_brace });
            }
        }
    }

    fn consumeUntilSemi(p: *Parser) Allocator.Error!void {
        while (true) {
            switch (p.token_tags[p.index]) {
                .semi => {
                    p.index += 1;
                    break;
                },
                .eof => break,
                else => {},
            }
            p.index += 1;
        }
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
            .l_angle_equal => 14,
            .r_angle_equal => 14,
            .plus => 20,
            .minus => 20,
            .asterisk => 21,
            .at => 21,
            .slash => 21,
            .slash_slash => 21,
            .percent => 22,
            .asterisk_asterisk => 23,
            .pipe => 30,
            .ampersand => 31,
            .caret => 32,
            .r_angle_r_angle => 33,
            .l_angle_l_angle => 33,
            else => -1,
        };
    }

    pub fn module(p: *Parser) !Node.Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.current() == .eof) break;
            const node = try p.statement();
            try p.scratch.append(@intFromEnum(node));
        }

        const stmts = p.scratch.items[scratch_top..];
        const pl = try p.addSlice(stmts);

        return p.addNode(.{
            .main_token = .unused,
            .data = .{ .module = .{ .stmts = pl } },
        });
    }

    // expression parsing
    fn expression(p: *Parser) Error!Node.Index {
        const expr = try p.unary(true);
        return p.associateBinary(expr, min_precedence);
    }

    fn primary(p: *Parser, accept_l_brace: bool) Error!Node.Index {
        _ = accept_l_brace;
        return switch (p.current()) {
            .l_paren => {
                _ = try p.expect(.l_paren);
                const inner_node = try p.expression();
                _ = try p.expect(.r_paren);

                return inner_node;
            },
            .ident => p.identifier(), //if (accept_l_brace and p.next(1) == .l_brace) p.structLiteral() else p.identifier(),
            .int_lit => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .integer_literal = {} },
            }),
            .float_lit => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .float_literal = {} },
            }),
            .k_true => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .bool_literal = {} },
            }),
            .k_false => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .bool_literal = {} },
            }),
            .k_none => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .none_literal = {} },
            }),
            .str_lit => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .string_literal = {} },
            }),
            .l_bracket => p.listLiteral(),
            // .l_paren => p.tupleLiteral(),
            // .l_brace => p.block(),
            // .k_if => p.branch(),
            else => {
                std.debug.print("{}\n", .{p.current()});
                return Error.UnexpectedToken;
            },
        };
    }

    fn associateBinary(p: *Parser, l: Node.Index, expr_precedence: i32) !Node.Index {
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
                .data = .{ .binary = .{
                    .left = l_node,
                    .right = r_node,
                } },
            });
        }
    }

    fn postfix(p: *Parser, accept_l_brace: bool) Error!Node.Index {
        var expr = try p.primary(accept_l_brace);

        while (true) {
            expr = switch (p.current()) {
                // handles subscript and slice, since we can't
                // yet look far enough to know which it is
                .l_bracket => try p.subscript(expr),
                .period => try p.attribute(expr),
                .l_paren => try p.call(expr),
                else => return expr,
            };
        }
    }

    fn unary(p: *Parser, accept_l_brace: bool) Error!Node.Index {
        switch (p.current()) {
            .tilde,
            .k_not,
            .minus,
            .plus,
            => {
                const unary_token = p.eatCurrent();
                const expr = try p.unary(accept_l_brace);
                return p.addNode(.{
                    .main_token = unary_token,
                    .data = .{ .unary = expr },
                });
            },
            else => return p.postfix(accept_l_brace),
        }
    }

    // identifier used as an expression (like a variable or type name)
    // expressions that need an identifier, like a decl or struct init,
    // just use main_token
    fn identifier(p: *Parser) !Node.Index {
        const ident_token = try p.expect(.ident);
        return p.addNode(.{
            .main_token = ident_token,
            .data = .{ .ident = {} },
        });
    }

    fn listLiteral(p: *Parser) !Node.Index {
        const l_bracket_token: TokenIndex = @enumFromInt(p.index);
        const elements = try p.parseList(expression, .{ .open = .l_bracket, .close = .r_bracket });
        return p.addNode(.{
            .main_token = l_bracket_token,
            .data = .{ .list_literal = .{ .elements = elements } },
        });
    }

    fn tupleLiteral(p: *Parser) !Node.Index {
        const l_paren_token = p.index;
        const elements = try p.parseList(expression, .{ .open = .l_paren, .close = .r_paren });
        return p.addNode(.{
            .main_token = l_paren_token,
            .data = .{ .tuple_literal = .{ .elements = elements } },
        });
    }

    // fn structLiteral(p: *Parser) Error!Node.Index {
    //     const start_token = p.index;
    //     const type_node = try p.identifier(); // TODO: this should be a proper type expression
    //
    //     const fields = try p.parseList(fieldInitializer, .{ .open = .l_brace, .close = .r_brace });
    //     return p.addNode(.{
    //         .main_token = start_token,
    //         .data = .{ .struct_literal = .{ .struct_type = type_node, .fields = fields } },
    //     });
    // }

    // .field = value
    // fn fieldInitializer(p: *Parser) !Node.Index {
    //     var err = false;
    //
    //     const dot_token = try p.expect(.period);
    //     _ = p.expect(.ident) catch token: {
    //         try p.errors.append(.{ .tag = .missing_identifier, .token = p.index });
    //         err = true;
    //         break :token undefined;
    //     };
    //
    //     _ = try p.expect(.equal);
    //
    //     const initializer = p.expression() catch node: {
    //         try p.errors.append(.{ .tag = .missing_expression, .token = p.index });
    //         err = true;
    //         break :node undefined;
    //     };
    //
    //     if (err) return error.HandledUserError;
    //     return p.addNode(.{
    //         .main_token = dot_token,
    //         .data = .{ .field_initializer = initializer },
    //     });
    // }

    // operand[index]
    fn subscript(p: *Parser, operand: Node.Index) Error!Node.Index {
        const l_bracket_token = try p.expect(.l_bracket);
        const index = try p.expression();
        if (p.current() == .colon) {
            return p.slice(l_bracket_token, operand, index);
        }
        _ = try p.expect(.r_bracket);

        return p.addNode(.{
            .main_token = l_bracket_token,
            .data = .{ .subscript = .{ .operand = operand, .index = index } },
        });
    }

    // operand[start .. end]
    fn slice(p: *Parser, token: TokenIndex, op: Node.Index, start: Node.Index) Error!Node.Index {
        _ = try p.expect(.colon);
        const end = try p.expression();

        if (p.current() == .colon) {
            // step
            const step = try p.expression();
            _ = try p.expect(.r_bracket);

            return p.addNode(.{
                .main_token = token,
                .data = .{ .slice_step = .{
                    .operand = op,
                    .range = try p.addExtra(Node.SliceStep{
                        .start = start,
                        .end = end,
                        .step = step,
                    }),
                } },
            });
        }

        _ = try p.expect(.r_bracket);
        return p.addNode(.{
            .main_token = token,
            .data = .{ .slice_simple = .{
                .operand = op,
                .range = try p.addExtra(Node.SliceSimple{
                    .start = start,
                    .end = end,
                }),
            } },
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

    inline fn typeExpression(p: *Parser) !Node.Index {
        return p.postfixType();
    }

    fn postfixType(p: *Parser) Error!Node.Index {
        const expr = try p.primaryType();
        return expr;

        // while (true) {
        //     expr = switch (p.current()) {
        //         .asterisk => try p.pointerType(expr),
        //         .l_bracket => switch (p.next(1)) {
        //             .r_bracket => try p.sliceType(expr),
        //             .asterisk => try p.manyPointerType(expr),
        //             .eof => return expr,
        //             else => try p.arrayType(expr),
        //         },
        //         .k_mut => switch (p.next(1)) {
        //             .asterisk => try p.pointerType(expr),
        //             .l_bracket => switch (p.next(2)) {
        //                 .r_bracket => try p.sliceType(expr),
        //                 .asterisk => try p.manyPointerType(expr),
        //                 .eof => return expr,
        //                 else => try p.arrayType(expr),
        //             },
        //             else => return error.UnexpectedToken,
        //         },
        //         else => return expr,
        //     };
        // }
    }

    fn primaryType(p: *Parser) Error!Node.Index {
        return switch (p.current()) {
            // even though parentheses aren't necessary due to the ast
            // being nested, they are added to facilitate 1-1 mapping between
            // source and ast for tooling and testing
            .l_paren => try p.typeExpression(),
            // .k_struct => try p.structType(),
            // .k_fn => try p.functionType(),
            .ident => node: {
                const ident_token = try p.expect(.ident);
                break :node try p.addNode(.{
                    .main_token = ident_token,
                    .data = .{ .ident = {} },
                });
            },
            else => unreachable,
        };
    }

    fn function(p: *Parser) !Node.Index {
        const def_token = try p.expect(.k_def);
        _ = try p.expect(.ident);
        const params = try p.parseList(param, .{ .open = .l_paren, .close = .r_paren });

        var return_type: Node.Index = .null;
        if (p.eat(.minus_r_angle)) |_| {
            return_type = try p.typeExpression();
        }

        const signature = try p.addExtra(Node.FunctionSignature{
            .params = params,
            .ret = return_type,
        });

        _ = try p.expect(.colon);
        _ = try p.expect(.newline);
        const body = try p.block();

        return p.addNode(.{
            .main_token = def_token,
            .data = .{ .function = .{
                .signature = signature,
                .body = body,
            } },
        });
    }

    fn param(p: *Parser) !Node.Index {
        const ident_token = try p.expect(.ident);
        var type_node: Node.Index = .null;
        if (p.current() == .colon) {
            _ = p.eatCurrent();
            type_node = try p.typeExpression();
        }

        return p.addNode(.{
            .main_token = ident_token,
            .data = .{ .param = type_node },
        });
    }

    // fn structField(p: *Parser) !Node.Index {
    //     var err = false;
    //     const ident_token = p.expect(.ident) catch token: {
    //         // We're missing an identifier flag it
    //         try p.errors.append(.{ .tag = .missing_identifier, .token = p.index });
    //         err = true;
    //         break :token undefined;
    //     };
    //
    //     _ = p.expect(.colon) catch {
    //         // We're missing a colon flag it
    //         try p.errors.append(.{ .tag = .missing_colon, .token = p.index });
    //         err = true;
    //     };
    //     const type_node = p.typeExpression() catch node: {
    //         // We're missing a type annotation
    //         try p.errors.append(.{ .tag = .missing_type_annotation, .token = p.index });
    //         err = true;
    //         break :node undefined;
    //     };
    //
    //     if (err) return error.HandledUserError;
    //     return p.addNode(.{
    //         .main_token = ident_token,
    //         .data = .{ .field = type_node },
    //     });
    // }

    // // parses a distinct type declaration (distinct type Point = ...)
    // fn expectDistinctTypeDecl(self: *Parser) !Node.Index {
    //     const distinct_token = try self.expect(.k_distinct);
    //     _ = try self.expect(.k_type);
    //     _ = try self.expect(.ident); // not stored, (main_token == distinct_token) + 2
    //     _ = try self.expect(.equal);
    //     const type_node = try self.typeExpression();
    //
    //     return self.addNode(.{
    //         .main_token = distinct_token,
    //         .data = .{ .distinct_type_decl = type_node },
    //     });
    // }

    fn block(p: *Parser) !Node.Index {
        const indent_token = try p.expect(.indent);

        // since each block may create an arbitrary number of statements,
        // we collect the toplevel statement indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eat(.dedent)) |_| break;
            if (p.statement()) |stmt_node| {
                try p.scratch.append(@intFromEnum(stmt_node));
            } else |err| switch (err) {
                error.HandledUserError => continue,
                else => return err,
            }
        }

        const stmts = p.scratch.items[scratch_top..];
        const pl = try p.addSlice(stmts);

        return p.addNode(.{
            .main_token = indent_token,
            .data = .{ .block = .{ .stmts = pl } },
        });
    }

    fn statement(p: *Parser) Error!Node.Index {
        const node = switch (p.token_tags[p.index]) {
            .k_def => return p.function(),
            .k_return => p.ret(),
            .k_del => p.del(),
            .k_for => return p.forLoop(),
            .k_while => return p.whileLoop(),
            .k_if => return p.branch(),
            .k_pass => p.pass(),
            .k_break => p.brk(),
            .k_continue => p.cont(),
            else => node: {
                const expr = try p.expression();
                break :node switch (p.token_tags[p.index]) {
                    .equal,
                    .plus_equal,
                    .minus_equal,
                    .asterisk_equal,
                    .slash_equal,
                    .asterisk_asterisk_equal,
                    .slash_slash_equal,
                    .percent_equal,
                    .ampersand_equal,
                    .pipe_equal,
                    .caret_equal,
                    .l_angle_l_angle_equal,
                    .r_angle_r_angle_equal,
                    => p.assignment(expr),
                    else => expr,
                };
            },
        };

        _ = p.eat(.newline);
        return node;
    }

    fn call(p: *Parser, ptr: Node.Index) !Node.Index {
        const l_paren_token: TokenIndex = @enumFromInt(p.index);
        const args = try p.parseList(expression, .{ .open = .l_paren, .close = .r_paren });

        return p.addNode(.{
            .main_token = l_paren_token,
            .data = .{ .call = .{
                .ptr = ptr,
                .args = args,
            } },
        });
    }

    fn assignment(p: *Parser, ptr: Node.Index) !Node.Index {
        switch (p.current()) {
            .equal => {
                const equal_token = try p.expect(.equal);
                const val = try p.expression();
                return p.addNode(.{
                    .main_token = equal_token,
                    .data = .{ .assign_simple = .{ .ptr = ptr, .val = val } },
                });
            },
            .plus_equal,
            .minus_equal,
            .asterisk_equal,
            .slash_equal,
            .asterisk_asterisk_equal,
            .slash_slash_equal,
            .percent_equal,
            .l_angle_l_angle_equal,
            .r_angle_r_angle_equal,
            => {
                const operator_token = p.eatCurrent();
                const val = try p.expression();
                return p.addNode(.{
                    .main_token = operator_token,
                    .data = .{ .assign_binary = .{ .ptr = ptr, .val = val } },
                });
            },
            else => unreachable,
            // else => unreachable, {
            //     try p.errors.append(.{
            //         .tag = .unexpected_token,
            //         .token = p.index,
            //     });
            //     return error.HandledUserError;
            // },
        }
    }

    fn ret(p: *Parser) !Node.Index {
        const ret_token = try p.expect(.k_return);
        return p.addNode(.{
            .main_token = ret_token,
            .data = if (p.current() == .newline) .{
                .return_none = {},
            } else .{
                .return_val = try p.expression(),
            },
        });
    }

    fn del(p: *Parser) !Node.Index {
        const del_token = try p.expect(.k_del);
        const target = try p.expression();
        return p.addNode(.{
            .main_token = del_token,
            .data = .{ .del = target },
        });
    }

    fn yield(p: *Parser) !Node.Index {
        const yield_token = try p.expect(.k_yield);
        const yield_val = try p.expression();
        return p.addNode(.{
            .main_token = yield_token,
            .data = .{ .yield_val = yield_val },
        });
    }

    fn branch(p: *Parser) !Node.Index {
        // all conditional branches start with the if keyword
        const if_token = try p.expect(.k_if);

        // we have three kinds of if statements: simple, else, and chain
        // which we progressively try to match against
        const condition = try p.expression();

        _ = try p.expect(.colon);
        _ = try p.expect(.newline);
        const exec_true = try p.block();

        if (p.current() != .k_else) {
            // simple if
            return p.addNode(.{
                .main_token = if_token,
                .data = .{ .if_simple = .{
                    .condition = condition,
                    .exec_true = exec_true,
                } },
            });
        }

        _ = try p.expect(.k_else);
        // _ = p.eatCurrent();
        // if (p.current() == .k_if) {
        //     // chained if
        //     const chain_next = try p.branch();
        //     const pl = try p.addExtra(Node.IfChain{
        //         .exec_true = exec_true,
        //         .next = chain_next,
        //     });
        //     return p.addNode(.{
        //         .main_token = if_token,
        //         .data = .{ .if_chain = .{
        //             .condition = condition,
        //             .chain = pl,
        //         } },
        //     });
        // } else {
        // if else
        _ = try p.expect(.colon);
        _ = try p.expect(.newline);
        const exec_false = try p.block();

        const exec = try p.addExtra(Node.IfElse{
            .exec_true = exec_true,
            .exec_false = exec_false,
        });
        return p.addNode(.{
            .main_token = if_token,
            .data = .{ .if_else = .{
                .condition = condition,
                .exec = exec,
            } },
        });
        // }
    }

    fn forLoop(p: *Parser) !Node.Index {
        const for_token = try p.expect(.k_for);
        const target = try p.identifier();
        _ = try p.expect(.k_in);
        const iterable = try p.expression();

        _ = try p.expect(.colon);
        _ = try p.expect(.newline);
        const body = try p.block();

        return p.addNode(.{
            .main_token = for_token,
            .data = .{ .for_loop = .{
                .signature = try p.addExtra(Node.ForSignature{
                    .target = target,
                    .iterable = iterable,
                }),
                .body = body,
            } },
        });
    }

    fn whileLoop(p: *Parser) !Node.Index {
        const while_token = try p.expect(.k_while);
        const condition = try p.expression();

        _ = try p.expect(.colon);
        _ = try p.expect(.newline);
        const body = try p.block();

        return p.addNode(.{
            .main_token = while_token,
            .data = .{ .while_loop = .{
                .condition = condition,
                .body = body,
            } },
        });
    }

    fn pass(p: *Parser) !Node.Index {
        const pass_token = try p.expect(.k_pass);
        return p.addNode(.{
            .main_token = pass_token,
            .data = .{ .pass = {} },
        });
    }

    fn brk(p: *Parser) !Node.Index {
        const break_token = try p.expect(.k_break);
        return p.addNode(.{
            .main_token = break_token,
            .data = .{ .brk = {} },
        });
    }

    fn cont(p: *Parser) !Node.Index {
        const continue_token = try p.expect(.k_continue);
        return p.addNode(.{
            .main_token = continue_token,
            .data = .{ .cont = {} },
        });
    }

    // fn attribute(p: *Parser, comptime tag: Token.Tag) !Node.Index {
    //     const attr_token = try p.expect(tag);
    //     if (p.token_tags[p.index] == .l_paren) {
    //         const args = try p.parseList(expression, .{ .open = .l_paren, .close = .r_paren });
    //         return p.addNode(.{
    //             .main_token = attr_token,
    //             .data = .{ .attr_args = args },
    //         });
    //     } else {
    //         return p.addNode(.{
    //             .main_token = attr_token,
    //             .data = .{ .attr_simple = {} },
    //         });
    //     }
    // }
};

// fn testParse(source: [:0]const u8, allocator: Allocator, anything_changed: *bool) ![]u8 {
//     const stderr = std.io.getStdErr().writer();
//
//     var tree = try parse(allocator, source);

// for (tree.errors) |parse_error| {
//
// }
