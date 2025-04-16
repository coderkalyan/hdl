const std = @import("std");
const lex = @import("lex.zig");

const Allocator = std.mem.Allocator;
const Lexer = lex.Lexer;
const ByteOffset = lex.ByteOffset;

// lighter version of lex.Token, doesn't include the end index
// even though the lexer generates end offsets, storing them
// is a low return on investment because we don't need them
// for most tokens (only identifiers, strings, and literals)
// therefore, we instead re-lex a single token to get the
// end offset/length as necessary
pub const Token = struct {
    tag: lex.Token.Tag,
    start: ByteOffset,
};

// we store an array of token tags and start locations
// to reference during parsing. CST nodes don't store tokens
// or strings themselves, but rather the index to the relevant
// token(s) in this array. note that tokens themselves don't own
// any strings, but reference character offsets in the global
// source array.
pub const TokenList = std.MultiArrayList(Token);
pub const TokenIndex = enum(u32) { unused, _ };

pub const Index = enum(u32) { null, _ };
pub const ExtraIndex = enum(u32) { _ };

pub const Item = struct {
    // index to the "main" token representing this node, if applicable.
    // examples include 'fn' for function declarations, `let` for
    // const or var declarations, and operator tokens for unary and
    // binary operations. strings such as identifiers can be extracted
    // from this information using a fixed offset (i.e. +1 for const decls)
    tag: Tag,
    main_token: TokenIndex,
    payload: [2]u32,

    pub const Tag = enum(u8) {
        // primary values
        null,
        ident,
        integer,
        bool,
        // aggregate types
        array,
        // bundle,
        // expressions
        unary,
        binary,
        // inst,
        subscript,
        // slice,
        member,
        // declarations
        signal,
        type,
        // control flow
        yield,
        // @"switch",
        // structure
        block,
        module,
        toplevel,
    };
};

pub const Node = struct {
    main_token: TokenIndex,
    payload: Payload,

    pub const Payload = union(enum) {
        // empty node at the beginning
        null,
        // a single "named" thing like a variable, field, or type name
        ident,
        // an integer literal, in any base
        integer,
        // a boolean literal, 'true' or 'false'
        bool,

        // array type '[n]T'
        array: struct {
            // expression node for the array size
            count: Index,
            // type node for the array element type
            element: Index,
        },

        // expressions
        // unary expression '[+-~]expr'
        unary: Index,
        // binary expression 'expr [+-&|...] expr'
        binary: struct {
            l: Index,
            r: Index,
        },
        // subscript (element access in array or value)
        subscript: struct {
            // expression node for the array/value
            value: Index,
            // expression node for the index
            index: Index,
        },
        // member (element access in a bundle)
        member: struct {
            // expression node for the bundle
            bundle: Index,
            // expression node for the field name
            field: Index,
        },

        // declaration of an (immutable) signal
        signal: struct {
            // null if inferred
            type: Index,
            value: Index,
        },
        // declaration of a type
        type: Index,

        // yields a value from an expression block
        yield: Index,

        // block of code (list of nodes)
        block: []const Index,
        // hardware module (input, output, block)
        module: struct {
            // list of one or more input ports
            inpuoverts: []const Index,
            // list of one or more output ports
            outputs: []const Index,
            // body block
            body: Index,
        },
        // named input or output to a module
        // main_token references name, payload is the type
        port: Index,
        // toplevel list of type and module declarations
    };

    // represents a slice into the extra array
    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };
};

pub const Cst = struct {
    // represents the entire, immutable, CST of a source file, once parsed.
    // in-progess mutable parsing data is stored in the `Parser` struct in
    // parser.zig. the CST owns the source
    // extra data
    source: [:0]const u8,
    tokens: TokenList.Slice,
    items: std.MultiArrayList(Item).Slice,
    extra: []u32,

    pub fn deinit(self: *const Cst, gpa: Allocator) void {
        gpa.free(self.source);
        self.tokens.deinit(gpa);
        self.items.deinit(gpa);
        gpa.free(self.extra);
    }

    pub fn extraData(self: *const Cst, index: usize, comptime T: type) T {
        const fields = std.meta.fields(T);
        var result: T = undefined;
        inline for (fields, 0..) |field, i| {
            comptime std.debug.assert(field.type == Node.Index);
            @field(result, field.name) = self.extra[index + i];
        }
        return result;
    }

    pub fn extraSlice(tree: *const Cst, sl: Cst.Node.ExtraSlice) []const u32 {
        const start: u32 = @intCast(sl.start);
        const end: u32 = @intCast(sl.end);
        return tree.extra[start..end];
    }

    pub fn tokenString(tree: *const Cst, index: TokenIndex) []const u8 {
        const tokens = tree.tokens;
        const token_start = tokens.items(.start)[index];
        var lexer = Lexer.init_index(tree.source, token_start);
        const token = lexer.next();

        return tree.source[token.loc.start..token.loc.end];
    }

    pub fn mainToken(tree: *const Cst, node: Node.Index) TokenIndex {
        return tree.nodes.items(.main_token)[node];
    }

    pub fn tokenTag(tree: *const Cst, index: TokenIndex) Token.Tag {
        return tree.tokens.items(.tag)[index];
    }

    pub fn data(tree: *const Cst, node: Node.Index) Node.Data {
        return tree.nodes.items(.data)[node];
    }
};
