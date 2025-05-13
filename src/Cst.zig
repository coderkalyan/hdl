const std = @import("std");
const lex = @import("lex.zig");
const parse = @import("parse.zig");

const Allocator = std.mem.Allocator;
const Lexer = lex.Lexer;
const ByteOffset = lex.ByteOffset;
const Parser = parse.Parser;
const Cst = @This();

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
pub const TokenIndex = enum(u32) { null, _ };
pub const Index = enum(u32) { null, _ };
pub const ExtraIndex = enum(u32) { _ };

// start and end index into the extra data array
pub const ExtraSlice = struct {
    start: ExtraIndex,
    end: ExtraIndex,
};

// represents a list of node indices, stored flat in the extra data array
// alias to the ExtraIndex that stores the ExtraSlice to the list of indices
pub const Indices = ExtraIndex;

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

        // struct literal
        struct_literal: struct {
            // expression node for the struct type
            type: Index,
            // list of field initializers
            fields: Indices,
        },
        // field initializer for struct literals
        field_init: Index,
        // module instantiation
        // NOTE: module type should be a full node to support scoping
        instance: Indices,
        port_assign: Index,

        // array type '[n]T'
        array: struct {
            // expression node for the array size
            count: Index,
            // type node for the array element type
            element_type: Index,
        },
        // bundle type 'bundle { field: T, ... }'
        bundle: Indices,
        // field inside a bundle
        field: Index,

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

        // definition of an (immutable) signal
        signal: struct {
            // null if inferred
            type: Index,
            value: Index,
        },
        // definition of a type
        typedef: Index,
        // forward declaration of a signal
        decl: Index,

        // yields a value from an expression block
        yield: Index,

        // block of code (list of nodes)
        block: Indices,

        @"enum": struct {
            type: Index,
            // list of variants
            variants: Indices,
        },

        // hardware module (input, output, block)
        module: struct {
            // list of input and output ports
            ports: Index,
            // body block
            body: Index,
        },
        // named input or output to a module
        // main_token references name, payload is the type
        port: Index,
        // list of input and output ports to a module, effectively the signature
        ports: struct {
            // list of one or more input ports
            inputs: Indices,
            // list of one or more output ports
            outputs: Indices,
        },
        // toplevel list of declarations
        toplevel: Indices,
    };
};

// To improve in memory layout of the CST, nodes are flattened into these Items. The
// underlying data is not changed but is more amenable to struct of arrays (MultiArrayList).
pub const Item = struct {
    main_token: TokenIndex,
    tag: Tag,
    payload: Payload,

    const u = @typeInfo(Node.Payload).@"union";
    const Tag = u.tag_type.?;
    const Payload = @Type(.{ .@"union" = .{
        .layout = u.layout,
        .tag_type = null,
        .fields = u.fields,
        .decls = &.{},
    } });

    pub fn serialize(node: Node) Item {
        const tag = std.meta.activeTag(node.payload);
        return .{
            .main_token = node.main_token,
            .tag = tag,
            .payload = switch (tag) {
                inline else => |t| @unionInit(
                    Payload,
                    @tagName(t),
                    @field(node.payload, @tagName(t)),
                ),
            },
        };
    }

    pub fn deserialize(item: Item) Node {
        return .{
            .main_token = item.main_token,
            .payload = switch (item.tag) {
                inline else => |t| @unionInit(
                    Node.Payload,
                    @tagName(t),
                    @field(item, @tagName(t)),
                ),
            },
        };
    }
};

// represents the entire, immutable, CST of a source file, once parsed.
// in-progess mutable parsing data is stored in the `Parser` struct in
// parser.zig. the CST owns the source
// extra data
source: [:0]const u8,
tokens: TokenList.Slice,
items: std.MultiArrayList(Item).Slice,
extra: []u32,

pub fn deinit(self: *Cst, gpa: Allocator) void {
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
