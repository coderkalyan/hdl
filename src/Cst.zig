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
    tag: Tag,
    start: ByteOffset,

    pub const Tag = lex.Token.Tag;
};

// we store an array of token tags and start locations
// to reference during parsing. CST nodes don't store tokens
// or strings themselves, but rather the index to the relevant
// token(s) in this array. note that tokens themselves don't own
// any strings, but reference character offsets in the global
// source array.
pub const TokenList = std.MultiArrayList(Token);
pub const TokenIndex = enum(u32) {
    null,
    _,

    pub fn advance(index: TokenIndex, offset: u32) TokenIndex {
        const i = @intFromEnum(index);
        return @enumFromInt(i + offset);
    }
};

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
        // empty node at the beginning - used to fill .null enum slot
        null,

        // leaf nodes - these correspond to simple values and tokens
        // a single "named" thing like a variable, field, or type name
        ident,
        // an integer literal, in any base
        integer,
        // a boolean literal, 'true' or 'false'
        bool,

        // bundle related nodes
        // bundle type 'bundle { field: T, ... }'
        // each child index in the bundle is a field
        // bundles are anonymous (unless assigned to a named type alias)
        bundle: Indices,
        // a bundle contains zero or more of these fields,
        // each corresponding to a named and typed member of the bundle
        // main_token is the field name, and the index is the type
        field: Index,
        // bundle literals initialize a value of a bundle type, either
        // with a named type 'Request { .address = 0x1234, ... }'
        // or anonymously '.{ .address = 0x1234, ... }'
        bundle_literal: struct {
            // expression node for the bundle type
            // if anonymous, this is .null
            type: Index,
            // list of field initializers
            inits: Indices,
        },
        // each initializer in the bundle literal is a named
        // field assignment '.field = expr'
        // main_token is the field name, and the index is the value
        field_init: Index,

        // array related nodes
        // array type '[n]T'
        array: struct {
            // expression node for the array length
            len: Index,
            // type node for the array child type
            child: Index,
        },
        // array literal '[expr, expr, ...]'
        array_literal: Indices,

        // module related nodes
        // module type 'module(inputs...) -> output { block }'
        module: struct {
            // list of input ports and output port
            ports: Index,
            // body block
            body: Index,
        },
        // list of input and output ports to a module, effectively the signature
        ports: struct {
            // list of zero or more named input ports
            inputs: Indices,
            // output port type (output port not named since only one)
            output: Index,
        },
        // input port to a module
        // main_token is the port name and the index is the type
        input: Index,
        // module instantiation literal 'Foo(input=expr, ...)'
        module_literal: struct {
            // module type being instantiated
            type: Index,
            // list of input port initializers
            inits: Indices,
        },
        // each input initializer is a named port assignment
        // 'port=expr'
        input_init: Index,

        // expressions
        // unary expression '[-~]expr'
        // list of unary expressions:
        // -   integer negation
        // ~   bitwise negation
        // not logical negation
        unary: Index,
        // binary expression 'expr [+-&|...] expr'
        // list of binary expressions:
        // +       integer addition
        // -       integer subtraction
        // &       bitwise and
        // |       bitwise or
        // ^       bitwise xor
        // and     logical and
        // or      logical or
        // xor     logical exclusive or
        // implies logical implication
        binary: struct {
            l: Index,
            r: Index,
        },
        // subscript (array element access by index)
        subscript: struct {
            // expression node for the array
            value: Index,
            // expression node for the index
            index: Index,
        },
        // member (bundle field access by name)
        // main_token is the field name, and the index is the bundle expression
        member: Index,

        // definition of an signal 'let signal[: type] = expr'
        // all signals are immutable once defined (SSA form)
        // main_token is the `let` which can be used to seek to the name token
        def: struct {
            // type expression node, null if inferred
            type: Index,
            // value expression node
            value: Index,
        },
        // forward declaration of a signal
        // this is used for (usually sequential) feedback loops
        // main_token is the `decl` which can be used to seek to the name token
        // no value here, so the index is the type
        decl: Index,
        // definition of a named type alias
        // all types are anonymous by default
        // main_token is the `type` which can be used to seek to the name token
        // the index is the type expression
        type: Index,

        // yields a value from an expression block
        yield: Index,

        // block of code (list of nodes)
        block: Indices,

        // @"enum": struct {
        //     type: Index,
        //     // list of variants
        //     variants: Indices,
        // },

        // toplevel list of statements, similar to block
        // but can only hold type declarations and imports
        root: Indices,
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
                    @field(item.payload, @tagName(t)),
                ),
            },
        };
    }
};

// represents the entire, immutable, CST of a source file, once parsed.
// in-progess mutable parsing data is stored in the `Parser` struct in
// parser.zig. the CST owns the source
source: [:0]const u8,
tokens: TokenList.Slice,
items: std.MultiArrayList(Item).Slice,
extra: []const u32,
root: Index,

pub fn deinit(self: *Cst, gpa: Allocator) void {
    gpa.free(self.source);
    self.tokens.deinit(gpa);
    self.items.deinit(gpa);
    gpa.free(self.extra);
}

pub fn extraData(tree: *const Cst, index: ExtraIndex, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        // comptime std.debug.assert(field.type == Index);
        @field(result, field.name) = @enumFromInt(tree.extra[@intFromEnum(index) + i]);
    }
    return result;
}

pub fn indices(tree: *const Cst, ids: Cst.Indices) []const Index {
    const slice = tree.extraData(ids, Cst.ExtraSlice);
    const start: u32 = @intFromEnum(slice.start);
    const end: u32 = @intFromEnum(slice.end);
    return @ptrCast(tree.extra[start..end]);
}

pub fn mainToken(tree: *const Cst, node: Index) TokenIndex {
    return tree.items.items(.main_token)[@intFromEnum(node)];
}

pub fn data(tree: *const Cst, node: Index) Node {
    const item = tree.items.get(@intFromEnum(node));
    return item.deserialize();
}

pub fn payload(tree: *const Cst, node: Index) Node.Payload {
    const index = @intFromEnum(node);
    const tag = tree.items.items(.tag)[index];
    const pl = tree.items.items(.payload)[index];

    const deserialized = Item.deserialize(.{ .main_token = .null, .tag = tag, .payload = pl });
    return deserialized.payload;
}

pub fn tokenString(tree: *const Cst, index: TokenIndex) []const u8 {
    const tokens = tree.tokens;
    const token_start = tokens.items(.start)[@intFromEnum(index)];
    var lexer = Lexer.initIndex(tree.source, token_start);
    const token = lexer.next();

    return tree.source[token.loc.start..token.loc.end];
}

pub fn tokenTag(tree: *const Cst, index: TokenIndex) Token.Tag {
    return tree.tokens.items(.tag)[@intFromEnum(index)];
}
