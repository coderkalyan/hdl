const std = @import("std");
const InternPool = @import("../InternPool.zig");

const Allocator = std.mem.Allocator;
const Mir = @This();

pub const Index = enum(u32) { null, _ };
pub const ExtraIndex = enum(u32) { null, _ };

/// Represents a slice into the extra data array. The
/// values are indices that outlive the lifetime of a
/// single pointer/slice in an Arraylist.
pub const ExtraSlice = struct {
    start: ExtraIndex,
    end: ExtraIndex,
};

/// Represents a list of node indices, stored flat in the extra data array.
pub const Indices = ExtraIndex;

/// The Middle Intermediate Representation (MIR) is a list of nodes,
/// similar to an abstract syntax tree. It is intended to map to the
/// (System)Verilog source being translated to and is taylored for
/// actual language constructs that the compiler needs to generate.
/// It contains very limited type information,
pub const Node = struct {
    tag: Tag,
    payload: Payload,

    pub const Tag = enum(u8) {
        /// Empty node at the beginning - used to fill .null enum slot.
        null,
        /// A named identifier to be printed verbatim. It is guaranteed
        /// to be within scope wrt. Verilog semantic rules.
        /// payload: `ip`
        ident,
        /// Integer literal in decimal format without size (bitwidth).
        /// payload: `ip`
        unsized_literal,
        /// Integer literal in binary format with size.
        /// payload: `ip`
        bin_literal,
        /// Integer literal in octal format with size.
        /// payload: `ip`
        oct_literal,
        /// Integer literal in decimal format with size.
        /// payload: `ip`
        dec_literal,
        /// Integer literal in hexadecimal format with size.
        /// payload: `ip`
        hex_literal,
        /// Unsigned wire net type with size.
        /// payload: `size`
        net_wire_unsigned,
        /// Signed wire net type with size.
        /// payload: `size`
        net_wire_signed,
        /// Unsigned reg net type with size.
        /// payload: `size`
        net_reg_unsigned,
        /// Signed reg net type with size.
        /// payload: `size`
        net_reg_signed,
        /// Unary integer negation operator.
        /// payload: `unary`
        unary_ineg,
        /// Unary bitwise negation operator.
        /// payload: `unary`
        unary_bnot,
        /// Unary logical negation operator.
        /// payload: `unary`
        unary_lnot,
        /// Parenthesis.
        /// payload: `unary`
        paren,
        /// Binary addition operator.
        /// payload: `binary`
        bin_iadd,
        /// Binary subtraction operator.
        /// payload: `binary`
        bin_isub,
        /// Binary bitwise or operator.
        /// payload: `binary`
        bin_bor,
        /// Binary bitwise and operator.
        /// payload: `binary`
        bin_band,
        /// Binary bitwise exclusive or operator.
        /// payload: `binary`
        bin_bxor,
        /// Binary logical or operator.
        /// payload: `binary`
        bin_lor,
        /// Binary logical and operator.
        /// payload: `binary`
        bin_land,
        /// Bit-select operator (single index).
        /// payload: `binary`
        bit_select,
        /// Part-select operator (inclusive range of indices).
        // payload: `op_extra` = Range
        part_select,
        /// Representation of a net with type and name. No
        /// direct correspondance to verilog but is referenced
        /// below in the declaration and definition nodes.
        /// payload: `binary`
        net,
        /// Net decleration with type and name but no rvalue.
        /// payload: `unary`
        net_decl,
        /// Net definition with type, name and rvalue.
        /// payload: `binary`
        net_def,
        /// Net (continuous) assignment with name and rvalue.
        net_assign,
        /// Input port declaration with type and name.
        /// payload: `unary`
        port_input,
        /// Output port declaration with type and name.
        /// payload: `unary`
        port_output,
        /// Block containing a list of statements.
        /// payload: `indices`
        block,
        /// Module declaration with name, ports, and body.
        /// payload: `extra` -> Module
        module,
    };

    pub const Payload = union {
        /// Node data is entirely captured by the tag.
        placeholder: void,
        /// Intern pool reference to a string or value.
        ip: InternPool.Index,
        /// Size of a net type, i.e. number of bits.
        size: u32,
        /// One operand node, such as unary operators.
        unary: Index,
        /// Two operand nodes, such as binary operators.
        binary: struct {
            l: Index,
            r: Index,
        },
        /// List of node indices.
        indices: Indices,
        /// Pointer to the extra data array, context dependent.
        extra: ExtraIndex,
        /// Operand and extra data.
        op_extra: struct {
            op: Index,
            extra: ExtraIndex,
        },
    };

    pub const Module = struct {
        /// Name of the module.
        name: InternPool.Index,
        /// List of input and output ports.
        ports: Indices,
        /// Block of statements that make up the module body.
        body: Index,
    };

    pub const Range = struct {
        /// Upper index of the range.
        upper: Index,
        /// Lower index of the range.
        lower: Index,
    };
};

nodes: std.MultiArrayList(Node).Slice,
extra: []const u32,
toplevel: Index,

pub fn deinit(mir: *Mir, gpa: Allocator) void {
    mir.nodes.deinit(gpa);
    gpa.free(mir.extra);
}

pub fn tag(mir: *const Mir, node: Index) Node.Tag {
    const i = @intFromEnum(node);
    return mir.nodes.items(.tag)[i];
}

pub fn payload(mir: *const Mir, node: Index) Node.Payload {
    const i = @intFromEnum(node);
    return mir.nodes.items(.payload)[i];
}

pub fn extraData(mir: *const Mir, index: ExtraIndex, comptime T: type) T {
    // TODO: replace this with the new memcpy based extra data implementation
    const base = @intFromEnum(index);
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        @field(result, field.name) = switch (field.type) {
            u32 => mir.extra[base + i],
            inline else => @enumFromInt(mir.extra[base + i]),
        };
    }
    return result;
}

pub fn indices(mir: *const Mir, ids: Mir.Indices) []const Index {
    const slice = mir.extraData(ids, Mir.ExtraSlice);
    const start: u32 = @intFromEnum(slice.start);
    const end: u32 = @intFromEnum(slice.end);
    return @ptrCast(mir.extra[start..end]);
}
