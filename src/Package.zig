const std = @import("std");
const InternPool = @import("InternPool.zig");

pub const Decl = struct {
    kind: Kind,
    name: InternPool.Index,
    /// For type declarations, this is the named type being declared.
    /// For constant declarations, this is the type of the constant.
    type: InternPool.Index,

    pub const Kind = enum {
        type,
        // constant,
    };
};
