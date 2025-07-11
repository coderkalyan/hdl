const std = @import("std");
const Cst = @import("Cst.zig");
const lex = @import("lex.zig");
const Type = @import("type.zig").Type;

const io = std.io;

const Node = Cst.Node;
const TokenIndex = Cst.TokenIndex;

pub const SourceError = struct {
    tag: Tag,
    token: TokenIndex,

    pub const Tag = enum {
        // missing_colon,
        // missing_type_annotation,
        // missing_return_type,
        // missing_identifier,
        // unexpected_identifier,
        // unexpected_tld_token,
        // unmatched_parenth,
        // unmatched_brace,
        // missing_equals,
        // missing_fn_brace,
        // unexpected_statement_token,
        // missing_expression,
        // unexpected_token,

        shadow_signal_type,
        shadow_type_signal,
        unknown_identifier,
        type_coerce_fail,
        type_unary_invalid,
        type_binary_diff,
        type_binary_invalid,
        field_duplicate,
        field_unknown,
        // shadows_builtin_type,
        // shadows_keyword,
        // named_var_type_context,
        // named_type_var_context,
        // const_variable_assign,
        // invalid_lvalue,
        // invalid_type,
        // call_nonfunc,
        // call_argcount,
        // binary_diffsign,
        // coerce_sint_to_uint,
        // invalid_val_expr,
        // invalid_ptr_expr,
        // invalid_ref_expr,

        // unsized_type_alloc,
        // const_pointer_write,
        // not_indexable,
        // operator_invalid,
    };
};

pub const LocatedSourceError = struct {
    src_error: SourceError,
    line_number: u32,
    column_number: u32,
    line_start: Cst.ByteOffset,

    pub fn locateErrors(gpa: std.mem.Allocator, cst: *const Cst, src_errors: []const SourceError) ![]LocatedSourceError {
        var errors = std.ArrayListUnmanaged(LocatedSourceError){};
        try errors.ensureUnusedCapacity(gpa, src_errors.len);

        for (src_errors) |src_error| {
            errors.appendAssumeCapacity(.{
                .src_error = src_error,
                .line_number = undefined,
                .column_number = undefined,
                .line_start = undefined,
            });
        }
        std.mem.sort(LocatedSourceError, errors.items, {}, comparator);

        var error_index: usize = 0;
        var byte_offset: Cst.ByteOffset = 0;
        var line_number: u32 = 0;
        var column_number: u32 = 0;
        var line_start: Cst.ByteOffset = 0;
        while (byte_offset < cst.source.len) : (byte_offset += 1) {
            if (error_index >= errors.items.len) break;
            if (cst.source[byte_offset] == '\n') {
                line_number += 1;
                column_number = 0;
                line_start = byte_offset + 1;
            } else {
                column_number += 1;
            }

            var token = errors.items[error_index].src_error.token;
            while (cst.tokens.items(.start)[@intFromEnum(token)] == byte_offset) {
                errors.items[error_index].line_number = line_number;
                errors.items[error_index].column_number = column_number;
                errors.items[error_index].line_start = line_start;
                error_index += 1;
                if (error_index >= errors.items.len) break;
                token = errors.items[error_index].src_error.token;
            }
        }

        return errors.toOwnedSlice(gpa);
    }

    fn comparator(context: void, l: LocatedSourceError, r: LocatedSourceError) bool {
        _ = context;
        const lt = @intFromEnum(l.src_error.token);
        const rt = @intFromEnum(r.src_error.token);
        return lt < rt;
    }
};

pub fn CompileErrorRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        allocator: std.mem.Allocator,
        cst: *const Cst,
        source: [:0]const u8,
        filename: []const u8,
        errors: []LocatedSourceError,

        pub const Self = @This();

        pub fn init(writer: anytype, allocator: std.mem.Allocator, cst: *const Cst, filename: []const u8, errors: []LocatedSourceError) Self {
            return .{
                .stream = indentingWriter(width, writer),
                .allocator = allocator,
                .cst = cst,
                .source = cst.source,
                .filename = filename,
                .errors = errors,
            };
        }

        pub fn render(r: *Self) !void {
            for (r.errors, 0..) |src_error, i| {
                try r.formatError(src_error);
                if (i < r.errors.len - 1) try r.stream.newline();
            }
        }

        fn skipWhitespace(r: *Self, source_index: Cst.ByteOffset) !Cst.ByteOffset {
            var i = source_index;
            while (i < r.source.len) : (i += 1) {
                if (!std.ascii.isWhitespace(r.source[i])) break;
            }
            return i;
        }

        fn formatError(r: *Self, err: LocatedSourceError) !void {
            const preview_message = "";

            const error_message = switch (err.src_error.tag) {
                // .missing_colon => "missing colon",
                // .missing_equals => "missing an '='",
                // .missing_expression => "missing an expression",
                // .missing_fn_brace => "malformed function block",
                // .missing_identifier => "missing an identifier",
                // .missing_return_type => "missing a return type",
                // .missing_type_annotation => "missing a type annotation",
                // .unexpected_identifier => "unexpected identifier",
                // .unexpected_statement_token => "unexpected statement",
                // .unexpected_tld_token => "unexpected statement", // TODO: better name for this?
                // .unmatched_brace => "unmatched brace",
                // .unmatched_parenth => "Unmatched parenthesis",
                // .unexpected_token => "unexpected token",
                .shadow_signal_type => "Signal name cannot shadow existing type name",
                .shadow_type_signal => "Type name cannot shadow existing signal name",
                // .shadows_builtin_type => "identifier shadows builtin type",
                // .shadows_keyword => "identifier shadows builtin keyword",
                // .named_var_type_context => "use of variable identifier in type expression",
                // .named_type_var_context => "use of type identifier in expression",
                // .const_variable_assign => "cannot assign new value to constant",
                .unknown_identifier => "identifier doesn't exist in current scope",
                .type_coerce_fail => "expression type cannot coerce to result location",
                .type_unary_invalid => "invalid expression type for unary operator",
                .type_binary_diff => "both expressions in a binary expression must have the same type",
                .type_binary_invalid => "invalid expression type for binary operator",
                .field_duplicate => "duplicate field name",
                .field_unknown => "unknown field initializer name in literal",
                // .call_nonfunc => "attempted to call non-function type",
                // .call_argcount => "incorrect number of arguments to function call",
                // .binary_diffsign => "operands to binary arithmetic must have same sign",
                // .coerce_sint_to_uint => "cannot coerce signed integer to unsigned integer",
                // .invalid_lvalue => "cannot use expression as lvalue",
                // .invalid_type => "cannot use expression as type",
                // .unsized_type_alloc => "cannot create mutable variable of comptime or unsized type",
                // .invalid_val_expr => "cannot use expression as value",
                // .invalid_ptr_expr => "invalid expression for assignment location",
                // .invalid_ref_expr => "cannot take reference to expression",
                // .const_pointer_write => "cannot modify through const pointer",
                // .not_indexable => "type does not support indexing",
                // .operator_invalid => "invalid operator for type",
            };

            r.stream.indent();
            var writer = r.stream.writer();

            try writer.print("\x1b[3;36m{s}\x1b[39m:{}:{} \x1b[0;31merror: {s}", .{ r.filename, err.line_number, err.column_number, error_message });
            try r.stream.newline();

            const line_pos_nw = try r.skipWhitespace(err.line_start);
            const skipped_ws = line_pos_nw - err.line_start;

            var next_line_start: Cst.ByteOffset = err.line_start;
            while (r.source[next_line_start] != '\n') : (next_line_start += 1) {}
            try writer.print("\x1b[0m{s}{s}", .{ preview_message, r.source[line_pos_nw..next_line_start] });
            try r.stream.newline();

            var num_spaces = preview_message.len + err.column_number;

            // Prevent undeflows
            if (num_spaces >= skipped_ws) num_spaces -= skipped_ws;
            if (num_spaces >= 1) num_spaces -= 1;

            try writer.writeByteNTimes(' ', num_spaces);
            try writer.print("\x1b[1;32m", .{});
            try writer.writeByteNTimes('^', r.cst.tokenString(err.src_error.token).len);
            try writer.print(" here\x1b[0m", .{});
            r.stream.dedent();
            try r.stream.newline();
        }
    };
}

fn IndentingWriter(comptime width: u32, comptime WriterType: type) type {
    return struct {
        depth: u32,
        underlying_writer: WriterType,
        needs_indent: bool,

        const Self = @This();
        pub const Error = WriterType.Error;
        pub const Writer = io.Writer(*Self, Error, write);

        pub fn newline(self: *Self) !void {
            if (self.needs_indent) try self.writeIndent();
            try self.underlying_writer.writeAll("\n");
            self.needs_indent = true;
        }

        pub fn indent(self: *Self) void {
            self.depth += 1;
        }

        pub fn dedent(self: *Self) void {
            std.debug.assert(self.depth >= 1);
            self.depth -= 1;
        }

        fn writeIndent(self: *Self) !void {
            self.needs_indent = false;

            var i: u32 = 0;
            while (i < self.depth) : (i += 1) {
                try self.underlying_writer.writeAll(" " ** width);
            }
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            if (self.needs_indent) try self.writeIndent();
            return self.underlying_writer.write(bytes);
        }
    };
}

fn indentingWriter(comptime width: u32, underlying_stream: anytype) IndentingWriter(width, @TypeOf(underlying_stream)) {
    return .{ .depth = 0, .underlying_writer = underlying_stream, .needs_indent = false };
}
