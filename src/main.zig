const std = @import("std");
const lex = @import("lex.zig");
const Cst = @import("Cst.zig");
const parse = @import("parse.zig");

const io = std.io;
const max_file_size = std.math.maxInt(u32);

const Allocator = std.mem.Allocator;
const Lexer = lex.Lexer;

pub fn readSource(gpa: Allocator, input_filename: []const u8) ![:0]u8 {
    var file = try std.fs.cwd().openFile(input_filename, .{});
    defer file.close();
    const stat = try file.stat();
    if (stat.size > max_file_size) {
        std.log.err("File size too large, must be at most {} bytes", .{max_file_size});
        std.process.exit(1);
    }

    const source = try gpa.allocSentinel(u8, @intCast(stat.size), 0);
    const size = try file.readAll(source);
    if (stat.size != size) {
        std.log.err("Failed to read entire source file", .{});
        std.process.exit(1);
    }

    return source;
}

pub fn main() !void {
    var allocator: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(allocator.deinit() == .ok);
    const gpa = allocator.allocator();

    var args = try std.process.argsWithAllocator(gpa);
    defer args.deinit();

    _ = args.next(); // skip executable
    const filename = args.next().?;

    // read in the entire source file, hard to stream in because the lexer
    // needs a lookahead
    const source = try readSource(gpa, filename);

    // the Cst owns the source
    var tree = try parse.parse(gpa, source);
    defer tree.deinit(gpa);
}
