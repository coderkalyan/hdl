const std = @import("std");
const lex = @import("lex.zig");

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

    const source = try readSource(gpa, filename);
    defer gpa.free(source);

    var lexer: Lexer = .init(source);
    while (true) {
        const token = lexer.next();
        std.debug.print("{}\n", .{token});
        if (token.tag == .eof) break;
    }
}
