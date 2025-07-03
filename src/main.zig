const std = @import("std");
const lex = @import("lex.zig");
const Cst = @import("Cst.zig");
const parse = @import("parse.zig");
const InternPool = @import("InternPool.zig");
const Sema = @import("Sema.zig");
const Builder = @import("verilog/Builder.zig");
const Mir = @import("verilog/Mir.zig");
const Formatter = @import("verilog/print.zig").Formatter;

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
    const root = args.next().?;

    // read in the entire source file, hard to stream in because the lexer
    // needs a lookahead
    const source = try readSource(gpa, filename);

    // the Cst owns the source
    var tree = try parse.parse(gpa, source);
    defer tree.deinit(gpa);

    var pool = try InternPool.init(gpa);
    defer pool.deinit();

    Sema.analyze(gpa, &pool, &tree) catch |err| switch (err) {
        error.SourceError => return,
        else => return err,
    };

    const stdout = std.io.getStdOut().writer();

    const root_ip = try pool.put(.{ .str = root });
    const decl_index = pool.decls_map.get(root_ip).?;
    const decl = pool.declPtr(decl_index);
    const module = pool.get(decl.type).ty.module;
    var mir = try Builder.build(gpa, &pool, module);
    defer mir.deinit(gpa);

    var formatter: Formatter(@TypeOf(stdout)) = .init(stdout, &pool, &mir);
    try formatter.format(mir.toplevel);
    // std.debug.print("{any}\n", .{mir.nodes.items(.tag)});

    // var decls = pool.decls_map.iterator();
    // while (decls.next()) |entry| {
    //     const name = pool.get(entry.key_ptr.*).str;
    //     const decl = entry.value_ptr.*;
    //     std.debug.print("{s} {}\n", .{ name, decl });
    // }
    //
    // var airs = pool.airs.iterator(0);
    // while (airs.next()) |air| {
    //     try codegen.generate(gpa, stdout, &pool, air);
    // }
}
