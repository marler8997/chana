const std = @import("std");

fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.log.err(fmt, args);
    std.process.exit(0xff);
}

fn oom(err: std.mem.Allocator.Error) noreturn {
    _ = err catch {};
    @panic("Out of memory");
}

const global = struct {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    pub const arena = arena_instance.allocator();
};

pub fn main() !void {
    const cmd_args = blk: {
        const all_args = try std.process.argsAlloc(global.arena);
        if (all_args.len <= 1) {
            std.log.err("Usage: chana DEST_PATH INCLUDE_PATH", .{});
            std.process.exit(0xff);
        }
        break :blk all_args[1..];
    };
    if (cmd_args.len != 2) {
        fatal("expected 2 cmdline arguments but got {}", .{cmd_args.len});
    }

    const dest_path = cmd_args[0];
    const inc_path = cmd_args[1];
    std.log.info("{s} and {s}", .{dest_path, inc_path});

    const inc_dir = std.fs.cwd().openDir(inc_path, .{ .iterate = true }) catch |err|
        fatal("failed to open include path '{s}' with {s}", .{inc_path, @errorName(err)});
    // no need to close

    try std.fs.cwd().makePath(dest_path);
    const dest_dir = try std.fs.cwd().openDir(dest_path, .{});
    // no need to close
    try analyzeDir("", dest_dir, inc_dir);
}

fn analyzeDir(path: []const u8, dest_dir: std.fs.Dir, inc_dir: std.fs.Dir) !void {
    var it = inc_dir.iterate();
    while (try it.next()) |entry| {
        const child_path = try std.fs.path.join(global.arena, &.{path, entry.name});
        defer global.arena.free(child_path);
        switch (entry.kind) {
            .file => {
                const sep: []const u8 = if (path.len == 0) "" else "/";
                std.log.info("{s}{s}{s}", .{path, sep, entry.name});
                const file = try dest_dir.createFile(child_path, .{});
                defer file.close();
                try analyzeFile(file.writer());
            },
            .directory => {
                var child_dir = try inc_dir.openDir(entry.name, .{ .iterate = true });
                defer child_dir.close();
                try dest_dir.makeDir(child_path);
                try analyzeDir(child_path, dest_dir, child_dir);
            },
            else => |kind| fatal("unsupported file type {s}", .{@tagName(kind)}),
        }
    }

}

fn analyzeFile(writer: anytype) !void {
    try writer.writeAll("todo: implement analyze");
}
