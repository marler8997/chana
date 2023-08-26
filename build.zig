// tested using zig version 0.11.0-dev.2619+bd3e248c7
const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const arocc_dep = b.dependency("arocc", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "chana",
        .root_source_file = .{ .path = "chana.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("aro", arocc_dep.module("aro"));
    b.installArtifact(exe);
}
