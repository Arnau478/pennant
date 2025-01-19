const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const pennant = b.addModule("pennant", .{
        .root_source_file = b.path("src/pennant.zig"),
    });

    inline for (&.{"basic"}) |example| {
        const exe = b.addExecutable(.{
            .name = example,
            .root_source_file = b.path(b.fmt("examples/{s}.zig", .{example})),
            .target = target,
            .optimize = optimize,
        });

        const tls = b.step(b.fmt("example-{s}", .{example}), b.fmt("Run the \"{s}\" example", .{example}));
        const run = b.addRunArtifact(exe);
        if (b.args) |args| {
            run.addArgs(args);
        }
        tls.dependOn(&run.step);

        exe.root_module.addImport("pennant", pennant);
    }

    const tests = b.addTest(.{
        .root_source_file = b.path("src/pennant.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}
