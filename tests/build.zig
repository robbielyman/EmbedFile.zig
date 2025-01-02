const std = @import("std");
const EmbedFile = @import("embed-file");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const tests = b.addTest(.{
        .root_source_file = b.path("tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    const tests_run = b.addRunArtifact(tests);

    const write_file = b.addWriteFiles();
    _ = write_file.add("a.include", "a.a");
    _ = write_file.add("b.include", "a.b");
    _ = write_file.add("c" ++ std.fs.path.sep_str ++ "a.include", "a.c.a");
    _ = write_file.add("d.exclude", "a.d");

    const second_write_file = b.addWriteFile("path.txt", "this is to test addFile");
    const path = second_write_file.getDirectory().path(b, "path.txt");

    const embed_file = EmbedFile.addEmbedFile(b, "name with spaces", "names can have spaces", null);
    embed_file.addDirectory(write_file.getDirectory(), .{ .exclude_extensions = &.{".exclude"}, .include_extensions = &.{".include"} }, "a", 16);
    embed_file.addFile(path, "other", null);

    tests.root_module.addImport("assets", embed_file.module);

    const output = embed_file.writeSources("embed-file-tests");
    const install_output = b.addInstallDirectory(.{
        .source_dir = output.getDirectory(),
        .install_dir = .{ .custom = "test-output" },
        .install_subdir = "",
    });

    const tests_step = b.step("test", "run the tests");
    tests_step.dependOn(&tests_run.step);
    tests_step.dependOn(&install_output.step);
}
