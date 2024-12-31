const std = @import("std");

pub const EmbedFile = @import("src/EmbedFile.zig");

pub fn addEmbedFiles(b: *std.Build) *EmbedFile {
    return EmbedFile.create(b);
}

pub fn addEmbedFile(b: *std.Build, name: []const u8, bytes: []const u8, alignment: ?u29) *EmbedFile {
    const ret = EmbedFile.create(b);
    ret.add(name, bytes, alignment);
    ret.step.name = b.fmt("EmbedFile {s}", .{name});
    return ret;
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    _ = b.addModule("EmbedFile", .{
        .root_source_file = b.path("src/EmbedFile.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "test EmbedFile");
    test_step.dependOn(tests(b));
}

fn tests(b: *std.Build) *std.Build.Step {
    const test_module = b.addTest(.{
        .root_source_file = b.path("src/test.zig"),
    });

    const write_file = b.addWriteFiles();
    _ = write_file.add("a.include", "a.a");
    _ = write_file.add("b.include", "a.b");
    _ = write_file.add("c" ++ std.fs.path.sep_str ++ "a.include", "a.c.a");
    _ = write_file.add("d.exclude", "a.d");

    const second_write_file = b.addWriteFile("path.txt", "this is to test addFile");

    const embed_file = addEmbedFile(b, "name with spaces", "names can have spaces", null);
    embed_file.addDirectory(write_file.getDirectory(), "", .{ .exclude_extensions = &.{".exclude"}, .include_extensions = &.{".include"} }, "a", 16);
    embed_file.addFile(second_write_file.getDirectory(), "path.txt", "other", null);

    embed_file.step.dependOn(&write_file.step);

    test_module.root_module.addImport("assets", embed_file.createModule());

    const install_output_file = b.addInstallFileWithDir(embed_file.getSource(), .{ .custom = "test-output" }, "module.zig");

    const run_tests = b.addRunArtifact(test_module);
    run_tests.step.dependOn(&install_output_file.step);
    return &run_tests.step;
}
