//! EmbedFile is used to create a module whose declarations
//! are calls to `@embedFile` with contents provided by the Zig build system
//!
//! EmbedFile.zig is NOT an official part of the Zig build system;
//! please report any issues at https://github.com/robbielyman/EmbedFile.zig
const EmbedFile = @This();

step: std.Build.Step,
contents: std.ArrayListUnmanaged(Declaration),
generated_file: std.Build.GeneratedFile,
wf: *std.Build.Step.WriteFile,

pub const base_id: std.Build.Step.Id = .custom;

pub const Declaration = struct {
    alignment: ?u29,
    name: []const u8,
    kind: Kind,

    const Kind = enum { file, directory };
};

/// creates a new EmbedFile step
pub fn create(owner: *std.Build) *EmbedFile {
    const embed_file = owner.allocator.create(EmbedFile) catch @panic("OOM");
    embed_file.* = .{
        .step = std.Build.Step.init(.{
            .id = base_id,
            .name = "EmbedFile",
            .owner = owner,
            .makeFn = make,
        }),
        .contents = .{},
        .generated_file = .{ .step = &embed_file.step },
        .wf = std.Build.Step.WriteFile.create(owner),
    };
    embed_file.wf.step.name = "EmbedFile: WriteFile";
    embed_file.step.dependOn(&embed_file.wf.step);
    embed_file.wf.getDirectory().addStepDependencies(&embed_file.step);
    return embed_file;
}

/// adds a declaration by name, contents and alignment directly
pub fn add(embed_file: *EmbedFile, name: []const u8, bytes: []const u8, alignment: ?u29) void {
    const b = embed_file.step.owner;
    const gpa = b.allocator;
    _ = embed_file.wf.add(name, bytes);
    embed_file.contents.append(gpa, .{
        .alignment = alignment,
        .name = b.dupe(name),
        .kind = .file,
    }) catch @panic("OOM");
}

/// name is the eventual declaration name to be used, while sub_path is the name as it exists on the file system
pub fn addFile(
    embed_file: *EmbedFile,
    source: std.Build.LazyPath,
    sub_path: []const u8,
    name: []const u8,
    alignment: ?u29,
) void {
    const b = embed_file.step.owner;
    const gpa = b.allocator;
    source.addStepDependencies(&embed_file.step);
    _ = embed_file.wf.addCopyFile(source.path(b, sub_path), name);
    embed_file.contents.append(gpa, .{
        .alignment = alignment,
        .name = b.dupe(name),
        .kind = .file,
    }) catch @panic("OOM");
}

/// name is the eventual declaration name to be used as a namespace,
/// while sub_path is the name as it exists on the file system
/// files matching the Directory.Options specification
/// are made available as declarations namespaced under "name";
/// the filename (minus any extension) is used as the declaration name
/// alignment, if non-null, is used as the alignment for all sub-declarations
pub fn addDirectory(
    embed_file: *EmbedFile,
    source: std.Build.LazyPath,
    sub_path: []const u8,
    options: std.Build.Step.WriteFile.Directory.Options,
    name: []const u8,
    alignment: ?u29,
) void {
    const b = embed_file.step.owner;
    const gpa = b.allocator;
    source.addStepDependencies(&embed_file.step);
    _ = embed_file.wf.addCopyDirectory(source.path(b, sub_path), name, options);
    embed_file.contents.append(gpa, .{
        .alignment = alignment,
        .name = b.dupe(name),
        .kind = .directory,
    }) catch @panic("OOM");
}

/// returns a `LazyPath` representing the Zig source file generated from this `EmbedFile`
pub fn getSource(embed_file: *EmbedFile) std.Build.LazyPath {
    return .{ .generated = .{ .file = &embed_file.generated_file } };
}

/// returns a `Module` containing the Zig source file generated from this `EmbedFile`
pub fn createModule(embed_file: *EmbedFile) *std.Build.Module {
    return embed_file.step.owner.createModule(.{ .root_source_file = embed_file.getSource() });
}

fn make(step: *std.Build.Step, prog_node: std.Progress.Node) !void {
    _ = prog_node;
    const b = step.owner;
    const gpa = b.allocator;
    const embed_file: *EmbedFile = @fieldParentPtr("step", step);

    var man = b.graph.cache.obtain();
    defer man.deinit();

    man.hash.add(@as(u32, 0x478502ab));

    // embed_file.wf.generated_directory.path.? is an absolute path
    const len = b.cache_root.path.?.len;
    const gen_dir_path = embed_file.wf.generated_directory.path.?[len + 1 ..];
    var dir = try b.cache_root.handle.openDir(gen_dir_path, .{});
    defer dir.close();

    for (embed_file.contents.items) |decl| {
        man.hash.addBytes(decl.name);
        man.hash.addOptional(decl.alignment);
        man.hash.add(decl.kind);
        switch (decl.kind) {
            .file => _ = try man.addFile(b.cache_root.join(gpa, &.{ gen_dir_path, decl.name }) catch @panic("OOM"), null),
            .directory => {
                var sub_dir = try dir.openDir(decl.name, .{ .iterate = true });
                defer sub_dir.close();

                var it = try sub_dir.walk(gpa);
                while (try it.next()) |entry| {
                    if (entry.kind == .file) _ = try man.addFile(b.cache_root.join(gpa, &.{ gen_dir_path, decl.name, entry.path }) catch @panic("OOM"), null);
                }
            },
        }
    }

    if (try step.cacheHit(&man)) {
        const digest = man.final();
        embed_file.generated_file.path = try b.cache_root.join(b.allocator, &.{
            gen_dir_path, digest ++ ".zig",
        });
        return;
    }

    var contents: std.ArrayListUnmanaged(u8) = .{};
    defer contents.deinit(gpa);
    const writer = contents.writer(gpa);
    writer.writeAll(
        \\//! This file is automatically generated by EmbedFile.zig!
        \\//! EmbedFile.zig is NOT an official part of the Zig build system;
        \\//! please report any issues at https://github.com/robbielyman/EmbedFile.zig
        \\//!
        \\//! If you would like to install or commit this file and have it compile successfully,
        \\//! be sure to install or write its entire parent directory in the Zig cache along with it.
        \\
    ) catch @panic("OOM");

    for (embed_file.contents.items) |decl| {
        const path = b.pathJoin(&.{ gen_dir_path, decl.name });
        switch (decl.kind) {
            .file => if (decl.alignment) |@"align"|
                writer.print("pub const @\"{s}\" align({d}) = @embedFile(\"{s}\").*;\n", .{
                    decl.name, @"align", decl.name,
                }) catch @panic("OOM")
            else
                writer.print("pub const @\"{s}\" = @embedFile(\"{s}\").*;\n", .{
                    decl.name, decl.name,
                }) catch @panic("OOM"),
            .directory => {
                var sub_dir = dir.openDir(decl.name, .{ .iterate = true }) catch |err| {
                    return step.fail("unable to open cache directory '{s}': {s}", .{
                        path, @errorName(err),
                    });
                };
                defer sub_dir.close();

                var root: Node = .{ .node = .{ .name = b.dupe(decl.name), .path = b.dupe(decl.name), .children = .{} } };
                defer root.deinit(gpa);

                var it = try sub_dir.walk(gpa);
                defer it.deinit();
                while (try it.next()) |entry| {
                    const node: Node = switch (entry.kind) {
                        .file => file: {
                            const extension = std.fs.path.extension(entry.basename);
                            const extensionless = entry.basename[0 .. entry.basename.len - extension.len];
                            break :file .{ .leaf = .{
                                .name = b.dupe(extensionless),
                                .path = b.pathJoin(&.{ decl.name, entry.path }),
                                .alignment = decl.alignment,
                            } };
                        },
                        .directory => .{ .node = .{
                            .name = b.dupe(entry.basename),
                            .path = b.pathJoin(&.{ decl.name, entry.path }),
                            .children = .{},
                        } },
                        else => continue,
                    };
                    root.addChild(gpa, node) catch |err| switch (err) {
                        error.OutOfMemory => @panic("OOM"),
                        error.RootSibling => unreachable,
                    };
                }
                writer.print("{}", .{root}) catch @panic("OOM");
            },
        }
    }

    const digest = man.final();
    const sub_path = digest ++ ".zig";

    dir.writeFile(.{ .sub_path = sub_path, .data = contents.items }) catch |err| {
        return step.fail("unable to write file '{s}: {s}", .{
            b.pathJoin(&.{ gen_dir_path, sub_path }), @errorName(err),
        });
    };

    embed_file.generated_file.path = try b.cache_root.join(b.allocator, &.{
        gen_dir_path, sub_path,
    });

    // canonicalize by running zig fmt
    const arena = b.allocator;
    var argv: std.ArrayListUnmanaged([]const u8) = .{};
    try argv.ensureUnusedCapacity(arena, 2 + 1);
    argv.appendAssumeCapacity(b.graph.zig_exe);
    argv.appendAssumeCapacity("fmt");
    argv.appendAssumeCapacity(b.pathFromRoot(embed_file.generated_file.path.?));
    try step.evalChildProcess(argv.items);

    try step.writeManifest(&man);
}

const Node = union(enum) {
    leaf: struct { name: []const u8, path: []const u8, alignment: ?u29 },
    node: struct { name: []const u8, path: []const u8, children: std.ArrayListUnmanaged(Node) },

    pub fn format(node: Node, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (node) {
            .leaf => |leaf| if (leaf.alignment) |@"align"|
                try writer.print("pub const @\"{s}\" align({d}) = @embedFile(\"{s}\").*;\n", .{
                    leaf.name, @"align", leaf.path,
                })
            else
                try writer.print("pub const @\"{s}\" = @embedFile(\"{s}\").*;\n", .{
                    leaf.name, leaf.path,
                }),
            .node => |root| {
                try writer.print("pub const @\"{s}\" = struct {{\n", .{root.name});
                for (root.children.items) |child| try writer.print("{}", .{child});
                try writer.writeAll("};\n");
            },
        }
    }

    fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        switch (node.*) {
            .leaf => |leaf| {
                allocator.free(leaf.name);
                allocator.free(leaf.path);
            },
            .node => |*root| {
                allocator.free(root.name);
                allocator.free(root.path);
                for (root.children.items) |*child| child.deinit(allocator);
                root.children.deinit(allocator);
            },
        }
    }

    fn addChild(root: *Node, gpa: std.mem.Allocator, child: Node) (std.mem.Allocator.Error || error{RootSibling})!void {
        const path = switch (child) {
            inline else => |n| n.path,
        };
        if (!std.mem.startsWith(u8, path, root.node.path)) return error.RootSibling;
        var parent: *Node = root;
        find_parent: while (true) {
            for (parent.node.children.items) |*node|
                switch (node.*) {
                    .leaf => continue,
                    .node => |other_child| if (std.mem.startsWith(u8, path, other_child.path)) {
                        parent = node;
                        continue :find_parent;
                    },
                };
            break :find_parent;
        }
        try parent.node.children.append(gpa, child);
    }
};

test Node {
    const gpa = std.testing.allocator;
    var root: Node = .{ .node = .{
        .name = "root",
        .path = "/",
        .children = .{},
    } };
    defer root.deinit(gpa);
    try std.testing.expectError(error.RootSibling, root.addChild(gpa, .{ .leaf = .{
        .name = "causes error!",
        .path = "doesn't start with slash",
        .alignment = null,
    } }));
    const children: []const Node = &.{
        .{ .leaf = .{ .name = "a", .path = "/a", .alignment = null } },
        .{ .node = .{ .name = "a", .path = "/a", .children = .{} } },
        .{ .node = .{ .name = "a", .path = "/a/a", .children = .{} } },
        .{ .leaf = .{ .name = "a", .path = "/a/a", .alignment = null } },
        .{ .leaf = .{ .name = "a", .path = "/a/a/a", .alignment = null } },
        .{ .leaf = .{ .name = "b", .path = "/a/b", .alignment = null } },
        .{ .leaf = .{ .name = "b", .path = "/a/a/b", .alignment = null } },
    };
    for (children) |child| try root.addChild(gpa, child);

    const root_children: []const Node = &.{
        .{ .leaf = .{ .name = "a", .path = "/a", .alignment = null } },
        .{ .node = .{
            .name = "a",
            .path = "/a",
            .children = .{
                .items = @constCast(&.{
                    .{ .node = .{
                        .name = "a",
                        .path = "/a/a",
                        .children = .{
                            .items = @constCast(&.{
                                .{ .leaf = .{ .name = "a", .path = "/a/a/a", .alignment = null } },
                                .{ .leaf = .{ .name = "b", .path = "/a/a/b", .alignment = null } },
                            }),
                            .capacity = undefined,
                        },
                    } },
                    .{ .leaf = .{ .name = "a", .path = "/a/a", .alignment = null } },
                    .{ .leaf = .{ .name = "b", .path = "/a/b", .alignment = null } },
                }),
                .capacity = undefined,
            },
        } },
    };
    try testHelper(root_children, root.node.children.items);
}

fn testHelper(expected_list: []const Node, actual_list: []const Node) !void {
    try std.testing.expectEqual(expected_list.len, actual_list.len);
    for (actual_list, expected_list) |actual, expected| {
        const actual_name, const actual_path = switch (actual) {
            inline else => |e| .{ e.name, e.path },
        };
        const expected_name, const expected_path = switch (expected) {
            inline else => |e| .{ e.name, e.path },
        };
        std.testing.expectEqualStrings(expected_name, actual_name) catch |err| {
            std.log.err("expected node: {{ name: {s}, path: {s} }} got: {{ name: {s}, path: {s} }}", .{
                expected_name, expected_path, actual_name, actual_path,
            });
            return err;
        };
        std.testing.expectEqualStrings(expected_path, actual_path) catch |err| {
            std.log.err("expected node: {{ name: {s}, path: {s} }} got: {{ name: {s}, path: {s} }}", .{
                expected_name, expected_path, actual_name, actual_path,
            });
            return err;
        };
        const tag = std.meta.activeTag(expected);
        std.testing.expectEqual(tag, actual) catch |err| {
            std.log.err("node: {{ name: {s}, path: {s} }} should be {s}, is {s}", .{
                actual_name, actual_path, @tagName(tag), @tagName(actual),
            });
            return err;
        };
        if (tag != .node) continue;
        testHelper(expected.node.children.items, actual.node.children.items) catch |err| {
            std.log.err("error in children of node: {{ name: {s}, path: {s} }}", .{
                actual_name, actual_path,
            });
            return err;
        };
    }
}

const std = @import("std");
