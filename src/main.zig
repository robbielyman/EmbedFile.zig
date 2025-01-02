const std = @import("std");

pub fn main() !void {
    var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    const args = std.process.argsAlloc(gpa) catch @panic("OOM");
    defer std.process.argsFree(gpa, args);
    const options = processArgs(gpa, args) catch @panic("OOM");
    defer options.deinit(gpa);

    run(gpa, options) catch |err| {
        fatal("error while creating EmbedFile module: {s}", .{@errorName(err)});
    };
}

const Options = struct {
    input: []const u8,
    output: ?[]const u8 = null,
    alignment: ?u29 = null,
    include_extensions: ?[]const []const u8 = null,
    exclude_extensions: []const []const u8 = &.{},
    rename: ?[]const u8 = null,

    fn deinit(opts: Options, allocator: std.mem.Allocator) void {
        if (opts.include_extensions) |e| allocator.free(e);
        allocator.free(opts.exclude_extensions);
    }
};

fn processArgs(gpa: std.mem.Allocator, args: []const []const u8) std.mem.Allocator.Error!Options {
    if (args.len < 2) fatalHelp();
    for (args) |arg|
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) fatalHelp();
    var idx: usize = 2;
    var ret: Options = .{
        .input = args[1],
    };
    var maybe_exclude_extensions: ?[]const []const u8 = null;

    while (idx < args.len) : (idx += 1) {
        const arg = args[idx];
        alignment: {
            const alignment = std.fmt.parseUnsigned(u29, arg, 10) catch break :alignment;
            if (ret.alignment != null) fatalHelp();
            ret.alignment = alignment;
            continue;
        }
        if (std.mem.eql(u8, arg, "--output")) {
            idx += 1;
            if (idx == args.len or std.mem.startsWith(u8, args[idx], "--")) fatal("error: directory expected after --output\n", .{});
            if (ret.output != null) fatal("error: duplicate --output argument\n", .{});
            ret.output = args[idx];
            continue;
        }
        if (std.mem.eql(u8, arg, "--exclude")) {
            idx += 1;
            if (idx == args.len or std.mem.startsWith(u8, args[idx], "--")) fatal("error: comma-separated list of extensions expected after --exclude\n", .{});
            if (maybe_exclude_extensions != null) fatal("error: duplicate --exclude argument\n", .{});
            var list: std.ArrayListUnmanaged([]const u8) = .{};
            defer list.deinit(gpa);
            var it = std.mem.tokenizeScalar(u8, args[idx], ',');
            while (it.next()) |next| try list.append(gpa, next);
            maybe_exclude_extensions = try list.toOwnedSlice(gpa);
            continue;
        }
        if (std.mem.eql(u8, arg, "--include")) {
            idx += 1;
            if (idx == args.len or std.mem.startsWith(u8, args[idx], "--")) fatal("error: comma-separated list of extensions expected after --include\n", .{});
            if (ret.include_extensions != null) fatal("error: duplicate --include argument\n", .{});
            var list: std.ArrayListUnmanaged([]const u8) = .{};
            defer list.deinit(gpa);
            var it = std.mem.tokenizeScalar(u8, args[idx], ',');
            while (it.next()) |next| try list.append(gpa, next);
            ret.include_extensions = try list.toOwnedSlice(gpa);
            continue;
        }
        if (std.mem.eql(u8, arg, "--rename")) {
            idx += 1;
            if (idx == args.len) fatal("name expected after --rename\n", .{});
            if (ret.rename != null) fatal("error: duplicate --rename argument\n", .{});
            ret.rename = args[idx];
            continue;
        }

        fatal("error: unexpected argument: {s}\n", .{arg});
    }

    if (maybe_exclude_extensions) |exclude_extensions| ret.exclude_extensions = exclude_extensions;

    return ret;
}

fn run(gpa: std.mem.Allocator, options: Options) !void {
    errdefer {
        std.debug.dumpStackTrace(@errorReturnTrace().?.*);
    }
    const status = try std.fs.cwd().statFile(options.input);
    var dir: std.fs.Dir, var root: Node = switch (status.kind) {
        .file => file: {
            const dir = dir: {
                const sub_path = std.fs.path.dirname(options.input) orelse break :dir std.fs.cwd();
                break :dir try std.fs.cwd().openDir(sub_path, .{});
            };
            const basename = std.fs.path.basename(options.input);
            break :file .{ dir, .{ .leaf = .{
                .name = options.rename orelse stripExtension(basename),
                .path = basename,
                .alignment = options.alignment,
            } } };
        },
        .directory => directory: {
            if (options.rename != null) fatal("error: --rename is only valid when passed a file", .{});
            var dir = try std.fs.cwd().openDir(options.input, .{ .iterate = true });
            var root: Node = .{ .node = .{
                .name = "",
                .path = "",
                .children = .{},
            } };

            var it = try dir.walk(gpa);
            defer it.deinit();

            next_entry: while (try it.next()) |entry| {
                if (entry.kind == .file) {
                    for (options.exclude_extensions) |ext|
                        if (std.mem.endsWith(u8, entry.path, ext)) continue :next_entry;
                    if (options.include_extensions) |extensions| for (extensions) |ext| {
                        if (std.mem.endsWith(u8, entry.path, ext)) break;
                    } else continue :next_entry;
                }
                const node: Node = switch (entry.kind) {
                    else => continue,
                    .file => .{ .leaf = .{
                        .name = gpa.dupe(u8, stripExtension(entry.basename)) catch @panic("OOM"),
                        .path = gpa.dupe(u8, entry.path) catch @panic("OOM"),
                        .alignment = options.alignment,
                    } },
                    .directory => .{ .node = .{
                        .name = gpa.dupe(u8, entry.basename) catch @panic("OOM"),
                        .path = gpa.dupe(u8, entry.path) catch @panic("OOM"),
                        .children = .{},
                    } },
                };
                try root.addChild(gpa, node);
            }
            break :directory .{ dir, root };
        },
        else => fatal("directory expected, got: {s}", .{@tagName(status.kind)}),
    };
    defer dir.close();
    defer switch (root) {
        .leaf => {},
        .node => root.deinit(gpa),
    };

    var contents: std.ArrayListUnmanaged(u8) = .{};
    defer contents.deinit(gpa);
    const writer = contents.writer(gpa);
    writer.writeAll(
        \\//! This file is automatically generated by EmbedFile.zig!
        \\//! EmbedFile.zig is NOT an official part of the Zig build system;
        \\//! please report any issues at https://github.com/robbielyman/EmbedFile.zig
        \\//!
        \\//! If you would like to commit this file and have it compile successfully,
        \\//! be sure to commit its entire parent directory along with it.
        \\
    ) catch @panic("OOM");

    switch (root) {
        .leaf => writer.print("{}", .{root}) catch @panic("OOM"),
        .node => |node| for (node.children.items) |child| {
            writer.print("{}", .{child}) catch @panic("OOM");
        },
    }

    var output_dir = if (options.output) |path| try std.fs.cwd().makeOpenPath(path, .{}) else std.fs.cwd();
    defer if (options.output != null) output_dir.close();

    switch (root) {
        .leaf => try root.updateAll(gpa, dir, output_dir),
        .node => |node| for (node.children.items) |child| try child.updateAll(gpa, dir, output_dir),
    }

    try std.io.getStdOut().writeAll(contents.items);
}

fn stripExtension(basename: []const u8) []const u8 {
    const extension = std.fs.path.extension(basename);
    return basename[0 .. basename.len - extension.len];
}

const Node = union(enum) {
    leaf: struct { name: []const u8, path: []const u8, alignment: ?u29 },
    node: struct { name: []const u8, path: []const u8, children: std.ArrayListUnmanaged(Node) },

    fn updateAll(root: Node, gpa: std.mem.Allocator, src_dir: std.fs.Dir, dest_dir: std.fs.Dir) !void {
        switch (root) {
            .leaf => |leaf| _ = try src_dir.updateFile(leaf.path, dest_dir, leaf.path, .{}),
            .node => |node| {
                var open_dir = try dest_dir.makeOpenPath(node.path, .{});
                defer open_dir.close();
                for (node.children.items) |child| try child.updateAll(gpa, src_dir, dest_dir);
            },
        }
    }

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
                    .node => |other_child| if (std.mem.startsWith(u8, path, other_child.path) and path.len > other_child.path.len) {
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
    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer _ = arena_impl.deinit();
    const arena = arena_impl.allocator();
    try std.testing.expectError(error.RootSibling, root.addChild(arena, .{ .leaf = .{
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
    for (children) |child| try root.addChild(arena, child);

    const root_great_grandchildren: []const Node = &.{
        .{ .leaf = .{ .name = "a", .path = "/a/a/a", .alignment = null } },
        .{ .leaf = .{ .name = "b", .path = "/a/a/b", .alignment = null } },
    };

    const root_grandchildren: []const Node = &.{
        .{ .node = .{
            .name = "a",
            .path = "/a/a",
            .children = .{
                .items = @constCast(root_great_grandchildren),
                .capacity = undefined,
            },
        } },
        .{ .leaf = .{ .name = "a", .path = "/a/a", .alignment = null } },
        .{ .leaf = .{ .name = "b", .path = "/a/b", .alignment = null } },
    };

    const root_children: []const Node = &.{
        .{ .leaf = .{ .name = "a", .path = "/a", .alignment = null } },
        .{ .node = .{
            .name = "a",
            .path = "/a",
            .children = .{
                .items = @constCast(root_grandchildren),
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
        std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual)) catch |err| {
            std.log.err("node: {{ name: {s}, path: {s} }} should be {s}, is {s}", .{
                actual_name, actual_path, @tagName(expected), @tagName(actual),
            });
            return err;
        };
        if (actual != .node) continue;
        testHelper(expected.node.children.items, actual.node.children.items) catch |err| {
            std.log.err("error in children of node: {{ name: {s}, path: {s} }}", .{
                actual_name, actual_path,
            });
            return err;
        };
    }
}

fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print(fmt, args);
    std.process.exit(1);
}

fn fatalHelp() noreturn {
    fatal(
        \\Usage: embed-file INPUT_PATH [ALIGNMENT] [options]
        \\
        \\General Options:
        \\  --help, -h              Print usage
        \\  --output path           Output matching files to path
        \\                          (which will be created if it does not exist);
        \\                          defaults to CWD
        \\  --include extensions    Comma-separated list of file extension (e.g. ".lua,.txt")
        \\                          Only matching extensions will be included
        \\  --exclude extensions    Matching extensions will be excluded
        \\  --rename name           Only valid when passed a file as INPUT_PATH
        \\                          The resulting declaration will be named `@"name"`
        \\
        \\
    , .{});
}
