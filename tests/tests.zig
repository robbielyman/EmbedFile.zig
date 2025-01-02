const assets = @import("assets");
const std = @import("std");
const assert = std.debug.assert;
const eql = std.mem.eql;

test {
    const bytes: []const []const u8 = &.{
        &assets.a.a, &assets.a.b, &assets.a.c.a,
    };
    const expected: []const []const u8 = &.{
        "a.a", "a.b", "a.c.a",
    };
    for (bytes, expected) |actual, expectation| {
        try std.testing.expectEqualStrings(expectation, actual);
        try std.testing.expect(std.mem.isAligned(@intFromPtr(actual.ptr), 16));
    }
    try std.testing.expect(!@hasDecl(assets.a, "d"));
    try std.testing.expectEqualStrings("names can have spaces", &assets.@"name with spaces");
    try std.testing.expectEqualStrings("this is to test addFile", &assets.other);
}
