const testy align(256) = @embedFile("src/test.zig").*;

test {
    try std.testing.expect(std.mem.isAligned(@intFromPtr(&testy), 256));
}

const std = @import("std");
