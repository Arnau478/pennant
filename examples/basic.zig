const std = @import("std");
const pennant = @import("pennant");

pub const Options = struct {
    help: bool = false,
    letter: ?enum { a, b, c } = null,
    uppercase: bool = false,

    pub const shorthands = .{
        .h = "help",
    };

    pub const opposites = .{
        .uppercase = "lowercase",
    };
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const res = try pennant.parseForProcess(Options, allocator);
    defer res.deinit(allocator);

    switch (res) {
        .valid => |args| {
            if (args.options.uppercase) {
                std.log.info("HELLO", .{});
            } else {
                std.log.info("hello", .{});
            }

            if (args.options.letter) |letter| {
                std.log.info("Letter: {s}", .{@tagName(letter)});
            }
        },
        .err => |err| {
            std.log.err("{}", .{err});
        },
    }
}
