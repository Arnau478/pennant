const std = @import("std");
const pennant = @import("pennant");

pub const Options = struct {
    help: bool = false,
    letter: ?enum { a, b, c } = null,
    uppercase: bool = false,
    text: ?[]const u8 = null,

    pub const shorthands = .{
        .h = "help",
    };

    pub const opposites = .{
        .uppercase = "lowercase",
    };

    pub const descriptions = .{
        .help = "Show this help message",
        .letter = "Choose a letter",
        .uppercase = "Use lowercase or uppercase",
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
            if (args.options.help) {
                pennant.printHelp(Options, .{ .text = 
                    \\An example command
                });
            } else {
                if (args.options.uppercase) {
                    std.log.info("HELLO", .{});
                } else {
                    std.log.info("hello", .{});
                }

                if (args.options.letter) |letter| {
                    std.log.info("Letter: {s}", .{@tagName(letter)});
                }

                if (args.options.text) |text| {
                    std.log.info("Text: \"{s}\"", .{text});
                }
            }
        },
        .err => |err| {
            std.log.err("{}", .{err});
        },
    }
}
