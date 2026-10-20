const std = @import("std");

pub fn ParseResult(comptime Options: type) type {
    return union(enum) {
        pub const Valid = struct {
            options: Options,
            positionals: []const []const u8,
        };
        pub const Error = union(enum) {
            /// An unknown flag
            unknown_flag: []const u8,
            /// A shorthand that's not valid (e.g. "-aaa")
            invalid_shorthand: []const u8,
            /// An unknown shorthand
            unknown_shorthand: []const u8,
            /// Opposite flags are passed at the same time
            contradictory_flags: []const u8,
            /// A flag is passed multiple times
            duplicate_flag: []const u8,
            /// Multiple equal signs in a flag
            multiple_equal_signs: []const u8,
            /// A non-bool flag was passed without a value
            flag_without_value: []const u8,
            /// A bool flag was passed with a value
            unexpected_value: []const u8,
            /// A flag was given an invalid value
            invalid_value: struct { key: []const u8, value: []const u8 },

            pub fn format(err: Error, writer: *std.Io.Writer) std.Io.Writer.Error!void {
                switch (err) {
                    .unknown_flag => |flag| try writer.print("Unknown flag: \"{s}\"", .{flag}),
                    .invalid_shorthand => |shorthand| try writer.print("Invalid shorthand: \"{s}\"", .{shorthand}),
                    .unknown_shorthand => |shorthand| try writer.print("Unknown shorthand: \"{s}\"", .{shorthand}),
                    .contradictory_flags => |flag| try writer.print("Contradiction found for flag \"{s}\"", .{flag}),
                    .duplicate_flag => |flag| try writer.print("Duplicate flag: \"{s}\"", .{flag}),
                    .multiple_equal_signs => |arg| try writer.print("Multiple equal signs in \"{s}\"", .{arg}),
                    .flag_without_value => |flag| try writer.print("Flag \"{s}\" was given no value", .{flag}),
                    .unexpected_value => |flag| try writer.print("Unexpected value for flag \"{s}\"", .{flag}),
                    .invalid_value => |flag| try writer.print("Invalid value \"{s}\" for flag \"{s}\"", .{ flag.value, flag.key }),
                }
            }
        };

        valid: Valid,
        err: Error,

        pub fn deinit(result: @This(), allocator: std.mem.Allocator) void {
            switch (result) {
                .valid => |valid| allocator.free(valid.positionals),
                .err => {},
            }
        }
    };
}

/// Parse arguments
pub fn parse(comptime Options: type, allocator: std.mem.Allocator, arg_iter: *std.process.Args.Iterator) !ParseResult(Options) {
    std.debug.assert(arg_iter.next() != null);

    var positionals: std.ArrayList([]const u8) = .empty;
    defer positionals.deinit(allocator);

    var used_flags = std.StringHashMap(void).init(allocator);
    defer used_flags.deinit();

    var options = Options{};

    // Check that all opposites are unique and correspond to valid base flags
    if (@hasDecl(Options, "opposites")) {
        const field_names = @typeInfo(@TypeOf(Options.opposites)).@"struct".field_names;

        inline for (field_names, 0..) |f1, idx1| {
            if (!@hasField(Options, f1)) {
                @compileError("Cannot define opposite for unknown flag \"" ++ f1 ++ "\"");
            }

            // Iterate only over unexamined pairs
            inline for (field_names[idx1 + 1 ..]) |f2| {
                if (comptime std.mem.eql(u8, @field(Options.opposites, f1), @field(Options.opposites, f2))) {
                    @compileError("Options " ++ f1 ++ " and " ++ f2 ++ " have the same opposite");
                }
            }
        }
    }

    var raw_parse = false;

    var last_arg_key: ?[]const u8 = null;
    var last_arg_value: ?[]const u8 = null;
    while (true) {
        if (last_arg_value != null) std.debug.assert(last_arg_key != null);

        const arg = last_arg_value orelse arg_iter.next() orelse break;

        std.debug.assert(arg.len > 0);

        if (last_arg_key) |key| {
            last_arg_key = null;
            last_arg_value = null;

            if (try used_flags.fetchPut(key, {})) |_| return .{ .err = .{ .duplicate_flag = key } };

            find_flag: {
                inline for (@typeInfo(Options).@"struct".field_names) |field_name| {
                    if (std.mem.eql(u8, key, field_name)) {
                        const RawT = @FieldType(Options, field_name);
                        const T = if (@typeInfo(RawT) == .optional) std.meta.Child(RawT) else RawT;
                        switch (T) {
                            bool => return .{ .err = .{ .unexpected_value = key } },
                            []const u8 => {
                                @field(options, field_name) = arg;
                            },
                            else => switch (@typeInfo(T)) {
                                .@"enum" => {
                                    find_value: {
                                        inline for (@typeInfo(T).@"enum".field_names) |enum_name| {
                                            if (std.mem.eql(u8, arg, enum_name)) {
                                                @field(options, field_name) = @field(T, enum_name);

                                                break :find_value;
                                            }
                                        }

                                        return .{ .err = .{ .invalid_value = .{ .key = key, .value = arg } } };
                                    }
                                },
                                .int => {
                                    @field(options, field_name) = std.fmt.parseInt(T, arg, 10) catch {
                                        return .{ .err = .{ .invalid_value = .{ .key = key, .value = arg } } };
                                    };
                                },
                                else => |other| @compileError(@tagName(other) ++ " types not supported"),
                            },
                        }

                        break :find_flag;
                    }
                }

                return .{ .err = .{ .unknown_flag = key } };
            }
        } else {
            if (arg[0] == '-' and !raw_parse) {
                if (arg.len == 1) {
                    try positionals.append(allocator, arg);
                } else if (arg[1] == '-') {
                    if (arg.len == 2) {
                        raw_parse = true;
                    } else {
                        var equal_iter = std.mem.splitScalar(u8, arg[2..], '=');
                        const name = equal_iter.next().?;
                        const value = equal_iter.next();
                        if (equal_iter.next()) |_| {
                            return .{ .err = .{ .multiple_equal_signs = arg } };
                        }

                        if (value) |v| {
                            if (v.len == 0) {
                                return .{ .err = .{ .flag_without_value = name } };
                            }

                            last_arg_key = name;
                            last_arg_value = v;
                        } else {
                            find_flag: {
                                inline for (@typeInfo(Options).@"struct".field_names) |field_name| {
                                    if (std.mem.eql(u8, name, field_name)) {
                                        const RawT = @FieldType(Options, field_name);
                                        const T = if (@typeInfo(RawT) == .optional) std.meta.Child(RawT) else RawT;
                                        switch (T) {
                                            bool => {
                                                @field(options, field_name) = true;
                                                if (try used_flags.fetchPut(field_name, {})) |_| return .{ .err = .{ .duplicate_flag = field_name } };
                                            },
                                            else => {
                                                last_arg_key = name;
                                            },
                                        }

                                        break :find_flag;
                                    }

                                    if (@hasDecl(Options, "opposites")) {
                                        if (@hasField(@TypeOf(Options.opposites), field_name)) {
                                            const opposite = @field(Options.opposites, field_name);
                                            if (std.mem.eql(u8, name, opposite)) {
                                                const RawT = @FieldType(Options, field_name);
                                                const T = if (@typeInfo(RawT) == .optional) std.meta.Child(RawT) else RawT;
                                                if (T != bool) {
                                                    @compileError("Option " ++ field_name ++ " has an opposite but is not bool");
                                                }

                                                @field(options, field_name) = false;
                                                if (try used_flags.fetchPut(opposite, {})) |_| return .{ .err = .{ .duplicate_flag = opposite } };
                                                break :find_flag;
                                            }
                                        }
                                    }
                                }

                                return .{ .err = .{ .unknown_flag = name } };
                            }
                        }
                    }
                } else {
                    if (arg.len > 2) {
                        return .{ .err = .{ .invalid_shorthand = arg } };
                    } else {
                        const shorthand = arg[1];
                        find_shorthand: {
                            if (@hasDecl(Options, "shorthands")) {
                                inline for (@typeInfo(@TypeOf(Options.shorthands)).@"struct".field_names) |field_name| {
                                    if (field_name.len != 1) {
                                        @compileError("Shorthand " ++ field_name ++ " is not a single character long");
                                    }

                                    if (shorthand == field_name[0]) {
                                        if (!@hasField(Options, @field(Options.shorthands, field_name))) {
                                            @compileError("Shorthand " ++ field_name ++ " assigned to non-existent property " ++ @field(Options.shorthands, field_name));
                                        }

                                        const RawT = @FieldType(Options, @field(Options.shorthands, field_name));
                                        const T = if (@typeInfo(RawT) == .optional) std.meta.Child(RawT) else RawT;
                                        if (T != bool) {
                                            @compileError("Property " ++ @field(Options.shorthands, field_name) ++ " with shorthand " ++ field_name ++ " is not bool");
                                        }

                                        @field(options, @field(Options.shorthands, field_name)) = true;
                                        if (try used_flags.fetchPut(@field(Options.shorthands, field_name), {})) |_| return .{ .err = .{ .duplicate_flag = @field(Options.shorthands, field_name) } };
                                        break :find_shorthand;
                                    }
                                }
                            }
                            return .{ .err = .{ .unknown_shorthand = arg } };
                        }
                    }
                }
            } else {
                try positionals.append(allocator, arg);
            }
        }
    }

    if (last_arg_key) |key| {
        return .{ .err = .{ .flag_without_value = key } };
    }

    if (@hasDecl(Options, "opposites")) {
        inline for (@typeInfo(@TypeOf(Options.opposites)).@"struct".field_names) |field_name| {
            if (used_flags.get(field_name)) |_| {
                if (used_flags.get(@field(Options.opposites, field_name))) |_| {
                    return .{ .err = .{ .contradictory_flags = field_name } };
                }
            }
        }
    }

    return .{
        .valid = .{
            .options = options,
            .positionals = try positionals.toOwnedSlice(allocator),
        },
    };
}

pub const PrintHelpOptions = struct {
    // Custom text to print
    text: ?[]const u8 = null,
};

pub fn printHelp(comptime Options: type, options: PrintHelpOptions) void {
    if (options.text) |text| {
        std.debug.print("{s}\n\n", .{text});
    }

    if (@hasDecl(Options, "descriptions")) {
        inline for (@typeInfo(@TypeOf(Options.descriptions)).@"struct".field_names) |field_name| {
            if (!@hasField(Options, field_name)) {
                @compileError("Cannot define description for unknown flag \"" ++ field_name ++ "\"");
            }
        }
    }

    comptime var flags: [@typeInfo(Options).@"struct".field_names.len][]const u8 = undefined;

    inline for (@typeInfo(Options).@"struct".field_names, 0..) |flag, i| {
        comptime var f: []const u8 = "";

        if (@hasDecl(Options, "shorthands")) {
            inline for (@typeInfo(@TypeOf(Options.shorthands)).@"struct".field_names) |field_name| {
                if (comptime std.mem.eql(u8, @field(Options.shorthands, field_name), flag)) {
                    f = f ++ "-" ++ field_name ++ ", ";
                }
            }
        }

        f = f ++ "--" ++ flag;

        if (@hasDecl(Options, "opposites")) {
            if (@hasField(@TypeOf(Options.opposites), flag)) {
                f = f ++ ", --" ++ @field(Options.opposites, flag);
            }
        }

        flags[i] = f;
    }

    comptime var max_flag_len = 0;
    inline for (flags) |flag| {
        max_flag_len = @max(max_flag_len, flag.len);
    }

    std.debug.print("Flags:\n", .{});
    inline for (flags, 0..) |flag, i| {
        const base_flag = @typeInfo(Options).@"struct".field_names[i];

        std.debug.print("  " ++ flag, .{});
        for (0..max_flag_len - flag.len + 2) |_| {
            std.debug.print(" ", .{});
        }

        if (@hasDecl(Options, "descriptions") and @hasField(@TypeOf(Options.descriptions), base_flag)) {
            std.debug.print(@field(Options.descriptions, base_flag), .{});
        }

        std.debug.print("\n", .{});

        const RawT = @FieldType(Options, base_flag);
        const T = if (@typeInfo(RawT) == .optional) std.meta.Child(RawT) else RawT;
        if (@typeInfo(T) == .@"enum") {
            inline for (@typeInfo(T).@"enum".field_names) |enum_name| {
                for (0..max_flag_len + 6) |_| {
                    std.debug.print(" ", .{});
                }

                std.debug.print("- " ++ enum_name ++ "\n", .{});
            }
        }
    }
}

fn testParse(Options: type, args: []const [*:0]const u8, expected: ParseResult(Options)) !void {
    var iter: std.process.Args.Iterator = .{ .inner = .{ .remaining = args } };

    const res = try parse(Options, std.testing.allocator, &iter);
    defer res.deinit(std.testing.allocator);

    try std.testing.expectEqualDeep(expected, res);
}

test "raw parsing" {
    try testParse(struct {
        foo: bool = false,
        bar: bool = false,
    }, &.{ "name", "--foo", "--", "--bar" }, .{
        .valid = .{
            .options = .{
                .foo = true,
                .bar = false,
            },
            .positionals = &.{"--bar"},
        },
    });
}

test "number parsing" {
    try testParse(struct {
        foo: usize = 3,
    }, &.{ "name", "--foo", "4" }, .{
        .valid = .{
            .options = .{
                .foo = 4,
            },
            .positionals = &.{},
        },
    });
}

test "number overflow" {
    try testParse(struct {
        foo: u8 = 0,
    }, &.{ "name", "--foo", "256" }, .{
        .err = .{
            .invalid_value = .{
                .key = "foo",
                .value = "256",
            },
        },
    });
}

test "multiple equal signs" {
    try testParse(struct {}, &.{ "name", "--foo=bar=baz" }, .{
        .err = .{ .multiple_equal_signs = "--foo=bar=baz" },
    });
}
