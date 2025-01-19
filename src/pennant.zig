const std = @import("std");

/// Parse arguments for the running process
pub fn parseForProcess(comptime Options: type, allocator: std.mem.Allocator) !ParseResult(Options) {
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    return parse(Options, allocator, &args);
}

pub fn ParseResult(comptime Options: type) type {
    return union(enum) {
        valid: struct {
            options: Options,
            positionals: []const []const u8,
        },
        err: union(enum) {
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
        },

        pub fn deinit(result: @This(), allocator: std.mem.Allocator) void {
            switch (result) {
                .valid => |valid| allocator.free(valid.positionals),
                .err => {},
            }
        }
    };
}

/// Parse arguments
pub fn parse(comptime Options: type, allocator: std.mem.Allocator, arg_iter: anytype) !ParseResult(Options) {
    std.debug.assert(arg_iter.next() != null);

    var positionals = std.ArrayList([]const u8).init(allocator);
    defer positionals.deinit();

    var used_flags = std.StringHashMap(void).init(allocator);
    defer used_flags.deinit();

    var options = Options{};

    // Check that all opposites are unique and correspond to valid base flags
    if (@hasDecl(Options, "opposites")) {
        inline for (std.meta.fields(@TypeOf(Options.opposites)), 0..) |f1, idx1| {
            if (!@hasField(Options, f1.name)) {
                @compileError("Cannot define opposite for unknown flag \"" ++ f1.name ++ "\"");
            }

            inline for (std.meta.fields(@TypeOf(Options.opposites)), 0..) |f2, idx2| {
                if (idx2 <= idx1) continue;

                if (comptime std.mem.eql(u8, @field(Options.opposites, f1.name), @field(Options.opposites, f2.name))) {
                    @compileLog(@field(Options.opposites, f1.name));
                    @compileLog(@field(Options.opposites, f2.name));
                    @compileError("Options " ++ f1.name ++ " and " ++ f2.name ++ " have the same opposite");
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
                inline for (std.meta.fields(Options)) |field| {
                    if (std.mem.eql(u8, key, field.name)) {
                        const RawT = @TypeOf(@field(options, field.name));
                        const T = if (@typeInfo(RawT) == .optional) std.meta.Child(RawT) else RawT;
                        switch (T) {
                            bool => return .{ .err = .{ .unexpected_value = key } },
                            else => switch (@typeInfo(T)) {
                                .@"enum" => {
                                    find_value: {
                                        inline for (std.meta.fields(T)) |enum_field| {
                                            if (std.mem.eql(u8, arg, enum_field.name)) {
                                                @field(options, field.name) = @field(T, enum_field.name);

                                                break :find_value;
                                            }
                                        }

                                        return .{ .err = .{ .invalid_value = .{ .key = key, .value = arg } } };
                                    }
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
                    try positionals.append(arg);
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
                                inline for (std.meta.fields(Options)) |field| {
                                    if (std.mem.eql(u8, name, field.name)) {
                                        const T = @TypeOf(@field(options, field.name));
                                        switch (T) {
                                            bool => {
                                                @field(options, field.name) = true;
                                                if (try used_flags.fetchPut(field.name, {})) |_| return .{ .err = .{ .duplicate_flag = field.name } };
                                            },
                                            else => {
                                                last_arg_key = name;
                                            },
                                        }

                                        break :find_flag;
                                    }

                                    if (@hasDecl(Options, "opposites")) {
                                        if (@hasField(@TypeOf(Options.opposites), field.name)) {
                                            const opposite = @field(Options.opposites, field.name);
                                            if (std.mem.eql(u8, name, opposite)) {
                                                if (@TypeOf(@field(options, field.name)) != bool) {
                                                    @compileError("Option " ++ field.name ++ " has an opposite but is not bool");
                                                }

                                                @field(options, field.name) = false;
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
                        if (@hasDecl(Options, "shorthands")) {
                            inline for (std.meta.fields(@TypeOf(Options.shorthands))) |field| {
                                if (field.name.len != 1) {
                                    @compileError("Shorthand " ++ field.name ++ " is not a single character long");
                                }

                                if (shorthand == field.name[0]) {
                                    if (!@hasField(Options, @field(Options.shorthands, field.name))) {
                                        @compileError("Shorthand " ++ field.name ++ " assigned to non-existent property " ++ @field(Options.shorthands, field.name));
                                    }

                                    if (@TypeOf(@field(options, @field(Options.shorthands, field.name))) != bool) {
                                        @compileError("Property " ++ @field(Options.shorthands, field.name) ++ " with shorthand " ++ field.name ++ " is not bool");
                                    }

                                    @field(options, @field(Options.shorthands, field.name)) = true;
                                }
                            }
                        }
                    }
                }
            } else {
                try positionals.append(arg);
            }
        }
    }

    if (last_arg_key) |key| {
        return .{ .err = .{ .flag_without_value = key } };
    }

    if (@hasDecl(Options, "opposites")) {
        inline for (std.meta.fields(@TypeOf(Options.opposites))) |field| {
            if (used_flags.get(field.name)) |_| {
                if (used_flags.get(@field(Options.opposites, field.name))) |_| {
                    return .{ .err = .{ .contradictory_flags = field.name } };
                }
            }
        }
    }

    return .{
        .valid = .{
            .options = options,
            .positionals = try positionals.toOwnedSlice(),
        },
    };
}

fn testParse(Options: type, args: []const []const u8, expected: ParseResult(Options)) !void {
    const Iterator = struct {
        args: []const []const u8,
        idx: usize = 0,

        pub fn next(iterator: *@This()) ?[]const u8 {
            if (iterator.idx >= iterator.args.len) return null;
            defer iterator.idx += 1;
            return iterator.args[iterator.idx];
        }
    };

    var iter: Iterator = .{ .args = args };

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

test "multiple equal signs" {
    try testParse(struct {}, &.{ "name", "--foo=bar=baz" }, .{
        .err = .{ .multiple_equal_signs = "--foo=bar=baz" },
    });
}
