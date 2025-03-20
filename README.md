<div align="center">
  <img src="logo.svg" width="50%" />
</div>

# Pennant

Argument parser for Zig.

## Features
- Opposite flags (`--foo`/`--no-foo`, `--uppercase`/`--lowercase`), which act on a single variable and are mutually exclusive
- Values with and without equals sign (`--foo bar` and `--foo=bar` are both valid and equivalent)
- Bool flags never take a value, non-bool flags always do
- Shorthands for bool flags
- Everything is defined in a single struct, with the power of Zig's `comptime`

## Examples

Examples are available in the `examples` folder. They can also be run via `zig build example-[name]`.

## Roadmap
- [x] Basic boolean flags
- [x] Opposite flags
- [x] Shorthands
- [x] Enum flags
- [ ] Help message
- [ ] Error messages
- [ ] Fuzzing
