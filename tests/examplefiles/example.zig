const std = @import("std");
const Allocator = mem.Allocator;
const mem = std.mem;
const ast = std.zig.ast;
const Visib = @import("visib.zig").Visib;
const event = std.event;
const Value = @import("value.zig").Value;
const Token = std.zig.Token;
const errmsg = @import("errmsg.zig");
const Scope = @import("scope.zig").Scope;
const Compilation = @import("compilation.zig").Compilation;

pub const Decl = struct {
    id: Id,
    name: []const u8,
    visib: Visib,
    resolution: event.Future(Compilation.BuildError!void),
    parent_scope: *Scope,

    // TODO when we destroy the decl, deref the tree scope
    tree_scope: *Scope.AstTree,

    pub const Table = std.HashMap([]const u8, *Decl, mem.hash_slice_u8, mem.eql_slice_u8);

    pub fn cast(base: *Decl, comptime T: type) ?*T {
        if (base.id != @field(Id, @typeName(T))) return null;
        return @fieldParentPtr(T, "base", base);
    }

    pub fn isExported(base: *const Decl, tree: *ast.Tree) bool {
        switch (base.id) {
            Id.Fn => {
                const fn_decl = @fieldParentPtr(Fn, "base", base);
                return fn_decl.isExported(tree);
            },
            else => return false,
        }
    }

    pub fn getSpan(base: *const Decl) errmsg.Span {
        switch (base.id) {
            Id.Fn => {
                const fn_decl = @fieldParentPtr(Fn, "base", base);
                const fn_proto = fn_decl.fn_proto;
                const start = fn_proto.fn_token;
                const end = fn_proto.name_token orelse start;
                return errmsg.Span{
                    .first = start,
                    .last = end + 1,
                };
            },
            else => @panic("TODO"),
        }
    }

    pub fn findRootScope(base: *const Decl) *Scope.Root {
        return base.parent_scope.findRoot();
    }

    pub const Id = enum {
        Var,
        Fn,
        CompTime,
    };

    pub const Var = struct {
        base: Decl,
    };

    pub const Fn = struct {
        base: Decl,
        value: Val,
        fn_proto: *ast.Node.FnProto,

        // TODO https://github.com/ziglang/zig/issues/683 and then make this anonymous
        pub const Val = union(enum) {
            Unresolved: void,
            Fn: *Value.Fn,
            FnProto: *Value.FnProto,
        };

        pub fn externLibName(self: Fn, tree: *ast.Tree) ?[]const u8 {
            return if (self.fn_proto.extern_export_inline_token) |tok_index| x: {
                const token = tree.tokens.at(tok_index);
                break :x switch (token.id) {
                    Token.Id.Extern => tree.tokenSlicePtr(token),
                    else => null,
                };
            } else null;
        }

        pub fn isExported(self: Fn, tree: *ast.Tree) bool {
            if (self.fn_proto.extern_export_inline_token) |tok_index| {
                const token = tree.tokens.at(tok_index);
                return token.id == Token.Id.Keyword_export;
            } else {
                return false;
            }
        }
    };

    pub const CompTime = struct {
        base: Decl,
    };
};

pub const info_zen =
    \\
    \\ * Communicate intent precisely.
    \\ * Edge cases matter.
    \\ * Favor reading code over writing code.
    \\ * Only one obvious way to do things.
    \\ * Runtime crashes are better than bugs.
    \\ * Compile errors are better than runtime crashes.
    \\ * Incremental improvements.
    \\ * Avoid local maximums.
    \\ * Reduce the amount one must remember.
    \\ * Minimize energy spent on coding style.
    \\ * Together we serve end users.
    \\
    \\
;

fn cmdZen(allocator: *Allocator, args: []const []const u8) !void {
    try stdout.write(info_zen);
}

const usage_internal =
    \\usage: zig internal [subcommand]
    \\
    \\Sub-Commands:
    \\  build-info                   Print static compiler build-info
    \\
    \\
;

fn cmdInternal(allocator: *Allocator, args: []const []const u8) !void {
    if (args.len == 0) {
        try stderr.write(usage_internal);
        os.exit(1);
    }

    const sub_commands = []Command{Command{
        .name = "build-info",
        .exec = cmdInternalBuildInfo,
    }};

    for (sub_commands) |sub_command| {
        if (mem.eql(u8, sub_command.name, args[0])) {
            try sub_command.exec(allocator, args[1..]);
            return;
        }
    }

    try stderr.print("unknown sub command: {}\n\n", args[0]);
    try stderr.write(usage_internal);
}

fn cmdInternalBuildInfo(allocator: *Allocator, args: []const []const u8) !void {
    try stdout.print(
        \\ZIG_CMAKE_BINARY_DIR {}
        \\ZIG_CXX_COMPILER     {}
        \\ZIG_LLVM_CONFIG_EXE  {}
        \\ZIG_LLD_INCLUDE_PATH {}
        \\ZIG_LLD_LIBRARIES    {}
        \\ZIG_STD_FILES        {}
        \\ZIG_C_HEADER_FILES   {}
        \\ZIG_DIA_GUIDS_LIB    {}
        \\
    ,
        std.cstr.toSliceConst(c.ZIG_CMAKE_BINARY_DIR),
        std.cstr.toSliceConst(c.ZIG_CXX_COMPILER),
        std.cstr.toSliceConst(c.ZIG_LLVM_CONFIG_EXE),
        std.cstr.toSliceConst(c.ZIG_LLD_INCLUDE_PATH),
        std.cstr.toSliceConst(c.ZIG_LLD_LIBRARIES),
        std.cstr.toSliceConst(c.ZIG_STD_FILES),
        std.cstr.toSliceConst(c.ZIG_C_HEADER_FILES),
        std.cstr.toSliceConst(c.ZIG_DIA_GUIDS_LIB),
    );
}

fn test__floatuntisf(a: u128, expected: f32) void {
    const x = __floatuntisf(a);
    testing.expect(x == expected);
}

test "floatuntisf" {
    test__floatuntisf(0, 0.0);

    test__floatuntisf(1, 1.0);
    test__floatuntisf(2, 2.0);
    test__floatuntisf(20, 20.0);

    test__floatuntisf(0x7FFFFF8000000000, 0x1.FFFFFEp+62);
    test__floatuntisf(0x7FFFFF0000000000, 0x1.FFFFFCp+62);

    test__floatuntisf(make_ti(0x8000008000000000, 0), 0x1.000001p+127);
    test__floatuntisf(make_ti(0x8000000000000800, 0), 0x1.0p+127);
    test__floatuntisf(make_ti(0x8000010000000000, 0), 0x1.000002p+127);

    test__floatuntisf(make_ti(0x8000000000000000, 0), 0x1.000000p+127);

    test__floatuntisf(0x0007FB72E8000000, 0x1.FEDCBAp+50);

    test__floatuntisf(0x0007FB72EA000000, 0x1.FEDCBA8p+50);
    test__floatuntisf(0x0007FB72EB000000, 0x1.FEDCBACp+50);

    test__floatuntisf(0x0007FB72EC000000, 0x1.FEDCBBp+50);

    test__floatuntisf(0x0007FB72E6000000, 0x1.FEDCB98p+50);
    test__floatuntisf(0x0007FB72E7000000, 0x1.FEDCB9Cp+50);
    test__floatuntisf(0x0007FB72E4000000, 0x1.FEDCB9p+50);

    test__floatuntisf(0xFFFFFFFFFFFFFFFE, 0x1p+64);
    test__floatuntisf(0xFFFFFFFFFFFFFFFF, 0x1p+64);

    test__floatuntisf(0x0007FB72E8000000, 0x1.FEDCBAp+50);

    test__floatuntisf(0x0007FB72EA000000, 0x1.FEDCBAp+50);
    test__floatuntisf(0x0007FB72EB000000, 0x1.FEDCBAp+50);
    test__floatuntisf(0x0007FB72EBFFFFFF, 0x1.FEDCBAp+50);
    test__floatuntisf(0x0007FB72EC000000, 0x1.FEDCBCp+50);
    test__floatuntisf(0x0007FB72E8000001, 0x1.FEDCBAp+50);

    test__floatuntisf(0x0007FB72E6000000, 0x1.FEDCBAp+50);
    test__floatuntisf(0x0007FB72E7000000, 0x1.FEDCBAp+50);
    test__floatuntisf(0x0007FB72E7FFFFFF, 0x1.FEDCBAp+50);
    test__floatuntisf(0x0007FB72E4000001, 0x1.FEDCBAp+50);
    test__floatuntisf(0x0007FB72E4000000, 0x1.FEDCB8p+50);

    test__floatuntisf(make_ti(0x0000000000001FED, 0xCB90000000000001), 0x1.FEDCBAp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBA0000000000000), 0x1.FEDCBAp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBAFFFFFFFFFFFFF), 0x1.FEDCBAp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBB0000000000000), 0x1.FEDCBCp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBB0000000000001), 0x1.FEDCBCp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBBFFFFFFFFFFFFF), 0x1.FEDCBCp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBC0000000000000), 0x1.FEDCBCp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBC0000000000001), 0x1.FEDCBCp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBD0000000000000), 0x1.FEDCBCp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBD0000000000001), 0x1.FEDCBEp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBDFFFFFFFFFFFFF), 0x1.FEDCBEp+76);
    test__floatuntisf(make_ti(0x0000000000001FED, 0xCBE0000000000000), 0x1.FEDCBEp+76);
}

fn trimStart(slice: []const u8, ch: u8) []const u8 {
    var i: usize = 0;
    const test_string = "test\"string";
    for (slice) |b| {
        if (b == '\xa3') break;
        if (b == '\ua3d3') break;
        if (b == '\Ua3d3d3') break;
        if (b == '\t') break;
        if (b == '\n') break;
        if (b == '\\') break;
        if (b == '\'') break;
        if (b == '"') break;
        if (b != 'n') break;
        if (b != '-') break;
        i += 1;
    }

    return slice[i..];
}
