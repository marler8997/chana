// works like ccache, put this in front of your CC command, i.e.
//
//    chana OUT_DIR -- CC ...
//
// TODO: maybe provide a "--find-cc" command-line option to find the C compiler instead?
// TODO: probably require a "--" so options can be specified?
const builtin = @import("builtin");
const std = @import("std");
const aro = @import("aro");

fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.log.err(fmt, args);
    std.os.exit(0xff);
}

fn oom(err: std.mem.Allocator.Error) noreturn {
    _ = err catch {};
    @panic("Out of memory");
}

pub fn main() !u8 {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const cmd_args = blk: {
        const all_args = try std.process.argsAlloc(arena);
        if (all_args.len <= 1) {
            std.log.err("Usage: chana OUT_DIR -- CC_COMMAND...", .{});
            return 0x7f;
        }
        break :blk all_args[1..];
    };

    var opt = struct {
        out_dir: ?[]const u8 = null,
    }{};

    const cc_args = blk: {
        for (cmd_args, 0..) |arg, arg_index| {
            if (std.mem.eql(u8, arg, "--"))
                break :blk cmd_args[arg_index + 1..];

            if (!std.mem.startsWith(u8, arg, "-")) {
                if (opt.out_dir) |_| {
                    std.log.err("chana error: too many non-option cmdline arguments", .{});
                    return 0x7f;
                }
                opt.out_dir = arg;
            } else {

                std.log.err("chana error: unknown cmdline option '{s}'", .{arg});
                return 0x7f;
            }
        }
        std.log.err("chana error: missing '--' to delineate CC command", .{});
        return 0x7f;
    };

    const out_dir = opt.out_dir orelse {
        std.log.err("chana error: missing OUT_DIR command-line argument", .{});
        return 0x7f;
    };

    std.fs.cwd().makeDir(out_dir) catch |e| switch (e) {
        error.PathAlreadyExists => {},
        else => |err| return err,
    };

    const target: std.Target = blk: {
        // TODO: get the target from the cc_args
        break :blk .{
            .cpu = std.Target.Cpu.baseline(.x86_64),
            .os = std.Target.Os.Tag.linux.defaultVersionRange(.x86_64),
            .abi = .musl,
            .ofmt = .elf,
        };
    };


    var cc_options = std.ArrayListUnmanaged([:0]u8){ };
    var cc_out_file: ?[:0]u8 = null;
    var c_files = std.ArrayListUnmanaged([:0]u8){ };

    {
        var i: usize = 0;
        while (i < cc_args.len) : (i += 1) {
            const cc_arg = cc_args[i];
            if (std.mem.endsWith(u8, cc_arg, ".c")) {
                c_files.append(arena, cc_arg) catch |e| oom(e);
            } else if (std.mem.eql(u8, cc_arg, "-o")) {
                i += 1;
                if (i == cc_args.len) fatal("-o requires an argument", .{});
                cc_out_file = cc_args[i];
            } else {
                cc_options.append(arena, cc_arg) catch |e| oom(e);
            }
        }
    }

    for (cc_options.items) |cc_option| {
        if (std.mem.eql(u8, cc_option, "-E")) {
            fatal("TODO: handle -E already being given", .{});
        }
    }

    for (c_files.items) |c_file| {
        const basename = std.fs.path.basename(c_file);

        const preprocessed_filename = blk: {
            const preprocessed_basename = std.mem.concat(arena, u8, &[_][]const u8 { basename, ".preprocessed" }) catch |e| oom(e);
            defer arena.free(preprocessed_basename);
            break :blk std.fs.path.join(arena, &.{out_dir, preprocessed_basename}) catch |e| oom(e);
        };
        defer arena.free(preprocessed_filename);
        try preprocess(cc_options.items, c_file, preprocessed_filename);

        const analyzed_filename = blk: {
            const analyzed_basename = std.mem.concat(arena, u8, &[_][]const u8 { basename, ".analyzed" }) catch |e| oom(e);
            defer arena.free(analyzed_basename);
            break :blk std.fs.path.join(arena, &.{out_dir, analyzed_basename}) catch |e| oom(e);
        };
        defer arena.free(analyzed_filename);
        const report = try std.fs.cwd().createFile(analyzed_filename, .{});
        defer report.close();
        try analyze(cc_options.items, target, c_file, preprocessed_filename, analyzed_filename, report.writer());
    }

    if (builtin.os.tag == .windows) {
        @panic("TODO: forward call without execve");
    }

    const cc_argv: [*:null]?[*:0]const u8 = blk: {
        var cc_argv_list = std.ArrayListUnmanaged(?[*:0]const u8){ };
        for (cc_options.items) |cc_opt| {
            cc_argv_list.append(arena, cc_opt) catch |e| oom(e);
        }
        if (cc_out_file) |out_file| {
            cc_argv_list.append(arena, "-o") catch |e| oom(e);
            cc_argv_list.append(arena, out_file) catch |e| oom(e);
        }
        for (c_files.items) |c_file| {
            cc_argv_list.append(arena, c_file) catch |e| oom(e);
        }
        break :blk cc_argv_list.toOwnedSliceSentinel(arena, null) catch |e| oom(e);
    };

    std.debug.assert(cc_argv[cc_args.len] == null);
    const envp: [*:null]?[*:0]const u8 = @ptrCast(std.os.environ.ptr);
    std.debug.assert(envp[std.os.environ.len] == null);

    if (false) {
        try std.io.getStdErr().writer().print("COMMAND:", .{});
        for(cc_args) |a| {
            try std.io.getStdErr().writer().print(" {s}", .{a});
        }
        try std.io.getStdErr().writer().print("\n", .{});
    }

    const err = std.os.execvpeZ_expandArg0(.expand, cc_args[0], cc_argv, envp);
    std.log.err("exec '{s}' failed with {s}", .{cc_args[0], @errorName(err)});
    return 0x7f;
}

fn preprocess(
    cc_options: []const []const u8,
    src_filename: []const u8,
    out_filename: []const u8,
) !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    var argv = std.ArrayListUnmanaged([]const u8) { };
    for (cc_options) |cc_option| {
        argv.append(arena, cc_option) catch |e| oom(e);
    }
    argv.append(arena, "-o") catch |e| oom(e);
    argv.append(arena, out_filename) catch |e| oom(e);
    argv.append(arena, "-E") catch |e| oom(e);
    argv.append(arena, src_filename) catch |e| oom(e);

    const result = try std.ChildProcess.exec(.{
        .allocator = arena,
        .argv = argv.items,
    });

    const failed = switch (result.term) {
        .Exited => |code| (code != 0),
        .Signal => true,
        .Stopped => true,
        .Unknown => true,
    };
    if (failed) {
        try std.io.getStdOut().writer().writeAll(result.stdout);
        try std.io.getStdErr().writer().writeAll(result.stderr);
        switch (result.term) {
            .Exited => |code| fatal("preprocess exited with code {}", .{code}),
            .Signal => |sig| fatal("preprocess terminted with signal {}", .{sig}),
            .Stopped => |sig| fatal("preprocess stopped with signal {}", .{sig}),
            .Unknown => |code| fatal("preprocess terminated for unknown reason, code={}", .{code}),
        }
    }
}

const LineMarker = struct {
    line_num: u32,
    filename: []const u8,
    new_file: bool = false,
    returning_to_file: bool = false,
    system_header: bool = false,
    extern_c: bool = false,
};

// format:
// # linenum filename flags
fn parseLineMarker(full_line: []const u8) !LineMarker {
    std.debug.assert(full_line[0] == '#');
    //std.log.info("parsing '{s}'", .{full_line});
    var rest = full_line[1..];
    if (!std.mem.startsWith(u8, rest, " ")) @panic("todo");
    rest = rest[1..];

    const line_num = blk: {
        const next_space = std.mem.indexOfScalar(u8, rest, ' ') orelse @panic("todo");
        const line_num_str = rest[0 .. next_space];
        rest = rest[next_space + 1..];
        break :blk try std.fmt.parseInt(u32, line_num_str, 10);
    };
    if (!std.mem.startsWith(u8, rest, "\"")) @panic("todo");
    rest = rest[1..];
    const filename = blk: {
        const next_quote = std.mem.indexOfScalar(u8, rest, '"') orelse @panic("todo");
        const filename = rest[0 .. next_quote];
        rest = rest[next_quote + 1..];
        break :blk filename;
    };

    var marker = LineMarker{
        .line_num = line_num,
        .filename = filename,
    };

    {
        var it = std.mem.split(u8, rest, " ");
        while (it.next()) |flag| {
            if (flag.len == 0) continue;
            if (std.mem.eql(u8, flag, "1")) {
                marker.new_file = true;
            } else if (std.mem.eql(u8, flag, "2")) {
                marker.returning_to_file = true;
            } else if (std.mem.eql(u8, flag, "3")) {
                marker.system_header = true;
            } else if (std.mem.eql(u8, flag, "4")) {
                marker.extern_c = true;
            } else {
                fatal("todo: parse flag '{s}'", .{flag});
            }
        }
    }
    return marker;
}

fn hasNonWhitespace(s: []const u8) bool {
    for (s) |c| {
        if (c != ' ' and c != '\t' and c != '\r' and c != '\n') return true;
    }
    return false;
}

const FilePart = struct {
    marker: LineMarker,
    start: usize,
    end: usize,
};
fn splitFiles(src: []const u8, parts: *std.ArrayList(FilePart), opt: struct {
    filter_empty_sections: bool = true,
}) !void {
    var last_file_offset: usize = 0;
    var last_marker_opt: ?LineMarker = null;

    var line_it = std.mem.split(u8, src, "\n");
    while (line_it.next()) |line_full| {
        const line = std.mem.trimRight(u8, line_full, "\r");
        if (!std.mem.startsWith(u8, line, "#")) continue;

        const line_file_offset = @intFromPtr(line.ptr) - @intFromPtr(src.ptr);
        const last_section_start = last_file_offset;
        const last_section_end = line_file_offset;
        last_file_offset = @min(src.len, line_file_offset + line_full.len + 1);

        if (last_marker_opt) |m| {
            if (!opt.filter_empty_sections or hasNonWhitespace(src[last_section_start..last_section_end])) {
                parts.append(FilePart{ .marker = m, .start = last_section_start, .end = last_section_end}) catch |e| oom(e);
            }
        } else {
            if (last_section_end != last_section_start) @panic("content from unmarked location, is this possible?");
        }
        last_marker_opt = try parseLineMarker(line);
    }
}

fn analyze(
    cc_options: []const []const u8,
    target: std.Target,
    original_filename: []const u8,
    preprocessed_filename: []const u8,
    report_filename: []const u8,
    report_writer: anytype,
) !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    _ = report_filename;

    try report_writer.print("// Source: {s}\n", .{original_filename});
    try report_writer.print("// Preprocessed: {s}\n", .{preprocessed_filename});
    try report_writer.print("// CC Command({} args):\n", .{cc_options.len});
    for (cc_options) |arg| {
        try report_writer.print("// {s}\n", .{arg});
    }

    // Split file into parts
    const src = blk: {
        var file = try std.fs.cwd().openFile(preprocessed_filename, .{});
        defer file.close();
        break :blk try file.readToEndAlloc(arena, std.math.maxInt(usize));
    };
    defer arena.free(src);

    var parts = std.ArrayList(FilePart).init(arena);
    defer parts.deinit();
    try splitFiles(src, &parts, .{.filter_empty_sections = false });

    if (parts.items.len == 0) {
        std.log.err("{s}: no line markers", .{preprocessed_filename});
        return error.UnexpectedPreprocessorOutput;
    }

    {
        const first = parts.items[0];
        if (!std.mem.eql(u8, original_filename, first.marker.filename)) {
            std.log.err("{s}: expected first line marker to be file '{s}' but got '{s}'", .{
                preprocessed_filename,
                original_filename,
                first.marker.filename,
            });
            return error.UnexpectedPreprocessorOutput;
        }
    }

    const dump_original = false;
    if (dump_original) {
        for (parts.items) |part| {
            const m = &part.marker;
            try report_writer.writeAll("// --------------------------------------------------------------------------------\n");
            try report_writer.print(
                "// {s}(new={}, system={}, extern-c={}): line {}\n",
                .{m.filename, m.new_file, m.system_header, m.extern_c, m.line_num},
            );
            try report_writer.writeAll("// --------------------------------------------------------------------------------\n");
            const content = src[part.start..part.end];
            try report_writer.writeAll(content);
            if (!std.mem.endsWith(u8, content, "\n")) {
                try report_writer.writeAll("\n");
            }
        }
    }

    _ = target;
    var analyzer = CAnalyzer.init(arena);
    for (parts.items) |part| {
        const content = src[part.start..part.end];

        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // TODO: also dump final state?
        if (std.mem.eql(u8, part.marker.filename, original_filename)) {
            try report_writer.print("line: {}\n", .{part.marker.line_num});
            try reportState(report_writer, try analyzer.getState());
            try report_writer.print("--------------------------------------------------------------------------------\n", .{});
        }

        if (hasNonWhitespace(content)) {
            //const suffix: []const u8 = if (part.marker.returning_to_file) "-returning" else "";
            //const path = std.fmt.allocPrint(arena, "{s}-line-{}{s}", .{part.marker.filename, part.marker.line_num, suffix}) catch |e| oom(e);
            //defer arena.free(path);
            try analyzer.feed(content);
        }
    }
    {
        try report_writer.print("final_state:\n", .{});
        try reportState(report_writer, try analyzer.getState());
    }

//    //fn parseHeaderIntoGraph(arena: Allocator, h_path: []const u8, header: Header, sys_include: []const u8) !Graph {
//    //var nodes = std.ArrayList(Node).init(arena);
//
//    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//    defer arena_instance.deinit();
//    const arena = arena_instance.allocator();
//
//    var comp = arocc.Compilation.init(arena);
//    defer comp.deinit();
//
//    comp.target = target;
//    // TODO: add system includes
//    //try comp.system_include_dirs.append(sys_include);
//
//    try comp.addDefaultPragmaHandlers();
//
//    if (comp.target.abi == .msvc or comp.target.os.tag == .windows) {
//        comp.langopts.setEmulatedCompiler(.msvc);
//    }
//
//    var pp = arocc.Preprocessor.init(&comp);
//    defer pp.deinit();
//
//    var macro_buf = std.ArrayList(u8).init(comp.gpa);
//    defer macro_buf.deinit();
//
//    //try pp.addBuiltinMacros();
//
//
//
//    //const source = try comp.addSourceFromBuffer(h_path, header.source_bytes);
//    const source = try comp.addSourceFromPath(src_filename);
//
//    var guard_name = pp.findIncludeGuard(source);
//
//    pp.preprocess_count += 1;
//    var tokenizer = arocc.Tokenizer{
//        .buf = source.buf,
//        .comp = pp.comp,
//        .source = source.id,
//    };
//
//    var if_level: u8 = 0;
//    //var if_kind = std.PackedIntArray(u2, 256).init([1]u2{0} ** 256);
//    //const until_else = 0;
//    //const until_endif = 1;
//    //const until_endif_seen_else = 2;
//
//    var start_of_line = true;
//
//    var last_write_loc: usize = 0;
//
//    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    if (true) return;
//    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//
//    while (true) {
//        var tok = tokenizer.next();
//
//        try report_writer.writeAll(source.buf[last_write_loc..tok.end]);
//        last_write_loc = tok.end;
//
//        switch (tok.id) {
//            .hash => if (!start_of_line) try pp.tokens.append(pp.gpa, lib.tokFromRaw(tok)) else {
//                const directive = tokenizer.nextNoWS();
//                switch (directive.id) {
//                    .keyword_error, .keyword_warning => {
//                        @panic("TODO keyword_error, keyword_warning");
//                    },
//                    .keyword_if => {
//                        @panic("TODO keyword_if");
//                    },
//                    .keyword_ifdef => {
//                        @panic("TODO keyword_ifdef");
//                    },
//                    .keyword_ifndef => {
//                        @panic("TODO keyword_ifndef");
//                    },
//                    .keyword_elif => {
//                        @panic("TODO keyword_elif");
//                    },
//                    .keyword_else => {
//                        @panic("TODO keyword_else");
//                    },
//                    .keyword_endif => {
//                        @panic("TODO keyword_endif");
//                    },
//                    .keyword_define => {
//                        //try lib.handleKeywordDefine(arena, source, &nodes, &tokenizer),
//                        @panic("TODO keyword_define");
//                    },
//                    .keyword_undef => {
//                        @panic("TODO keyword_undef");
//                    },
//                    .keyword_include => {
//                        @panic("TODO keyword_include");
//                    },
//                    .keyword_include_next => {
//                        @panic("TODO include_next");
//                    },
//                    .keyword_pragma => {
//                        @panic("TODO pragma");
//                    },
//                    .keyword_line => {
//                        @panic("unsupported directive: #line");
//                    },
//                    .pp_num => {
//                        @panic("TODO pp_num??");
//                    },
//                    .nl => {},
//                    .eof => {
//                        if (if_level != 0) @panic("unterminated_conditional_directive");
//                        @panic("TODO eof inside hash");
//                    },
//                    else => {
//                        try pp.err(tok, .invalid_preprocessing_directive);
//                        lib.skipToNl(&tokenizer);
//                    },
//                }
//            },
//            .whitespace => {},
//            .nl => {
//                start_of_line = true;
//            },
//            .eof => {
//                if (if_level != 0) @panic("unterminated_conditional_directive");
//                // The following check needs to occur here and not at the top of the function
//                // because a pragma may change the level during preprocessing
//                if (source.buf.len > 0 and source.buf[source.buf.len - 1] != '\n') {
//                    @panic("newline_eof");
//                }
//                if (guard_name) |name| {
//                    std.debug.print("found include guard: '{s}'\n", .{name});
//                    @panic("TODO handle include guard");
//                }
//                break;
//            },
//            else => {
//                //std.debug.print("TODO handle token '{s}'\n", .{@tagName(tok.id)});
//            },
//        }
//    }
}

fn reportState(report_writer: anytype, state: CAnalyzer.State) !void{
    try report_writer.print("extern_c: {}\n", .{state.extern_c});
    for (state.defs) |def| {
        try report_writer.print("def: {any}\n", .{def});
    }
}

const CAnalyzer = struct {
    allocator: std.mem.Allocator,
    comp: *aro.Compilation,
    sources: std.ArrayListUnmanaged(aro.Source) = .{},
    feed_count: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) CAnalyzer {
        const comp = allocator.create(aro.Compilation) catch |e| oom(e);
        comp.* = aro.Compilation.init(allocator);
        return .{
            .allocator = allocator,
            .comp = comp,
        };
    }
    pub fn deinit(self: *CAnalyzer) void {
        self.sources.deinit(self.allocator);
        self.comp.deinit();
        self.allocator.destroy(self.comp);
        self.* = undefined;
    }

    pub fn feed(self: *CAnalyzer, c_source: []const u8) !void {
        var path_buf: [100]u8 = undefined;
        const path = try std.fmt.bufPrint(&path_buf, "{}", .{self.feed_count});
        self.feed_count += 1;
        std.debug.assert(!self.comp.sources.contains(path));
        self.sources.append(self.allocator, try self.comp.addSourceFromBuffer(path, c_source)) catch |e| oom(e);
    }

    const Definition = struct {
        placeholder: u32,
    };
    pub const State = struct {
        extern_c: bool,
        defs: []const Definition,
    };
    pub fn getState(self: *CAnalyzer) !State {
        //std.log.info("getState --------------------------------------------------------------------------------", .{});
        var pp = aro.Preprocessor.init(self.comp);
        defer pp.deinit();

        // TODO: estimate initial tokens capacity
        // pp.tokens.ensureTotalCapcity

        for (self.sources.items) |source| {
            var tokenizer = aro.Tokenizer{
                .buf = source.buf,
                .comp = self.comp,
                .source = source.id,
            };
            while (true) {
                var tok = tokenizer.next();
                if (tok.id == .eof) break;
                //std.log.info("token '{any}'", .{tok});
                pp.tokens.append(self.allocator, tokFromRaw(tok)) catch |e| oom(e);
            }
        }
        pp.tokens.append(self.allocator, .{
            .id = .eof,
            .loc = .{ },
        }) catch |e| oom(e);
        //std.log.info("parsing --------------------------------------------------------------------------------", .{});
        //var tree = try aro.Parser.parse(&pp);
        //defer tree.deinit();

        return State{ .extern_c = false, .defs = &.{} };
    }
};

fn tokFromRaw(raw: aro.Tokenizer.Token) aro.Tree.Token {
    return .{
        .id = raw.id,
        .loc = .{
            .id = raw.source,
            .byte_offset = raw.start,
            .line = raw.line,
        },
    };
}
