# -*- coding: utf-8 -*-
"""
    pygments.lexers.math
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for math languages.

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, bygroups, include, do_insertions
from pygments.token import Comment, String, Punctuation, Keyword, Name, \
    Operator, Number, Text, Generic

from pygments.lexers.agile import PythonLexer

__all__ = ['MuPADLexer', 'MatlabLexer', 'MatlabSessionLexer', 'OctaveLexer',
           'ScilabLexer', 'NumPyLexer', 'RConsoleLexer', 'SLexer']


class MuPADLexer(RegexLexer):
    """
    A `MuPAD <http://www.mupad.com>`_ lexer.
    Contributed by Christopher Creutzig <christopher@creutzig.de>.

    *New in Pygments 0.8.*
    """
    name = 'MuPAD'
    aliases = ['mupad']
    filenames = ['*.mu']

    tokens = {
      'root' : [
        (r'//.*?$', Comment.Single),
        (r'/\*', Comment.Multiline, 'comment'),
        (r'"(?:[^"\\]|\\.)*"', String),
        (r'\(|\)|\[|\]|\{|\}', Punctuation),
        (r'''(?x)\b(?:
            next|break|end|
            axiom|end_axiom|category|end_category|domain|end_domain|inherits|
            if|%if|then|elif|else|end_if|
            case|of|do|otherwise|end_case|
            while|end_while|
            repeat|until|end_repeat|
            for|from|to|downto|step|end_for|
            proc|local|option|save|begin|end_proc|
            delete|frame
          )\b''', Keyword),
        (r'''(?x)\b(?:
            DOM_ARRAY|DOM_BOOL|DOM_COMPLEX|DOM_DOMAIN|DOM_EXEC|DOM_EXPR|
            DOM_FAIL|DOM_FLOAT|DOM_FRAME|DOM_FUNC_ENV|DOM_HFARRAY|DOM_IDENT|
            DOM_INT|DOM_INTERVAL|DOM_LIST|DOM_NIL|DOM_NULL|DOM_POLY|DOM_PROC|
            DOM_PROC_ENV|DOM_RAT|DOM_SET|DOM_STRING|DOM_TABLE|DOM_VAR
          )\b''', Name.Class),
        (r'''(?x)\b(?:
            PI|EULER|E|CATALAN|
            NIL|FAIL|undefined|infinity|
            TRUE|FALSE|UNKNOWN
          )\b''',
          Name.Constant),
        (r'\b(?:dom|procname)\b', Name.Builtin.Pseudo),
        (r'\.|,|:|;|=|\+|-|\*|/|\^|@|>|<|\$|\||!|\'|%|~=', Operator),
        (r'''(?x)\b(?:
            and|or|not|xor|
            assuming|
            div|mod|
            union|minus|intersect|in|subset
          )\b''',
          Operator.Word),
        (r'\b(?:I|RDN_INF|RD_NINF|RD_NAN)\b', Number),
        #(r'\b(?:adt|linalg|newDomain|hold)\b', Name.Builtin),
        (r'''(?x)
          ((?:[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)
          (?:::[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)*)\s*([(])''',
          bygroups(Name.Function, Punctuation)),
        (r'''(?x)
          (?:[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)
          (?:::[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)*''', Name.Variable),
        (r'[0-9]+(?:\.[0-9]*)?(?:e[0-9]+)?', Number),
        (r'\.[0-9]+(?:e[0-9]+)?', Number),
        (r'.', Text)
      ],
      'comment' : [
        (r'[^*/]', Comment.Multiline),
        (r'/\*', Comment.Multiline, '#push'),
        (r'\*/', Comment.Multiline, '#pop'),
        (r'[*/]', Comment.Multiline)
      ]
    }


class MatlabLexer(RegexLexer):
    """
    For Matlab source code.
    Contributed by Ken Schutte <kschutte@csail.mit.edu>.

    *New in Pygments 0.10.*
    """
    name = 'Matlab'
    aliases = ['matlab']
    filenames = ['*.m']
    mimetypes = ['text/matlab']

    #
    # These lists are generated automatically.
    # Run the following in bash shell:
    #
    # for f in elfun specfun elmat; do
    #   echo -n "$f = "
    #   matlab -nojvm -r "help $f;exit;" | perl -ne \
    #   'push(@c,$1) if /^    (\w+)\s+-/; END {print q{["}.join(q{","},@c).qq{"]\n};}'
    # done
    #
    # elfun: Elementary math functions
    # specfun: Special Math functions
    # elmat: Elementary matrices and matrix manipulation
    #
    # taken from Matlab version 7.4.0.336 (R2007a)
    #
    elfun = ["sin","sind","sinh","asin","asind","asinh","cos","cosd","cosh",
             "acos","acosd","acosh","tan","tand","tanh","atan","atand","atan2",
             "atanh","sec","secd","sech","asec","asecd","asech","csc","cscd",
             "csch","acsc","acscd","acsch","cot","cotd","coth","acot","acotd",
             "acoth","hypot","exp","expm1","log","log1p","log10","log2","pow2",
             "realpow","reallog","realsqrt","sqrt","nthroot","nextpow2","abs",
             "angle","complex","conj","imag","real","unwrap","isreal","cplxpair",
             "fix","floor","ceil","round","mod","rem","sign"]
    specfun = ["airy","besselj","bessely","besselh","besseli","besselk","beta",
               "betainc","betaln","ellipj","ellipke","erf","erfc","erfcx",
               "erfinv","expint","gamma","gammainc","gammaln","psi","legendre",
               "cross","dot","factor","isprime","primes","gcd","lcm","rat",
               "rats","perms","nchoosek","factorial","cart2sph","cart2pol",
               "pol2cart","sph2cart","hsv2rgb","rgb2hsv"]
    elmat = ["zeros","ones","eye","repmat","rand","randn","linspace","logspace",
             "freqspace","meshgrid","accumarray","size","length","ndims","numel",
             "disp","isempty","isequal","isequalwithequalnans","cat","reshape",
             "diag","blkdiag","tril","triu","fliplr","flipud","flipdim","rot90",
             "find","end","sub2ind","ind2sub","bsxfun","ndgrid","permute",
             "ipermute","shiftdim","circshift","squeeze","isscalar","isvector",
             "ans","eps","realmax","realmin","pi","i","inf","nan","isnan",
             "isinf","isfinite","j","why","compan","gallery","hadamard","hankel",
             "hilb","invhilb","magic","pascal","rosser","toeplitz","vander",
             "wilkinson"]

    tokens = {
        'root': [
            # line starting with '!' is sent as a system command.  not sure what
            # label to use...
            (r'^!.*', String.Other),
            (r'%.*$', Comment),
            (r'^\s*function', Keyword, 'deffunc'),

            # from 'iskeyword' on version 7.11 (R2010):
            (r'(break|case|catch|classdef|continue|else|elseif|end|enumerated|'
             r'events|for|function|global|if|methods|otherwise|parfor|'
             r'persistent|properties|return|spmd|switch|try|while)\b', Keyword),

            ("(" + "|".join(elfun+specfun+elmat) + r')\b',  Name.Builtin),

            # operators:
            (r'-|==|~=|<|>|<=|>=|&&|&|~|\|\|?', Operator),
            # operators requiring escape for re:
            (r'\.\*|\*|\+|\.\^|\.\\|\.\/|\/|\\', Operator),

            # punctuation:
            (r'\[|\]|\(|\)|\{|\}|:|@|\.|,', Punctuation),
            (r'=|:|;', Punctuation),

            # quote can be transpose, instead of string:
            # (not great, but handles common cases...)
            (r'(?<=[\w\)\]])\'', Operator),

            (r'(?<![\w\)\]])\'', String, 'string'),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'.', Text),
        ],
        'string': [
            (r'[^\']*\'', String, '#pop')
        ],
        'deffunc': [
            (r'(\s*)(?:(.+)(\s*)(=)(\s*))?(.+)(\()(.*)(\))(\s*)',
             bygroups(Text.Whitespace, Text, Text.Whitespace, Punctuation,
                      Text.Whitespace, Name.Function, Punctuation, Text,
                      Punctuation, Text.Whitespace), '#pop'),
        ],
    }

    def analyse_text(text):
        if re.match('^\s*%', text, re.M): # comment
            return 0.9
        elif re.match('^!\w+', text, re.M): # system cmd
            return 0.9
        return 0.1


line_re  = re.compile('.*?\n')

class MatlabSessionLexer(Lexer):
    """
    For Matlab sessions.  Modeled after PythonConsoleLexer.
    Contributed by Ken Schutte <kschutte@csail.mit.edu>.

    *New in Pygments 0.10.*
    """
    name = 'Matlab session'
    aliases = ['matlabsession']

    def get_tokens_unprocessed(self, text):
        mlexer = MatlabLexer(**self.options)

        curcode = ''
        insertions = []

        for match in line_re.finditer(text):
            line = match.group()

            if line.startswith('>>'):
                insertions.append((len(curcode),
                                   [(0, Generic.Prompt, line[:3])]))
                curcode += line[3:]

            elif line.startswith('???'):

                idx = len(curcode)

                # without is showing error on same line as before...?
                line = "\n" + line
                token = (0, Generic.Traceback, line)
                insertions.append((idx, [token]))

            else:
                if curcode:
                    for item in do_insertions(
                        insertions, mlexer.get_tokens_unprocessed(curcode)):
                        yield item
                    curcode = ''
                    insertions = []

                yield match.start(), Generic.Output, line

        if curcode: # or item:
            for item in do_insertions(
                insertions, mlexer.get_tokens_unprocessed(curcode)):
                yield item


class OctaveLexer(RegexLexer):
    """
    For GNU Octave source code.

    *New in Pygments 1.5.*
    """
    name = 'Octave'
    aliases = ['octave']
    filenames = ['*.m']
    mimetypes = ['text/octave']

    # These lists are generated automatically.
    # Run the following in bash shell:
    #
    # First dump all of the Octave manual into a plain text file:
    #
    #   $ info octave --subnodes -o octave-manual
    #
    # Now grep through it:

    # for i in \
    #     "Built-in Function" "Command" "Function File" \
    #     "Loadable Function" "Mapping Function";
    # do
    #     perl -e '@name = qw('"$i"');
    #              print lc($name[0]),"_kw = [\n"';
    #
    #     perl -n -e 'print "\"$1\",\n" if /-- '"$i"': .* (\w*) \(/;' \
    #         octave-manual | sort | uniq ;
    #     echo "]" ;
    #     echo;
    # done

    # taken from Octave Mercurial changeset 8cc154f45e37 (30-jan-2011)

    builtin_kw = [ "addlistener", "addpath", "addproperty", "all",
                   "and", "any", "argnames", "argv", "assignin",
                   "atexit", "autoload",
                   "available_graphics_toolkits", "beep_on_error",
                   "bitand", "bitmax", "bitor", "bitshift", "bitxor",
                   "cat", "cell", "cellstr", "char", "class", "clc",
                   "columns", "command_line_path",
                   "completion_append_char", "completion_matches",
                   "complex", "confirm_recursive_rmdir", "cputime",
                   "crash_dumps_octave_core", "ctranspose", "cumprod",
                   "cumsum", "debug_on_error", "debug_on_interrupt",
                   "debug_on_warning", "default_save_options",
                   "dellistener", "diag", "diff", "disp",
                   "doc_cache_file", "do_string_escapes", "double",
                   "drawnow", "e", "echo_executing_commands", "eps",
                   "eq", "errno", "errno_list", "error", "eval",
                   "evalin", "exec", "exist", "exit", "eye", "false",
                   "fclear", "fclose", "fcntl", "fdisp", "feof",
                   "ferror", "feval", "fflush", "fgetl", "fgets",
                   "fieldnames", "file_in_loadpath", "file_in_path",
                   "filemarker", "filesep", "find_dir_in_path",
                   "fixed_point_format", "fnmatch", "fopen", "fork",
                   "formula", "fprintf", "fputs", "fread", "freport",
                   "frewind", "fscanf", "fseek", "fskipl", "ftell",
                   "functions", "fwrite", "ge", "genpath", "get",
                   "getegid", "getenv", "geteuid", "getgid",
                   "getpgrp", "getpid", "getppid", "getuid", "glob",
                   "gt", "gui_mode", "history_control",
                   "history_file", "history_size",
                   "history_timestamp_format_string", "home",
                   "horzcat", "hypot", "ifelse",
                   "ignore_function_time_stamp", "inferiorto",
                   "info_file", "info_program", "inline", "input",
                   "intmax", "intmin", "ipermute",
                   "is_absolute_filename", "isargout", "isbool",
                   "iscell", "iscellstr", "ischar", "iscomplex",
                   "isempty", "isfield", "isfloat", "isglobal",
                   "ishandle", "isieee", "isindex", "isinteger",
                   "islogical", "ismatrix", "ismethod", "isnull",
                   "isnumeric", "isobject", "isreal",
                   "is_rooted_relative_filename", "issorted",
                   "isstruct", "isvarname", "kbhit", "keyboard",
                   "kill", "lasterr", "lasterror", "lastwarn",
                   "ldivide", "le", "length", "link", "linspace",
                   "logical", "lstat", "lt", "make_absolute_filename",
                   "makeinfo_program", "max_recursion_depth", "merge",
                   "methods", "mfilename", "minus", "mislocked",
                   "mkdir", "mkfifo", "mkstemp", "mldivide", "mlock",
                   "mouse_wheel_zoom", "mpower", "mrdivide", "mtimes",
                   "munlock", "nargin", "nargout",
                   "native_float_format", "ndims", "ne", "nfields",
                   "nnz", "norm", "not", "numel", "nzmax",
                   "octave_config_info", "octave_core_file_limit",
                   "octave_core_file_name",
                   "octave_core_file_options", "ones", "or",
                   "output_max_field_width", "output_precision",
                   "page_output_immediately", "page_screen_output",
                   "path", "pathsep", "pause", "pclose", "permute",
                   "pi", "pipe", "plus", "popen", "power",
                   "print_empty_dimensions", "printf",
                   "print_struct_array_contents", "prod",
                   "program_invocation_name", "program_name",
                   "putenv", "puts", "pwd", "quit", "rats", "rdivide",
                   "readdir", "readlink", "read_readline_init_file",
                   "realmax", "realmin", "rehash", "rename",
                   "repelems", "re_read_readline_init_file", "reset",
                   "reshape", "resize", "restoredefaultpath",
                   "rethrow", "rmdir", "rmfield", "rmpath", "rows",
                   "save_header_format_string", "save_precision",
                   "saving_history", "scanf", "set", "setenv",
                   "shell_cmd", "sighup_dumps_octave_core",
                   "sigterm_dumps_octave_core", "silent_functions",
                   "single", "size", "size_equal", "sizemax",
                   "sizeof", "sleep", "source", "sparse_auto_mutate",
                   "split_long_rows", "sprintf", "squeeze", "sscanf",
                   "stat", "stderr", "stdin", "stdout", "strcmp",
                   "strcmpi", "string_fill_char", "strncmp",
                   "strncmpi", "struct", "struct_levels_to_print",
                   "strvcat", "subsasgn", "subsref", "sum", "sumsq",
                   "superiorto", "suppress_verbose_help_message",
                   "symlink", "system", "tic", "tilde_expand",
                   "times", "tmpfile", "tmpnam", "toc", "toupper",
                   "transpose", "true", "typeinfo", "umask", "uminus",
                   "uname", "undo_string_escapes", "unlink", "uplus",
                   "upper", "usage", "usleep", "vec", "vectorize",
                   "vertcat", "waitpid", "warning", "warranty",
                   "whos_line_format", "yes_or_no", "zeros",
                   "inf", "Inf", "nan", "NaN"]

    command_kw = [ "close", "load", "who", "whos", ]

    function_kw = [ "accumarray", "accumdim", "acosd", "acotd",
                   "acscd", "addtodate", "allchild", "ancestor",
                   "anova", "arch_fit", "arch_rnd", "arch_test",
                   "area", "arma_rnd", "arrayfun", "ascii", "asctime",
                   "asecd", "asind", "assert", "atand",
                   "autoreg_matrix", "autumn", "axes", "axis", "bar",
                   "barh", "bartlett", "bartlett_test", "beep",
                   "betacdf", "betainv", "betapdf", "betarnd",
                   "bicgstab", "bicubic", "binary", "binocdf",
                   "binoinv", "binopdf", "binornd", "bitcmp",
                   "bitget", "bitset", "blackman", "blanks",
                   "blkdiag", "bone", "box", "brighten", "calendar",
                   "cast", "cauchy_cdf", "cauchy_inv", "cauchy_pdf",
                   "cauchy_rnd", "caxis", "celldisp", "center", "cgs",
                   "chisquare_test_homogeneity",
                   "chisquare_test_independence", "circshift", "cla",
                   "clabel", "clf", "clock", "cloglog", "closereq",
                   "colon", "colorbar", "colormap", "colperm",
                   "comet", "common_size", "commutation_matrix",
                   "compan", "compare_versions", "compass",
                   "computer", "cond", "condest", "contour",
                   "contourc", "contourf", "contrast", "conv",
                   "convhull", "cool", "copper", "copyfile", "cor",
                   "corrcoef", "cor_test", "cosd", "cotd", "cov",
                   "cplxpair", "cross", "cscd", "cstrcat", "csvread",
                   "csvwrite", "ctime", "cumtrapz", "curl", "cut",
                   "cylinder", "date", "datenum", "datestr",
                   "datetick", "datevec", "dblquad", "deal",
                   "deblank", "deconv", "delaunay", "delaunayn",
                   "delete", "demo", "detrend", "diffpara", "diffuse",
                   "dir", "discrete_cdf", "discrete_inv",
                   "discrete_pdf", "discrete_rnd", "display",
                   "divergence", "dlmwrite", "dos", "dsearch",
                   "dsearchn", "duplication_matrix", "durbinlevinson",
                   "ellipsoid", "empirical_cdf", "empirical_inv",
                   "empirical_pdf", "empirical_rnd", "eomday",
                   "errorbar", "etime", "etreeplot", "example",
                   "expcdf", "expinv", "expm", "exppdf", "exprnd",
                   "ezcontour", "ezcontourf", "ezmesh", "ezmeshc",
                   "ezplot", "ezpolar", "ezsurf", "ezsurfc", "factor",
                   "factorial", "fail", "fcdf", "feather", "fftconv",
                   "fftfilt", "fftshift", "figure", "fileattrib",
                   "fileparts", "fill", "findall", "findobj",
                   "findstr", "finv", "flag", "flipdim", "fliplr",
                   "flipud", "fpdf", "fplot", "fractdiff", "freqz",
                   "freqz_plot", "frnd", "fsolve",
                   "f_test_regression", "ftp", "fullfile", "fzero",
                   "gamcdf", "gaminv", "gampdf", "gamrnd", "gca",
                   "gcbf", "gcbo", "gcf", "genvarname", "geocdf",
                   "geoinv", "geopdf", "geornd", "getfield", "ginput",
                   "glpk", "gls", "gplot", "gradient",
                   "graphics_toolkit", "gray", "grid", "griddata",
                   "griddatan", "gtext", "gunzip", "gzip", "hadamard",
                   "hamming", "hankel", "hanning", "hggroup",
                   "hidden", "hilb", "hist", "histc", "hold", "hot",
                   "hotelling_test", "housh", "hsv", "hurst",
                   "hygecdf", "hygeinv", "hygepdf", "hygernd",
                   "idivide", "ifftshift", "image", "imagesc",
                   "imfinfo", "imread", "imshow", "imwrite", "index",
                   "info", "inpolygon", "inputname", "interpft",
                   "interpn", "intersect", "invhilb", "iqr", "isa",
                   "isdefinite", "isdir", "is_duplicate_entry",
                   "isequal", "isequalwithequalnans", "isfigure",
                   "ishermitian", "ishghandle", "is_leap_year",
                   "isletter", "ismac", "ismember", "ispc", "isprime",
                   "isprop", "isscalar", "issquare", "isstrprop",
                   "issymmetric", "isunix", "is_valid_file_id",
                   "isvector", "jet", "kendall",
                   "kolmogorov_smirnov_cdf",
                   "kolmogorov_smirnov_test", "kruskal_wallis_test",
                   "krylov", "kurtosis", "laplace_cdf", "laplace_inv",
                   "laplace_pdf", "laplace_rnd", "legend", "legendre",
                   "license", "line", "linkprop", "list_primes",
                   "loadaudio", "loadobj", "logistic_cdf",
                   "logistic_inv", "logistic_pdf", "logistic_rnd",
                   "logit", "loglog", "loglogerr", "logm", "logncdf",
                   "logninv", "lognpdf", "lognrnd", "logspace",
                   "lookfor", "ls_command", "lsqnonneg", "magic",
                   "mahalanobis", "manova", "matlabroot",
                   "mcnemar_test", "mean", "meansq", "median", "menu",
                   "mesh", "meshc", "meshgrid", "meshz", "mexext",
                   "mget", "mkpp", "mode", "moment", "movefile",
                   "mpoles", "mput", "namelengthmax", "nargchk",
                   "nargoutchk", "nbincdf", "nbininv", "nbinpdf",
                   "nbinrnd", "nchoosek", "ndgrid", "newplot", "news",
                   "nonzeros", "normcdf", "normest", "norminv",
                   "normpdf", "normrnd", "now", "nthroot", "null",
                   "ocean", "ols", "onenormest", "optimget",
                   "optimset", "orderfields", "orient", "orth",
                   "pack", "pareto", "parseparams", "pascal", "patch",
                   "pathdef", "pcg", "pchip", "pcolor", "pcr",
                   "peaks", "periodogram", "perl", "perms", "pie",
                   "pink", "planerot", "playaudio", "plot",
                   "plotmatrix", "plotyy", "poisscdf", "poissinv",
                   "poisspdf", "poissrnd", "polar", "poly",
                   "polyaffine", "polyarea", "polyderiv", "polyfit",
                   "polygcd", "polyint", "polyout", "polyreduce",
                   "polyval", "polyvalm", "postpad", "powerset",
                   "ppder", "ppint", "ppjumps", "ppplot", "ppval",
                   "pqpnonneg", "prepad", "primes", "print",
                   "print_usage", "prism", "probit", "qp", "qqplot",
                   "quadcc", "quadgk", "quadl", "quadv", "quiver",
                   "qzhess", "rainbow", "randi", "range", "rank",
                   "ranks", "rat", "reallog", "realpow", "realsqrt",
                   "record", "rectangle_lw", "rectangle_sw",
                   "rectint", "refresh", "refreshdata",
                   "regexptranslate", "repmat", "residue", "ribbon",
                   "rindex", "roots", "rose", "rosser", "rotdim",
                   "rref", "run", "run_count", "rundemos", "run_test",
                   "runtests", "saveas", "saveaudio", "saveobj",
                   "savepath", "scatter", "secd", "semilogx",
                   "semilogxerr", "semilogy", "semilogyerr",
                   "setaudio", "setdiff", "setfield", "setxor",
                   "shading", "shift", "shiftdim", "sign_test",
                   "sinc", "sind", "sinetone", "sinewave", "skewness",
                   "slice", "sombrero", "sortrows", "spaugment",
                   "spconvert", "spdiags", "spearman", "spectral_adf",
                   "spectral_xdf", "specular", "speed", "spencer",
                   "speye", "spfun", "sphere", "spinmap", "spline",
                   "spones", "sprand", "sprandn", "sprandsym",
                   "spring", "spstats", "spy", "sqp", "stairs",
                   "statistics", "std", "stdnormal_cdf",
                   "stdnormal_inv", "stdnormal_pdf", "stdnormal_rnd",
                   "stem", "stft", "strcat", "strchr", "strjust",
                   "strmatch", "strread", "strsplit", "strtok",
                   "strtrim", "strtrunc", "structfun", "studentize",
                   "subplot", "subsindex", "subspace", "substr",
                   "substruct", "summer", "surf", "surface", "surfc",
                   "surfl", "surfnorm", "svds", "swapbytes",
                   "sylvester_matrix", "symvar", "synthesis", "table",
                   "tand", "tar", "tcdf", "tempdir", "tempname",
                   "test", "text", "textread", "textscan", "tinv",
                   "title", "toeplitz", "tpdf", "trace", "trapz",
                   "treelayout", "treeplot", "triangle_lw",
                   "triangle_sw", "tril", "trimesh", "triplequad",
                   "triplot", "trisurf", "triu", "trnd", "tsearchn",
                   "t_test", "t_test_regression", "type", "unidcdf",
                   "unidinv", "unidpdf", "unidrnd", "unifcdf",
                   "unifinv", "unifpdf", "unifrnd", "union", "unique",
                   "unix", "unmkpp", "unpack", "untabify", "untar",
                   "unwrap", "unzip", "u_test", "validatestring",
                   "vander", "var", "var_test", "vech", "ver",
                   "version", "view", "voronoi", "voronoin",
                   "waitforbuttonpress", "wavread", "wavwrite",
                   "wblcdf", "wblinv", "wblpdf", "wblrnd", "weekday",
                   "welch_test", "what", "white", "whitebg",
                   "wienrnd", "wilcoxon_test", "wilkinson", "winter",
                   "xlabel", "xlim", "ylabel", "yulewalker", "zip",
                   "zlabel", "z_test", ]

    loadable_kw = [ "airy", "amd", "balance", "besselh", "besseli",
                   "besselj", "besselk", "bessely", "bitpack",
                   "bsxfun", "builtin", "ccolamd", "cellfun",
                   "cellslices", "chol", "choldelete", "cholinsert",
                   "cholinv", "cholshift", "cholupdate", "colamd",
                   "colloc", "convhulln", "convn", "csymamd",
                   "cummax", "cummin", "daspk", "daspk_options",
                   "dasrt", "dasrt_options", "dassl", "dassl_options",
                   "dbclear", "dbdown", "dbstack", "dbstatus",
                   "dbstop", "dbtype", "dbup", "dbwhere", "det",
                   "dlmread", "dmperm", "dot", "eig", "eigs",
                   "endgrent", "endpwent", "etree", "fft", "fftn",
                   "fftw", "filter", "find", "full", "gcd",
                   "getgrent", "getgrgid", "getgrnam", "getpwent",
                   "getpwnam", "getpwuid", "getrusage", "givens",
                   "gmtime", "gnuplot_binary", "hess", "ifft",
                   "ifftn", "inv", "isdebugmode", "issparse", "kron",
                   "localtime", "lookup", "lsode", "lsode_options",
                   "lu", "luinc", "luupdate", "matrix_type", "max",
                   "min", "mktime", "pinv", "qr", "qrdelete",
                   "qrinsert", "qrshift", "qrupdate", "quad",
                   "quad_options", "qz", "rand", "rande", "randg",
                   "randn", "randp", "randperm", "rcond", "regexp",
                   "regexpi", "regexprep", "schur", "setgrent",
                   "setpwent", "sort", "spalloc", "sparse", "spparms",
                   "sprank", "sqrtm", "strfind", "strftime",
                   "strptime", "strrep", "svd", "svd_driver", "syl",
                   "symamd", "symbfact", "symrcm", "time", "tsearch",
                   "typecast", "urlread", "urlwrite", ]

    mapping_kw = [ "abs", "acos", "acosh", "acot", "acoth", "acsc",
                  "acsch", "angle", "arg", "asec", "asech", "asin",
                  "asinh", "atan", "atanh", "beta", "betainc",
                  "betaln", "bincoeff", "cbrt", "ceil", "conj", "cos",
                  "cosh", "cot", "coth", "csc", "csch", "erf", "erfc",
                  "erfcx", "erfinv", "exp", "finite", "fix", "floor",
                  "fmod", "gamma", "gammainc", "gammaln", "imag",
                  "isalnum", "isalpha", "isascii", "iscntrl",
                  "isdigit", "isfinite", "isgraph", "isinf",
                  "islower", "isna", "isnan", "isprint", "ispunct",
                  "isspace", "isupper", "isxdigit", "lcm", "lgamma",
                  "log", "lower", "mod", "real", "rem", "round",
                  "roundb", "sec", "sech", "sign", "sin", "sinh",
                  "sqrt", "tan", "tanh", "toascii", "tolower", "xor",
                  ]

    builtin_consts = [ "EDITOR", "EXEC_PATH", "I", "IMAGE_PATH", "NA",
                   "OCTAVE_HOME", "OCTAVE_VERSION", "PAGER",
                   "PAGER_FLAGS", "SEEK_CUR", "SEEK_END", "SEEK_SET",
                   "SIG", "S_ISBLK", "S_ISCHR", "S_ISDIR", "S_ISFIFO",
                   "S_ISLNK", "S_ISREG", "S_ISSOCK", "WCONTINUE",
                   "WCOREDUMP", "WEXITSTATUS", "WIFCONTINUED",
                   "WIFEXITED", "WIFSIGNALED", "WIFSTOPPED", "WNOHANG",
                   "WSTOPSIG", "WTERMSIG", "WUNTRACED", ]

    tokens = {
        'root': [
            #We should look into multiline comments
            (r'[%#].*$', Comment),
            (r'^\s*function', Keyword, 'deffunc'),

            # from 'iskeyword' on hg changeset 8cc154f45e37
            (r'(__FILE__|__LINE__|break|case|catch|classdef|continue|do|else|'
             r'elseif|end|end_try_catch|end_unwind_protect|endclassdef|'
             r'endevents|endfor|endfunction|endif|endmethods|endproperties|'
             r'endswitch|endwhile|events|for|function|get|global|if|methods|'
             r'otherwise|persistent|properties|return|set|static|switch|try|'
             r'until|unwind_protect|unwind_protect_cleanup|while)\b', Keyword),

            ("(" + "|".join(  builtin_kw + command_kw
                            + function_kw + loadable_kw
                            + mapping_kw) + r')\b',  Name.Builtin),

            ("(" + "|".join(builtin_consts) + r')\b', Name.Constant),

            # operators in Octave but not Matlab:
            (r'-=|!=|!|/=|--', Operator),
            # operators:
            (r'-|==|~=|<|>|<=|>=|&&|&|~|\|\|?', Operator),
            # operators in Octave but not Matlab requiring escape for re:
            (r'\*=|\+=|\^=|\/=|\\=|\*\*|\+\+|\.\*\*',Operator),
            # operators requiring escape for re:
            (r'\.\*|\*|\+|\.\^|\.\\|\.\/|\/|\\', Operator),


            # punctuation:
            (r'\[|\]|\(|\)|\{|\}|:|@|\.|,', Punctuation),
            (r'=|:|;', Punctuation),

            (r'"[^"]*"', String),

            # quote can be transpose, instead of string:
            # (not great, but handles common cases...)
            (r'(?<=[\w\)\]])\'', Operator),
            (r'(?<![\w\)\]])\'', String, 'string'),

            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'.', Text),
        ],
        'string': [
            (r"[^']*'", String, '#pop'),
        ],
        'deffunc': [
            (r'(\s*)(?:(.+)(\s*)(=)(\s*))?(.+)(\()(.*)(\))(\s*)',
             bygroups(Text.Whitespace, Text, Text.Whitespace, Punctuation,
                      Text.Whitespace, Name.Function, Punctuation, Text,
                      Punctuation, Text.Whitespace), '#pop'),
        ],
    }

    def analyse_text(text):
        if re.match('^\s*[%#]', text, re.M): #Comment
            return 0.9
        return 0.1


class ScilabLexer(RegexLexer):
    """
    For Scilab source code.

    *New in Pygments 1.5.*
    """
    name = 'Scilab'
    aliases = ['scilab']
    filenames = ['*.sci', '*.sce', '*.tst' ]
    mimetypes = ['text/scilab']

    # These lists are generated automatically.
    # Run the following in a Scilab script:
    #
    # varType=["functions", "commands", "macros", "variables" ];
    # fd = mopen('list.txt','wt');
    # 
    # for j=1:size(varType,"*")
    #     myStr="";
    #     a=completion("",varType(j)); 
    #     myStr=varType(j)+"_kw = [";
    #     for i=1:size(a,"*")
    #         myStr = myStr + """" + a(i) + """";
    #         if size(a,"*") <> i then
    #            myStr = myStr + ","; end
    #         end
    #     myStr = myStr + "]";
    #     mputl(myStr,fd);
    # end
    # mclose(fd);

    functions_kw = ["%XMLAttr_6","%XMLAttr_e","%XMLAttr_i_XMLElem","%XMLAttr_length","%XMLAttr_p","%XMLAttr_size","%XMLDoc_6","%XMLDoc_e","%XMLDoc_i_XMLList","%XMLDoc_p","%XMLElem_6","%XMLElem_e","%XMLElem_i_XMLDoc","%XMLElem_i_XMLElem","%XMLElem_i_XMLList","%XMLElem_p","%XMLList_6","%XMLList_e","%XMLList_i_XMLElem","%XMLList_i_XMLList","%XMLList_length","%XMLList_p","%XMLList_size","%XMLNs_6","%XMLNs_e","%XMLNs_i_XMLElem","%XMLNs_p","%XMLSet_6","%XMLSet_e","%XMLSet_length","%XMLSet_p","%XMLSet_size","%XMLValid_p","%b_i_XMLList","%c_i_XMLAttr","%c_i_XMLDoc","%c_i_XMLElem","%c_i_XMLList","%ce_i_XMLList","%fptr_i_XMLList","%h_i_XMLList","%hm_i_XMLList","%i_abs","%i_cumprod","%i_cumsum","%i_diag","%i_i_XMLList","%i_matrix","%i_max","%i_maxi","%i_min","%i_mini","%i_mput","%i_p","%i_prod","%i_sum","%i_tril","%i_triu","%ip_i_XMLList","%l_i_XMLList","%lss_i_XMLList","%mc_i_XMLList","%msp_full","%msp_i_XMLList","%msp_spget","%p_i_XMLList","%ptr_i_XMLList","%r_i_XMLList","%s_i_XMLList","%sp_i_XMLList","%spb_i_XMLList","%st_i_XMLList","Calendar","ClipBoard","Matplot","Matplot1","PlaySound","TCL_DeleteInterp","TCL_DoOneEvent","TCL_EvalFile","TCL_EvalStr","TCL_ExistArray","TCL_ExistInterp","TCL_ExistVar","TCL_GetVar","TCL_GetVersion","TCL_SetVar","TCL_UnsetVar","TCL_UpVar","_","_code2str","_str2code","about","abs","acos","addcb","addf","addhistory","addinter","amell","and","argn","arl2_ius","ascii","asin","atan","backslash","balanc","banner","base2dec","basename","bdiag","beep","besselh","besseli","besselj","besselk","bessely","beta","bezout","bfinit","blkfc1i","blkslvi","bool2s","browsehistory","browsevar","bsplin3val","buildDocv2","buildouttb","bvode","c_link","calerf","call","callblk","captions","cd","cdfbet","cdfbin","cdfchi","cdfchn","cdff","cdffnc","cdfgam","cdfnbn","cdfnor","cdfpoi","cdft","ceil","champ","champ1","chdir","chol","clc","clean","clear","clear_pixmap","clearfun","clearglobal","closeEditor","closeXcos","code2str","coeff","comp","completion","conj","contour2di","contr","conv2","convstr","copy","copyfile","corr","cos","coserror","createdir","cshep2d","ctree2","ctree3","ctree4","cumprod","cumsum","curblock","curblockc","dasrt","dassl","data2sig","debug","dec2base","deff","definedfields","degree","delbpt","delete","deletefile","delip","delmenu","det","dgettext","dhinf","diag","diary","diffobjs","disp","dispbpt","displayhistory","disposefftwlibrary","dlgamma","dnaupd","dneupd","double","draw","drawaxis","drawlater","drawnow","dsaupd","dsearch","dseupd","duplicate","editor","editvar","emptystr","end_scicosim","ereduc","errcatch","errclear","error","eval_cshep2d","exec","execstr","exists","exit","exp","expm","exportUI","export_to_hdf5","eye","fadj2sp","fec","feval","fft","fftw","fftw_flags","fftw_forget_wisdom","fftwlibraryisloaded","file","filebrowser","fileext","fileinfo","fileparts","filesep","find","findBD","findfiles","floor","format","fort","fprintfMat","freq","frexp","fromc","fromjava","fscanfMat","fsolve","fstair","full","fullpath","funcprot","funptr","gamma","gammaln","geom3d","get","get_absolute_file_path","get_fftw_wisdom","getblocklabel","getcallbackobject","getdate","getdebuginfo","getdefaultlanguage","getdrives","getdynlibext","getenv","getfield","gethistory","gethistoryfile","getinstalledlookandfeels","getio","getlanguage","getlongpathname","getlookandfeel","getmd5","getmemory","getmodules","getos","getpid","getrelativefilename","getscicosvars","getscilabmode","getshortpathname","gettext","getvariablesonstack","getversion","glist","global","glue","grand","grayplot","grep","gsort","gstacksize","havewindow","helpbrowser","hess","hinf","historymanager","historysize","host","iconvert","iconvert","ieee","ilib_verbose","imag","impl","import_from_hdf5","imult","inpnvi","int","int16","int2d","int32","int3d","int8","interp","interp2d","interp3d","intg","intppty","inttype","inv","is_handle_valid","isalphanum","isascii","isdef","isdigit","isdir","isequal","isequalbitwise","iserror","isfile","isglobal","isletter","isreal","iswaitingforinput","javaclasspath","javalibrarypath","kron","lasterror","ldiv","ldivf","legendre","length","lib","librarieslist","libraryinfo","linear_interpn","lines","link","linmeq","list","load","loadScicos","loadfftwlibrary","loadhistory","log","log1p","lsq","lsq_splin","lsqrsolve","lsslist","lstcat","lstsize","ltitr","lu","ludel","lufact","luget","lusolve","macr2lst","macr2tree","matfile_close","matfile_listvar","matfile_open","matfile_varreadnext","matfile_varwrite","matrix","max","maxfiles","mclearerr","mclose","meof","merror","messagebox","mfprintf","mfscanf","mget","mgeti","mgetl","mgetstr","min","mlist","mode","model2blk","mopen","move","movefile","mprintf","mput","mputl","mputstr","mscanf","mseek","msprintf","msscanf","mtell","mtlb_mode","mtlb_sparse","mucomp","mulf","nearfloat","newaxes","newest","newfun","nnz","notify","number_properties","ode","odedc","ones","opentk","optim","or","ordmmd","parallel_concurrency","parallel_run","param3d","param3d1","part","pathconvert","pathsep","phase_simulation","plot2d","plot2d1","plot2d2","plot2d3","plot2d4","plot3d","plot3d1","pointer_xproperty","poly","ppol","pppdiv","predef","print","printf","printfigure","printsetupbox","prod","progressionbar","prompt","pwd","qld","qp_solve","qr","raise_window","rand","rankqr","rat","rcond","rdivf","read","read4b","readb","readgateway","readmps","real","realtime","realtimeinit","regexp","relocate_handle","remez","removedir","removelinehistory","res_with_prec","resethistory","residu","resume","return","ricc","ricc_old","rlist","roots","rotate_axes","round","rpem","rtitr","rubberbox","save","saveafterncommands","saveconsecutivecommands","savehistory","schur","sci_haltscicos","sci_tree2","sci_tree3","sci_tree4","sciargs","scicos_debug","scicos_debug_count","scicos_time","scicosim","scinotes","sctree","semidef","set","set_blockerror","set_fftw_wisdom","set_xproperty","setbpt","setdefaultlanguage","setenv","setfield","sethistoryfile","setlanguage","setlookandfeel","setmenu","sfact","sfinit","show_pixmap","show_window","showalluimenushandles","sident","sig2data","sign","simp","simp_mode","sin","size","slash","sleep","sorder","sparse","spchol","spcompack","spec","spget","splin","splin2d","splin3d","spones","sprintf","sqrt","stacksize","str2code","strcat","strchr","strcmp","strcspn","strindex","string","stringbox","stripblanks","strncpy","strrchr","strrev","strsplit","strspn","strstr","strsubst","strtod","strtok","subf","sum","svd","swap_handles","symfcti","syredi","system_getproperty","system_setproperty","ta2lpd","tan","taucs_chdel","taucs_chfact","taucs_chget","taucs_chinfo","taucs_chsolve","tempname","testmatrix","timer","tlist","tohome","tokens","toolbar","toprint","tr_zer","tril","triu","type","typename","uiDisplayTree","uicontextmenu","uicontrol","uigetcolor","uigetdir","uigetfile","uigetfont","uimenu","uint16","uint32","uint8","uipopup","uiputfile","uiwait","ulink","umf_ludel","umf_lufact","umf_luget","umf_luinfo","umf_lusolve","umfpack","unglue","unix","unsetmenu","unzoom","updatebrowsevar","usecanvas","user","var2vec","varn","vec2var","waitbar","warnBlockByUID","warning","what","where","whereis","who","winsid","with_embedded_jre","with_module","writb","write","write4b","x_choose","x_choose_modeless","x_dialog","x_mdialog","xarc","xarcs","xarrows","xchange","xchoicesi","xclick","xcos","xcosAddToolsMenu","xcosConfigureXmlFile","xcosDiagramToScilab","xcosPalCategoryAdd","xcosPalDelete","xcosPalDisable","xcosPalEnable","xcosPalGenerateIcon","xcosPalLoad","xcosPalMove","xcosUpdateBlock","xdel","xfarc","xfarcs","xfpoly","xfpolys","xfrect","xget","xgetech","xgetmouse","xgraduate","xgrid","xlfont","xls_open","xls_read","xmlAddNs","xmlAsNumber","xmlAsText","xmlDTD","xmlDelete","xmlDocument","xmlDump","xmlElement","xmlFormat","xmlGetNsByHref","xmlGetNsByPrefix","xmlGetOpenDocs","xmlIsValidObject","xmlNs","xmlRead","xmlReadStr","xmlRelaxNG","xmlRemove","xmlSchema","xmlSetAttributes","xmlValidate","xmlWrite","xmlXPath","xname","xpause","xpoly","xpolys","xrect","xrects","xs2bmp","xs2eps","xs2gif","xs2jpg","xs2pdf","xs2png","xs2ppm","xs2ps","xs2svg","xsegs","xset","xsetech","xstring","xstringb","xtitle","zeros","znaupd","zneupd","zoom_rect"]
    
    commands_kw = ["abort","apropos","break","case","catch","clc","clear","continue","do","else","elseif","end","endfunction","exit","for","function","help","if","pause","pwd","quit","resume","return","select","then","try","what","while","who"]

    macros_kw = ["%0_i_st","%3d_i_h","%Block_xcosUpdateBlock","%TNELDER_p","%TNELDER_string","%TNMPLOT_p","%TNMPLOT_string","%TOPTIM_p","%TOPTIM_string","%TSIMPLEX_p","%TSIMPLEX_string","%_gsort","%_strsplit","%ar_p","%asn","%b_a_b","%b_a_s","%b_c_s","%b_c_spb","%b_cumprod","%b_cumsum","%b_d_s","%b_diag","%b_e","%b_f_s","%b_f_spb","%b_g_s","%b_g_spb","%b_h_s","%b_h_spb","%b_i_b","%b_i_ce","%b_i_h","%b_i_hm","%b_i_s","%b_i_sp","%b_i_spb","%b_i_st","%b_iconvert","%b_l_b","%b_l_s","%b_m_b","%b_m_s","%b_matrix","%b_n_hm","%b_o_hm","%b_p_s","%b_prod","%b_r_b","%b_r_s","%b_s_b","%b_s_s","%b_string","%b_sum","%b_tril","%b_triu","%b_x_b","%b_x_s","%c_a_c","%c_b_c","%c_b_s","%c_diag","%c_e","%c_eye","%c_f_s","%c_i_c","%c_i_ce","%c_i_h","%c_i_hm","%c_i_lss","%c_i_r","%c_i_s","%c_i_st","%c_matrix","%c_n_l","%c_n_st","%c_o_l","%c_o_st","%c_ones","%c_rand","%c_tril","%c_triu","%cblock_c_cblock","%cblock_c_s","%cblock_e","%cblock_f_cblock","%cblock_p","%cblock_size","%ce_6","%ce_c_ce","%ce_e","%ce_f_ce","%ce_i_ce","%ce_i_s","%ce_i_st","%ce_matrix","%ce_p","%ce_size","%ce_string","%ce_t","%champdat_i_h","%choose","%diagram_xcos","%dir_p","%fptr_i_st","%grayplot_i_h","%h_i_st","%hm_1_hm","%hm_1_s","%hm_2_hm","%hm_2_s","%hm_3_hm","%hm_3_s","%hm_4_hm","%hm_4_s","%hm_5","%hm_a_hm","%hm_a_r","%hm_a_s","%hm_abs","%hm_and","%hm_bool2s","%hm_c_hm","%hm_ceil","%hm_conj","%hm_cos","%hm_cumprod","%hm_cumsum","%hm_d_hm","%hm_d_s","%hm_degree","%hm_e","%hm_exp","%hm_f_hm","%hm_fft","%hm_find","%hm_floor","%hm_g_hm","%hm_h_hm","%hm_i_b","%hm_i_ce","%hm_i_hm","%hm_i_i","%hm_i_p","%hm_i_r","%hm_i_s","%hm_i_st","%hm_iconvert","%hm_imag","%hm_int","%hm_isnan","%hm_isreal","%hm_j_hm","%hm_j_s","%hm_k_hm","%hm_k_s","%hm_log","%hm_m_p","%hm_m_r","%hm_m_s","%hm_matrix","%hm_maxi","%hm_mean","%hm_median","%hm_mini","%hm_n_b","%hm_n_c","%hm_n_hm","%hm_n_i","%hm_n_p","%hm_n_s","%hm_o_b","%hm_o_c","%hm_o_hm","%hm_o_i","%hm_o_p","%hm_o_s","%hm_ones","%hm_or","%hm_p","%hm_prod","%hm_q_hm","%hm_r_s","%hm_rand","%hm_real","%hm_round","%hm_s","%hm_s_hm","%hm_s_r","%hm_s_s","%hm_sign","%hm_sin","%hm_size","%hm_sqrt","%hm_st_deviation","%hm_string","%hm_sum","%hm_x_hm","%hm_x_p","%hm_x_s","%hm_zeros","%i_1_s","%i_2_s","%i_3_s","%i_4_s","%i_Matplot","%i_a_i","%i_a_s","%i_and","%i_ascii","%i_b_s","%i_bezout","%i_champ","%i_champ1","%i_contour","%i_contour2d","%i_d_i","%i_d_s","%i_e","%i_fft","%i_g_i","%i_gcd","%i_h_i","%i_i_ce","%i_i_h","%i_i_hm","%i_i_i","%i_i_s","%i_i_st","%i_j_i","%i_j_s","%i_l_s","%i_lcm","%i_length","%i_m_i","%i_m_s","%i_mfprintf","%i_mprintf","%i_msprintf","%i_n_s","%i_o_s","%i_or","%i_p_i","%i_p_s","%i_plot2d","%i_plot2d1","%i_plot2d2","%i_q_s","%i_r_i","%i_r_s","%i_round","%i_s_i","%i_s_s","%i_sign","%i_string","%i_x_i","%i_x_s","%ip_a_s","%ip_i_st","%ip_m_s","%ip_n_ip","%ip_o_ip","%ip_p","%ip_s_s","%ip_string","%k","%l_i_h","%l_i_s","%l_i_st","%l_isequal","%l_n_c","%l_n_l","%l_n_m","%l_n_p","%l_n_s","%l_n_st","%l_o_c","%l_o_l","%l_o_m","%l_o_p","%l_o_s","%l_o_st","%lss_a_lss","%lss_a_p","%lss_a_r","%lss_a_s","%lss_c_lss","%lss_c_p","%lss_c_r","%lss_c_s","%lss_e","%lss_eye","%lss_f_lss","%lss_f_p","%lss_f_r","%lss_f_s","%lss_i_ce","%lss_i_lss","%lss_i_p","%lss_i_r","%lss_i_s","%lss_i_st","%lss_inv","%lss_l_lss","%lss_l_p","%lss_l_r","%lss_l_s","%lss_m_lss","%lss_m_p","%lss_m_r","%lss_m_s","%lss_n_lss","%lss_n_p","%lss_n_r","%lss_n_s","%lss_norm","%lss_o_lss","%lss_o_p","%lss_o_r","%lss_o_s","%lss_ones","%lss_r_lss","%lss_r_p","%lss_r_r","%lss_r_s","%lss_rand","%lss_s","%lss_s_lss","%lss_s_p","%lss_s_r","%lss_s_s","%lss_size","%lss_t","%lss_v_lss","%lss_v_p","%lss_v_r","%lss_v_s","%lt_i_s","%m_n_l","%m_o_l","%mc_i_h","%mc_i_s","%mc_i_st","%mc_n_st","%mc_o_st","%mc_string","%mps_p","%mps_string","%msp_a_s","%msp_abs","%msp_e","%msp_find","%msp_i_s","%msp_i_st","%msp_length","%msp_m_s","%msp_maxi","%msp_n_msp","%msp_nnz","%msp_o_msp","%msp_p","%msp_sparse","%msp_spones","%msp_t","%p_a_lss","%p_a_r","%p_c_lss","%p_c_r","%p_cumprod","%p_cumsum","%p_d_p","%p_d_r","%p_d_s","%p_det","%p_e","%p_f_lss","%p_f_r","%p_i_ce","%p_i_h","%p_i_hm","%p_i_lss","%p_i_p","%p_i_r","%p_i_s","%p_i_st","%p_inv","%p_j_s","%p_k_p","%p_k_r","%p_k_s","%p_l_lss","%p_l_p","%p_l_r","%p_l_s","%p_m_hm","%p_m_lss","%p_m_r","%p_matrix","%p_n_l","%p_n_lss","%p_n_r","%p_o_l","%p_o_lss","%p_o_r","%p_o_sp","%p_p_s","%p_prod","%p_q_p","%p_q_r","%p_q_s","%p_r_lss","%p_r_p","%p_r_r","%p_r_s","%p_s_lss","%p_s_r","%p_simp","%p_string","%p_sum","%p_v_lss","%p_v_p","%p_v_r","%p_v_s","%p_x_hm","%p_x_r","%p_y_p","%p_y_r","%p_y_s","%p_z_p","%p_z_r","%p_z_s","%r_a_hm","%r_a_lss","%r_a_p","%r_a_r","%r_a_s","%r_c_lss","%r_c_p","%r_c_r","%r_c_s","%r_clean","%r_cumprod","%r_d_p","%r_d_r","%r_d_s","%r_det","%r_diag","%r_e","%r_eye","%r_f_lss","%r_f_p","%r_f_r","%r_f_s","%r_i_ce","%r_i_hm","%r_i_lss","%r_i_p","%r_i_r","%r_i_s","%r_i_st","%r_inv","%r_j_s","%r_k_p","%r_k_r","%r_k_s","%r_l_lss","%r_l_p","%r_l_r","%r_l_s","%r_m_hm","%r_m_lss","%r_m_p","%r_m_r","%r_m_s","%r_matrix","%r_n_lss","%r_n_p","%r_n_r","%r_n_s","%r_norm","%r_o_lss","%r_o_p","%r_o_r","%r_o_s","%r_ones","%r_p","%r_p_s","%r_prod","%r_q_p","%r_q_r","%r_q_s","%r_r_lss","%r_r_p","%r_r_r","%r_r_s","%r_rand","%r_s","%r_s_hm","%r_s_lss","%r_s_p","%r_s_r","%r_s_s","%r_simp","%r_size","%r_string","%r_sum","%r_t","%r_tril","%r_triu","%r_v_lss","%r_v_p","%r_v_r","%r_v_s","%r_x_p","%r_x_r","%r_x_s","%r_y_p","%r_y_r","%r_y_s","%r_z_p","%r_z_r","%r_z_s","%s_1_hm","%s_1_i","%s_2_hm","%s_2_i","%s_3_hm","%s_3_i","%s_4_hm","%s_4_i","%s_5","%s_a_b","%s_a_hm","%s_a_i","%s_a_ip","%s_a_lss","%s_a_msp","%s_a_r","%s_a_sp","%s_and","%s_b_i","%s_b_s","%s_c_b","%s_c_cblock","%s_c_lss","%s_c_r","%s_c_sp","%s_d_b","%s_d_i","%s_d_p","%s_d_r","%s_d_sp","%s_e","%s_f_b","%s_f_cblock","%s_f_lss","%s_f_r","%s_f_sp","%s_g_b","%s_g_s","%s_h_b","%s_h_s","%s_i_b","%s_i_c","%s_i_ce","%s_i_h","%s_i_hm","%s_i_i","%s_i_lss","%s_i_p","%s_i_r","%s_i_s","%s_i_sp","%s_i_spb","%s_i_st","%s_j_i","%s_k_hm","%s_k_p","%s_k_r","%s_k_sp","%s_l_b","%s_l_hm","%s_l_i","%s_l_lss","%s_l_p","%s_l_r","%s_l_s","%s_l_sp","%s_m_b","%s_m_hm","%s_m_i","%s_m_ip","%s_m_lss","%s_m_msp","%s_m_r","%s_matrix","%s_n_hm","%s_n_i","%s_n_l","%s_n_lss","%s_n_r","%s_n_st","%s_o_hm","%s_o_i","%s_o_l","%s_o_lss","%s_o_r","%s_o_st","%s_or","%s_p_b","%s_p_i","%s_pow","%s_q_hm","%s_q_i","%s_q_p","%s_q_r","%s_q_sp","%s_r_b","%s_r_i","%s_r_lss","%s_r_p","%s_r_r","%s_r_s","%s_r_sp","%s_s_b","%s_s_hm","%s_s_i","%s_s_ip","%s_s_lss","%s_s_r","%s_s_sp","%s_simp","%s_v_lss","%s_v_p","%s_v_r","%s_v_s","%s_x_b","%s_x_hm","%s_x_i","%s_x_r","%s_y_p","%s_y_r","%s_y_sp","%s_z_p","%s_z_r","%s_z_sp","%sn","%sp_a_s","%sp_a_sp","%sp_and","%sp_c_s","%sp_ceil","%sp_cos","%sp_cumprod","%sp_cumsum","%sp_d_s","%sp_d_sp","%sp_diag","%sp_e","%sp_exp","%sp_f_s","%sp_floor","%sp_gsort","%sp_i_ce","%sp_i_h","%sp_i_s","%sp_i_sp","%sp_i_st","%sp_int","%sp_inv","%sp_k_s","%sp_k_sp","%sp_l_s","%sp_l_sp","%sp_length","%sp_norm","%sp_or","%sp_p_s","%sp_prod","%sp_q_s","%sp_q_sp","%sp_r_s","%sp_r_sp","%sp_round","%sp_s_s","%sp_s_sp","%sp_sin","%sp_sqrt","%sp_string","%sp_sum","%sp_tril","%sp_triu","%sp_y_s","%sp_y_sp","%sp_z_s","%sp_z_sp","%spb_and","%spb_c_b","%spb_cumprod","%spb_cumsum","%spb_diag","%spb_e","%spb_f_b","%spb_g_b","%spb_g_spb","%spb_h_b","%spb_h_spb","%spb_i_b","%spb_i_ce","%spb_i_h","%spb_i_st","%spb_or","%spb_prod","%spb_sum","%spb_tril","%spb_triu","%st_6","%st_c_st","%st_e","%st_f_st","%st_i_b","%st_i_c","%st_i_fptr","%st_i_h","%st_i_i","%st_i_ip","%st_i_lss","%st_i_msp","%st_i_p","%st_i_r","%st_i_s","%st_i_sp","%st_i_spb","%st_i_st","%st_matrix","%st_n_c","%st_n_l","%st_n_mc","%st_n_p","%st_n_s","%st_o_c","%st_o_l","%st_o_mc","%st_o_p","%st_o_s","%st_o_tl","%st_p","%st_size","%st_string","%st_t","%ticks_i_h","%xls_e","%xls_p","%xlssheet_e","%xlssheet_p","%xlssheet_size","%xlssheet_string","DominationRank","G_make","IsAScalar","NDcost","OS_Version","PlotSparse","ReadHBSparse","ReadmiMatrix","TCL_CreateSlave","WritemiMatrix","abcd","abinv","accept_func_default","accept_func_vfsa","acf","acosd","acosh","acoshm","acosm","acot","acotd","acoth","acsc","acscd","acsch","add_demo","add_help_chapter","add_module_help_chapter","add_param","add_profiling","adj2sp","aff2ab","ana_style","analpf","analyze","aplat","apropos","arhnk","arl2","arma2p","armac","armax","armax1","arobasestring2strings","arsimul","ascii2string","asciimat","asec","asecd","asech","asind","asinh","asinhm","asinm","assert_checkalmostequal","assert_checkequal","assert_checkerror","assert_checkfalse","assert_checkfilesequal","assert_checktrue","assert_comparecomplex","assert_computedigits","assert_cond2reltol","assert_cond2reqdigits","assert_generror","atand","atanh","atanhm","atanm","atomsAutoload","atomsAutoloadAdd","atomsAutoloadDel","atomsAutoloadList","atomsCategoryList","atomsCheckModule","atomsDepTreeShow","atomsGetConfig","atomsGetInstalled","atomsGetLoaded","atomsGetLoadedPath","atomsInstall","atomsIsInstalled","atomsIsLoaded","atomsList","atomsLoad","atomsRemove","atomsRepositoryAdd","atomsRepositoryDel","atomsRepositoryList","atomsRestoreConfig","atomsSaveConfig","atomsSearch","atomsSetConfig","atomsShow","atomsSystemInit","atomsSystemUpdate","atomsTest","atomsUpdate","atomsVersion","augment","auread","auwrite","balreal","bench_run","bilin","bilt","bin2dec","binomial","bitand","bitcmp","bitget","bitor","bitset","bitxor","black","blanks","bloc2exp","bloc2ss","block_parameter_error","bode","bstap","buttmag","bvodeS","bytecode","bytecodewalk","cainv","calendar","calfrq","canon","casc","cat","cat_code","cb_m2sci_gui","ccontrg","cell","cell2mat","cellstr","center","cepstrum","cfspec","char","chart","cheb1mag","cheb2mag","check_gateways","check_help","check_modules_xml","check_versions","chepol","chfact","chsolve","classmarkov","clean_help","clock","cls2dls","cmb_lin","cmndred","cmoment","coding_ga_binary","coding_ga_identity","coff","coffg","colcomp","colcompr","colinout","colregul","companion","complex","compute_initial_temp","cond","cond2sp","condestsp","config","configure_msifort","configure_msvc","cont_frm","cont_mat","contrss","conv","convert_to_float","convertindex","convol","convol2d","copfac","correl","cosd","cosh","coshm","cosm","cotd","cotg","coth","cothm","covar","createfun","createstruct","crossover_ga_binary","crossover_ga_default","csc","cscd","csch","csgn","csim","cspect","ctr_gram","czt","dae","daeoptions","damp","datafit","date","datenum","datevec","dbphi","dcf","ddp","dec2bin","dec2hex","dec2oct","del_help_chapter","del_module_help_chapter","demo_begin","demo_choose","demo_compiler","demo_end","demo_file_choice","demo_folder_choice","demo_function_choice","demo_gui","demo_mdialog","demo_message","demo_run","demo_viewCode","denom","derivat","derivative","des2ss","des2tf","detectmsifort64tools","detectmsvc64tools","determ","detr","detrend","devtools_run_builder","dft","dhnorm","diff","diophant","dir","dirname","dispfiles","dllinfo","dscr","dsimul","dt_ility","dtsi","edit","edit_error","eigenmarkov","ell1mag","enlarge_shape","entropy","eomday","epred","eqfir","eqiir","equil","equil1","erf","erfc","erfcx","erfinv","etime","eval","evans","evstr","expression2code","extract_help_examples","factor","factorial","factors","faurre","ffilt","fft2","fftshift","fieldnames","filt_sinc","filter","findABCD","findAC","findBDK","findR","find_freq","find_links","find_scicos_version","findm","findmsifortcompiler","findmsvccompiler","findx0BD","firstnonsingleton","fit_dat","fix","fixedpointgcd","flipdim","flts","fminsearch","format_txt","fourplan","fprintf","frep2tf","freson","frfit","frmag","fscanf","fseek_origin","fsfirlin","fspec","fspecg","fstabst","ftest","ftuneq","fullfile","fullrf","fullrfk","fun2string","g_margin","gainplot","gamitg","gcare","gcd","gencompilationflags_unix","generateBlockImage","generateBlockImages","generic_i_ce","generic_i_h","generic_i_hm","generic_i_s","generic_i_st","genlib","genlib_old","genmarkov","geomean","getDiagramVersion","getModelicaPath","get_file_path","get_function_path","get_param","get_profile","get_scicos_version","getd","getscilabkeywords","getshell","gettklib","gfare","gfrancis","givens","glever","gmres","group","gschur","gspec","gtild","h2norm","h_cl","h_inf","h_inf_st","h_norm","hallchart","halt","hank","hankelsv","harmean","haveacompiler","head_comments","help","help_from_sci","help_skeleton","hermit","hex2dec","hilb","hilbert","horner","householder","hrmt","htrianr","hypermat","ifft","iir","iirgroup","iirlp","iirmod","ilib_build","ilib_compile","ilib_for_link","ilib_gen_Make","ilib_gen_Make_unix","ilib_gen_cleaner","ilib_gen_gateway","ilib_gen_loader","ilib_include_flag","ilib_mex_build","im_inv","importScicosDiagram","importScicosPal","importXcosDiagram","imrep2ss","ind2sub","inistate","init_ga_default","init_param","initial_scicos_tables","input","instruction2code","intc","intdec","integrate","interp1","interpln","intersect","intl","intsplin","inttrap","inv_coeff","invr","invrs","invsyslin","iqr","isLeapYear","is_absolute_path","is_param","iscell","iscellstr","isempty","isfield","isinf","isnan","isnum","issparse","isstruct","isvector","jmat","justify","kalm","karmarkar","kernel","kpure","krac2","kroneck","lattn","launchtest","lcf","lcm","lcmdiag","leastsq","leqe","leqr","lev","levin","lex_sort","lft","lin","lin2mu","lincos","lindquist","linf","linfn","linsolve","linspace","list2vec","list_param","listfiles","listfunctions","listvarinfile","lmisolver","lmitool","loadXcosLibs","loadmatfile","loadwave","log10","log2","logm","logspace","lqe","lqg","lqg2stan","lqg_ltr","lqr","ls","lyap","m2sci_gui","m_circle","macglov","macrovar","mad","makecell","manedit","mapsound","markp2ss","matfile2sci","mdelete","mean","meanf","median","mese","meshgrid","mfft","mfile2sci","minreal","minss","mkdir","modulo","moment","mrfit","msd","mstr2sci","mtlb","mtlb_0","mtlb_a","mtlb_all","mtlb_any","mtlb_axes","mtlb_axis","mtlb_beta","mtlb_box","mtlb_choices","mtlb_close","mtlb_colordef","mtlb_cond","mtlb_conv","mtlb_cov","mtlb_cumprod","mtlb_cumsum","mtlb_dec2hex","mtlb_delete","mtlb_diag","mtlb_diff","mtlb_dir","mtlb_double","mtlb_e","mtlb_echo","mtlb_error","mtlb_eval","mtlb_exist","mtlb_eye","mtlb_false","mtlb_fft","mtlb_fftshift","mtlb_filter","mtlb_find","mtlb_findstr","mtlb_fliplr","mtlb_fopen","mtlb_format","mtlb_fprintf","mtlb_fread","mtlb_fscanf","mtlb_full","mtlb_fwrite","mtlb_get","mtlb_grid","mtlb_hold","mtlb_i","mtlb_ifft","mtlb_image","mtlb_imp","mtlb_int16","mtlb_int32","mtlb_int8","mtlb_is","mtlb_isa","mtlb_isfield","mtlb_isletter","mtlb_isspace","mtlb_l","mtlb_legendre","mtlb_linspace","mtlb_logic","mtlb_logical","mtlb_loglog","mtlb_lower","mtlb_max","mtlb_mean","mtlb_median","mtlb_mesh","mtlb_meshdom","mtlb_min","mtlb_more","mtlb_num2str","mtlb_ones","mtlb_pcolor","mtlb_plot","mtlb_prod","mtlb_qr","mtlb_qz","mtlb_rand","mtlb_randn","mtlb_rcond","mtlb_realmax","mtlb_realmin","mtlb_repmat","mtlb_s","mtlb_semilogx","mtlb_semilogy","mtlb_setstr","mtlb_size","mtlb_sort","mtlb_sortrows","mtlb_sprintf","mtlb_sscanf","mtlb_std","mtlb_strcmp","mtlb_strcmpi","mtlb_strfind","mtlb_strrep","mtlb_subplot","mtlb_sum","mtlb_t","mtlb_toeplitz","mtlb_tril","mtlb_triu","mtlb_true","mtlb_type","mtlb_uint16","mtlb_uint32","mtlb_uint8","mtlb_upper","mtlb_var","mtlb_zeros","mu2lin","mutation_ga_binary","mutation_ga_default","mvcorrel","mvvacov","nancumsum","nand2mean","nanmax","nanmean","nanmeanf","nanmedian","nanmin","nanstdev","nansum","narsimul","ndgrid","ndims","nehari","neigh_func_csa","neigh_func_default","neigh_func_fsa","neigh_func_vfsa","neldermead_cget","neldermead_configure","neldermead_costf","neldermead_defaultoutput","neldermead_destroy","neldermead_display","neldermead_function","neldermead_get","neldermead_log","neldermead_new","neldermead_restart","neldermead_search","neldermead_updatesimp","nextpow2","nfreq","nicholschart","nlev","nmplot_cget","nmplot_configure","nmplot_contour","nmplot_destroy","nmplot_display","nmplot_function","nmplot_get","nmplot_historyplot","nmplot_log","nmplot_new","nmplot_outputcmd","nmplot_restart","nmplot_search","nmplot_simplexhistory","noisegen","nonreg_test_run","norm","now","null","num2cell","numdiff","numer","nyquist","nyquistfrequencybounds","obs_gram","obscont","observer","obsv_mat","obsvss","oct2dec","odeoptions","optim_ga","optim_moga","optim_nsga","optim_nsga2","optim_sa","optimbase_cget","optimbase_checkbounds","optimbase_checkcostfun","optimbase_checkx0","optimbase_configure","optimbase_destroy","optimbase_display","optimbase_function","optimbase_get","optimbase_hasbounds","optimbase_hasconstraints","optimbase_hasnlcons","optimbase_histget","optimbase_histset","optimbase_incriter","optimbase_isfeasible","optimbase_isinbounds","optimbase_isinnonlincons","optimbase_log","optimbase_logshutdown","optimbase_logstartup","optimbase_new","optimbase_outputcmd","optimbase_outstruct","optimbase_proj2bnds","optimbase_set","optimbase_stoplog","optimbase_terminate","optimget","optimplotfunccount","optimplotfval","optimplotx","optimset","optimsimplex_center","optimsimplex_check","optimsimplex_compsomefv","optimsimplex_computefv","optimsimplex_deltafv","optimsimplex_deltafvmax","optimsimplex_destroy","optimsimplex_dirmat","optimsimplex_fvmean","optimsimplex_fvstdev","optimsimplex_fvvariance","optimsimplex_getall","optimsimplex_getallfv","optimsimplex_getallx","optimsimplex_getfv","optimsimplex_getn","optimsimplex_getnbve","optimsimplex_getve","optimsimplex_getx","optimsimplex_gradientfv","optimsimplex_log","optimsimplex_new","optimsimplex_print","optimsimplex_reflect","optimsimplex_setall","optimsimplex_setallfv","optimsimplex_setallx","optimsimplex_setfv","optimsimplex_setn","optimsimplex_setnbve","optimsimplex_setve","optimsimplex_setx","optimsimplex_shrink","optimsimplex_size","optimsimplex_sort","optimsimplex_tostring","optimsimplex_xbar","orth","p_margin","pack","pareto_filter","parrot","pbig","pca","pcg","pdiv","pen2ea","pencan","pencost","penlaur","perctl","perl","perms","permute","pertrans","pfactors","pfss","phasemag","phaseplot","phc","pinv","playsnd","plotprofile","plzr","pmodulo","pol2des","pol2str","polar","polfact","prbs_a","prettyprint","primes","princomp","profile","proj","projsl","projspec","psmall","pspect","qmr","qpsolve","quart","quaskro","rafiter","randpencil","range","rank","read_csv","readxls","recompilefunction","recons","reglin","regress","remezb","remove_param","remove_profiling","repfreq","replace_Ix_by_Fx","repmat","reset_profiling","resize_matrix","returntoscilab","rhs2code","ric_desc","riccati","rmdir","routh_t","rowcomp","rowcompr","rowinout","rowregul","rowshuff","rref","sample","samplef","samwr","savematfile","savewave","scanf","sci2exp","sciGUI_init","sci_sparse","scicos_getvalue","scicos_simulate","scicos_workspace_init","scisptdemo","scitest","sdiff","sec","secd","sech","selection_ga_elitist","selection_ga_random","sensi","set_param","setdiff","sgrid","show_margins","show_pca","showprofile","signm","sinc","sincd","sind","sinh","sinhm","sinm","sm2des","sm2ss","smga","smooth","solve","sound","soundsec","sp2adj","spaninter","spanplus","spantwo","specfact","speye","sprand","spzeros","sqroot","sqrtm","squarewave","squeeze","srfaur","srkf","ss2des","ss2ss","ss2tf","sscanf","sskf","ssprint","ssrand","st_deviation","st_i_generic","st_ility","stabil","statgain","stdev","stdevf","steadycos","strange","strcmpi","struct","sub2ind","sva","svplot","sylm","sylv","sysconv","sysdiag","sysfact","syslin","syssize","system","systmat","tabul","tand","tanh","tanhm","tanm","tbx_build_blocks","tbx_build_cleaner","tbx_build_gateway","tbx_build_gateway_clean","tbx_build_gateway_loader","tbx_build_help","tbx_build_help_loader","tbx_build_loader","tbx_build_macros","tbx_build_src","tbx_builder","tbx_builder_gateway","tbx_builder_gateway_lang","tbx_builder_help","tbx_builder_help_lang","tbx_builder_macros","tbx_builder_src","tbx_builder_src_lang","temp_law_csa","temp_law_default","temp_law_fsa","temp_law_huang","temp_law_vfsa","test_clean","test_on_columns","test_run","test_run_level","testexamples","tf2des","tf2ss","thrownan","tic","time_id","toc","toeplitz","tokenpos","toolboxes","trace","trans","translatepaths","tree2code","trfmod","trianfml","trimmean","trisolve","trzeros","typeof","ui_observer","union","unique","unit_test_run","unix_g","unix_s","unix_w","unix_x","unobs","unpack","variance","variancef","vec2list","vectorfind","ver","warnobsolete","wavread","wavwrite","wcenter","weekday","wfir","wfir_gui","whereami","who_user","whos","wiener","wigner","winclose","window","winlist","with_javasci","with_macros_source","with_modelica_compiler","with_pvm","with_texmacs","with_tk","write_csv","xcosBlockEval","xcosBlockInterface","xcosCodeGeneration","xcosConfigureModelica","xcosPal","xcosPalAdd","xcosPalAddBlock","xcosPalExport","xcosShowBlockWarning","xcosValidateBlockSet","xcosValidateCompareBlock","xcos_compile","xcos_run","xcos_simulate","xcos_workspace_init","xmltochm","xmltoformat","xmltohtml","xmltojar","xmltopdf","xmltops","xmltoweb","yulewalk","zeropen","zgrid","zpbutt","zpch1","zpch2","zpell"]

    builtin_consts = ["$","%F","%T","%e","%eps","%f","%fftw","%gui","%i","%inf","%io","%modalWarning","%nan","%pi","%s","%t","%tk","%toolboxes","%toolboxes_dir","%z","PWD","SCI","SCIHOME","TMPDIR","a","ans","assertlib","atomslib","cacsdlib","compatibility_functilib","corelib","data_structureslib","demo_toolslib","development_toolslib","differential_equationlib","dynamic_linklib","elementary_functionslib","fd","fileiolib","functionslib","genetic_algorithmslib","helptoolslib","home","i","integerlib","interpolationlib","iolib","j","linear_algebralib","m2scilib","matiolib","modules_managerlib","myStr","neldermeadlib","optimbaselib","optimizationlib","optimsimplexlib","output_streamlib","overloadinglib","parameterslib","polynomialslib","scicos_autolib","scicos_utilslib","scinoteslib","signal_processinglib","simulated_annealinglib","soundlib","sparselib","special_functionslib","spreadsheetlib","statisticslib","stringlib","tclscilib","timelib","umfpacklib","varType","xcoslib"]

    tokens = {
        'root': [
            (r'//.*?$', Comment.Single),
            (r'^\s*function', Keyword, 'deffunc'),

            (r'(__FILE__|__LINE__|break|case|catch|classdef|continue|do|else|'
             r'elseif|end|end_try_catch|end_unwind_protect|endclassdef|'
             r'endevents|endfor|endfunction|endif|endmethods|endproperties|'
             r'endswitch|endwhile|events|for|function|get|global|if|methods|'
             r'otherwise|persistent|properties|return|set|static|switch|try|'
             r'until|unwind_protect|unwind_protect_cleanup|while)\b', Keyword),

            ("(" + "|".join(  functions_kw + commands_kw
                            + macros_kw
                            ) + r')\b',  Name.Builtin),

            ("(" + "|".join(builtin_consts) + r')\b', Name.Constant),

            # operators:
            (r'-|==|~=|<|>|<=|>=|&&|&|~|\|\|?', Operator),
            # operators requiring escape for re:
            (r'\.\*|\*|\+|\.\^|\.\\|\.\/|\/|\\', Operator),


            # punctuation:
            (r'\[|\]|\(|\)|\{|\}|:|@|\.|,', Punctuation),
            (r'=|:|;', Punctuation),

            (r'"[^"]*"', String),

            # quote can be transpose, instead of string:
            # (not great, but handles common cases...)
            (r'(?<=[\w\)\]])\'', Operator),
            (r'(?<![\w\)\]])\'', String, 'string'),

            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'.', Text),
        ],
        'string': [
            (r"[^']*'", String, '#pop'),
        ],
        'deffunc': [
            (r'(\s*)(?:(.+)(\s*)(=)(\s*))?(.+)(\()(.*)(\))(\s*)',
             bygroups(Text.Whitespace, Text, Text.Whitespace, Punctuation,
                      Text.Whitespace, Name.Function, Punctuation, Text,
                      Punctuation, Text.Whitespace), '#pop'),
        ],
    }

    def analyse_text(text):
        if re.match('^\s*[//#]', text, re.M): #Comment
            return 0.9
        return 0.1


class NumPyLexer(PythonLexer):
    """
    A Python lexer recognizing Numerical Python builtins.

    *New in Pygments 0.10.*
    """

    name = 'NumPy'
    aliases = ['numpy']

    # override the mimetypes to not inherit them from python
    mimetypes = []
    filenames = []

    EXTRA_KEYWORDS = set([
        'abs', 'absolute', 'accumulate', 'add', 'alen', 'all', 'allclose',
        'alltrue', 'alterdot', 'amax', 'amin', 'angle', 'any', 'append',
        'apply_along_axis', 'apply_over_axes', 'arange', 'arccos', 'arccosh',
        'arcsin', 'arcsinh', 'arctan', 'arctan2', 'arctanh', 'argmax', 'argmin',
        'argsort', 'argwhere', 'around', 'array', 'array2string', 'array_equal',
        'array_equiv', 'array_repr', 'array_split', 'array_str', 'arrayrange',
        'asanyarray', 'asarray', 'asarray_chkfinite', 'ascontiguousarray',
        'asfarray', 'asfortranarray', 'asmatrix', 'asscalar', 'astype',
        'atleast_1d', 'atleast_2d', 'atleast_3d', 'average', 'bartlett',
        'base_repr', 'beta', 'binary_repr', 'bincount', 'binomial',
        'bitwise_and', 'bitwise_not', 'bitwise_or', 'bitwise_xor', 'blackman',
        'bmat', 'broadcast', 'byte_bounds', 'bytes', 'byteswap', 'c_',
        'can_cast', 'ceil', 'choose', 'clip', 'column_stack', 'common_type',
        'compare_chararrays', 'compress', 'concatenate', 'conj', 'conjugate',
        'convolve', 'copy', 'corrcoef', 'correlate', 'cos', 'cosh', 'cov',
        'cross', 'cumprod', 'cumproduct', 'cumsum', 'delete', 'deprecate',
        'diag', 'diagflat', 'diagonal', 'diff', 'digitize', 'disp', 'divide',
        'dot', 'dsplit', 'dstack', 'dtype', 'dump', 'dumps', 'ediff1d', 'empty',
        'empty_like', 'equal', 'exp', 'expand_dims', 'expm1', 'extract', 'eye',
        'fabs', 'fastCopyAndTranspose', 'fft', 'fftfreq', 'fftshift', 'fill',
        'finfo', 'fix', 'flat', 'flatnonzero', 'flatten', 'fliplr', 'flipud',
        'floor', 'floor_divide', 'fmod', 'frexp', 'fromarrays', 'frombuffer',
        'fromfile', 'fromfunction', 'fromiter', 'frompyfunc', 'fromstring',
        'generic', 'get_array_wrap', 'get_include', 'get_numarray_include',
        'get_numpy_include', 'get_printoptions', 'getbuffer', 'getbufsize',
        'geterr', 'geterrcall', 'geterrobj', 'getfield', 'gradient', 'greater',
        'greater_equal', 'gumbel', 'hamming', 'hanning', 'histogram',
        'histogram2d', 'histogramdd', 'hsplit', 'hstack', 'hypot', 'i0',
        'identity', 'ifft', 'imag', 'index_exp', 'indices', 'inf', 'info',
        'inner', 'insert', 'int_asbuffer', 'interp', 'intersect1d',
        'intersect1d_nu', 'inv', 'invert', 'iscomplex', 'iscomplexobj',
        'isfinite', 'isfortran', 'isinf', 'isnan', 'isneginf', 'isposinf',
        'isreal', 'isrealobj', 'isscalar', 'issctype', 'issubclass_',
        'issubdtype', 'issubsctype', 'item', 'itemset', 'iterable', 'ix_',
        'kaiser', 'kron', 'ldexp', 'left_shift', 'less', 'less_equal', 'lexsort',
        'linspace', 'load', 'loads', 'loadtxt', 'log', 'log10', 'log1p', 'log2',
        'logical_and', 'logical_not', 'logical_or', 'logical_xor', 'logspace',
        'lstsq', 'mat', 'matrix', 'max', 'maximum', 'maximum_sctype',
        'may_share_memory', 'mean', 'median', 'meshgrid', 'mgrid', 'min',
        'minimum', 'mintypecode', 'mod', 'modf', 'msort', 'multiply', 'nan',
        'nan_to_num', 'nanargmax', 'nanargmin', 'nanmax', 'nanmin', 'nansum',
        'ndenumerate', 'ndim', 'ndindex', 'negative', 'newaxis', 'newbuffer',
        'newbyteorder', 'nonzero', 'not_equal', 'obj2sctype', 'ogrid', 'ones',
        'ones_like', 'outer', 'permutation', 'piecewise', 'pinv', 'pkgload',
        'place', 'poisson', 'poly', 'poly1d', 'polyadd', 'polyder', 'polydiv',
        'polyfit', 'polyint', 'polymul', 'polysub', 'polyval', 'power', 'prod',
        'product', 'ptp', 'put', 'putmask', 'r_', 'randint', 'random_integers',
        'random_sample', 'ranf', 'rank', 'ravel', 'real', 'real_if_close',
        'recarray', 'reciprocal', 'reduce', 'remainder', 'repeat', 'require',
        'reshape', 'resize', 'restoredot', 'right_shift', 'rint', 'roll',
        'rollaxis', 'roots', 'rot90', 'round', 'round_', 'row_stack', 's_',
        'sample', 'savetxt', 'sctype2char', 'searchsorted', 'seed', 'select',
        'set_numeric_ops', 'set_printoptions', 'set_string_function',
        'setbufsize', 'setdiff1d', 'seterr', 'seterrcall', 'seterrobj',
        'setfield', 'setflags', 'setmember1d', 'setxor1d', 'shape',
        'show_config', 'shuffle', 'sign', 'signbit', 'sin', 'sinc', 'sinh',
        'size', 'slice', 'solve', 'sometrue', 'sort', 'sort_complex', 'source',
        'split', 'sqrt', 'square', 'squeeze', 'standard_normal', 'std',
        'subtract', 'sum', 'svd', 'swapaxes', 'take', 'tan', 'tanh', 'tensordot',
        'test', 'tile', 'tofile', 'tolist', 'tostring', 'trace', 'transpose',
        'trapz', 'tri', 'tril', 'trim_zeros', 'triu', 'true_divide', 'typeDict',
        'typename', 'uniform', 'union1d', 'unique', 'unique1d', 'unravel_index',
        'unwrap', 'vander', 'var', 'vdot', 'vectorize', 'view', 'vonmises',
        'vsplit', 'vstack', 'weibull', 'where', 'who', 'zeros', 'zeros_like'
    ])

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
                PythonLexer.get_tokens_unprocessed(self, text):
            if token is Name and value in self.EXTRA_KEYWORDS:
                yield index, Keyword.Pseudo, value
            else:
                yield index, token, value


class RConsoleLexer(Lexer):
    """
    For R console transcripts or R CMD BATCH output files.
    """

    name = 'RConsole'
    aliases = ['rconsole', 'rout']
    filenames = ['*.Rout']

    def get_tokens_unprocessed(self, text):
        slexer = SLexer(**self.options)

        current_code_block = ''
        insertions = []

        for match in line_re.finditer(text):
            line = match.group()
            if line.startswith('>') or line.startswith('+'):
                # Colorize the prompt as such,
                # then put rest of line into current_code_block
                insertions.append((len(current_code_block),
                                   [(0, Generic.Prompt, line[:2])]))
                current_code_block += line[2:]
            else:
                # We have reached a non-prompt line!
                # If we have stored prompt lines, need to process them first.
                if current_code_block:
                    # Weave together the prompts and highlight code.
                    for item in do_insertions(insertions,
                          slexer.get_tokens_unprocessed(current_code_block)):
                        yield item
                    # Reset vars for next code block.
                    current_code_block = ''
                    insertions = []
                # Now process the actual line itself, this is output from R.
                yield match.start(), Generic.Output, line

        # If we happen to end on a code block with nothing after it, need to
        # process the last code block. This is neither elegant nor DRY so
        # should be changed.
        if current_code_block:
            for item in do_insertions(insertions,
                    slexer.get_tokens_unprocessed(current_code_block)):
                yield item


class SLexer(RegexLexer):
    """
    For S, S-plus, and R source code.

    *New in Pygments 0.10.*
    """

    name = 'S'
    aliases = ['splus', 's', 'r']
    filenames = ['*.S', '*.R']
    mimetypes = ['text/S-plus', 'text/S', 'text/R']

    tokens = {
        'comments': [
            (r'#.*$', Comment.Single),
        ],
        'valid_name': [
            (r'[a-zA-Z][0-9a-zA-Z\._]+', Text),
            (r'`.+`', String.Backtick),
        ],
        'punctuation': [
            (r'\[|\]|\[\[|\]\]|\$|\(|\)|@|:::?|;|,', Punctuation),
        ],
        'keywords': [
            (r'for(?=\s*\()|while(?=\s*\()|if(?=\s*\()|(?<=\s)else|'
             r'(?<=\s)break(?=;|$)|return(?=\s*\()|function(?=\s*\()',
             Keyword.Reserved)
        ],
        'operators': [
            (r'<-|-|==|<=|>=|<|>|&&|&|!=|\|\|?', Operator),
            (r'\*|\+|\^|/|%%|%/%|=', Operator),
            (r'%in%|%*%', Operator)
        ],
        'builtin_symbols': [
            (r'(NULL|NA|TRUE|FALSE|NaN)\b', Keyword.Constant),
            (r'(T|F)\b', Keyword.Variable),
        ],
        'numbers': [
            (r'(?<![0-9a-zA-Z\)\}\]`\"])(?=\s*)[-\+]?[0-9]+'
             r'(\.[0-9]*)?(E[0-9][-\+]?(\.[0-9]*)?)?', Number),
            (r'\.[0-9]*(E[0-9][-\+]?(\.[0-9]*)?)?', Number),
        ],
        'statements': [
            include('comments'),
            # whitespaces
            (r'\s+', Text),
            (r'\'', String, 'string_squote'),
            (r'\"', String, 'string_dquote'),
            include('builtin_symbols'),
            include('numbers'),
            include('keywords'),
            include('punctuation'),
            include('operators'),
            include('valid_name'),
        ],
        'root': [
            include('statements'),
            # blocks:
            (r'\{|\}', Punctuation),
            #(r'\{', Punctuation, 'block'),
            (r'.', Text),
        ],
        #'block': [
        #    include('statements'),
        #    ('\{', Punctuation, '#push'),
        #    ('\}', Punctuation, '#pop')
        #],
        'string_squote': [
            (r'[^\']*\'', String, '#pop'),
        ],
        'string_dquote': [
            (r'[^\"]*\"', String, '#pop'),
        ],
    }

    def analyse_text(text):
        return '<-' in text
