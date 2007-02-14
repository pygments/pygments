# -*- coding: utf-8 -*-
"""
    pygments.cmdline
    ~~~~~~~~~~~~~~~~

    Command line interface.

    :copyright: 2006-2007 by Georg Brandl.
    :license: BSD, see LICENSE for more details.
"""
import sys
import getopt
from textwrap import dedent

from pygments import __version__, __author__, highlight
from pygments.util import ClassNotFound, OptionError, docstring_headline
from pygments.lexers import get_all_lexers, get_lexer_by_name, get_lexer_for_filename, \
     find_lexer_class
from pygments.formatters import get_all_formatters, get_formatter_by_name, \
     get_formatter_for_filename, find_formatter_class, \
     TerminalFormatter  # pylint:disable-msg=E0611
from pygments.filters import get_all_filters, find_filter_class
from pygments.styles import get_all_styles, get_style_by_name


USAGE = """\
Usage: %s [-l <lexer>] [-F <filter>[:<options]] [-f <formatter>]
          [-O <options>] [-o <outfile>] [<infile>]

       %s -S <style> -f <formatter> [-a <arg>] [-O <options>]
       %s -L [<which> ...]
       %s -H <type> <name>
       %s -h | -V

Highlight the input file and write the result to <outfile>.

If no input file is given, use stdin, if -o is not given, use stdout.

<lexer> is a lexer name (query all lexer names with -L). If -l is not
given, the lexer is guessed from the extension of the input file name
(this obviously doesn't work if the input is stdin).

Likewise, <formatter> is a formatter name, and will be guessed from
the extension of the output file name. If no output file is given,
the terminal formatter will be used by default.

With the -O option, you can give the lexer and formatter a comma-
separated list of options, e.g. ``-O bg=light,python=cool``.

With the -F option, you can add filters to the token stream, you can
give options in the same way as for -O after a colon (note: there must
not be spaces around the colon).

The -O and -F options can be given multiple times.

With the -S option, print out style definitions for style <style>
for formatter <formatter>. The argument given by -a is formatter
dependent.

The -L option lists lexers, formatters, styles or filters -- set
`which` to the thing you want to list (e.g. "styles"), or omit it to
list everything.

The -H option prints detailed help for the object <name> of type <type>,
where <type> is one of "lexer", "formatter" or "filter".

The -h option prints this help.
The -V option prints the package version.
"""


def _parse_options(o_strs):
    opts = {}
    if not o_strs:
        return opts
    for o_str in o_strs:
        if not o_str:
            continue
        o_args = o_str.split(',')
        for o_arg in o_args:
            o_arg = o_arg.strip()
            try:
                o_key, o_val = o_arg.split('=')
                o_key = o_key.strip()
                o_val = o_val.strip()
            except ValueError:
                opts[o_arg] = True
            else:
                opts[o_key] = o_val
    return opts


def _parse_filters(f_strs):
    filters = []
    if not f_strs:
        return filters
    for f_str in f_strs:
        if ':' in f_str:
            fname, fopts = f_str.split(':', 1)
            filters.append((fname, _parse_options([fopts])))
        else:
            filters.append((f_str, {}))
    return filters


def _print_help(what, name):
    try:
        if what == 'lexer':
            cls = find_lexer_class(name)
            print "Help on the %s lexer:" % cls.name
            print dedent(cls.__doc__)
        elif what == 'formatter':
            cls = find_formatter_class(name)
            print "Help on the %s formatter:" % cls.name
            print dedent(cls.__doc__)
        elif what == 'filter':
            cls = find_filter_class(name)
            print "Help on the %s filter:" % name
            print dedent(cls.__doc__)
    except ClassNotFound:
        print >>sys.stderr, "%s not found!" % what


def _print_list(what):
    if what == 'lexer':
        print
        print "Lexers:"
        print "~~~~~~~"

        info = []
        for fullname, names, exts, _ in get_all_lexers():
            tup = (', '.join(names)+':', fullname,
                   exts and '(filenames ' + ', '.join(exts) + ')' or '')
            info.append(tup)
        info.sort()
        for i in info:
            print ('* %s\n    %s %s') % i

    elif what == 'formatter':
        print
        print "Formatters:"
        print "~~~~~~~~~~~"

        info = []
        for cls in get_all_formatters():
            doc = docstring_headline(cls)
            tup = (', '.join(cls.aliases) + ':', doc,
                   cls.filenames and '(filenames ' + ', '.join(cls.filenames) + ')' or '')
            info.append(tup)
        info.sort()
        for i in info:
            print ('* %s\n    %s %s') % i

    elif what == 'filter':
        print
        print "Filters:"
        print "~~~~~~~~"

        for name in get_all_filters():
            cls = find_filter_class(name)
            print "* " + name + ':'
            print "    %s" % docstring_headline(cls)

    elif what == 'style':
        print
        print "Styles:"
        print "~~~~~~~"
        
        for name in get_all_styles():
            cls = get_style_by_name(name)
            print "* " + name + ':'
            print "    %s" % docstring_headline(cls) 


def main(args):
    """
    Main command line entry point.
    """
    # pylint: disable-msg=R0911,R0912,R0915
    
    usage = USAGE % ((args[0],) * 5)

    try:
        popts, args = getopt.getopt(args[1:], "l:f:F:o:O:LS:a:hVH")
    except getopt.GetoptError, err:
        print >>sys.stderr, usage
        return 2
    opts = {}
    O_opts = []
    F_opts = []
    for opt, arg in popts:
        if opt == '-O':
            O_opts.append(arg)
        elif opt == '-F':
            F_opts.append(arg)
        opts[opt] = arg

    if not opts and not args:
        print usage
        return 0

    if opts.pop('-h', None) is not None:
        print usage
        return 0

    if opts.pop('-V', None) is not None:
        print 'Pygments version %s, (c) 2006 by %s.' % (__version__, __author__)
        return 0

    # handle ``pygmentize -L``
    L_opt = opts.pop('-L', None)
    if L_opt is not None:
        if opts:
            print >>sys.stderr, usage
            return 2

        # print version
        main(['', '-V'])
        if not args:
            args = ['lexer', 'formatter', 'filter', 'style']
        for arg in args:
            _print_list(arg.rstrip('s'))
        return 0

    # handle ``pygmentize -H``
    H_opt = opts.pop('-H', None)
    if H_opt is not None:
        if opts or len(args) != 2:
            print >>sys.stderr, usage
            return 2

        what, name = args
        if what not in ('lexer', 'formatter', 'filter'):
            print >>sys.stderr, usage
            return 2

        _print_help(what, name)
        return 0

    # parse -O options
    O_opts = _parse_options(O_opts)
    # parse -F options
    F_opts = _parse_filters(F_opts)

    # handle ``pygmentize -S``
    S_opt = opts.pop('-S', None)
    a_opt = opts.pop('-a', None)
    if S_opt is not None:
        f_opt = opts.pop('-f', None)
        if not f_opt:
            print >>sys.stderr, usage
            return 2
        if opts or args:
            print >>sys.stderr, usage
            return 2

        try:
            O_opts['style'] = S_opt
            fmter = get_formatter_by_name(f_opt, **O_opts)
        except ClassNotFound, err:
            print >>sys.stderr, err
            return 1

        arg = a_opt or ''
        print fmter.get_style_defs(arg)
        return 0

    # if no -S is given, -a is not allowed
    if a_opt is not None:
        print >>sys.stderr, usage
        return 2

    # select formatter
    outfn = opts.pop('-o', None)
    fmter = opts.pop('-f', None)
    if fmter:
        try:
            fmter = get_formatter_by_name(fmter, **O_opts)
        except (OptionError, ClassNotFound), err:
            print >>sys.stderr, 'Error:', err
            return 1

    if outfn:
        if not fmter:
            try:
                fmter = get_formatter_for_filename(outfn, **O_opts)
            except (OptionError, ClassNotFound), err:
                print >>sys.stderr, 'Error:', err
                return 1
        try:
            outfile = file(outfn, 'wb')
        except Exception, err:
            print >>sys.stderr, 'Error: cannot open outfile:', err
            return 1
    else:
        if not fmter:
            fmter = TerminalFormatter(**O_opts)
        outfile = sys.stdout

    # select lexer
    lexer = opts.pop('-l', None)
    if lexer:
        try:
            lexer = get_lexer_by_name(lexer, **O_opts)
        except (OptionError, ClassNotFound), err:
            print >>sys.stderr, 'Error:', err
            return 1

    if args:
        if len(args) > 1:
            print >>sys.stderr, usage
            return 2

        infn = args[0]
        if not lexer:
            try:
                lexer = get_lexer_for_filename(infn, **O_opts)
            except (OptionError, ClassNotFound), err:
                print >>sys.stderr, 'Error:', err
                return 1

        try:
            code = file(infn).read()
        except Exception, err:
            print >>sys.stderr, 'Error: cannot read infile:', err
            return 1
    else:
        if not lexer:
            print >>sys.stderr, 'Error: no lexer name given and reading from stdin'
            return 2
        code = sys.stdin.read()

    # ... and do it!
    try:
        # process filters
        for fname, fopts in F_opts:
            lexer.add_filter(fname, **fopts)
        highlight(code, lexer, fmter, outfile)
    except Exception, err:
        import traceback
        info = traceback.format_exception(*sys.exc_info())
        msg = info[-1].strip()
        if len(info) >= 3:
            # extract relevant file and position info
            msg += '\n   (f%s)' % info[-2].split('\n')[0].strip()[1:]
        print >>sys.stderr
        print >>sys.stderr, '*** Error while highlighting:'
        print >>sys.stderr, msg
        return 1

    return 0
