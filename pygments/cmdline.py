# -*- coding: utf-8 -*-
"""
    pygments.cmdline
    ~~~~~~~~~~~~~~~~

    Command line interface.

    :copyright: 2006 by Georg Brandl.
    :license: BSD, see LICENSE for more details.
"""
import getopt

def main(args):
    """
    Main command line entry point.
    """

    USAGE = """\
Usage: %s [-l <lexer>] [-f <formatter>] [-O <options>] [-o <outfile>] [<infile>]
       %s -S <style> -f <formatter> [-a <arg>] [-O <options>]
       %s -L | -h | -V

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

With the -S option, print out style definitions for style <style>
for formatter <formatter>. The argument given by -a is formatter
dependent.

The -L option lists all available lexers and formatters.
The -h option prints this help.
The -V option prints the package version.
""" % ((args[0],)*3)

    try:
        opts, args = getopt.getopt(args[1:], "l:f:o:O:LhVS:a:")
    except getopt.GetoptError:
        print >>sys.stderr, USAGE
        return 2
    opts = dict(opts)

    if not opts and not args:
        print USAGE
        return 0

    if opts.pop('-h', None) is not None:
        print USAGE
        return 0

    if opts.pop('-V', None) is not None:
        print 'Pygments version %s, (c) 2006 by %s.' % (__version__, __author__)
        return 0

    L_opt = opts.pop('-L', None)
    if L_opt is not None:
        if opts or args:
            print >>sys.stderr, USAGE
            return 2

        # print version
        cmdline_main(['', '-V'])
        print
        print "Lexers:"
        print "~~~~~~~"

        info = []
        maxlen = 0
        for _, fullname, names, exts, _ in LEXERS.itervalues():
            tup = (', '.join(names)+':', fullname,
                   exts and '(extensions ' + ', '.join(exts) + ')' or '')
            info.append(tup)
            if len(tup[0]) > maxlen: maxlen = len(tup[0])
        info.sort()
        for i in info:
            print ('%-'+str(maxlen)+'s %s %s') % i

        print
        print "Formatters:"
        print "~~~~~~~~~~~"

        info = []
        maxlen = 0
        for fullname, names, exts, doc in FORMATTERS.itervalues():
            tup = (', '.join(names)+':', doc,
                   exts and '(extensions ' + ', '.join(exts) + ')' or '')
            info.append(tup)
            if len(tup[0]) > maxlen: maxlen = len(tup[0])
        info.sort()
        for i in info:
            print ('%-'+str(maxlen)+'s %s %s') % i
        return 0

    O_opts = {}
    o_str = opts.pop('-O', None)
    if o_str:
        try:
            o_args = o_str.split(',')
            for o_arg in o_args:
                try:
                    o_key, o_val = o_arg.split('=')
                except ValueError:
                    O_opts[o_arg] = True
                else:
                    O_opts[o_key] = o_val
        except ValueError:
            print >>sys.stderr, 'Error in -O specification.'
            return 2

    S_opt = opts.pop('-S', None)
    a_opt = opts.pop('-a', None)
    if S_opt is not None:
        f_opt = opts.pop('-f', None)
        if not f_opt:
            print >>sys.stderr, USAGE
            return 2
        if opts or args:
            print >>sys.stderr, USAGE
            return 2

        try:
            O_opts['style'] = S_opt
            fmter = get_formatter_by_name(f_opt, **O_opts)
        except ValueError, err:
            print >>sys.stderr, err
            return 1

        arg = a_opt or ''
        print fmter.get_style_defs(arg)
        return 0

    if a_opt is not None:
        print >>sys.stderr, USAGE
        return 2

    outfn = opts.pop('-o', None)
    fmter = opts.pop('-f', None)
    if fmter:
        try:
            fmter = get_formatter_by_name(fmter, **O_opts)
        except (OptionError, ValueError), err:
            print >>sys.stderr, 'Error:', err
            return 1

    if outfn:
        if not fmter:
            try:
                fmter = get_formatter_for_filename(outfn, **O_opts)
            except (OptionError, ValueError), err:
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

    lexer = opts.pop('-l', None)
    if lexer:
        try:
            lexer = get_lexer_by_name(lexer, **O_opts)
        except (OptionError, ValueError), err:
            print >>sys.stderr, 'Error:', err
            return 1

    if args:
        infn = args[0]
        if not lexer:
            try:
                lexer = get_lexer_for_filename(infn, **O_opts)
            except (OptionError, ValueError), err:
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

    try:
        highlight(code, lexer, fmter, outfile)
    except Exception, err:
        import traceback
        print >>sys.stderr, 'Error while highlighting:'
        print >>sys.stderr, traceback.format_exc(0).splitlines()[-1]
        return 1
    return 0
