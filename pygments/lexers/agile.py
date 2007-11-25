# -*- coding: utf-8 -*-
"""
    pygments.lexers.agile
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for agile languages.

    :copyright: 2006-2007 by Georg Brandl, Armin Ronacher,
                Lukas Meuser, Tim Hatch, Jarrett Billingsley,
                Tassilo Schweyer.
    :license: BSD, see LICENSE for more details.
"""

import re
try:
    set
except NameError:
    from sets import Set as set

from pygments.lexer import Lexer, RegexLexer, ExtendedRegexLexer, \
     LexerContext, include, combined, do_insertions, bygroups, using
from pygments.token import Error, Text, \
     Comment, Operator, Keyword, Name, String, Number, Generic, Punctuation
from pygments.util import get_bool_opt, get_list_opt, shebang_matches


__all__ = ['PythonLexer', 'PythonConsoleLexer', 'PythonTracebackLexer',
           'RubyLexer', 'RubyConsoleLexer', 'PerlLexer', 'LuaLexer',
           'MiniDLexer', 'IoLexer']

# b/w compatibility
from pygments.lexers.functional import SchemeLexer

line_re  = re.compile('.*?\n')


class PythonLexer(RegexLexer):
    """
    For `Python <http://www.python.org>`_ source code.
    """

    name = 'Python'
    aliases = ['python', 'py']
    filenames = ['*.py', '*.pyw', '*.sc', 'SConstruct', 'SConscript']
    mimetypes = ['text/x-python', 'application/x-python']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'^(\s*)("""(?:.|\n)*?""")', bygroups(Text, String.Doc)),
            (r"^(\s*)('''(?:.|\n)*?''')", bygroups(Text, String.Doc)),
            (r'[^\S\n]+', Text),
            (r'#.*$', Comment),
            (r'[]{}:(),;[]', Punctuation),
            (r'\\\n', Text),
            (r'\\', Text),
            (r'(in|is|and|or|not)\b', Operator.Word),
            (r'!=|==|<<|>>|[-~+/*%=<>&^|.]', Operator),
            (r'(assert|break|continue|del|elif|else|except|exec|'
             r'finally|for|global|if|lambda|pass|print|raise|'
             r'return|try|while|yield|as|with)\b', Keyword),
            (r'(def)(\s+)', bygroups(Keyword, Text), 'funcname'),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'(from)(\s+)', bygroups(Keyword, Text), 'fromimport'),
            (r'(import)(\s+)', bygroups(Keyword, Text), 'import'),
            (r'@[a-zA-Z0-9_]+', Name.Decorator),
            (r'(?<!\.)(__import__|abs|apply|basestring|bool|buffer|callable|'
             r'chr|classmethod|cmp|coerce|compile|complex|delattr|dict|dir|'
             r'divmod|enumerate|eval|execfile|exit|file|filter|float|getattr|'
             r'globals|hasattr|hash|hex|id|input|int|intern|isinstance|'
             r'issubclass|iter|len|list|locals|long|map|max|min|object|oct|'
             r'open|ord|pow|property|range|raw_input|reduce|reload|repr|'
             r'round|setattr|slice|staticmethod|str|sum|super|tuple|type|'
             r'unichr|unicode|vars|xrange|zip)\b', Name.Builtin),
            (r'(?<!\.)(self|None|Ellipsis|NotImplemented|False|True'
             r')\b', Name.Builtin.Pseudo),
            (r'(?<!\.)(ArithmeticError|AssertionError|AttributeError|'
             r'DeprecationWarning|EOFError|EnvironmentError|'
             r'Exception|FloatingPointError|FutureWarning|IOError|'
             r'ImportError|IndentationError|IndexError|KeyError|'
             r'KeyboardInterrupt|LookupError|MemoryError|NameError|'
             r'NotImplemented|NotImplementedError|OSError|OverflowError|'
             r'OverflowWarning|PendingDeprecationWarning|ReferenceError|'
             r'RuntimeError|RuntimeWarning|StandardError|StopIteration|'
             r'SyntaxError|SyntaxWarning|SystemError|SystemExit|TabError|'
             r'TypeError|UnboundLocalError|UnicodeDecodeError|'
             r'UnicodeEncodeError|UnicodeError|UnicodeTranslateError|'
             r'UserWarning|ValueError|Warning|ZeroDivisionError'
             r')\b', Name.Exception),
            ('`.*?`', String.Backtick),
            ('(?:[rR]|[uU][rR]|[rR][uU])"""', String, 'tdqs'),
            ("(?:[rR]|[uU][rR]|[rR][uU])'''", String, 'tsqs'),
            ('(?:[rR]|[uU][rR]|[rR][uU])"', String, 'dqs'),
            ("(?:[rR]|[uU][rR]|[rR][uU])'", String, 'sqs'),
            ('[uU]?"""', String, combined('stringescape', 'tdqs')),
            ("[uU]?'''", String, combined('stringescape', 'tsqs')),
            ('[uU]?"', String, combined('stringescape', 'dqs')),
            ("[uU]?'", String, combined('stringescape', 'sqs')),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'0\d+', Number.Oct),
            (r'0x[a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+', Number.Integer)
        ],
        'funcname': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name.Function, '#pop')
        ],
        'classname': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop')
        ],
        'import': [
            (r'(\s+)(as)(\s+)', bygroups(Text, Keyword, Text)),
            (r'[a-zA-Z_][a-zA-Z0-9_.]*', Name.Namespace),
            (r'(\s*)(,)(\s*)', bygroups(Text, Operator, Text)),
            (r'', Text, '#pop') # all else: go back
        ],
        'fromimport': [
            (r'(\s+)(import)\b', bygroups(Text, Keyword), '#pop'),
            (r'[a-zA-Z_.][a-zA-Z0-9_.]*', Name.Namespace),
        ],
        'stringescape': [
            (r'\\([\\abfnrtv"\']|\n|N{.*?}|u[a-fA-F0-9]{4}|'
             r'U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'strings': [
            (r'%(\([a-zA-Z0-9]+\))?[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?'
             '[hlL]?[diouxXeEfFgGcrs%]', String.Interpol),
            (r'[^\\\'"%\n]+', String),
            # quotes, percents and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # unhandled string formatting sign
            (r'%', String)
            # newlines are an error (use "nl" state)
        ],
        'nl': [
            (r'\n', String)
        ],
        'dqs': [
            (r'"', String, '#pop'),
            (r'\\\\|\\"|\\\n', String.Escape), # included here again for raw strings
            include('strings')
        ],
        'sqs': [
            (r"'", String, '#pop'),
            (r"\\\\|\\'|\\\n", String.Escape), # included here again for raw strings
            include('strings')
        ],
        'tdqs': [
            (r'"""', String, '#pop'),
            include('strings'),
            include('nl')
        ],
        'tsqs': [
            (r"'''", String, '#pop'),
            include('strings'),
            include('nl')
        ],
    }

    def analyse_text(text):
        return shebang_matches(text, r'pythonw?(2\.\d)?')


class PythonConsoleLexer(Lexer):
    """
    For Python console output or doctests, such as:

    .. sourcecode:: pycon

        >>> a = 'foo'
        >>> print a
        foo
        >>> 1 / 0
        Traceback (most recent call last):
          File "<stdin>", line 1, in <module>
        ZeroDivisionError: integer division or modulo by zero
    """
    name = 'Python console session'
    aliases = ['pycon']
    mimetypes = ['text/x-python-doctest']

    def get_tokens_unprocessed(self, text):
        pylexer = PythonLexer(**self.options)
        tblexer = PythonTracebackLexer(**self.options)

        curcode = ''
        insertions = []
        curtb = ''
        tbindex = 0
        tb = 0
        for match in line_re.finditer(text):
            line = match.group()
            if line.startswith('>>> ') or line.startswith('... '):
                tb = 0
                insertions.append((len(curcode),
                                   [(0, Generic.Prompt, line[:4])]))
                curcode += line[4:]
            elif line.rstrip() == '...':
                tb = 0
                insertions.append((len(curcode),
                                   [(0, Generic.Prompt, '...')]))
                curcode += line[3:]
            else:
                if curcode:
                    for item in do_insertions(insertions,
                                    pylexer.get_tokens_unprocessed(curcode)):
                        yield item
                    curcode = ''
                    insertions = []
                if line.startswith('Traceback (most recent call last):'):
                    tb = 1
                    curtb = line
                    tbindex = match.start()
                elif tb:
                    curtb += line
                    if not (line.startswith(' ') or line.strip() == '...'):
                        tb = 0
                        for i, t, v in tblexer.get_tokens_unprocessed(curtb):
                            yield tbindex+i, t, v
                else:
                    yield match.start(), Generic.Output, line
        if curcode:
            for item in do_insertions(insertions,
                                      pylexer.get_tokens_unprocessed(curcode)):
                yield item


class PythonTracebackLexer(RegexLexer):
    """
    For Python tracebacks.

    *New in Pygments 0.7.*
    """

    name = 'Python Traceback'
    aliases = ['pytb']
    filenames = ['*.pytb']
    mimetypes = ['text/x-python-traceback']

    tokens = {
        'root': [
            (r'^Traceback \(most recent call last\):\n', Generic.Traceback, 'intb'),
        ],
        'intb': [
            (r'^(  File )("[^"]+")(, line )(\d+)(, in )(.+)(\n)',
             bygroups(Text, Name.Builtin, Text, Number, Text, Name.Identifier, Text)),
            (r'^(    )(.+)(\n)',
             bygroups(Text, using(PythonLexer), Text)),
            (r'^[ \t]*(...)(\n)',
             bygroups(Comment, Text)), # for doctests...
            (r'^(.+)(: )(.+)(\n)',
             bygroups(Name.Class, Text, Name.Identifier, Text), '#pop'),
            (r'^([a-zA-Z_][a-zA-Z0-9_]*)(:?\n)',
             bygroups(Name.Class, Text), '#pop')
        ],
    }


class RubyLexer(ExtendedRegexLexer):
    """
    For `Ruby <http://www.ruby-lang.org>`_ source code.
    """

    name = 'Ruby'
    aliases = ['rb', 'ruby']
    filenames = ['*.rb', '*.rbw', 'Rakefile', '*.rake', '*.gemspec', '*.rbx']
    mimetypes = ['text/x-ruby', 'application/x-ruby']

    flags = re.DOTALL | re.MULTILINE

    def heredoc_callback(self, match, ctx):
        # okay, this is the hardest part of parsing Ruby...
        # match: 1 = <<-?, 2 = quote? 3 = name 4 = quote? 5 = rest of line

        start = match.start(1)
        yield start, Operator, match.group(1)        # <<-?
        yield match.start(2), String.Heredoc, match.group(2)  # quote ", ', `
        yield match.start(3), Name.Constant, match.group(3)   # heredoc name
        yield match.start(4), String.Heredoc, match.group(4)  # quote again

        heredocstack = ctx.__dict__.setdefault('heredocstack', [])
        outermost = not bool(heredocstack)
        heredocstack.append((match.group(1) == '<<-', match.group(3)))

        ctx.pos = match.start(5)
        ctx.end = match.end(5)
        # this may find other heredocs
        for i, t, v in self.get_tokens_unprocessed(context=ctx):
            yield i+start, t, v
        ctx.pos = match.end()

        if outermost:
            # this is the outer heredoc again, now we can process them all
            for tolerant, hdname in heredocstack:
                lines = []
                for match in line_re.finditer(ctx.text, ctx.pos):
                    if tolerant:
                        check = match.group().strip()
                    else:
                        check = match.group().rstrip()
                    if check == hdname:
                        for amatch in lines:
                            yield amatch.start(), String.Heredoc, amatch.group()
                        yield match.start(), Name.Constant, match.group()
                        ctx.pos = match.end()
                        break
                    else:
                        lines.append(match)
                else:
                    # end of heredoc not found -- error!
                    for amatch in lines:
                        yield amatch.start(), Error, amatch.group()
            ctx.end = len(ctx.text)
            del heredocstack[:]


    def gen_rubystrings_rules():
        def intp_regex_callback(self, match, ctx):
            yield match.start(1), String.Regex, match.group(1)    # begin
            nctx = LexerContext(match.group(3), 0, ['interpolated-regex'])
            for i, t, v in self.get_tokens_unprocessed(context=nctx):
                yield match.start(3)+i, t, v
            yield match.start(4), String.Regex, match.group(4)    # end[mixounse]*
            ctx.pos = match.end()

        def intp_string_callback(self, match, ctx):
            yield match.start(1), String.Other, match.group(1)
            nctx = LexerContext(match.group(3), 0, ['interpolated-string'])
            for i, t, v in self.get_tokens_unprocessed(context=nctx):
                yield match.start(3)+i, t, v
            yield match.start(4), String.Other, match.group(4)    # end
            ctx.pos = match.end()

        states = {}
        states['strings'] = [
            # easy ones
            (r'\:([a-zA-Z_][\w_]*[\!\?]?|\*\*?|[-+]@?|'
             r'[/%&|^`~]|\[\]=?|<<|>>|<=?>|>=?|===?)', String.Symbol),
            (r":'(\\\\|\\'|[^'])*'", String.Symbol),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
            (r':"', String.Symbol, 'simple-sym'),
            (r'"', String.Double, 'simple-string'),
            (r'(?<!\.)`', String.Backtick, 'simple-backtick'),
        ]
        # double-quoted string and symbol

        for name, ttype, end in ('string', String.Double, '"'), \
                                ('sym', String.Symbol, '"'), \
                                ('backtick', String.Backtick, '`'):
            states['simple-'+name] = [
                include('string-intp-escaped'),
                (r'[^\\%s#]+' % end, ttype),
                (r'[\\#]', ttype),
                (end, ttype, '#pop'),
            ]

        # braced quoted strings

        for lbrace, rbrace, name in ('\\{', '\\}', 'cb'), \
                                    ('\\[', '\\]', 'sb'), \
                                    ('\\(', '\\)', 'pa'), \
                                    ('<', '>', 'ab'):
            states[name+'-intp-string'] = [
                (r'\\[\\' + lbrace + rbrace + ']', String.Other),
                (r'(?<!\\)' + lbrace, String.Other, '#push'),
                (r'(?<!\\)' + rbrace, String.Other, '#pop'),
                include('string-intp-escaped'),
                (r'[\\#' + lbrace + rbrace + ']', String.Other),
                (r'[^\\#' + lbrace + rbrace + ']+', String.Other),
            ]
            states['strings'].append((r'%[QWx]?' + lbrace, String.Other,
                                      name+'-intp-string'))
            states[name+'-string'] = [
                (r'\\[\\' + lbrace + rbrace + ']', String.Other),
                (r'(?<!\\)' + lbrace, String.Other, '#push'),
                (r'(?<!\\)' + rbrace, String.Other, '#pop'),
                (r'[\\#' + lbrace + rbrace + ']', String.Other),
                (r'[^\\#' + lbrace + rbrace + ']+', String.Other),
            ]
            states['strings'].append((r'%[qsw]' + lbrace, String.Other,
                                      name+'-string'))
            states[name+'-regex'] = [
                (r'\\[\\' + lbrace + rbrace + ']', String.Regex),
                (r'(?<!\\)' + lbrace, String.Regex, '#push'),
                (r'(?<!\\)' + rbrace + '[mixounse]*', String.Regex, '#pop'),
                include('string-intp'),
                (r'[\\#' + lbrace + rbrace + ']', String.Regex),
                (r'[^\\#' + lbrace + rbrace + ']+', String.Regex),
            ]
            states['strings'].append((r'%r' + lbrace, String.Regex,
                                      name+'-regex'))

        # these must come after %<brace>!
        states['strings'] += [
            # %r regex
            (r'(%r(.))(.*?)(\2[mixounse]*)', intp_regex_callback),
            # regular fancy strings
            (r'%[qsw](.).*?\1', String.Other),
            (r'(%[QWx](.))(.*?)(\2)', intp_string_callback),
            # special forms of fancy strings after operators or
            # in method calls with braces
            (r'(?<=[-+/*%=<>&!^|~,(])(\s*)(%([\t ]).*?\3)',
             bygroups(Text, String.Other, None)),
            # and because of fixed with lookbehinds the whole thing a
            # second time for line startings...
            (r'^(\s*)(%([\t ]).*?\3)',
             bygroups(Text, String.Other, None)),
            # all regular fancy strings
            (r'(%([^a-zA-Z0-9\s]))(.*?)(\2)', intp_string_callback),
        ]

        return states

    tokens = {
        'root': [
            (r'#.*?$', Comment.Single),
            (r'=begin\s.*?\n=end', Comment.Multiline),
            # keywords
            (r'(BEGIN|END|alias|begin|break|case|defined\?|'
             r'do|else|elsif|end|ensure|for|if|in|next|redo|'
             r'rescue|raise|retry|return|super|then|undef|unless|until|when|'
             r'while|yield)\b', Keyword),
            # start of function, class and module names
            (r'(module)(\s+)([a-zA-Z_][a-zA-Z0-9_]*(::[a-zA-Z_][a-zA-Z0-9_]*)*)',
             bygroups(Keyword, Text, Name.Namespace)),
            (r'(def)(\s+)', bygroups(Keyword, Text), 'funcname'),
            (r'def(?=[*%&^`~+-/\[<>=])', Keyword, 'funcname'),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),
            # special methods
            (r'(initialize|new|loop|include|extend|raise|attr_reader|'
             r'attr_writer|attr_accessor|attr|catch|throw|private|'
             r'module_function|public|protected|true|false|nil)\b', Keyword.Pseudo),
            (r'(not|and|or)\b', Operator.Word),
            (r'(autoload|block_given|const_defined|eql|equal|frozen|include|'
             r'instance_of|is_a|iterator|kind_of|method_defined|nil|'
             r'private_method_defined|protected_method_defined|'
             r'public_method_defined|respond_to|tainted)\?', Name.Builtin),
            (r'(chomp|chop|exit|gsub|sub)!', Name.Builtin),
            (r'(?<!\.)(Array|Float|Integer|String|__id__|__send__|abort|ancestors|'
             r'at_exit|autoload|binding|callcc|caller|'
             r'catch|chomp|chop|class_eval|class_variables|'
             r'clone|const_defined\?|const_get|const_missing|const_set|constants|'
             r'display|dup|eval|exec|exit|extend|fail|fork|'
             r'format|freeze|getc|gets|global_variables|gsub|'
             r'hash|id|included_modules|inspect|instance_eval|'
             r'instance_method|instance_methods|'
             r'instance_variable_get|instance_variable_set|instance_variables|'
             r'lambda|load|local_variables|loop|'
             r'method|method_missing|methods|module_eval|name|'
             r'object_id|open|p|print|printf|private_class_method|'
             r'private_instance_methods|'
             r'private_methods|proc|protected_instance_methods|'
             r'protected_methods|public_class_method|'
             r'public_instance_methods|public_methods|'
             r'putc|puts|raise|rand|readline|readlines|require|'
             r'scan|select|self|send|set_trace_func|singleton_methods|sleep|'
             r'split|sprintf|srand|sub|syscall|system|taint|'
             r'test|throw|to_a|to_s|trace_var|trap|type|untaint|untrace_var|'
             r'warn)\b', Name.Builtin),
            (r'__(FILE|LINE)__\b', Name.Builtin.Pseudo),
            # normal heredocs
            (r'(?<!\w)(<<-?)(["`\']?)([a-zA-Z_]\w*)(\2)(.*?\n)', heredoc_callback),
            # empty string heredocs
            (r'(<<-?)("|\')()(\2)(.*?\n)', heredoc_callback),
            (r'__END__', Comment.Preproc, 'end-part'),
            # multiline regex (after keywords or assignemnts)
            (r'(?:^|(?<=[=<>~!])|'
                 r'(?<=(?:\s|;)when\s)|'
                 r'(?<=(?:\s|;)or\s)|'
                 r'(?<=(?:\s|;)and\s)|'
                 r'(?<=(?:\s|;|\.)index\s)|'
                 r'(?<=(?:\s|;|\.)scan\s)|'
                 r'(?<=(?:\s|;|\.)sub\s)|'
                 r'(?<=(?:\s|;|\.)sub!\s)|'
                 r'(?<=(?:\s|;|\.)gsub\s)|'
                 r'(?<=(?:\s|;|\.)gsub!\s)|'
                 r'(?<=(?:\s|;|\.)match\s)|'
                 r'(?<=(?:\s|;)if\s)|'
                 r'(?<=(?:\s|;)elsif\s)|'
                 r'(?<=^when\s)|'
                 r'(?<=^index\s)|'
                 r'(?<=^scan\s)|'
                 r'(?<=^sub\s)|'
                 r'(?<=^gsub\s)|'
                 r'(?<=^sub!\s)|'
                 r'(?<=^gsub!\s)|'
                 r'(?<=^match\s)|'
                 r'(?<=^if\s)|'
                 r'(?<=^elsif\s)'
             r')(\s*)(/)(?!=)', bygroups(Text, String.Regex), 'multiline-regex'),
            # multiline regex (in method calls)
            (r'(?<=\(|,)/', String.Regex, 'multiline-regex'),
            # multiline regex (this time the funny no whitespace rule)
            (r'(\s+)(/[^\s=])', String.Regex, 'multiline-regex'),
            # lex numbers and ignore following regular expressions which
            # are division operators in fact (grrrr. i hate that. any
            # better ideas?)
            # since pygments 0.7 we also eat a "?" operator after numbers
            # so that the char operator does not work. Chars are not allowed
            # there so that you can use the terner operator.
            # stupid example:
            #   x>=0?n[x]:""
            (r'(0_?[0-7]+(?:_[0-7]+)*)(\s*)([/?])?',
             bygroups(Number.Oct, Text, Operator)),
            (r'(0x[0-9A-Fa-f]+(?:_[0-9A-Fa-f]+)*)(\s*)([/?])?',
             bygroups(Number.Hex, Text, Operator)),
            (r'(0b[01]+(?:_[01]+)*)(\s*)([/?])?',
             bygroups(Number.Bin, Text, Operator)),
            (r'([\d]+(?:_\d+)*)(\s*)([/?])?',
             bygroups(Number.Integer, Text, Operator)),
            # Names
            (r'@@[a-zA-Z_][a-zA-Z0-9_]*', Name.Variable.Class),
            (r'@[a-zA-Z_][a-zA-Z0-9_]*', Name.Variable.Instance),
            (r'\$[a-zA-Z0-9_]+', Name.Variable.Global),
            (r'\$[!@&`\'+~=/\\,;.<>_*$?:"]', Name.Variable.Global),
            (r'\$-[0adFiIlpvw]', Name.Variable.Global),
            (r'::', Operator),
            include('strings'),
            # chars
            (r'\?(\\[MC]-)*' # modifiers
             r'(\\([\\abefnrstv#"\']|x[a-fA-F0-9]{1,2}|[0-7]{1,3})|\S)'
             r'(?!\w)',
             String.Char),
            (r'[A-Z][a-zA-Z0-9_]+', Name.Constant),
            # this is needed because ruby attributes can look
            # like keywords (class) or like this: ` ?!?
            (r'(\.|::)([a-zA-Z_]\w*[\!\?]?|[*%&^`~+-/\[<>=])',
             bygroups(Operator, Name)),
            (r'[a-zA-Z_][\w_]*[\!\?]?', Name),
            (r'(\[|\]|\*\*|<<?|>>?|>=|<=|<=>|=~|={3}|'
             r'!~|&&?|\|\||\.{1,3})', Operator),
            (r'[-+/*%=<>&!^|~]=?', Operator),
            (r'[(){};,/?:\\]', Punctuation),
            (r'\s+', Text)
        ],
        'funcname': [
            (r'\(', Punctuation, 'defexpr'),
            (r'(?:([a-zA-Z_][a-zA-Z0-9_]*)(\.))?'
             r'([a-zA-Z_][\w_]*[\!\?]?|\*\*?|[-+]@?|'
             r'[/%&|^`~]|\[\]=?|<<|>>|<=?>|>=?|===?)',
             bygroups(Name.Class, Operator, Name.Function), '#pop'),
            (r'', Text, '#pop')
        ],
        'classname': [
            (r'\(', Punctuation, 'defexpr'),
            (r'<<', Operator, '#pop'),
            (r'[A-Z_][\w_]*', Name.Class, '#pop'),
            (r'', Text, '#pop')
        ],
        'defexpr': [
            (r'(\))(\.|::)?', bygroups(Punctuation, Operator), '#pop'),
            (r'\(', Operator, '#push'),
            include('root')
        ],
        'in-intp': [
            ('}', String.Interpol, '#pop'),
            include('root'),
        ],
        'string-intp': [
            (r'#{', String.Interpol, 'in-intp'),
            (r'#@@?[a-zA-Z_][a-zA-Z0-9_]*', String.Interpol),
            (r'#\$[a-zA-Z_][a-zA-Z0-9_]*', String.Interpol)
        ],
        'string-intp-escaped': [
            include('string-intp'),
            (r'\\([\\abefnrstv#"\']|x[a-fA-F0-9]{1,2}|[0-7]{1,3})', String.Escape)
        ],
        'interpolated-regex': [
            include('string-intp'),
            (r'[\\#]', String.Regex),
            (r'[^\\#]+', String.Regex),
        ],
        'interpolated-string': [
            include('string-intp'),
            (r'[\\#]', String.Other),
            (r'[^\\#]+', String.Other),
        ],
        'multiline-regex': [
            include('string-intp'),
            (r'\\/', String.Regex),
            (r'[\\#]', String.Regex),
            (r'[^\\/#]+', String.Regex),
            (r'/[mixounse]*', String.Regex, '#pop'),
        ],
        'end-part': [
            (r'.+', Comment.Preproc, '#pop')
        ]
    }
    tokens.update(gen_rubystrings_rules())

    def analyse_text(text):
        return shebang_matches(text, r'ruby(1\.\d)?')


class RubyConsoleLexer(Lexer):
    """
    For Ruby interactive console (**irb**) output like:

    .. sourcecode:: rbcon

        irb(main):001:0> a = 1
        => 1
        irb(main):002:0> puts a
        1
        => nil
    """
    name = 'Ruby irb session'
    aliases = ['rbcon', 'irb']
    mimetypes = ['text/x-ruby-shellsession']

    _prompt_re = re.compile('irb\([a-zA-Z_][a-zA-Z0-9_]*\):\d{3}:\d+[>*] ')

    def get_tokens_unprocessed(self, text):
        rblexer = RubyLexer(**self.options)

        curcode = ''
        insertions = []
        for match in line_re.finditer(text):
            line = match.group()
            m = self._prompt_re.match(line)
            if m is not None:
                end = m.end()
                insertions.append((len(curcode),
                                   [(0, Generic.Prompt, line[:end])]))
                curcode += line[end:]
            else:
                if curcode:
                    for item in do_insertions(insertions,
                                    rblexer.get_tokens_unprocessed(curcode)):
                        yield item
                    curcode = ''
                    insertions = []
                yield match.start(), Generic.Output, line
        if curcode:
            for item in do_insertions(insertions,
                                      rblexer.get_tokens_unprocessed(curcode)):
                yield item


class PerlLexer(RegexLexer):
    """
    For `Perl <http://www.perl.org>`_ source code.
    """

    name = 'Perl'
    aliases = ['perl', 'pl']
    filenames = ['*.pl', '*.pm']
    mimetypes = ['text/x-perl', 'application/x-perl']

    flags = re.DOTALL | re.MULTILINE
    # TODO: give this a perl guy who knows how to parse perl...
    tokens = {
        'root': [
            (r'\#.*?$', Comment.Single),
            (r'=[a-zA-Z0-9]+\s+.*?\n[.\n]*?\n\s*=cut', Comment.Multiline),
            (r'(case|continue|do|else|elsif|for|foreach|if|last|my|'
             r'next|our|redo|reset|then|unless|until|while|use|'
             r'print|new|BEGIN|END|return)\b', Keyword),
            (r'(eq|lt|gt|le|ge|ne|not|and|or|cmp)\b', Operator.Word),
            (r's/(\\\\|\\/|[^/])*/(\\\\|\\/|[^/])*/[egimosx]*', String.Regex),
            (r'm?/(\\\\|\\/|[^/\n])*/[gcimosx]*', String.Regex),
            (r'((?<==~)|(?<=\())\s*/(\\\\|\\/|[^/])*/[gcimosx]*', String.Regex),
            (r'\s+', Text),
            (r'(abs|accept|alarm|atan2|bind|binmode|bless|caller|chdir|'
             r'chmod|chomp|chop|chown|chr|chroot|close|closedir|connect|'
             r'continue|cos|crypt|dbmclose|dbmopen|defined|delete|die|'
             r'dump|each|endgrent|endhostent|endnetent|endprotoent|'
             r'endpwent|endservent|eof|eval|exec|exists|exit|exp|fcntl|'
             r'fileno|flock|fork|format|formline|getc|getgrent|getgrgid|'
             r'getgrnam|gethostbyaddr|gethostbyname|gethostent|getlogin|'
             r'getnetbyaddr|getnetbyname|getnetent|getpeername|getpgrp|'
             r'getppid|getpriority|getprotobyname|getprotobynumber|'
             r'getprotoent|getpwent|getpwnam|getpwuid|getservbyname|'
             r'getservbyport|getservent|getsockname|getsockopt|glob|gmtime|'
             r'goto|grep|hex|import|index|int|ioctl|join|keys|kill|last|'
             r'lc|lcfirst|length|link|listen|local|localtime|log|lstat|'
             r'map|mkdir|msgctl|msgget|msgrcv|msgsnd|my|next|no|oct|open|'
             r'opendir|ord|our|pack|package|pipe|pop|pos|printf|'
             r'prototype|push|quotemeta|rand|read|readdir|'
             r'readline|readlink|readpipe|recv|redo|ref|rename|require|'
             r'reverse|rewinddir|rindex|rmdir|scalar|seek|seekdir|'
             r'select|semctl|semget|semop|send|setgrent|sethostent|setnetent|'
             r'setpgrp|setpriority|setprotoent|setpwent|setservent|'
             r'setsockopt|shift|shmctl|shmget|shmread|shmwrite|shutdown|'
             r'sin|sleep|socket|socketpair|sort|splice|split|sprintf|sqrt|'
             r'srand|stat|study|substr|symlink|syscall|sysopen|sysread|'
             r'sysseek|system|syswrite|tell|telldir|tie|tied|time|times|tr|'
             r'truncate|uc|ucfirst|umask|undef|unlink|unpack|unshift|untie|'
             r'utime|values|vec|wait|waitpid|wantarray|warn|write'
             r')\b', Name.Builtin),
            (r'((__(DATA|DIE|WARN)__)|(STD(IN|OUT|ERR)))\b', Name.Builtin.Pseudo),
            (r'<<([a-zA-Z_][a-zA-Z0-9_]*)\n.*?\n\1\n', String),
            (r'__END__', Comment.Preproc, 'end-part'),
            (r'\$\^[ADEFHILMOPSTWX]', Name.Variable.Global),
            (r"\$[\\\"\[\]'&`+*.,;=%~?@$!<>(^|/-](?!\w)", Name.Variable.Global),
            (r'[$@%#]+', Name.Variable, 'varname'),
            (r'0_?[0-7]+(_[0-7]+)*', Number.Oct),
            (r'0x[0-9A-Fa-f]+(_[0-9A-Fa-f]+)*', Number.Hex),
            (r'0b[01]+(_[01]+)*', Number.Bin),
            (r'\d+', Number.Integer),
            (r"'(\\\\|\\'|[^'])*'", String),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r'`(\\\\|\\`|[^`])*`', String.Backtick),
            (r'(q|qq|qw|qr|qx)\{', String.Other, 'cb-string'),
            (r'(q|qq|qw|qr|qx)\(', String.Other, 'rb-string'),
            (r'(q|qq|qw|qr|qx)\[', String.Other, 'sb-string'),
            (r'(q|qq|qw|qr|qx)\<', String.Other, 'lt-string'),
            (r'(q|qq|qw|qr|qx)(.)[.\n]*?\1', String.Other),
            (r'package\s+', Keyword, 'modulename'),
            (r'sub\s+', Keyword, 'funcname'),
            (r'(\[\]|\*\*|::|<<|>>|>=|<=|<=>|={3}|!=|=~|'
             r'!~|&&?|\|\||\.{1,3})', Operator),
            (r'[-+/*%=<>&^|!\\~]=?', Operator),
            (r'[\(\)\[\]:;,<>/\?\{\}]', Punctuation), # yes, there's no shortage
                                                      # of punctuation in Perl!
            (r'(?=\w)', Name, 'name'),
        ],
        'varname': [
            (r'\s+', Text),
            (r'\{', Punctuation, '#pop'), # hash syntax?
            (r'\)|,', Punctuation, '#pop'), # argument specifier
            (r'[a-zA-Z0-9_]+::', Name.Namespace),
            (r'[a-zA-Z0-9_:]+', Name.Variable, '#pop'),
        ],
        'name': [
            (r'[a-zA-Z0-9_]+::', Name.Namespace),
            (r'[a-zA-Z0-9_:]+', Name, '#pop'),
            (r'[A-Z_]+(?=[^a-zA-Z0-9_])', Name.Constant, '#pop'),
            (r'(?=[^a-zA-Z0-9_])', Text, '#pop'),
        ],
        'modulename': [
            (r'[a-zA-Z_][\w_]*', Name.Namespace, '#pop')
        ],
        'funcname': [
            (r'[a-zA-Z_][\w_]*[\!\?]?', Name.Function),
            (r'\s+', Text),
            # argument declaration
            (r'(\([$@%]*\))(\s*)', bygroups(Punctuation, Text)),
            (r'.*?{', Punctuation, '#pop'),
            (r';', Punctuation, '#pop'),
        ],
        'cb-string': [
            (r'\\[\{\}\\]', String.Other),
            (r'\\', String.Other),
            (r'\{', String.Other, 'cb-string'),
            (r'\}', String.Other, '#pop'),
            (r'[^\{\}\\]+', String.Other)
        ],
        'rb-string': [
            (r'\\[\(\)\\]', String.Other),
            (r'\\', String.Other),
            (r'\(', String.Other, 'rb-string'),
            (r'\)', String.Other, '#pop'),
            (r'[^\(\)]+', String.Other)
        ],
        'sb-string': [
            (r'\\[\[\]\\]', String.Other),
            (r'\\', String.Other),
            (r'\[', String.Other, 'sb-string'),
            (r'\]', String.Other, '#pop'),
            (r'[^\[\]]+', String.Other)
        ],
        'lt-string': [
            (r'\\[\<\>\\]', String.Other),
            (r'\\', String.Other),
            (r'\<', String.Other, 'lt-string'),
            (r'\>', String.Other, '#pop'),
            (r'[^\<\>]]+', String.Other)
        ],
        'end-part': [
            (r'.+', Comment.Preproc, '#pop')
        ]
    }

    def analyse_text(text):
        return shebang_matches(text, r'perl(\d\.\d\.\d)?')


class LuaLexer(RegexLexer):
    """
    For `Lua <http://www.lua.org>`_ source code.

    Additional options accepted:

    `func_name_highlighting`
        If given and ``True``, highlight builtin function names
        (default: ``True``).
    `disabled_modules`
        If given, must be a list of module names whose function names
        should not be highlighted. By default all modules are highlighted.

        To get a list of allowed modules have a look into the
        `_luabuiltins` module:

        .. sourcecode:: pycon

            >>> from pygments.lexers._luabuiltins import MODULES
            >>> MODULES.keys()
            ['string', 'coroutine', 'modules', 'io', 'basic', ...]
    """

    name = 'Lua'
    aliases = ['lua']
    filenames = ['*.lua']
    mimetypes = ['text/x-lua', 'application/x-lua']

    tokens = {
        'root': [
            (r'(?s)--\[(=*)\[.*?\]\1\]', Comment.Multiline),
            ('--.*$', Comment.Single),

            (r'(?i)(\d*\.\d+|\d+\.\d*)(e[+-]?\d+)?', Number.Float),
            (r'(?i)\d+e[+-]?\d+', Number.Float),
            ('(?i)0x[0-9a-f]*', Number.Hex),
            (r'\d+', Number.Integer),

            (r'\n', Text),
            (r'[^\S\n]', Text),
            (r'[\[\]\{\}\(\)\.,:;]', Punctuation),

            (r'(==|~=|<=|>=|\.\.|\.\.\.|[=+\-*/%^<>#])', Operator),
            (r'(and|or|not)\b', Operator.Word),

            ('(break|do|else|elseif|end|for|if|in|repeat|return|then|until|'
             r'while)\b', Keyword),
            (r'(local)\b', Keyword.Declaration),
            (r'(true|false|nil)\b', Keyword.Constant),

            (r'(function)(\s+)', bygroups(Keyword, Text), 'funcname'),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),

            (r'[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)?', Name),

            # multiline strings
            (r'(?s)\[(=*)\[(.*?)\]\1\]', String),
            ("'", String.Single, combined('stringescape', 'sqs')),
            ('"', String.Double, combined('stringescape', 'dqs'))
        ],

        'funcname': [
            ('[A-Za-z_][A-Za-z0-9_]*', Name.Function, '#pop'),
            # inline function
            ('\(', Punctuation, '#pop'),
        ],

        'classname': [
            ('[A-Za-z_][A-Za-z0-9_]*', Name.Class, '#pop')
        ],

        # if I understand correctly, every character is valid in a lua string,
        # so this state is only for later corrections
        'string': [
            ('.', String)
        ],

        'stringescape': [
            (r'''\\([abfnrtv\\"']|\d{1,3})''', String.Escape)
        ],

        'sqs': [
            ("'", String, '#pop'),
            include('string')
        ],

        'dqs': [
            ('"', String, '#pop'),
            include('string')
        ]
    }

    def __init__(self, **options):
        self.func_name_highlighting = get_bool_opt(
            options, 'func_name_highlighting', True)
        self.disabled_modules = get_list_opt(options, 'disabled_modules', [])

        self._functions = set()
        if self.func_name_highlighting:
            from pygments.lexers._luabuiltins import MODULES
            for mod, func in MODULES.iteritems():
                if mod not in self.disabled_modules:
                    self._functions.update(func)
        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
            RegexLexer.get_tokens_unprocessed(self, text):
            if token is Name:
                if value in self._functions:
                    yield index, Name.Function, value
                    continue
                elif '.' in value:
                    a, b = value.split('.')
                    yield index, Name, a
                    yield index + len(a), Punctuation, u'.'
                    yield index + len(a) + 1, Name, b
                    continue
            yield index, token, value


class MiniDLexer(RegexLexer):
    """
    For `MiniD <http://www.dsource.org/projects/minid>`_ (a D-like scripting
    language) source.
    """
    name = 'MiniD'
    filenames = ['*.md']
    aliases = ['minid']
    mimetypes = ['text/x-minidsrc']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text),
            # Comments
            (r'//(.*?)\n', Comment),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment),
            (r'/\+', Comment, 'nestedcomment'),
            # Keywords
            (r'(as|break|case|class|catch|continue|coroutine|default'
             r'|do|else|finally|for|foreach|function|global|namespace'
             r'|if|import|in|is|local|module|return|super|switch'
             r'|this|throw|try|vararg|while|with|yield)\b', Keyword),
            (r'(false|true|null)\b', Keyword.Constant),
            # FloatLiteral
            (r'([0-9][0-9_]*)?\.[0-9_]+([eE][+\-]?[0-9_]+)?', Number.Float),
            # IntegerLiteral
            # -- Binary
            (r'0[Bb][01_]+', Number),
            # -- Octal
            (r'0[Cc][0-7_]+', Number.Oct),
            # -- Hexadecimal
            (r'0[xX][0-9a-fA-F_]+', Number.Hex),
            # -- Decimal
            (r'(0|[1-9][0-9_]*)', Number.Integer),
            # CharacterLiteral
            (r"""'(\\['"?\\abfnrtv]|\\x[0-9a-fA-F]{2}|\\[0-9]{1,3}"""
             r"""|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|.)'""",
             String.Char
            ),
            # StringLiteral
            # -- WysiwygString
            (r'@"[^"]*"', String),
            # -- AlternateWysiwygString
            (r'`[^`]*`', String),
            # -- DoubleQuotedString
            (r'"(\\\\|\\"|[^"])*"', String),
            # Tokens
            (
             r'(~=|\^=|%=|\*=|==|!=|>>>=|>>>|>>=|>>|>=|<=>|\?='
             r'|<<=|<<|<=|\+\+|\+=|--|-=|\|\||\|=|&&|&=|\.\.|/=)'
             r'|[-/.&|\+<>!()\[\]{}?,;:=*%^~#]', Punctuation
            ),
            # Identifier
            (r'[a-zA-Z_](\w|::)*', Name),
        ],
        'nestedcomment': [
            (r'[^+/]+', Comment),
            (r'/\+', Comment, '#push'),
            (r'\+/', Comment, '#pop'),
            (r'[+/]', Comment),
        ],
    }


class IoLexer(RegexLexer):
    """
    For `Io <http://iolanguage.com/>`_ (a small, prototype-based
    programming language) source.
    """
    name = 'Io'
    filenames = ['*.io']
    aliases = ['io']
    mimetypes = ['text/x-iosrc']
    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text),
            # Comments
            (r'//(.*?)\n', Comment),
            (r'#(.*?)\n', Comment),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment),
            (r'/\+', Comment, 'nestedcomment'),
            # DoubleQuotedString
            (r'"(\\\\|\\"|[^"])*"', String),
            # Operators
            (r':=|=|\(|\)|;|,|\*|-|\+|>|<|@|!|/|\||\^|\.|%|&|\[|\]|\{|\}', Operator),
            # keywords
            (r'(clone|do|doFile|doString|method|for|if|else|elseif|then)', Keyword),
            # constants
            (r'nil|false|true', Name.Constant),
            # names
            ('Object|list|List|Map|args|Sequence|Coroutine|File', Name.Builtin),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
            # numbers
            (r'(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'\d+', Number.Integer)
        ],
        'nestedcomment': [
            (r'[^+/]+', Comment),
            (r'/\+', Comment, '#push'),
            (r'\+/', Comment, '#pop'),
            (r'[+/]', Comment),
        ]
    }
