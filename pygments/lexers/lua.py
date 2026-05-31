"""
    pygments.lexers.lua
    ~~~~~~~~~~~~~~~~~~

    Lexers for the Lua language family.

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, bygroups, default, words, combined
from pygments.token import (
    Comment, Keyword, Name, Number, Operator,
    Punctuation, String, Text, Whitespace,
)
from pygments.lexers._lua_builtins import BUILTIN_ATTRIBUTES, BUILTIN_FUNCTIONS
from pygments.lexers._luajit_builtins import all_luajit_builtins
from pygments.lexers._luau_builtins import (
    LUAU_ATTRIBUTES, LUAU_FUNCTIONS, ROBLOX_ATTRIBUTES, ROBLOX_FUNCTIONS,
)

__all__ = ['LuaLexer', 'LuauLexer', 'LuaJITLexer', 'MoonScriptLexer', 'TealLexer', 'FennelLexer']


class LuaLexer(RegexLexer):
    """
    For Lua source code.

    Additional options accepted:

    `func_name_highlighting`
        If given and ``True``, highlight builtin function names
        (default: ``True``).
    `disabled_modules`
        If given, must be a list of module names whose function names
        should not be highlighted. By default all modules are highlighted.

        To get a list of allowed modules have a look into the
        `_lua_builtins` module:

        .. sourcecode:: pycon

            >>> from pygments.lexers._lua_builtins import MODULES
            >>> MODULES.keys()
            ['string', 'coroutine', 'modules', 'io', 'basic', ...]
    """

    name = 'Lua'
    url = 'https://www.lua.org/'
    aliases = ['lua']
    filenames = ['*.lua', '*.wlua']
    mimetypes = ['text/x-lua', 'application/x-lua']
    version_added = ''

    _comment_multiline = r'(?:--\[(?P<level>=*)\[[\w\W]*?\](?P=level)\])'
    _comment_single = r'(?:--.*$)'
    _space = r'(?:\s+(?!\s))'
    _s = rf'(?:{_comment_multiline}|{_comment_single}|{_space})'
    # A lookahead-safe version of _s that avoids catastrophic backtracking.
    # The _comment_multiline pattern contains [\w\W]*? which, when used
    # inside a lookahead with a * quantifier, causes exponential blowup.
    # This version skips only whitespace; comments between an identifier
    # and a following [.:] or ( are rare enough to sacrifice.
    _s_la = r'\s'
    _name = r'(?:[^\W\d]\w*)'

    tokens = {
        'root': [
            # Lua allows a file to start with a shebang.
            (r'#!.*', Comment.Preproc),
            default('base'),
        ],
        'ws': [
            (_comment_multiline, Comment.Multiline),
            (_comment_single, Comment.Single),
            (_space, Whitespace),
        ],
        'base': [
            include('ws'),

            (r'(?i)0x[\da-f]*(\.[\da-f]*)?(p[+-]?\d+)?', Number.Hex),
            (r'(?i)(\d*\.\d+|\d+\.\d*)(e[+-]?\d+)?', Number.Float),
            (r'(?i)\d+e[+-]?\d+', Number.Float),
            (r'\d+', Number.Integer),

            # multiline strings
            (r'(?s)\[(=*)\[.*?\]\1\]', String),

            (r'::', Punctuation, 'label'),
            (r'\.{3}', Punctuation),
            (r'[=<>|~&+\-*/%#^]+|\.\.', Operator),
            (r'[\[\]{}().,:;]+', Punctuation),
            (r'(and|or|not)\b', Operator.Word),

            (words([
                'break', 'do', 'else', 'elseif', 'end', 'for', 'if', 'in',
                'repeat', 'return', 'then', 'until', 'while'
            ], suffix=r'\b'), Keyword.Reserved),
            (r'goto\b', Keyword.Reserved, 'goto'),
            (r'(local)\b', Keyword.Declaration),
            (r'(true|false|nil)\b', Keyword.Constant),

            (r'(function)\b', Keyword.Reserved, 'funcname'),

            (words(all_lua_builtins(), suffix=r"\b"), Name.Builtin),
            (fr'[A-Za-z_]\w*(?={_s_la}*[.:])', Name.Variable, 'varname'),
            (fr'[A-Za-z_]\w*(?={_s_la}*\()', Name.Function),
            (r'[A-Za-z_]\w*', Name.Variable),

            ("'", String.Single, combined('stringescape', 'sqs')),
            ('"', String.Double, combined('stringescape', 'dqs'))
        ],

        'varname': [
            include('ws'),
            (r'\.\.', Operator, '#pop'),
            (r'[.:]', Punctuation),
            (rf'{_name}(?={_s_la}*[.:])', Name.Property),
            (rf'{_name}(?={_s_la}*\()', Name.Function, '#pop'),
            (_name, Name.Property, '#pop'),
        ],

        'funcname': [
            include('ws'),
            (r'[.:]', Punctuation),
            (rf'{_name}(?={_s_la}*[.:])', Name.Class),
            (_name, Name.Function, '#pop'),
            # inline function
            (r'\(', Punctuation, '#pop'),
        ],

        'goto': [
            include('ws'),
            (_name, Name.Label, '#pop'),
        ],

        'label': [
            include('ws'),
            (r'::', Punctuation, '#pop'),
            (_name, Name.Label),
        ],

        'stringescape': [
            (r'\\([abfnrtv\\"\']|[\r\n]{1,2}|z\s*|x[0-9a-fA-F]{2}|\d{1,3}|'
             r'u\{[0-9a-fA-F]+\})', String.Escape),
        ],

        'sqs': [
            (r"'", String.Single, '#pop'),
            (r"[^\\']+", String.Single),
        ],

        'dqs': [
            (r'"', String.Double, '#pop'),
            (r'[^\\"]+', String.Double),
        ]
    }

    def __init__(self, **options):
        self.func_name_highlighting = get_bool_opt(
            options, 'func_name_highlighting', True)
        self.disabled_modules = get_list_opt(options, 'disabled_modules', [])

        self._functions = set()
        if self.func_name_highlighting:
            from pygments.lexers._lua_builtins import MODULES
            for mod, func in MODULES.items():
                if mod not in self.disabled_modules:
                    self._functions.update(func)
        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
                RegexLexer.get_tokens_unprocessed(self, text):
            if token is Name.Builtin and value not in self._functions:
                if '.' in value:
                    a, b = value.split('.')
                    yield index, Name, a
                    yield index + len(a), Punctuation, '.'
                    yield index + len(a) + 1, Name, b
                else:
                    yield index, Name, value
                continue
            yield index, token, value

def _luau_make_expression(should_pop, _s, _s_la):
    temp_list = [
        (r'0[xX][\da-fA-F_]*', Number.Hex, '#pop'),
        (r'0[bB][\d_]*', Number.Bin, '#pop'),
        (r'\.?\d[\d_]*(?:\.[\d_]*)?(?:[eE][+-]?[\d_]+)?', Number.Float, '#pop'),

        (words((
            'true', 'false', 'nil'
        ), suffix=r'\b'), Keyword.Constant, '#pop'),

        (r'\[(=*)\[[.\n]*?\]\1\]', String, '#pop'),

        (r'(\.)([a-zA-Z_]\w*)(?=%s*[({"\'])', bygroups(Punctuation, Name.Function), '#pop'),
        (r'(\.)([a-zA-Z_]\w*)', bygroups(Punctuation, Name.Variable), '#pop'),

        (rf'[a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*(?={_s_la}*[({{"\'])', Name.Other, '#pop'),
        (r'[a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*', Name, '#pop'),
    ]
    if should_pop:
        return temp_list
    return [entry[:2] for entry in temp_list]

def _luau_make_expression_special(should_pop):
    temp_list = [
        (r'\{', Punctuation, ('#pop', 'closing_brace_base', 'expression')),
        (r'\(', Punctuation, ('#pop', 'closing_parenthesis_base', 'expression')),

        (r'::?', Punctuation, ('#pop', 'type_end', 'type_start')),

        (r"'", String.Single, ('#pop', 'string_single')),
        (r'"', String.Double, ('#pop', 'string_double')),
        (r'`', String.Backtick, ('#pop', 'string_interpolated')),
    ]
    if should_pop:
        return temp_list
    return [(entry[0], entry[1], entry[2][1:]) for entry in temp_list]



def _luau_make_expression(should_pop, _s, _s_la):
    temp_list = [
        (r'0[xX][\da-fA-F_]*', Number.Hex, '#pop'),
        (r'0[bB][\d_]*', Number.Bin, '#pop'),
        (r'\.?\d[\d_]*(?:\.[\d_]*)?(?:[eE][+-]?[\d_]+)?', Number.Float, '#pop'),

        (words((
            'true', 'false', 'nil'
        ), suffix=r'\b'), Keyword.Constant, '#pop'),

        (r'\[(=*)\[[.\n]*?\]\1\]', String, '#pop'),

        (r'(\.)([a-zA-Z_]\w*)(?=%s*[({"\'])', bygroups(Punctuation, Name.Function), '#pop'),
        (r'(\.)([a-zA-Z_]\w*)', bygroups(Punctuation, Name.Variable), '#pop'),

        (rf'[a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*(?={_s_la}*[({{"\'])', Name.Other, '#pop'),
        (r'[a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*', Name, '#pop'),
    ]
    if should_pop:
        return temp_list
    return [entry[:2] for entry in temp_list]

def _luau_make_expression_special(should_pop):
    temp_list = [
        (r'\{', Punctuation, ('#pop', 'closing_brace_base', 'expression')),
        (r'\(', Punctuation, ('#pop', 'closing_parenthesis_base', 'expression')),

        (r'::?', Punctuation, ('#pop', 'type_end', 'type_start')),

        (r"'", String.Single, ('#pop', 'string_single')),
        (r'"', String.Double, ('#pop', 'string_double')),
        (r'`', String.Backtick, ('#pop', 'string_interpolated')),
    ]
    if should_pop:
        return temp_list
    return [(entry[0], entry[1], entry[2][1:]) for entry in temp_list]


class LuauLexer(RegexLexer):
    """
    For Luau source code.

    Additional options accepted:

    `include_luau_builtins`
        If given and ``True``, automatically highlight Luau builtins
        (default: ``True``).
    `include_roblox_builtins`
        If given and ``True``, automatically highlight Roblox-specific builtins
        (default: ``False``).
    `additional_builtins`
        If given, must be a list of additional builtins to highlight.
    `disabled_builtins`
        If given, must be a list of builtins that will not be highlighted.
    """

    name = 'Luau'
    url = 'https://luau-lang.org/'
    aliases = ['luau']
    filenames = ['*.luau']
    version_added = '2.18'

    _comment_multiline = r'(?:--\[(?P<level>=*)\[[\w\W]*?\](?P=level)\])'
    _comment_single = r'(?:--.*$)'
    _s = r'(?:{}|{}|{})'.format(_comment_multiline, _comment_single, r'\s+')
    # Lookahead-safe version — avoids catastrophic backtracking from
    # [\w\W]*? inside _comment_multiline when combined with * quantifier.
    _s_la = r'\s'

    tokens = {
        'root': [
            (r'#!.*', Comment.Hashbang, 'base'),
            default('base'),
        ],

        'ws': [
            (_comment_multiline, Comment.Multiline),
            (_comment_single, Comment.Single),
            (r'\s+', Whitespace),
        ],

        'base': [
            include('ws'),

            *_luau_make_expression_special(False),
            (r'\.\.\.', Punctuation),

            (rf'type\b(?={_s}+[a-zA-Z_])', Keyword.Reserved, 'type_declaration'),
            (rf'export\b(?={_s}+[a-zA-Z_])', Keyword.Reserved),

            (r'(?:\.\.|//|[+\-*\/%^<>=])=?', Operator, 'expression'),
            (r'~=', Operator, 'expression'),

            (words((
                'and', 'or', 'not'
            ), suffix=r'\b'), Operator.Word, 'expression'),

            (words((
                'elseif', 'for', 'if', 'in', 'repeat', 'return', 'until',
                'while'), suffix=r'\b'), Keyword.Reserved, 'expression'),
            (r'local\b', Keyword.Declaration, 'expression'),

            (r'function\b', Keyword.Reserved, ('expression', 'func_name')),

            (r'[\])};]+', Punctuation),

            include('expression_static'),
            *_luau_make_expression(False, _s, _s_la),

            (r'[\[.,]', Punctuation, 'expression'),
        ],
        'expression_static': [
            (words((
                'break', 'continue', 'do', 'else', 'elseif', 'end', 'for',
                'if', 'in', 'repeat', 'return', 'then', 'until', 'while'),
                suffix=r'\b'), Keyword.Reserved),
        ],
        'expression': [
            include('ws'),

            (r'if\b', Keyword.Reserved, ('ternary', 'expression')),

            (r'local\b', Keyword.Declaration),
            *_luau_make_expression_special(True),
            (r'\.\.\.', Punctuation, '#pop'),

            (r'function\b', Keyword.Reserved, 'func_name'),

            include('expression_static'),
            *_luau_make_expression(True, _s, _s_la),

            default('#pop'),
        ],
        'ternary': [
            include('ws'),

            (r'else\b', Keyword.Reserved, '#pop'),
            (words((
                'then', 'elseif',
            ), suffix=r'\b'), Operator.Reserved, 'expression'),

            default('#pop'),
        ],

        'closing_brace_pop': [
            (r'\}', Punctuation, '#pop'),
        ],
        'closing_parenthesis_pop': [
            (r'\)', Punctuation, '#pop'),
        ],
        'closing_gt_pop': [
            (r'>', Punctuation, '#pop'),
        ],

        'closing_parenthesis_base': [
            include('closing_parenthesis_pop'),
            include('base'),
        ],
        'closing_parenthesis_type': [
            include('closing_parenthesis_pop'),
            include('type'),
        ],
        'closing_brace_base': [
            include('closing_brace_pop'),
            include('base'),
        ],
        'closing_brace_type': [
            include('closing_brace_pop'),
            include('type'),
        ],
        'closing_gt_type': [
            include('closing_gt_pop'),
            include('type'),
        ],

        'string_escape': [
            (r'\\z\s*', String.Escape),
            (r'\\(?:[abfnrtvz\\"\'`\{\n])|[\r\n]{1,2}|x[\da-fA-F]{2}|\d{1,3}|'
             r'u\{\}[\da-fA-F]*\}', String.Escape),
        ],
        'string_single': [
            include('string_escape'),

            (r"'", String.Single, "#pop"),
            (r"[^\\']+", String.Single),
        ],
        'string_double': [
            include('string_escape'),

            (r'"', String.Double, "#pop"),
            (r'[^\\"]+', String.Double),
        ],
        'string_interpolated': [
            include('string_escape'),

            (r'\{', Punctuation, ('closing_brace_base', 'expression')),

            (r'`', String.Backtick, "#pop"),
            (r'[^\\`\{]+', String.Backtick),
        ],

        'func_name': [
            include('ws'),

            (r'[.:]', Punctuation),
            (rf'[a-zA-Z_]\w*(?={_s_la}*[.:])', Name.Class),
            (r'[a-zA-Z_]\w*', Name.Function),

            (r'<', Punctuation, 'closing_gt_type'),

            (r'\(', Punctuation, '#pop'),
        ],

        'type': [
            include('ws'),

            (r'\(', Punctuation, 'closing_parenthesis_type'),
            (r'\{', Punctuation, 'closing_brace_type'),
            (r'<', Punctuation, 'closing_gt_type'),

            (r"'", String.Single, 'string_single'),
            (r'"', String.Double, 'string_double'),

            (r'[|&\.,\[\]:=]+', Punctuation),
            (r'->', Punctuation),

            (r'typeof\(', Name.Builtin, ('closing_parenthesis_base',
                                         'expression')),
            (r'[a-zA-Z_]\w*', Name.Class),
        ],
        'type_start': [
            include('ws'),

            (r'\(', Punctuation, ('#pop', 'closing_parenthesis_type')),
            (r'\{', Punctuation, ('#pop', 'closing_brace_type')),
            (r'<', Punctuation, ('#pop', 'closing_gt_type')),

            (r"'", String.Single, ('#pop', 'string_single')),
            (r'"', String.Double, ('#pop', 'string_double')),

            (r'typeof\(', Name.Builtin, ('#pop', 'closing_parenthesis_base',
                                         'expression')),
            (r'[a-zA-Z_]\w*', Name.Class, '#pop'),
        ],
        'type_end': [
            include('ws'),

            (r'[|&\.]', Punctuation, 'type_start'),
            (r'->', Punctuation, 'type_start'),

            (r'<', Punctuation, 'closing_gt_type'),

            default('#pop'),
        ],
        'type_declaration': [
            include('ws'),

            (r'[a-zA-Z_]\w*', Name.Class),
            (r'<', Punctuation, 'closing_gt_type'),

            (r'=', Punctuation, ('#pop', 'type_end', 'type_start')),
        ],
    }

    def __init__(self, **options):
        self.include_luau_builtins = get_bool_opt(
            options, 'include_luau_builtins', True)
        self.include_roblox_builtins = get_bool_opt(
            options, 'include_roblox_builtins', False)
        self.additional_builtins = get_list_opt(options, 'additional_builtins', [])
        self.disabled_builtins = get_list_opt(options, 'disabled_builtins', [])

        self._builtins = set(self.additional_builtins)
        if self.include_luau_builtins:
            from pygments.lexers._luau_builtins import LUAU_BUILTINS
            self._builtins.update(LUAU_BUILTINS)
        if self.include_roblox_builtins:
            from pygments.lexers._luau_builtins import ROBLOX_BUILTINS
            self._builtins.update(ROBLOX_BUILTINS)
        if self.additional_builtins:
            self._builtins.update(self.additional_builtins)
        self._builtins.difference_update(self.disabled_builtins)

        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
                RegexLexer.get_tokens_unprocessed(self, text):
            if token is Name or token is Name.Other:
                split_value = value.split('.')
                complete_value = []
                new_index = index
                for position in range(len(split_value), 0, -1):
                    potential_string = '.'.join(split_value[:position])
                    if potential_string in self._builtins:
                        yield index, Name.Builtin, potential_string
                        new_index += len(potential_string)

                        if complete_value:
                            yield new_index, Punctuation, '.'
                            new_index += 1
                        break
                    complete_value.insert(0, split_value[position - 1])

                for position, substring in enumerate(complete_value):
                    if position + 1 == len(complete_value):
                        if token is Name:
                            yield new_index, Name.Variable, substring
                            continue
                        yield new_index, Name.Function, substring
                        continue
                    yield new_index, Name.Variable, substring
                    new_index += len(substring)
                    yield new_index, Punctuation, '.'
                    new_index += 1

                continue
            yield index, token, value



class MoonScriptLexer(LuaLexer):
    """
    For MoonScript source code.
    """

    name = 'MoonScript'
    url = 'http://moonscript.org'
    aliases = ['moonscript', 'moon']
    filenames = ['*.moon']
    mimetypes = ['text/x-moonscript', 'application/x-moonscript']
    version_added = '1.5'

    tokens = {
        'root': [
            (r'#!(.*?)$', Comment.Preproc),
            default('base'),
        ],
        'base': [
            ('--.*$', Comment.Single),
            (r'(?i)(\d*\.\d+|\d+\.\d*)(e[+-]?\d+)?', Number.Float),
            (r'(?i)\d+e[+-]?\d+', Number.Float),
            (r'(?i)0x[0-9a-f]*', Number.Hex),
            (r'\d+', Number.Integer),
            (r'\n', Whitespace),
            (r'[^\S\n]+', Text),
            (r'(?s)\[(=*)\[.*?\]\1\]', String),
            (r'(->|=>)', Name.Function),
            (r':[a-zA-Z_]\w*', Name.Variable),
            (r'(==|!=|~=|<=|>=|\.\.\.|\.\.|[=+\-*/%^<>#!.\\:])', Operator),
            (r'[;,]', Punctuation),
            (r'[\[\]{}()]', Keyword.Type),
            (r'[a-zA-Z_]\w*:', Name.Variable),
            (words((
                'class', 'extends', 'if', 'then', 'super', 'do', 'with',
                'import', 'export', 'while', 'elseif', 'return', 'for', 'in',
                'from', 'when', 'using', 'else', 'and', 'or', 'not', 'switch',
                'break'), suffix=r'\b'),
             Keyword),
            (r'(true|false|nil)\b', Keyword.Constant),
            (r'(and|or|not)\b', Operator.Word),
            (r'(self)\b', Name.Builtin.Pseudo),
            (r'@@?([a-zA-Z_]\w*)?', Name.Variable.Class),
            (r'[A-Z]\w*', Name.Class),  # proper name
            (words(all_lua_builtins(), suffix=r"\b"), Name.Builtin),
            (r'[A-Za-z_]\w*', Name),
            ("'", String.Single, combined('stringescape', 'sqs')),
            ('"', String.Double, combined('stringescape', 'dqs'))
        ],
        'stringescape': [
            (r'''\\([abfnrtv\\"']|\d{1,3})''', String.Escape)
        ],
        'strings': [
            (r'[^#\\\'"]+', String),
            # note that strings are multi-line.
            # hashmarks, quotes and backslashes must be parsed one at a time
        ],
        'interpoling_string': [
            (r'\}', String.Interpol, "#pop"),
            include('base')
        ],
        'dqs': [
            (r'"', String.Double, '#pop'),
            (r'\\.|\'', String),  # double-quoted string don't need ' escapes
            (r'#\{', String.Interpol, "interpoling_string"),
            (r'#', String),
            include('strings')
        ],
        'sqs': [
            (r"'", String.Single, '#pop'),
            (r'#|\\.|"', String),  # single quoted strings don't need " escapses
            include('strings')
        ]
    }

    def get_tokens_unprocessed(self, text):
        # set . as Operator instead of Punctuation
        for index, token, value in LuaLexer.get_tokens_unprocessed(self, text):
            if token == Punctuation and value == ".":
                token = Operator
            yield index, token, value

class LuaJITLexer(LuaLexer):
    """
    For LuaJIT source code (https://luajit.org/).

    LuaJIT extends Lua 5.1 with 64-bit integer literals (1LL, 0xFFULL),
    imaginary literals (1i), goto/label syntax, and ffi/bit/jit modules.

    .. versionadded:: 2.21
    """

    name = 'LuaJIT'
    url = 'https://luajit.org/'
    aliases = ['luajit']
    filenames = ['*.lj']
    mimetypes = ['text/x-luajit']
    version_added = '2.21'

    _LUAJIT_MODULES = frozenset(all_luajit_builtins())

    def get_tokens_unprocessed(self, text):
        import re as _re
        _LL = _re.compile(r'(0[xX][0-9a-fA-F]+|\d+)([uU]?[lL]{1,2}|[ij])')
        for index, token, value in LuaLexer.get_tokens_unprocessed(self, text):
            if token is Name and value in self._LUAJIT_MODULES:
                yield index, Name.Builtin, value
            elif token in (Number.Integer, Number.Hex, Number.Float):
                m = _LL.fullmatch(value)
                if m:
                    yield index, token, m.group(1)
                    yield index + len(m.group(1)), Number.Integer.Long, m.group(2)
                else:
                    yield index, token, value
            else:
                yield index, token, value



class TealLexer(RegexLexer):
    """
    For `Teal <https://github.com/teal-language/tl>`_, a typed dialect of Lua.

    .. versionadded:: 2.21
    """

    name = 'Teal'
    url = 'https://github.com/teal-language/tl'
    aliases = ['teal']
    filenames = ['*.tl']
    mimetypes = ['text/x-teal']
    version_added = '2.21'

    tokens = {
        'root': [
            (r'--\\[([=]*)\\[.*?\\]\\1\\]', Comment.Multiline),
            (r'--.*?$', Comment.Single),
            (r'(?s)(\\[([=]*)\\[.*?\\]\\2\\])', String),
            (r'\"', String.Double, 'string_double'),
            (r"\'", String.Single, 'string_single'),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'\\d+\\.\\d*([eE][+-]?\\d+)?', Number.Float),
            (r'\\d+', Number.Integer),
            (words((
                'and', 'break', 'do', 'else', 'elseif', 'end',
                'false', 'for', 'function', 'goto', 'if', 'in',
                'local', 'nil', 'not', 'or', 'repeat', 'return',
                'then', 'true', 'until', 'while',
                # Teal type keywords
                'global', 'record', 'enum', 'where', 'type', 'as', 'is',
            ), suffix=r'\\b'), Keyword),
            (r'[+\\-*/%^#&~|<>=(){}\\[\\];:,.]', Punctuation),
            (r'[a-zA-Z_]\\w*', Name),
            (r'\\s+', Whitespace),
        ],
        'string_double': [
            (r'[^"\\\\\\n]+', String.Double),
            (r'\\\\.', String.Escape),
            (r'\"', String.Double, '#pop'),
        ],
        'string_single': [
            (r"[^'\\\\\\n]+", String.Single),
            (r'\\\\.', String.Escape),
            (r"\'", String.Single, '#pop'),
        ],
    }



class FennelLexer(RegexLexer):
    """
    For `Fennel <https://fennel-lang.org/>`_, a Lisp that compiles to Lua.

    .. versionadded:: 2.21
    """

    name = 'Fennel'
    url = 'https://fennel-lang.org/'
    aliases = ['fennel', 'fnl']
    filenames = ['*.fnl']
    mimetypes = ['text/x-fennel']
    version_added = '2.21'

    tokens = {
        'root': [
            (r'\\s+', Whitespace),
            (r';.*$', Comment.Single),
            (r'#\\|', Comment.Multiline, 'block_comment'),
            (r'~@|['`~^@]', Operator),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'\\d+\\.\\d*([eE][+-]?\\d+)?', Number.Float),
            (r'\\d+', Number.Integer),
            (r'\"', String, 'string'),
            (r'\\:[\\.\\w#$%&*+\\-/<=>?@!^|~]+', Name.Constant),
            (words((
                'fn', 'lambda', 'λ', 'let', 'let*', 'local', 'global',
                'set', 'tset', 'do', 'if', 'when', 'unless', 'while',
                'each', 'for', 'icollect', 'collect', 'fcollect',
                'accumulate', 'match', 'match-try', 'case', 'case-try',
                'macro', 'macros', 'eval-compiler', 'import-macros',
                'require-macros', 'include', 'lua',
                'and', 'or', 'not', 'true', 'false', 'nil',
                'values', '...',
            ), suffix=r'(?=[\\s()])'), Keyword),
            (r'[()\\[\\]{}\\\']', Punctuation),
            (r'[\\.\\w#$%&*+\\-/<=>?@!^|~]+', Name),
        ],
        'string': [
            (r'[^"\\\\]+', String),
            (r'\\\\.', String.Escape),
            (r'\"', String, '#pop'),
        ],
        'block_comment': [
            (r'[^|#]+', Comment.Multiline),
            (r'\\|#', Comment.Multiline, '#pop'),
            (r'#\\|', Comment.Multiline, '#push'),
            (r'[|#]', Comment.Multiline),
        ],
    }
