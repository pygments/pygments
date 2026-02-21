"""
    pygments.lexers.papyrus
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Papyrus family languages.

    Papyrus is the scripting language used by games built on the Creation Engine
    by Bethesda Softworks (e.g. The Elder Scrolls V: Skyrim). There are multiple
    versions of this language depending on which version of the engine the game
    used, so each game has to have it's own lexer.

    :copyright: Copyright 2006-2026 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, default, words, include
from pygments.token import Comment, Literal, Keyword, Name, Number, Punctuation, Text, Operator, String, Whitespace


__all__ = ['PapyrusSkyrimLexer']


class PapyrusSkyrimLexer(RegexLexer):
    """
    Lexer of Papyrus source code (Elder Scrolls V: Skyrim version).
    """

    name = 'Papyrus (Skyrim)'
    aliases = ['papyrus-skyrim']
    url = ""  # No offical documentation is available.
    filenames = ['*.psc']
    version_added = '1.0'

    ident = r'[A-Z_][A-Z0-9_]*'
    object_type = rf'{ident}(?:\[\])?'
    primitive_type = r'(?:Bool|Int|Float|String)(?:\[\])?'

    flags = re.IGNORECASE
    tokens = {
        'root': [
            # Whitespace
            (r'(\s)+', Whitespace),
            # Comments
            (r';/(\n|.)*?/;', Comment.Multiline),
            (r';.*?\n', Comment.Single),
            (r'{(\n|.)*?}', Comment.Special),
            # Punctuation
            (r'(\(|\)|\[|\]|,|\\)', Punctuation),
            # Casts
            (rf'\b(As)([ \t]+)({primitive_type})',
             bygroups(Operator.Word, Whitespace, Keyword.Type)),
            (rf'\b(As)([ \t]+)({object_type})',
             bygroups(Operator.Word, Whitespace, Name.Class)),
            (r'\bAs\b', Operator.Word),
            # Array Creation
            (r'\b(New)([ \t]+)(Bool|Int|Float|String)([ \t]*)(\[)([ \t]*)([0-9]+)([ \t]*)(\])',
             bygroups(Operator.Word, Whitespace, Keyword.Type, Whitespace, Punctuation, Whitespace, Literal, Whitespace, Punctuation)),
            (rf'\b(New)([ \t]+)({ident})([ \t]*)(\[)([ \t]*)([0-9]+)([ \t]*)(\])',
             bygroups(Operator.Word, Whitespace, Name.Class, Whitespace, Punctuation, Whitespace, Literal, Whitespace, Punctuation)),
            (r'\bNew\b', Operator.Word),
            # Special Literals
            (r'\b(Self|Parent)\b', Name.Builtin.Pseudo),
            # Literals
            ('"', String.Double, 'string'),
            (r'-?[0-9]+\.[0-9]+', Number.Float),
            (r'0x[0-7A-F]+', Number.Hex),
            (r'-?[0-9]+', Number.Integer),
            (r'\b(True|False|None)\b', Literal),
            # Operators
            (r'(\+|\-|\*|%|/|!|>|<|=)=?', Operator),
            (r'(\|\||&&|\.)', Operator),
            # Various Declarations
            #
            # These allow for using known syntax to provide more accurate token
            # types rather than basically all identifiers falling back to
            # Name.Variable.
            (rf'\b(ScriptName)([ \t]+)({ident})\b',
             bygroups(Keyword, Whitespace, Name.Class)),
            (rf'\b(Extends)([ \t]+)({ident})\b',
             bygroups(Keyword, Whitespace, Name.Class)),
            (rf'\b(Import)([ \t]+)({ident})\b',
             bygroups(Keyword, Whitespace, Name.Class)),
            (rf'\b({primitive_type})([ \t]+)(Function)([ \t]+)({ident})([ \t]*)(\()',
             bygroups(Keyword.Type, Whitespace, Keyword.Declaration, Whitespace, Name.Function, Whitespace, Punctuation), 'parameters'),
            (rf'\b({object_type})([ \t]+)(Function)([ \t]+)({ident})([ \t]*)(\()',
             bygroups(Name.Class, Whitespace, Keyword.Declaration, Whitespace, Name.Function, Whitespace, Punctuation), 'parameters'),
            (rf'\b(Function)([ \t]+)({ident})([ \t]*)(\()',
             bygroups(Keyword.Declaration, Whitespace, Name.Function, Whitespace, Punctuation), 'parameters'),
            (rf'\b(Event)([ \t]+)({ident})([ \t]*)(\()',
             bygroups(Keyword.Declaration, Whitespace, Name.Function.Magic, Whitespace, Punctuation), 'parameters'),
            (rf'\b({primitive_type})([ \t]+)(Property)([ \t]+)({ident})\b',
             bygroups(Keyword.Type, Whitespace, Keyword.Declaration, Whitespace, Name.Property)),
            (rf'\b({object_type})([ \t]+)(Property)([ \t]+)({ident})\b',
             bygroups(Name.Class, Whitespace, Keyword.Declaration, Whitespace, Name.Property)),
            (rf'\b(Auto)([ \t]+)(State)([ \t]+)({ident})\b',
             bygroups(Keyword, Whitespace, Keyword.Declaration, Whitespace, Name.Label)),
            (rf'\b(State)([ \t]+)({ident})\b',
             bygroups(Keyword.Declaration, Whitespace, Name.Label)),
            (rf'\b({primitive_type})([ \t]+)({ident})([ \t]*)(=)',
             bygroups(Keyword.Type, Whitespace, Name.Variable, Whitespace, Operator), 'expression'),
            (rf'\b({object_type})([ \t]+)({ident})([ \t]*)(=)',
             bygroups(Name.Class, Whitespace, Name.Variable, Whitespace, Operator), 'expression'),
            # Built-ins
            #
            # Unlike other functions and events, these State-related ones are
            # never defined by another type (even as Native).
            (rf'\b(GoToState)([ \t]*)(\()([ \t]*)("{ident}")([ \t]*)(\))',
             bygroups(Name.Builtin, Whitespace, Punctuation, Whitespace, Name.Label, Whitespace, Punctuation)),
            (rf'\b(GetState)([ \t]*)(\()([ \t]*)(\))',
             bygroups(Name.Builtin, Whitespace, Punctuation, Whitespace, Punctuation)),
            (rf'\b(Event)([ \t]+)(OnBeginState)([ \t]*)(\()([ \t]*)(\))',
             bygroups(Keyword.Declaration, Whitespace, Name.Builtin, Whitespace, Punctuation, Whitespace, Punctuation)),
            (rf'\b(Event)([ \t]+)(OnEndState)([ \t]*)(\()([ \t]*)(\))',
             bygroups(Keyword.Declaration, Whitespace, Name.Builtin, Whitespace, Punctuation, Whitespace, Punctuation)),
            (rf'\b(GetState)([ \t]*)(\()([ \t]*)(\))',
             bygroups(Name.Builtin, Whitespace, Punctuation, Whitespace, Punctuation)),
            (r'\bLength\b', Name.Builtin),
            # Function Calls
            (rf'\b({ident})([ \t]*)(\()',
             bygroups(Name.Function, Whitespace, Punctuation), 'arguments'),
            # Keywords
            (words(
                ('Function', 'EndFunction', 'Event', 'EndEvent',
                 'Property', 'EndProperty', 'State', 'EndState'),
                prefix=r'\b', suffix=r'\b'), Keyword.Declaration),
            (words(
                ('ScriptName', 'Extends', 'Import', 'Auto', 'AutoReadOnly', 'Hidden', 'Conditional',
                 'Native', 'Global', 'If', 'Else', 'ElseIf', 'EndIf', 'While', 'EndWhile', 'Return'),
                prefix=r'\b', suffix=r'\b'), Keyword),
            # Types
            (rf'\b({primitive_type})\b', Keyword.Type),
            (rf'\b({ident}\[\])\b', Name.Class),  # Object array types
            # Variables
            (rf'({ident})', Name.Variable),
        ],
        # For parameter lists in Function and Event declarations.
        'parameters': [
            (rf'\b({primitive_type})([ \t]+)({ident})',
             bygroups(Keyword.Type, Whitespace, Name.Variable.Instance)),
            (rf'\b({object_type})([ \t]+)({ident})',
             bygroups(Name.Class, Whitespace, Name.Variable.Instance)),
            (r'=', Operator, 'expression'),
            (r',', Punctuation),
            (r'[ \t]+', Whitespace),
            (r'(\\)(\n)', bygroups(Punctuation, Whitespace)),
            (r';/(\n|.)*?/;', Comment.Multiline),
            (r'\)', Punctuation, '#pop'),
        ],
        # For argument lists in Function calls.
        'arguments': [
            (rf'\b({ident})([ \t]*)(=)',
             bygroups(Name.Variable.Instance, Whitespace, Operator), 'expression'),
            (r'[ \t]+', Whitespace),
            (r'(\\)(\n)', bygroups(Punctuation, Whitespace)),
            (r';/(\n|.)*?/;', Comment.Multiline),
            (r'\)', Punctuation, '#pop'),
            # Treat everything else as an expression.
            ('', Text, 'expression'),
        ],
        # For instances where an arbitrary expression is expected with a known
        # terminal (a newline, single-line comment, ')', ']', or ',').
        'expression': [
            (r'[ \t]+', Whitespace),
            # Comments
            (r';/(\n|.)*?/;', Comment.Multiline),
            # Punctuation
            (r'(\(|\[)', Punctuation, 'expression'),
            (r'(\\)(\n)', bygroups(Punctuation, Whitespace)),
            # Built-ins.
            (rf'(GetState)([ \t]*)(\()([ \t]*)(\))',
             bygroups(Name.Builtin, Whitespace, Punctuation, Whitespace, Punctuation)),
            (r'\bLength\b', Keyword.Builtin),
            # Function Call
            (rf'\b({ident})([ \t]*)(\()',
             bygroups(Name.Function, Whitespace, Punctuation), 'arguments'),
            # Type Casts
            (rf'\b(As)([ \t]+)({primitive_type})',
             bygroups(Operator.Word, Whitespace, Keyword.Type)),
            (rf'\b(As)([ \t]+)({object_type})',
             bygroups(Operator.Word, Whitespace, Name.Class)),
            (r'\bAs\b', Operator.Word),
            # Array Creation
            (r'\b(New)([ \t]+)(Bool|Int|Float|String)([ \t]*)(\[)([ \t]*)([0-9]+)([ \t]*)(\])',
             bygroups(Operator.Word, Whitespace, Keyword.Type, Whitespace, Punctuation, Whitespace, Literal, Whitespace, Punctuation)),
            (rf'\b(New)([ \t]+)({ident})([ \t]*)(\[)([ \t]*)([0-9]+)([ \t]*)(\])',
             bygroups(Operator.Word, Whitespace, Name.Class, Whitespace, Punctuation, Whitespace, Literal, Whitespace, Punctuation)),
            # Specials
            (r'\b(Self|Parent)\b', Name.Builtin.Pseudo),
            # Literals
            ('"', String.Double, 'string'),
            (r'-?[0-9]+\.[0-9]+', Number.Float),
            (r'0x[0-7A-F]+', Number.Hex),
            (r'-?[0-9]+', Number.Integer),
            (r'\b(True|False|None)\b', Literal),
            # Operators
            (r'(\+|\-|\*|%|/|!|>|<|=)=?', Operator),
            (r'(\|\||&&|\.)', Operator),
            # Types
            (rf'\b({primitive_type})\b', Keyword.Type),
            (rf'\b({ident}\[\])\b', Name.Class),
            # Variables
            (rf'({ident})', Name.Variable),
            # Terminals
            (r';.*?\n', Comment.Single, '#pop'),
            (r'\n', Whitespace, '#pop'),
            (r'(?:\)|\]|,)', Punctuation, '#pop'),
        ],
        # For string content.
        'string': [
            (r'"', String.Double, '#pop'),
            (r'\\(?:n|t|\\|\")', String.Escape),
            (r'[^"\\]+', String.Double),
        ],
    }

    def analyse_text(text):
        score = 0
        if re.search(r'ScriptName\s+[a-z0-9_]+', text, re.IGNORECASE):
            score += 0.7
            if re.search(r'^End(?:Event|Function|State)\b', text, re.IGNORECASE):
                score += 0.3
            if re.search(r'\b(?:ScriptEventName|StructVarName|EndGroup|CustomEventName|CustomEvent|DebugOnly|BetaOnly)\b', text, re.IGNORECASE):
                # These keywords are specific to other versions of Papyrus (i.e. Fallout 4 and later).
                score -= 0.5
            if re.search(r'\b(?:RequiresGuard|ProtectsFunctionLogic|SelfOnly|EndLockGuard|TryLockGuard|EndTryLockGuard)\b', text, re.IGNORECASE):
                # These keywords are specific to other versions of Papyrus (i.e. Starfield and later).
                score -= 0.5
        return max(0.0, min(score, 1.0))
