"""
    pygments.lexers.apl
    ~~~~~~~~~~~~~~~~~~~

    Lexers for APL.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['APLLexer']


class APLLexer(RegexLexer):
    """
    A simple APL lexer.
    """
    name = 'APL'
    url = 'https://en.m.wikipedia.org/wiki/APL_(programming_language)'
    aliases = ['apl']
    filenames = [
        '*.apl', '*.aplf', '*.aplo', '*.apln',  
        '*.aplc', '*.apli', '*.dyalog',
    ]
    version_added = '2.0'
    _example = 'apl/test.apl'

    tokens = {
        'root': [
            # Whitespace
            # ==========
            (r'\s+', Whitespace),
            #
            # Comment
            # =======
            # '⍝' is traditional; '#' is supported by GNU APL and NGN (but not Dyalog)
            (r'[⍝#].*$', Comment.Single),
            #
            # Strings
            # =======
            (r'\'((\'\')|[^\'])*\'', String.Single),
            (r'"(("")|[^"])*"', String.Double),  # supported by NGN APL
            #
            # Punctuation
            # ===========
            # This token type is used for diamond and parenthesis
            # but not for bracket and ; (see below)
            (r'[⋄◇()]', Punctuation),
            #
            # Array indexing
            # ==============
            # Since this token type is very important in APL, it is not included in
            # the punctuation token type but rather in the following one
            (r'[\[\];]', String.Regex),
            #
            # Distinguished names
            # ===================
            # following IBM APL2 standard
            (r'⎕[A-Za-zΔ∆⍙][A-Za-zΔ∆⍙_¯0-9]*', Name.Function),
            #
            # Labels
            # ======
            # following IBM APL2 standard
            # (r'[A-Za-zΔ∆⍙][A-Za-zΔ∆⍙_¯0-9]*:', Name.Label),
            #
            # Variables
            # =========
            # following IBM APL2 standard (with a leading _ ok for GNU APL and Dyalog)
            (r'[A-Za-zΔ∆⍙_][A-Za-zΔ∆⍙_¯0-9]*', Name.Variable),     
            #
            # Numbers
            # =======
            (r'¯?(0[Xx][0-9A-Fa-f]+|[0-9]*\.?[0-9]+([Ee][+¯]?[0-9]+)?|¯|∞)'
             r'([Jj]¯?(0[Xx][0-9A-Fa-f]+|[0-9]*\.?[0-9]+([Ee][+¯]?[0-9]+)?|¯|∞))?',
             Number),
            #
            # Operators
            # ==========
            (r'[\.\\\/⌿⍀¨⍣⍨⍠⍤∘⌸&⌶@⌺⍥⍛⍢]', Name.Attribute),  # closest token type
            (r'[+\-×÷⌈⌊∣|⍳?*⍟○!⌹<≤=>≥≠≡≢∊⍷∪∩~∨∧⍱⍲⍴,⍪⌽⊖⍉↑↓⊂⊃⌷⍋⍒⊤⊥⍕⍎⊣⊢⍁⍂≈⌸⍯↗⊆⊇⍸√⌾…⍮]',
             Operator),
            #
            # Constant
            # ========
            (r'⍬', Name.Constant),
            #
            # Quad symbol
            # ===========
            (r'[⎕⍞]', Name.Variable.Global),
            #
            # Arrows left/right
            # =================
            (r'[←→]', Keyword.Declaration),
            #
            # D-Fn
            # ====
            (r'[⍺⍵⍶⍹∇:]', Name.Builtin.Pseudo),
            (r'[{}]', Keyword.Type),
        ],
    }
