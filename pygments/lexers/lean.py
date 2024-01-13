"""
    pygments.lexers.lean
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for the Lean theorem prover.

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words, include
from pygments.token import Comment, Operator, Keyword, Name, String, \
    Number, Generic, Whitespace

__all__ = ['Lean3Lexer']

class Lean3Lexer(RegexLexer):
    """
    For the Lean 3 theorem prover.
    """
    name = 'Lean'
    url = 'https://leanprover-community.github.io/lean3'
    aliases = ['lean', 'lean3']
    filenames = ['*.lean']
    mimetypes = ['text/x-lean', 'text/x-lean3']
    version_added = '2.0'

    # from https://github.com/leanprover/vscode-lean/blob/1589ca3a65e394b3789409707febbd2d166c9344/syntaxes/lean.json#L186C20-L186C217
    _name_segment = (
        "(?![λΠΣ])[_a-zA-Zα-ωΑ-Ωϊ-ϻἀ-῾℀-⅏𝒜-𝖟]"
        "(?:(?![λΠΣ])[_a-zA-Zα-ωΑ-Ωϊ-ϻἀ-῾℀-⅏𝒜-𝖟0-9'ⁿ-₉ₐ-ₜᵢ-ᵪ])*")
    _name = _name_segment + r"(\." + _name_segment + r")*"

    tokens = {
        'expression': [
            (r'\s+', Whitespace),
            (r'/--', String.Doc, 'docstring'),
            (r'/-', Comment, 'comment'),
            (r'--.*?$', Comment.Single),
            (words((
                    'forall', 'fun', 'Pi', 'from', 'have', 'show', 'assume', 'suffices',
                    'let', 'if', 'else', 'then', 'in', 'with', 'calc', 'match',
                    'do'
                ), prefix=r'\b', suffix=r'\b'), Keyword),
            (words(('sorry', 'admit'), prefix=r'\b', suffix=r'\b'), Generic.Error),
            (words(('Sort', 'Prop', 'Type'), prefix=r'\b', suffix=r'\b'), Keyword.Type),
            (words((
                '(', ')', ':', '{', '}', '[', ']', '⟨', '⟩', '‹', '›', '⦃', '⦄', ':=', ',',
            )), Operator),
            (_name, Name),
            (r'``?' + _name, String.Symbol),
            (r'0x[A-Za-z0-9]+', Number.Integer),
            (r'0b[01]+', Number.Integer),
            (r'\d+', Number.Integer),
            (r'"', String.Double, 'string'),
            (r"'(?:(\\[\\\"'nt])|(\\x[0-9a-fA-F]{2})|(\\u[0-9a-fA-F]{4})|.)'", String.Char),
            (r'[~?][a-z][\w\']*:', Name.Variable),
            (r'\S', Name.Builtin.Pseudo),
        ],
        'root': [
            (words((
                'import', 'renaming', 'hiding',
                'namespace',
                'local',
                'private', 'protected', 'section',
                'include', 'omit', 'section',
                'protected', 'export',
                'open',
                'attribute',
            ), prefix=r'\b', suffix=r'\b'), Keyword.Namespace),
            (words((
                'lemma', 'theorem', 'def', 'definition', 'example',
                'axiom', 'axioms', 'constant', 'constants',
                'universe', 'universes',
                'inductive', 'coinductive', 'structure', 'extends',
                'class', 'instance',
                'abbreviation',

                'noncomputable theory',

                'noncomputable', 'mutual', 'meta',

                'attribute',

                'parameter', 'parameters',
                'variable', 'variables',

                'reserve', 'precedence',
                'postfix', 'prefix', 'notation', 'infix', 'infixl', 'infixr',

                'begin', 'by', 'end',

                'set_option',
                'run_cmd',
            ), prefix=r'\b', suffix=r'\b'), Keyword.Declaration),
            (r'@\[', Keyword.Declaration, 'attribute'),
            (words((
                '#eval', '#check', '#reduce', '#exit',
                '#print', '#help',
            ), suffix=r'\b'), Keyword),
            include('expression')
        ],
        'attribute': [
            (r'\]', Keyword.Declaration, '#pop'),
            include('expression'),
        ],
        'comment': [
            (r'[^/-]+', Comment.Multiline),
            (r'/-', Comment.Multiline, '#push'),
            (r'-/', Comment.Multiline, '#pop'),
            (r'[/-]', Comment.Multiline)
        ],
        'docstring': [
            (r'[^/-]+', String.Doc),
            (r'-/', String.Doc, '#pop'),
            (r'[/-]', String.Doc)
        ],
        'string': [
            (r'[^\\"]+', String.Double),
            (r"(?:(\\[\\\"'nt])|(\\x[0-9a-fA-F]{2})|(\\u[0-9a-fA-F]{4}))", String.Escape),
            ('"', String.Double, '#pop'),
        ],
    }


LeanLexer = Lean3Lexer
