"""
    pygments.lexers.mcschema
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for Data Schemas for Minecraft Add-on Development.
    
    official: https://learn.microsoft.com/en-us/minecraft/creator/reference/content/schemasreference/
    community example: https://www.mcbe-dev.net/addons/data-driven/manifest.html

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import bygroups, default, include, RegexLexer
from pygments.token import Text, Comment, Operator, Keyword, String, Number, Punctuation, Whitespace

__all__ = ['MCSchemaLexer']

class MCSchemaLexer(RegexLexer):
    name = 'MCSchema'
    url = 'https://learn.microsoft.com/en-us/minecraft/creator/reference/content/schemasreference/'
    aliases = ['mcschema']
    filenames = ['*.mcschema']
    mimetypes = ['text/mcschema']

    tokens = {
        'commentsandwhitespace': [
            (r'\s+', Whitespace),
            (r'//.*?$', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline)
        ],
        'slashstartsregex': [
            include('commentsandwhitespace'),
            (r'/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/'
             r'([gimuysd]+\b|\B)', String.Regex, '#pop'),
            (r'(?=/)', Text, ('#pop', 'badregex')),
            default('#pop')
        ],
        'badregex': [
            (r'\n', Whitespace, '#pop')
        ],
        'root': [
            (r'^(?=\s|/|<!--)', Text, 'slashstartsregex'),
            include('commentsandwhitespace'),
            
            # keywords for optional word and field types
            (r'(?<=: )opt', Operator.Word),
            (r'(?<=\s)[a-zA-Z0-9_-]*(?=(\s+"|\n))', Keyword.Declaration),
            
            # numeric literals
            (r'0[bB][01]+', Number.Bin),
            (r'0[oO]?[0-7]+', Number.Oct),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'(\.[0-9]+|[0-9]+\.[0-9]*|[0-9]+)([eE][-+]?[0-9]+)?', Number.Float),
            
            # possible punctuations
            (r'\.\.\.|=>', Punctuation),
            (r'\+\+|--|~|\?\?=?|\?|:|\\(?=\n)|'
             r'(<<|>>>?|==?|!=?|(?:\*\*|\|\||&&|[-<>+*%&|^/]))=?', Operator, 'slashstartsregex'),
            (r'[{(\[;,]', Punctuation, 'slashstartsregex'),
            (r'[})\].]', Punctuation),
            
            # strings
            (r'"(\\\\|\\[^\\]|[^"\\])*"', String.Double),
            (r"'(\\\\|\\[^\\]|[^'\\])*'", String.Single),
            
            # title line
            (r'[a-zA-Z0-9_-]*?(?=:{?\n)', String.Symbol),
            # title line with a version code, formatted
            # `major.minor.patch-prerelease+buildmeta`
            (r'([a-zA-Z0-9_-]*?)(:)(0|[1-9][0-9]*)(?:(\.)(0|[1-9][0-9]*)(?:(\.)(0|[1-9][0-9]*)(?:(\-)((?:(?:(?:(?:0[0-9]*[A-Za-z-])|[1-9A-Za-z-])[0-9A-Za-z-]*)|0)(?:\.(?:(?:(?:(?:0[0-9]*[A-Za-z-])|[1-9A-Za-z-])[0-9A-Za-z-]*)|0))*))?(?:(\+)([0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?)?)?(?=:{?\n)', bygroups(String.Symbol, Operator, Number.Integer, Operator, Number.Integer, Operator, Number.Integer, Operator, String, Operator, String)),
            
            (r'.*\n', Text),
        ]
    }