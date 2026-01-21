from pygments.lexer import ExtendedRegexLexer, bygroups
from pygments.token import Comment, Whitespace, Name, Keyword, Number, Punctuation


class CustomYamlLexer(ExtendedRegexLexer):
    """
    Custom Lexer for YAML, specifically designed to handle and highlight comments correctly.
    """

    name = 'CustomYAML'
    aliases = ['customyaml']
    filenames = ['*.yaml', '*.yml']
    mimetypes = ['text/x-yaml']

    tokens = {
        'root': [
            (r'[ ]+(?=#|$)', Whitespace),
            (r'\n+', Whitespace),
            (r'#[^\n]*', Comment.Single),
            (r'^%YAML(?=[ ]|$)', Name.Tag, 'yaml-directive'),
            (r'^%TAG(?=[ ]|$)', Name.Tag, 'tag-directive'),
            (r'^(?:---|\.\.\.)(?=[ ]|$)', Name.Namespace, 'block-line'),
            (r'[ ]*(?!\s|$)', Whitespace, ('block-line', 'indentation')),
            (r'(\w+)(:)([ ]*)([^\n]+)', bygroups(Name.Attribute, Punctuation, Whitespace, Name.Other)),
        ],
        'ignored-line': [
            (r'[ ]+(?=#|$)', Whitespace),
            (r'#[^\n]*', Comment.Single),
            (r'\n', Whitespace, '#pop:2'),
        ],
        'yaml-directive': [
            (r'([ ]+)([0-9]+\.[0-9]+)',
             bygroups(Whitespace, Number), 'ignored-line'),
        ],
        'tag-directive': [
            (r'([ ]+)(!|![\w-]*!)'
             r'([ ]+)(!|!?[\w;/?:@&=+$,.!~*\'()\[\]%-]+)',
             bygroups(Whitespace, Keyword.Type, Whitespace, Keyword.Type),
             'ignored-line'),
        ],
        'indentation': [
            (r'[ ]*$', Whitespace, '#pop:2'),
            (r'[ ]+(?=[?:-](?:[ ]|$))', Whitespace),
        ],
        'block-line': [
            (r'.+', Name.Other),  # Generic handling for lines within blocks
        ],
    }
