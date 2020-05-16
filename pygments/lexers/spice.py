# -*- coding: utf-8 -*-
"""
    pygments.lexers.spice
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for Spice.

    :copyright: Copyright 2013 Fabrice Salvaire.
    :license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import RegexLexer, bygroups
from pygments.token import (
    Text, Comment, Operator, Keyword, Name, Number, Punctuation,
    )

__all__ = ['SpiceLexer']


class SpiceLexer(RegexLexer):
    """ For Spice source code. """
    name = 'spice'
    aliases = ['spice']
    filenames = ['*.cir']
    mimetypes = ['text/x-spice']
    flags = re.MULTILINE | re.IGNORECASE

    directives = r'^\.(title|end|model|subckt|ends|global|include|lib|param|func|csparam'

    scale_factors = r'(T|G|Meg|k|mil|m|u|p|f)'

    model_types = r'(R|C|L|SW|CSW|URC|LTRA|D|NPN|PNP|NJF|PJF|NMOS|PMOS|NMF|PMF)'
    devices = r'^(R|C|L|K|S|W|V|I|G|E|F|H|B|T|O|U|Y|P|D|Q|J|Z|M|X)'

    element_name = r'[a-z0-9]+'
    node_name = r'[a-z0-9]+'

    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'^\*.*', Comment.Single),
            (r'[(){}]', Punctuation),
            (r'[=+\-\*\/]', Operator),
            (r'^(\.title)(\s+)(.*)', bygroups(Keyword, Text.Whitespace, Text)),
            (r'^(\.include)(\s+)(.*)', bygroups(Keyword, Text.Whitespace, Text)),
            (r'^(\.subckt)(\s+)(' + element_name + ')',
             bygroups(Keyword, Text.Whitespace, Name.Attribute)), # node_names
            (r'^(\.model)(\s+)(' + element_name + r')(\s+)(' + model_types + ')',
             bygroups(Keyword, Text.Whitespace, Name.Attribute, Text.Whitespace, Name.Builtin)),
            (r'^\.[a-z]+', Keyword),
            (devices + '(' + element_name + ')', bygroups(Name.Builtin, Name.Attribute)),
            (r'(\d+\.\d*|\.\d+|\d+)[e][+-]?\d+', Number.Float),
            (r'(\d+\.\d*|\.\d+)', Number.Float),
            (r'[0-9][a-z_][a-z0-9_]*', Name),
            (r'[a-z_][a-z0-9_]*', Name),
            (r'\d+', Number.Integer),
            ],
    }
