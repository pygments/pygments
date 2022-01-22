"""
    pygments.lexers.prolog
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Prolog and Prolog-like languages.

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import bygroups, inherit, words
from pygments.lexers import PrologLexer
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

__all__ = ['CplintLexer']


class CplintLexer(PrologLexer):
    """
    Lexer for Prolog files.
    """
    name = 'cplint'
    aliases = ['cplint']
    filenames = ['*.ecl', '*.prolog', '*.pro', '*.pl', '*.P', '*.lpad', '*.cpl']
    mimetypes = ['text/x-prolog']

    flags = re.UNICODE | re.MULTILINE

    tokens = {
        'root': [
            (words(('gaussian','uniform_dens','dirichlet','gamma','beta','poisson','binomial','geometric',
              'exponential','pascal','multinomial',
              'uniform','discrete','finite'), suffix=r'\b'),Name.Builtin),
            (r'([a-z]+)(:)', bygroups(String.Atom, Punctuation)),
            (r':',  Operator),
            (r'::', Operator),
            inherit,
        ],
    }


