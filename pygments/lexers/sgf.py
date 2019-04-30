# -*- coding: utf-8 -*-
"""
    pygments.lexers.sgf
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for Smart Game Format (sgf) file format.

    The format is used to store game records of board games for two players
    (mainly Go game).
    For more information about the definition of the format, see:
    https://www.red-bean.com/sgf/

    :copyright: Copyright 2006-2018 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.

    .. versionadded:: 2.4
"""

from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

__all__ = ["SmartGameFormatLexer"]


class SmartGameFormatLexer(RegexLexer):
    name = 'SmartGameFormat'
    aliases = ['sgf']
    filenames = ['*.sgf']

    tokens = {
        'root': [
            (r'[\s():;]', Punctuation),
            # tokens:
            (r'(A[BW]|AE|AN|AP|AR|AS|[BW]L|BM|[BW]R|[BW]S|[BW]T|CA|CH|CP|CR|DD|DM|DO|DT|EL|EV|EX|FF|FG|G[BW]|GC|GM|GN|HA|HO|ID|IP|IT|IY|KM|KO|L|LB|LN|LT|M|MA|MN|N|OB|OM|ON|OP|OT|OV|P[BW]|PC|PL|PM|RE|RG|RO|RU|SO|SC|SE|SI|SL|SO|SQ|ST|SU|SZ|T[BW]|TC|TE|TM|TR|UC|US|V|VW|[BW]|C)',
             Name.Builtin),
            # number:
            (r'(\[)([0-9.]+)(\])',
             bygroups(Punctuation, Literal.Number, Punctuation)),
            # date:
            (r'(\[)([0-9]{4}-[0-9]{2}-[0-9]{2})(\])',
             bygroups(Punctuation, Literal.Date, Punctuation)),
            # point:
            (r'(\[)([a-z]{2})(\])',
             bygroups(Punctuation, String, Punctuation)),
            # double points:
            (r'(\[)([a-z]{2})(:)([a-z]{2})(\])',
             bygroups(Punctuation, String, Punctuation, String, Punctuation)),

            (r'(\[)([\w\s#()+,\-.:?]+)(\])',
             bygroups(Punctuation, String, Punctuation)),
            (r'(\[)(\s.*)(\])',
             bygroups(Punctuation, Text, Punctuation)),
       ],
    }
