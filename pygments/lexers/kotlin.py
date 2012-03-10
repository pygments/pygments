# -*- coding: utf-8 -*-
"""
    pygments.lexers.kotlin
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Kotlin programming language.
    http://confluence.jetbrains.net/display/Kotlin/

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import re

from pygments.lexer import RegexLexer, DelegatingLexer, bygroups, include, \
     using, this
from pygments.token import Punctuation, \
     Text, Comment, Operator, Keyword, Name, String, Number, Literal, Other
from pygments.util import get_choice_opt
from pygments import unistring as uni

from pygments.lexers.web import XmlLexer

__all__ = ['KotlinLexer']


def _escape(st):
    return st.replace(u'\\', ur'\\').replace(u'-', ur'\-').\
           replace(u'[', ur'\[').replace(u']', ur'\]')

class KotlinLexer(RegexLexer):
    """
    For `Kotlin <http://confluence.jetbrains.net/display/Kotlin/>`_
    source code.

    Additional options accepted:

    `unicodelevel`
      Determines which Unicode characters this lexer allows for identifiers.
      The possible values are:

      * ``none`` -- only the ASCII letters and numbers are allowed. This
        is the fastest selection.
      * ``basic`` -- all Unicode characters from the specification except
        category ``Lo`` are allowed.
      * ``full`` -- all Unicode characters as specified in the C# specs
        are allowed.  Note that this means a considerable slowdown since the
        ``Lo`` category has more than 40,000 characters in it!

      The default value is ``basic``.

      *New in Pygments 0.8.*
    """

    name = 'Kotlin'
    aliases = ['kotlin']
    filenames = ['*.kt']
    mimetypes = ['text/x-kotlin'] # inferred

    flags = re.MULTILINE | re.DOTALL | re.UNICODE

    # for the range of allowed unicode characters in identifiers,
    # see http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-334.pdf

    levels = {
        'none': '@?[_a-zA-Z][a-zA-Z0-9_]*',
        'basic': ('@?[_' + uni.Lu + uni.Ll + uni.Lt + uni.Lm + uni.Nl + ']' +
                  '[' + uni.Lu + uni.Ll + uni.Lt + uni.Lm + uni.Nl +
                  uni.Nd + uni.Pc + uni.Cf + uni.Mn + uni.Mc + ']*'),
        'full': ('@?(?:_|[^' +
                 _escape(uni.allexcept('Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl')) + '])'
                 + '[^' + _escape(uni.allexcept('Lu', 'Ll', 'Lt', 'Lm', 'Lo',
                                                'Nl', 'Nd', 'Pc', 'Cf', 'Mn',
                                                'Mc')) + ']*'),
    }

    tokens = {}
    token_variants = True

    for levelname, cs_ident in levels.items():
        tokens[levelname] = {
            'root': [
                # method names
                (r'^([ \t]*(?:' + cs_ident + r'(?:\[\])?\s+)+?)' # return type
                 r'(' + cs_ident + ')'                           # method name
                 r'(\s*)(\()',                               # signature start
                 bygroups(using(this), Name.Function, Text, Punctuation)),
                (r'^\s*\[.*?\]', Name.Attribute),
                (r'[^\S\n]+', Text),
                (r'\\\n', Text), # line continuation
                (r'//.*?\n', Comment.Single),
                (r'/[*](.|\n)*?[*]/', Comment.Multiline),
                (r'\n', Text),
                (r'[~!%^&*()+=|\[\]:;,.<>/?-]', Punctuation),
                (r'[{}]', Punctuation),
                (r'@"(""|[^"])*"', String),
                (r'"(\\\\|\\"|[^"\n])*["\n]', String),
                (r"'\\.'|'[^\\]'", String.Char),
                (r"[0-9](\.[0-9]*)?([eE][+-][0-9]+)?"
                 r"[flFLdD]?|0[xX][0-9a-fA-F]+[Ll]?", Number),
                (r'#[ \t]*(if|endif|else|elif|define|undef|'
                 r'line|error|warning|region|endregion|pragma)\b.*?\n',
                 Comment.Preproc),
                (r'\b(extern)(\s+)(alias)\b', bygroups(Keyword, Text,
                 Keyword)),
                (r'(abstract|as|break|catch|'
                 r'fun|continue|default|delegate|'
                 r'do|else|enum|extern|false|finally|'
                 r'fixed|for|goto|if|implicit|in|interface|'
                 r'internal|is|lock|null|'
                 r'out|override|private|protected|public|readonly|'
                 r'ref|return|sealed|sizeof|'
                 r'when|this|throw|true|try|typeof|'
                 r'unchecked|unsafe|virtual|void|while|'
                 r'get|set|new|partial|yield|val|var)\b', Keyword),
                (r'(global)(::)', bygroups(Keyword, Punctuation)),
                (r'(bool|byte|char|decimal|double|dynamic|float|int|long|short)\b\??', Keyword.Type),
                (r'(class|struct)(\s+)', bygroups(Keyword, Text), 'class'),
                (r'(package|using)(\s+)', bygroups(Keyword, Text), 'package'),
                (cs_ident, Name),
            ],
            'class': [
                (cs_ident, Name.Class, '#pop')
            ],
            'package': [
                (r'(?=\()', Text, '#pop'), # using (resource)
                ('(' + cs_ident + r'|\.)+', Name.Namespace, '#pop')
            ]
        }

    def __init__(self, **options):
        level = get_choice_opt(options, 'unicodelevel', self.tokens.keys(), 'basic')
        if level not in self._all_tokens:
            # compile the regexes now
            self._tokens = self.__class__.process_tokendef(level)
        else:
            self._tokens = self._all_tokens[level]

        RegexLexer.__init__(self, **options)
