# -*- coding: utf-8 -*-
"""
    pygments.lexers.varnish
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Varnish configuration

    :copyright: Copyright 2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, using, this, inherit, words, \
    default
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Literal

__all__ = ['VCLLexer', 'VCLSnippetLexer']

class VCLLexer(RegexLexer):
    """
    For Varnish Configuration Language (VCL).

    """
    name = 'VCL'
    aliases = ['vcl']
    filenames = [ '*.vcl' ]
    mimetypes = ['text/x-vclsrc']

    tokens = {
        'time': [
            (r'\d+[sdwhm]',Literal.Date)
            ],
        'probe': [
            (r'(\s*\.\w+)(\s*=\s*)([^;]*)(;)',
            bygroups(Name.Attribute, Operator, using(this), Punctuation)),
            (r'\s*}', Punctuation, '#pop')
        ],
        'backend': [
            include('whitespace'),
            (r'(\.host|\.port)(\s*=\s*)([^;]*)(\s*;)',
            bygroups(Name.Attribute, Operator, using(this), Punctuation)),
            (r'(\.probe)(\s*=\s*)(\w+)(;)',
            bygroups(Name.Attribute,Operator,Name.Variable.Global,Punctuation)),
            (r'(\.probe)(\s*=\s*)({)',
            bygroups(Name.Attribute,Operator,Punctuation),'probe'),
            (r'{',Punctuation,'#push'),
            (r'}',Punctuation,'#pop')
        ],
        'statements': [
            include('time'),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'(hash_data)(\()(.+)(\)\s*;\s*$)',
            bygroups(Keyword, Punctuation, using(this), Punctuation)),
            (r'(set\s)([^\s]+)(\s*=\s*)(.+)(\s*;\s*)($|#.*$|//.*$|/\*.*$)',
            bygroups(Keyword, Name.Variable, Punctuation, using(this), Punctuation, using(this))),
            (r'(unset\s)(\s*[^\s]+)(\s*;)',
            bygroups(Keyword, Name.Variable, Punctuation)),
            (r'(regsub\s*)(\()(.*)(,)(.*)(,)(.*)(\))',
            bygroups(Keyword, Punctuation, using(this), Punctuation,
            using(this), Punctuation, using(this), Punctuation)),
            (r'(regsuball\s*)(\()(.*)(,)(.*)(,)(.*)(\))',
            bygroups(Keyword, Punctuation, using(this), Punctuation,
            using(this), Punctuation, using(this), Punctuation)),
            (r'(import\s)(\w+)(;\s*)$',
            bygroups(Keyword, Name.Variable.Global, Punctuation)),
            (words(('vcl_recv','vcl_pipe','vcl_pass','vcl_hash','vcl_purge',
                    'vcl_hit','vcl_miss','vcl_deliver','vcl_synth','vcl_backend_fetch',
                    'vcl_backend_response','vcl_backend_error','vcl_init','vcl_fini'),
                    suffix=r'\b'),Name.Function),
            (words(('if','else','elsif','synth',
                    'synthetic'), suffix=r'\b'),Keyword),
            (words(('true','false')),Name.Builtin),
            (r'(call \s*)([^\s;]+)(;)',
            bygroups(Keyword,Name.Variable.Global,Punctuation)),
            (r'obj\.ttl',Name.Variable),
            (r'(req|bereq|obj|resp|beresp)\.http\.[^\s]+',Name.Variable),
            (r'(req|bereq)\.(url|method|xid)',Name.Variable),
            (r'(resp|beresp|obj)\.(status|reason)',Name.Variable),
            (r'(beresp|obj)\.(ttl|grace)',Name.Variable),
            (r'(backend)(\s+\w+)(\s*{)',
            bygroups(Keyword, Name.Variable.Global, Punctuation), 'backend'),
            (r'(probe )(\s*\w+\s)({)',
            bygroups(Keyword,Name.Variable.Global,Punctuation),'probe'),
            (r'[();]', Punctuation),
            (r'(client|server)\.(ip|identity)',Name.Variable),
            (r'^(vcl )(4.0)(;)$',
            bygroups(Keyword.Reserved,Name.Constant,Punctuation)),
            ],
        'sub': [
            include('whitespace'),
            include('comments'),
            include('returns'),
            include('statements'),
            (r'\s*\{\s*',Punctuation,'#push'),
            (r'\s*\}\s*',Punctuation,'#pop') 
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline)
            ],
        'comments': [
            (r'#.*$', Comment),
            (r'/\*', Comment.Multiline, 'comment'),
            (r'//.*$', Comment)
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'[^"\n]+', String),  # all other characters

            ],
        'multistring': [
            (r'[^"}]', String),
            (r'"}', String, '#pop'),
            (r'["}]', String)
        ],
        'whitespace': [
            (r'L?"', String, 'string'),
            (r'{"', String, 'multistring'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text)  # line continuation
        ],
        'returns': [
            (r'(\s*return )(\()(hash|lookup|ok|deliver|miss|fetch|pass|pipe)(\)\s*;$)',
            bygroups(Keyword, Punctuation, Name.Constant, Punctuation)),
            (r'(\s*return )(\()(\s*synth\s*)(\()(\s*\d+\s*)(,)([^)]+)(\)\s*\)\s*;)',
            bygroups(Keyword, Punctuation, Keyword, Punctuation,Number,Punctuation,using(this),Punctuation)),
            (r'(\s*return )(\()(\s*synth\s*)(\()(\s*\d+\s*)(\)\s*\)\s*;)',
            bygroups(Keyword, Punctuation, Keyword, Punctuation,Number,Punctuation))
        ],
        'root': [
            include('whitespace'),
            include('comments'),
            include('returns'),
            (r'(sub\s+)([a-zA-Z]\w*)(\s*{)',
                bygroups(Keyword, Name.Function, Punctuation),'sub'),
            include('statements'),
            (r'\s+', Text)
        ],
    }

class VCLSnippetLexer(VCLLexer):
    """
    For Varnish Configuration Language snippets.
    """

    name = 'VCLSnippets'
    aliases = ['vclsnippets', 'vclsnippet']
    mimetypes = ['text/x-vclsnippet']
    tokens = {
        'snippetspre': [
            (r'\<variable\>', Name.Variable),
            (r'\<value\>', Name.Variable)
            ],
        'snippetspost': [
            (r'(req|bereq|obj|resp|beresp|client|server)(\.http)?\.\*',Name.Variable),
            (r'(req|bereq|obj|resp|beresp|client|server)',Name.Variable),
            (r'(backend)', Keyword.Reserved)
        ],
        'root': [
            include('snippetspre'),
            inherit,
            include('snippetspost')
            ]
    }
