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

    def analyse_text(text):
        # If the very first line is 'vcl 4.0;' it's pretty much guaranteed
        # that this is VCL
        if re.search('^vcl 4\.0;\n', text):
            return 1.0

        # Skip over comments and blank lines
        # This is accurate enough that returning 0.9 is reasonable.
        # Almost no VCL files start without some comments.
        if re.search('^((\s+)|(#[^\n]*\n)|(\n)|(\s*//[^\n]*\n)|(/\*[^*/]*\*/))*vcl 4\.0;', text):
            return 0.9

        return 0.0

    tokens = {
        'probe': [
            (r'(\s*\.\w+)(\s*=\s*)([^;]*)(;)',
            bygroups(Name.Attribute, Operator, using(this), Punctuation)),
            (r'\s*}', Punctuation, '#pop')
        ],
        'acl': [
            include('whitespace'),
            (r'(\.\w+)(\s*=\s*)([^;]*)(;)',
            bygroups(Name.Attribute, Operator, using(this), Punctuation)),
            (r'}', Punctuation, '#pop')
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
            (r'\d+[sdwhmy]',Literal.Date),
            (r'\d+ms',Literal.Date),
            (r'[~!^&*+=|<>/-]', Operator),
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
            bygroups(Keyword.Namespace, Name.Variable.Global, Punctuation)),
            (words(('vcl_recv','vcl_pipe','vcl_pass','vcl_hash','vcl_purge',
                    'vcl_hit','vcl_miss','vcl_deliver','vcl_synth','vcl_backend_fetch',
                    'vcl_backend_response','vcl_backend_error','vcl_init','vcl_fini'),
                    suffix=r'\b'),Name.Function),
            (words(('if','else','elsif','elif','synth',
                    'synthetic'), suffix=r'\b'),Keyword),
            (r'(new\s+)(\w+)(\s*=)(.*)(;)',
            bygroups(Keyword.Namespace,Name.Variable.Global,Punctuation,Text,Punctuation)),
            (r'(rollback\s*)(\(\s*\)\s*;)',
            bygroups(Keyword,Punctuation)),
            (r'storage\.\w+\.\w+\b', Name.Variable),
            (r'(local|remote)\.ip\b', Name.Variable),
            (r'now\b', Name.Variable),
            (words(('true','false')),Name.Builtin),
            (r'(call \s*)([^\s;]+)(;)',
            bygroups(Keyword,Name.Variable.Global,Punctuation)),
            (r'obj\.ttl',Name.Variable),
            (r'(req_top|req|bereq|obj|resp|beresp)\.http\.[^\s]+\b',Name.Variable),
            (r'(req_top|req|bereq)\.(url|method|xid)\b',Name.Variable),
            (r'(resp|beresp|obj)\.(status|reason)\b',Name.Variable),
            (r'(beresp|obj)\.(ttl|grace)\b',Name.Variable),
            (r'(backend)(\s+\w+)(\s*{)',
            bygroups(Keyword, Name.Variable.Global, Punctuation), 'backend'),
            (r'(ban\s*)(\()(.*)(\)\s*;)',
            bygroups(Keyword,Punctuation,using(this),Punctuation)),
            (r'(probe\s)(\s*\w+\s)({)',
            bygroups(Keyword,Name.Variable.Global,Punctuation),'probe'),
            (r'(acl\s)(\s*\w+\s)({)',
            bygroups(Keyword,Name.Variable.Global,Punctuation),'acl'),
            (r'[();]', Punctuation),
            (r'(client|server)\.(ip|identity)\b',Name.Variable),
            (r'(vcl )(4.0)(;)$',
            bygroups(Keyword.Reserved,Name.Constant,Punctuation)),
            (r'(include\s+)("[^"]+"\s*)(;)',
            bygroups(Keyword,String,Punctuation))
            ],
        'sub': [
            include('whitespace'),
            include('comments'),
            include('returns'),
            include('statements'),
            (r'{',Punctuation,'#push'),
            (r'}',Punctuation,'#pop') 
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
            (r'(return\s)(\()(hash|lookup|ok|deliver|miss|fetch|pass|pipe)(\)\s*;$)',
            bygroups(Keyword, Punctuation, Name.Constant, Punctuation)),
            (r'(return\s)(\()(\s*synth\s*)(\()(\s*\d+\s*)(,)([^)]+)(\)\s*\)\s*;)',
            bygroups(Keyword, Punctuation, Keyword, Punctuation,Number,Punctuation,using(this),Punctuation)),
            (r'(return\s)(\()(\s*synth\s*)(\()(\s*\d+\s*)(\)\s*\)\s*;)',
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
            (r'\<value\>', Name.Variable),
            (r'\.\.\.+', Comment)
            ],
        'snippetspost': [
            (r'(req|bereq|obj|resp|beresp|client|server)(\.http)?\.\*\b',Name.Variable),
            (r'(req|bereq|obj|resp|beresp|client|server)\b',Name.Variable),
            (r'(backend\b)', Keyword.Reserved)
        ],
        'root': [
            include('snippetspre'),
            inherit,
            include('snippetspost')
            ]
    }
