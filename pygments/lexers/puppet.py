# -*- coding: utf-8 -*-
"""
    pygments.lexers.puppet
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Puppet DSL.

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Text


__all__ = ['PuppetLexer']


class PuppetLexer(RegexLexer):
    name = 'Puppet'
    aliases = ['puppet']
    filenames = ['*.pp']

    tokens = {
        'root': [
            include('comments'),
            include('keywords'),
            include('names'),
            include('numbers'),
            include('operators'),
            include('strings'),

            (r'[]{}:(),;[]', Punctuation),

            # FIXME end of line comments don't work on this rule
            (r'(.*)(include)(\s*)(.*)$',
             bygroups(Text, Keyword, Text, Name.Variable)),

            (r'[^\S\n]+', Text),
        ],

        'comments': [
            (r'\s*#.*$', Comment),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            ],

        'operators': [
            (r'(=>|\?|<|>|=|\+|-|\/|\*|~|!|\|)', Operator),
            (r'\s+(in|and|or|not)\s+', Operator.Word),
            ],

        'names': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name.Attribute),
            # Vars starting with $
            (r'\$\S+', Name.Variable),
        ],

        'numbers': [
            # Copypasta from the Python lexer
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?j?', Number.Float),
            (r'\d+[eE][+-]?[0-9]+j?', Number.Float),
            (r'0[0-7]+j?', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+j?', Number.Integer)
        ],

        'keywords': [
            (r'(absent|alert|alias|audit|augeas|before|case)\b', Keyword),
            (r'(check|class|computer|configured|contained)\b', Keyword),
            (r'(create_resources|crit|cron|debug|default)\b', Keyword),
            (r'(define|defined|directory|else|elsif|emerg|err)\b', Keyword),
            (r'(exec|extlookup|fail|false|file|filebucket)\b', Keyword),
            (r'(fqdn_rand|generate|group|host|if|import)\b', Keyword),
            (r'(include|info|inherits|inline_template)\b', Keyword),
            (r'(installed|interface|k5login|latest|link)\b', Keyword),
            (r'(loglevel|macauthorization|mailalias|maillist)\b', Keyword),
            (r'(mcx|md5|mount|mounted|nagios_command)\b', Keyword),
            (r'(nagios_contact|nagios_contactgroup)\b', Keyword),
            (r'(nagios_host|nagios_hostdependency)\b', Keyword),
            (r'(nagios_hostescalation|nagios_hostextinfo)\b', Keyword),
            (r'(nagios_hostgroup|nagios_service)\b', Keyword),
            (r'(nagios_servicedependency)\b', Keyword),
            (r'(nagios_serviceescalation)\b', Keyword),
            (r'(nagios_serviceextinfo|nagios_servicegroup)\b', Keyword),
            (r'(nagios_timeperiod|node|noop|notice|notify)\b', Keyword),
            (r'(package|present|purged|realize|regsubst)\b', Keyword),
            (r'(require|resources|role|router|running)\b', Keyword),
            (r'(schedule|scheduled_task|search|selboolean)\b', Keyword),
            (r'(selmodule|service|sha1|shellquote|split)\b', Keyword),
            (r'(sprintf|ssh_authorized_key|sshkey|stage)\b', Keyword),
            (r'(stopped|subscribe|tag|tagged|template|tidy)\b', Keyword),
            (r'(true|undef|unmounted|user|versioncmp|vlan)\b', Keyword),
            (r'(warning|yumrepo|zfs|zone|zpool)\b', Keyword),
        ],

        'strings': [
            # Quoted strings
            (r'\'(.*?)\'', String),
            (r'"(.*?)"', String),

            # TODO FIXME more than one new line breaks this
            # Multi-line quoted strings
            (r'\'(.*?)\n(.*?)\'', String),
            (r'"(.*?)\n(.*?)"', String),
        ],

    }
