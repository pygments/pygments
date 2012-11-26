# -*- coding: utf-8 -*-
"""
    pygments.lexers.puppet
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Puppet DSL.

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Comment, Keyword, Name, Number, Operator
from pygments.token import Punctuation, String, Text


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
            (r'(\$\S+)(\[)(\S+)(\])', bygroups(Name.Variable, Punctuation,
                                               String, Punctuation)),
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
            # Left out 'group' and 'require'
            # Since they're often used as attributes
            (r'(?i)\s(absent|alert|alias|audit|augeas|before)\s', Keyword),
            (r'(?i)\s(case|check|class|computer|configured)\s', Keyword),
            (r'(?i)\s(contained|create_resources|crit|cron)\s', Keyword),
            (r'(?i)\s(debug|default|define|defined|directory)\s', Keyword),
            (r'(?i)\s(else|elsif|emerg|err|exec|extlookup)\s', Keyword),
            (r'(?i)\s(fail|false|file|filebucket|fqdn_rand)\s', Keyword),
            (r'(?i)\s(generate|host|if|import|include|info)\s', Keyword),
            (r'(?i)\s(inherits|inline_template|installed)\s', Keyword),
            (r'(?i)\s(interface|k5login|latest|link|loglevel)\s', Keyword),
            (r'(?i)\s(macauthorization|mailalias|maillist)\s', Keyword),
            (r'(?i)\s(mcx|md5|mount|mounted|nagios_command)\s', Keyword),
            (r'(?i)\s(nagios_contact|nagios_contactgroup)\s', Keyword),
            (r'(?i)\s(nagios_host|nagios_hostdependency)\s', Keyword),
            (r'(?i)\s(nagios_hostescalation|nagios_hostextinfo)\s', Keyword),
            (r'(?i)\s(nagios_hostgroup|nagios_service)\s', Keyword),
            (r'(?i)\s(nagios_servicedependency)\s', Keyword),
            (r'(?i)\s(nagios_serviceescalation)\s', Keyword),
            (r'(?i)\s(nagios_serviceextinfo)\s', Keyword),
            (r'(?i)\s(nagios_servicegroup|nagios_timeperiod)\s', Keyword),
            (r'(?i)\s(node|noop|notice|notify)\s', Keyword),
            (r'(?i)\s(package|present|purged|realize|regsubst)\s', Keyword),
            (r'(?i)\s(resources|role|router|running)\s', Keyword),
            (r'(?i)\s(schedule|scheduled_task|search|selboolean)\s', Keyword),
            (r'(?i)\s(selmodule|service|sha1|shellquote|split)\s', Keyword),
            (r'(?i)\s(sprintf|ssh_authorized_key|sshkey|stage)\s', Keyword),
            (r'(?i)\s(stopped|subscribe|tag|tagged|template|tidy)\s', Keyword),
            (r'(?i)\s(true|undef|unmounted|user|versioncmp|vlan)\s', Keyword),
            (r'(?i)\s(warning|yumrepo|zfs|zone|zpool)\s', Keyword),
        ],

        'strings': [
            (r'"([^"])*"', String),
            (r'\'([^\'])*\'', String),
        ],

    }
