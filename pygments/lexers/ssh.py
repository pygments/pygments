"""
    pygments.lexers.ssh
    ~~~~~~~~~~~~~~~~~~~

    Lexers for SSH related files.

    :copyright: Copyright 2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups
from pygments.token import Comment, Keyword, Name, Operator, Punctuation, String, Whitespace

__all__ = ['AuthorizedKeysLexer', 'PublicKeyLexer', 'KnownHostsLexer', 'SshConfigLexer', 'SshdConfigLexer']


class AuthorizedKeysLexer(RegexLexer):
    """
    Lexer for SSH authorized_keys files.

    .. versionadded:: 2.10
   """
    name = 'authorized_keys'
    aliases = ['authorized_keys']
    filenames = ['authorized_keys', 'authorized_keys2']

    flags = re.MULTILINE

    tokens = {
        'root': [
            (r'^(\s*)(#.*)$', bygroups(Whitespace, Comment)),  # comments only at beginning of line
            (r'[ \t]+|\n', Whitespace),
            (r',', String.Delimiter),
            (r'([^\s=]+)(=)(")([^"]*?)(")', bygroups(Name.Attribute, Operator, Punctuation, String, Punctuation)),
            (r'([^\s=]+)(=)([^\s,]+)', bygroups(Name.Attribute, Operator, String)),
            (r'((?:sk-)?(?:ecdsa|ssh)-\S+)([ \t]+)(\S+)', bygroups(Keyword.Type, Whitespace, String)),
            (r'\S.*?$', Comment.Special),
            (r'\S+', String),
        ],
    }

    def analyse_text(text):
        if re.search(r'^[ \t]*(\w\S+[ \t]+)?(sk-)?(ecdsa|ssh)-\S+[ \t]+[A-Za-z0-9+/]+={0,2}', text):
            return True


class PublicKeyLexer(RegexLexer):
    """
    Lexer for SSH public key files.

    .. versionadded:: 2.10
    """
    name = 'ssh_pubkey'
    aliases = ['ssh_pubkey', 'sshpub']
    filenames = ['id_*.pub']

    flags = re.MULTILINE

    tokens = {
        'root': [
            (r'^([ \t]*)'
             r'(?P<keytype>\S+)([ \t]+)'
             r'(?P<keydata>\S+)'
             r'(?:([ \t]+)(?P<keycomment>\S.*?))?'
             r'(\s*)$',
             bygroups(
                 Whitespace,
                 Keyword.Type,  # keytype
                 Whitespace,
                 String,  # keydata
                 Whitespace,
                 Comment.Special,  # keycomment
                 Whitespace
             )),
        ]
    }

    def analyse_text(text):
        match = re.search(r'^[ \t]*(sk-)?(ecdsa|ssh)-\S+[ \t]+[A-Za-z0-9+/]+={0,2}(?P<keycomment>[ \t]+\S)?', text)
        if match:
            # 0.9: pubkeys are more likely to contain key comments than known hosts files, 0.8: tie with known hosts
            return 0.9 if match.group('keycomment') else 0.8


class KnownHostsLexer(RegexLexer):
    """
    Lexer for SSH known hosts files.

    .. versionadded:: 2.10
    """
    name = 'known_hosts'
    aliases = ['known_hosts']
    filenames = ['known_hosts', 'known_hosts2']

    flags = re.MULTILINE

    tokens = {
        'root': [
            (r'^(\s*)(#.*?)(\s*)$', bygroups(Whitespace, Comment, Whitespace)),  # comments only at beginning of line
            (r'^([ \t]*)'
             r'(?:(?P<marker>@\S+)([ \t]+))?'
             r'(?P<hostnames>\S+)([ \t]+)'
             r'(?P<keytype>(?:sk-)?(?:ecdsa|ssh)-\S+)([ \t]+)'
             r'(?P<keydata>\S+)'
             r'(?:([ \t]+)(?P<keycomment>\S.*?))?'
             r'(\s*)$',
             bygroups(
                 Whitespace,
                 Name.Class,  # marker
                 Whitespace,
                 Name.Entity,  # hostnames
                 Whitespace,
                 Keyword.Type,  # keytype
                 Whitespace,
                 String,  # keydata
                 Whitespace,
                 Comment.Special,  # keycomment
                 Whitespace,
             )),
        ]
    }

    def analyse_text(text):
        match = re.search(r'^[ \t]*(?P<marker>@[\w-]+[ \t]+)?\S+[ \t]+(sk-)?(ecdsa|ssh)-\S+[ \t]+[A-Za-z0-9+/]+={0,2}', text)
        if match:
            # 0.8: tie with pubkeys
            return True if match.group('marker') else 0.8


class SshConfigLexer(RegexLexer):
    """
    Lexer for SSH client config files.

    .. versionadded:: 2.10
    """
    name = 'SSH client config'
    aliases = ['ssh_config']
    filenames = ['ssh_config']

    tokens = {
        'root': [
            (r'^([ \t]*)(#.*)', bygroups(Whitespace, Comment)),  # comments only at beginning of line
            (r'[ \t]+|\n', Whitespace),
            (r'(?i)(Host)(?:([ \t]*)(=)|([ \t]+))', bygroups(Keyword, Whitespace, Operator, Whitespace), 'host'),
            (r'(?i)(Match)(?:([ \t]*)(=)|([ \t]+))', bygroups(Keyword, Whitespace, Operator, Whitespace), 'match'),
            (r'(\w+)(?:([ \t]*)(=)|([ \t]+))', bygroups(Name.Attribute, Whitespace, Operator, Whitespace), 'attribute'),
        ],
        'host': [
            (r'[ \t]+', Whitespace),
            (r'\n', Whitespace, 'root'),
            (r'!', Operator),
            (r'\S+', String),
        ],
        'match': [
            (r'[ \t]+', Whitespace),
            (r'\n', Whitespace, 'root'),
            (r'([^\s=])+(?:(=)(\S+))?', bygroups(Name.Attribute, Operator, String)),
        ],
        'attribute': [
            (r'[ \t]+', Whitespace),
            (r'\n', Whitespace, 'root'),
            (r',', String.Delimiter),
            (r'[^\s,]+', String),
        ],
    }


class SshdConfigLexer(RegexLexer):
    """
    Lexer for SSH server config files.

    .. versionadded:: 2.10
    """
    name = 'SSH server config'
    aliases = ['sshd_config']
    filenames = ['sshd_config']

    tokens = {
        'root': [
            (r'^([ \t]*)(#.*)', bygroups(Whitespace, Comment)),  # comments only at beginning of line
            (r'[ \t+]|\n', Whitespace),
            (r'(?i)(Match)([ \t]+)', bygroups(Keyword, Whitespace), 'match'),
            (r'(\w+)([ \t]+)', bygroups(Name.Attribute, Whitespace), 'attribute'),
        ],
        'match': [
            (r'[ \t]+', Whitespace),
            (r'\n', Whitespace, 'root'),
            (r'(?i)(User|Group|Host|Address)([ \t]+)', bygroups(Name.Attribute, Whitespace)),
            (r'!', Operator),
            (r'\S+', String),
        ],
        'attribute': [
            (r'[ \t]+', Whitespace),
            (r'\n', Whitespace, 'root'),
            (r',', String.Delimiter),
            (r'[^\s,]+', String),
        ],
    }
