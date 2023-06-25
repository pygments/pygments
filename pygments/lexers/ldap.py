"""
    pygments.lexers.ldap
    ~~~~~~~~~~~~~~~~~~~~

    Pygments lexers for LDAP.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re


from pygments.lexer import RegexLexer, bygroups, default
from pygments.token import Operator, Comment, Keyword, Name, String, \
    Number, Punctuation, Whitespace, Escape

__all__ = ['LdifLexer']


class LdifLexer(RegexLexer):

    """
    Lexer for LDIF

    .. versionadded:: 2.17
    """

    name = 'LDIF'
    aliases = ['ldif']
    filenames = [ "*.ldif" ]
    mimetypes = ["text/x-ldif"]
    url = "https://datatracker.ietf.org/doc/html/rfc2849"

    tokens = {
       'root': [
            (r'\s*\n', Whitespace),
             (r'(-)(\n)', bygroups(Punctuation, Whitespace)),
            (r'(#.*)(\n)', bygroups(Comment.Single, Whitespace)),
            (r'(version)(:)([ \t]*)(.*)([ \t]*\n)', bygroups(Keyword, Punctuation, Whitespace, Number.Integer, Whitespace)),
            (r'(control)(:)([ \t]*)([\.0-9]+)([ \t]+)((?:true|false)?)([ \t]*)',
                bygroups(Keyword, Punctuation, Whitespace, Name.Other, Whitespace, Keyword, Whitespace), "after-control"),
            (r'(deleteoldrdn)(:)([ \n]*)([0-1]+)([ \t]*\n)', bygroups(Keyword, Punctuation, Whitespace, Number, Whitespace)),
            (r'(add|delete|replace)(::?)(\s*)(.*)([ \t]*\n)', bygroups(Keyword, Punctuation, Whitespace, Name.Attribute, Whitespace)),
            (r'(changetype)(:)([ \t]*)([a-z]*)([ \t]*\n)', bygroups(Keyword, Punctuation, Whitespace, Keyword, Whitespace)),
            (r'(dn|newrdn)(::)', bygroups(Keyword, Punctuation), "base64-dn"),
            (r'(dn|newrdn)(:)', bygroups(Keyword, Punctuation), "dn"),
            (r'(objectclass)(:)([ \t]*)([^ \t\n]*)([ \t]*\n)', bygroups(Keyword, Punctuation, Whitespace, Name.Class, Whitespace)),
            (r'([a-zA-Z]*|[0-9][0-9\.]*[0-9])(;)', bygroups(Name.Attribute, Punctuation), "property"),
            (r'([a-zA-Z]*|[0-9][0-9\.]*[0-9])(:<)', bygroups(Name.Attribute, Punctuation), "url"),
            (r'([a-zA-Z]*|[0-9][0-9\.]*[0-9])(::?)', bygroups(Name.Attribute, Punctuation), "value"),
        ],
        "after-control": [
            (r":<", Punctuation, ("#pop", "url")),
            (r"::?", Punctuation, ("#pop", "value")),
            default("#pop"),
        ],
        'property': [
            (r'([-a-zA-Z0-9]*)(;)', bygroups(Name.Property, Punctuation)),
            (r'([-a-zA-Z0-9]*)(:<)', bygroups(Name.Property, Punctuation), ("#pop", "url")),
            (r'([-a-zA-Z0-9]*)(::?)', bygroups(Name.Property, Punctuation), ("#pop", "value")),
        ],
        'value': [
            (r'(\s*)([^\n]+\S)(\n )', bygroups(Whitespace, String, Whitespace)),
            (r'(\s*)([^\n]+\S)(\n)', bygroups(Whitespace, String, Whitespace), "#pop"),
        ],
        'url': [
            (r'([ \t]*)(\S*)([ \t]*\n )', bygroups(Whitespace, Comment.PreprocFile, Whitespace)),
            (r'([ \t]*)(\S*)([ \t]*\n)', bygroups(Whitespace, Comment.PreprocFile, Whitespace), "#pop"),
        ],
        "dn": [
            (r'([ \t]*)([-a-zA-Z0-9\.]+)(=)', bygroups(Whitespace, Name.Attribute, Operator), ("#pop", "dn-value")),
        ],
        "dn-value": [
            (r'\\[^\n]', Escape),
            (r',', Punctuation, ("#pop", "dn")),
            (r'\+', Operator, ("#pop", "dn")),
            (r'[^,\+\n]+', String),
            (r'\n ', Whitespace),
            (r'\n', Whitespace, "#pop"),
        ],
        "base64-dn": [
            (r'([ \t]*)([^ \t\n][^ \t\n]*[^\n])([ \t]*\n )', bygroups(Whitespace, Name, Whitespace)),
            (r'([ \t]*)([^ \t\n][^ \t\n]*[^\n])([ \t]*\n)', bygroups(Whitespace, Name, Whitespace), "#pop"),
        ]
    }
