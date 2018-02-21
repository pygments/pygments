# -*- coding: utf-8 -*-
"""
    pygments.lexers.yang
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for YANG (IETF RFC 6020) modules
    https://tools.ietf.org/html/rfc6020
"""

from pygments.lexer import include, bygroups, RegexLexer
from pygments.token import *
import re

__all__ = ['YangLexer']

# TODO:
#  - More testing:
#   + for completeness, and
#   + weird, but compliant line formatting
#  - Support string concatenation with '+'
#  - Look closer at ABNF in RFC6020 for more fine-grained types
#    above and beyond the current (_str, _id, and _nodeid)
#  - We don't support YANG extensions (i.e. <prefix>:<keyword> form)

class YangLexer(RegexLexer):
	name = 'YANG'
	aliases = ['yang', 'Yang']
	filenames = ['*.yang']
	mimetypes = ['application/yang']

	# Loose interpretations of ABNF in RFC 6020
	# Single or double multi-line, or unquoted single-line string terminated
	# by semicolon
	_str = r'(\s+(?s)["\'].+?["\']|\s+[A-Za-z0-9\_\-\.\:\/\[\]]+)(;)'
	_date = r'(["\']?[0-9]{4}\-[0-9]{2}\-[0-9]{2}["\']?)'
	# Roughly equivalent to identifier-arg-str
	_id = r'(["\']?[A-Za-z_][A-Za-z0-9\_\-\.\:]+["\']?)'
	_nodeid = r'(["\']?[A-Za-z_][A-Za-z0-9\_\-\.\:\/]+["\']?)'
	_instanceid = r'(["\']?\/[A-Za-z_][A-Za-z0-9\_\-\.\:\/]+["\']?)'

	tokens = {
		'whitespace_stmts': [
			(r'\s+', Text)
		],
		'top_stmts': [
			(r'(module\s+)' + _id, bygroups(Keyword.Namespace, String)),
			(r'(submodule\s+)' + _id, bygroups(Keyword.Namespace, String)),
		],
		'module_header_stmts': [
			(r'(yang-version\s+)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(namespace)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(prefix)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(belongs-to\s+)' + _id, bygroups(Token.Keyword, String)),
		],
		'linkage_stmts': [
			(r'(import\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(include\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(revision-date\s+)' + _date, bygroups(Token.Keyword, String)),
		],
		'meta_stmts': [
			(r'(organization)' + _str, bygroups(Token.Keyword, String.Doc, Token.Punctuation)),
			(r'(contact)' + _str, bygroups(Token.Keyword, String.Doc, Token.Punctuation)),
			(r'(description)' + _str, bygroups(Token.Keyword, String.Doc, Token.Punctuation)),
			(r'(reference)' + _str, bygroups(Token.Keyword, String.Doc, Token.Punctuation)),
			(r'(revision\s+)' + _date, bygroups(Token.Keyword, String)),
		],
		'data_def_stmts': [
			(r'(container\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(leaf\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(leaf-list\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(list\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(choice\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(case\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(uses\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(refine\s+)' + _nodeid, bygroups(Token.Keyword, String)),
			(r'(anyxml\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(config)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(presence)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(when)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(must)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(error-message)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(error-app-tag)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
		],
		'body_stmts': [
			(r'(extension\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(argument\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(if-feature\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(feature\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(identity\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(typedef\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(grouping\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(rpc\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(input)', Token.Keyword),
			(r'(output)', Token.Keyword),
			(r'(notification\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(deviation\s+)' + _instanceid, bygroups(Token.Keyword, String)),
			(r'(deviate\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(augment\s+)' + _instanceid, bygroups(Token.Keyword, String)),
			(r'(uses\s+)' + _id, bygroups(Token.Keyword, String)),

		],
		'type_stmts': [
			(r'(type\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(units)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(default)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(status\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(reference\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(fraction-digits)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(range)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(length)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(pattern)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(enum\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(value)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(bit\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(path)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(require-instance)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(base\s+)' + _id, bygroups(Token.Keyword, String)),
			(r'(min-elements)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(max-elements)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(ordered-by)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
		],
		'list_stmts': [
			(r'(key)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(unique)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
			(r'(mandatory)' + _str, bygroups(Token.Keyword, String, Token.Punctuation)),
		],
		'comment_stmts': [
			(r'[^*/]', Comment.Multiline),
			(r'/\*', Comment.Multiline, '#push'),
			(r'\*/', Comment.Multiline, '#pop'),
			(r'[*/]', Comment.Multiline),
		],

		'root': [
			(r'{', Token.Punctuation),
			(r'}', Token.Punctuation),
			(r';', Token.Punctuation),
			include('top_stmts'),
			include('module_header_stmts'),
			include('linkage_stmts'),
			include('meta_stmts'),
			include('data_def_stmts'),
			include('body_stmts'),
			include('type_stmts'),
			include('list_stmts'),
			(r'/\*', Comment.Multiline, 'comment_stmts'),
			(r'//.*?$', Comment.Singleline),
			include('whitespace_stmts'),
		]
	}