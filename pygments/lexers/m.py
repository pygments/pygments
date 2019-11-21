# -*- coding: utf-8 -*-
"""
    pygments.lexers.m
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Cache and GT.M Mumps.

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import re

from pygments.lexer import RegexLexer, words, bygroups
from pygments.token import *

__all__ = ['MLexer']

class MLexer(RegexLexer):
	name = "M"
	aliases = ['m','mumps']
	filenames = ['*.mumps']
	mimetypes = ['text/mumps']

	keywords = (
		'BREAK','CLOSE','DO','ELSE','FOR','GOTO','HALT','HANG','IF','JOB',
		'KILL','LOCK','MERGE','NEW','OPEN','QUIT','READ','SET','USE','VIEW',
		'WRITE','XECUTE','B','C','D','E','F','G','H','I','J','K','L','M','N',
		'O','Q','R','S','U','V','b','c','d','e','f','g','h','i','j','k','l','m',
		'n','o','q','r','s','u','v'
	)

	tokens = {
		'root' : [
			(r'\s*;.*$',Comment),
			(r'^(?:\w|%)+',Name.Function,'func'),
			(r'\"',String,'string'),
			(r'\+|-|\*|/|\\|#|\*\*|!|&|=|_|\[|]|]]|\'|<|<=|>=|\?|@',Operator),
			(r'(\s+)(d|D|DO)(\s)([\w%]+?)(\(.*?\))',bygroups(Whitespace,Keyword,Whitespace,Name.Function,Text)),
			(words(keywords, prefix=r'\b', suffix=r'\b'), Keyword),
			(r'([\w%$]+?)(\^)',bygroups(Name.Function,Punctuation),'tagroutinecall'),
			(r'(?<!\$)\$\w+',Name.Function),
			(r'\$\$\w+',Name.Function),
			(r'\.',Punctuation),
			(r'\s+',Whitespace),
			(r'.',Text)
		],
		'string' : [
			(r'\"',String,'#pop'),
			(r'.+?\"',String,'#pop'),
		],
		'func' : [
			(r'(\()(.+?)(,)',bygroups(Punctuation,Name.Property,Punctuation)),
			(r'(.+?)(,)',bygroups(Name.Property,Punctuation)),
			(r'(\()(.+?)(\))',bygroups(Punctuation,Name.Property,Punctuation),'#pop'),
			(r'(.+?)(\))',bygroups(Name.Property,Punctuation),'#pop'),
			(r'\s*',Whitespace,'#pop')
		],
		'tagroutinecall' : [
			(r'\(.*',Text,"#pop"),
			(r'(.+?)(\()',bygroups(Keyword.Namespace,Punctuation),'#pop')
		]
	}

