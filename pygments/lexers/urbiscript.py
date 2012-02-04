# -*- coding: utf-8 -*-
"""
    pygments.lexers.urbiscript
    ~~~~~~~~~~~~~~~~~~~

    Lexers for urbiscript language.
    Based on JavascriptLexer and CppLexer.

    :copyright: 2011 Clément Prévost.
    :license: BSD, see LICENSE for more details.
"""

import re
try:
    set
except NameError:
    from sets import Set as set

from pygments.lexer import ExtendedRegexLexer, bygroups, using, include, this
from pygments.token import \
     Text, Comment, Operator, Keyword, Name, String, Number, Other, Punctuation
from pygments.util import get_bool_opt, get_list_opt, looks_like_xml, \
                          html_doctype_matches

__all__ = ['UrbiscriptLexer']


class UrbiscriptLexer(ExtendedRegexLexer):
    """
    For JavaScript source code.
    """

    name = 'UrbiScript'
    aliases = ['urbiscript']
    filenames = ['*.u']
    #mimetypes = ['text/plain']

    flags = re.DOTALL

    ## TODO
    # - handle Experimental and deprecated tags with specific tokens
    # - handle Angles and Durations with specific tokens
	
    def blob_callback(lexer, match, ctx):
	text_before_blob = match.group(1)
	blob_start = match.group(2)
	blob_size_str = match.group(3)
	blob_size = int(blob_size_str)	
        yield match.start(), String, text_before_blob 
	ctx.pos += len(text_before_blob) 
	
	# if blob size doesn't match blob format (example : "\B(2)(aaa)")
	# yield blob as a string 
	if ctx.text[match.end() + blob_size] != ")":
		result = "\\B(" + blob_size_str + ")("
        	yield match.start(), String, result
		ctx.pos += len(result)
		return

	# if blob is well formated, yield as Escape
	blob_text = blob_start + ctx.text[match.end():match.end()+blob_size] + ")"
	yield match.start(), String.Escape, blob_text
        ctx.pos = match.end() + blob_size + 1 # +1 is the ending ")"


    tokens = {
        'root': [
            (r'\s+', Text),
            # comments
            (r'//.*?\n', Comment),
            (r'/\*', Comment.Multiline, 'comment'), 
	    (r'(?:every|for|loop|while)(?:;|&|\||,)',Keyword),
            (r'(?:assert|at|break|case|catch|closure|compl|continue|'
	     r'default|else|enum|every|external|finally|for|freezeif|if|new|'
	     r'onleave|return|stopif|switch|this|throw|timeout|try|'
	     r'waituntil|whenever|while)\b', Keyword),
	    (r'(?:asm|auto|bool|char|const_cast|delete|double|dynamic_cast|'
	     r'explicit|export|extern|float|friend|goto|inline|int|'
	     r'long|mutable|namespace|register|reinterpret_cast|short|'
	     r'signed|sizeof|static_cast|struct|template|typedef|typeid|'
             r'typename|union|unsigned|using|virtual|volatile|'
	     r'wchar_t)\b', Keyword.Reserved),
	    # deprecated keywords, use a meaningfull token when available
	    (r'(?:emit|foreach|internal|loopn|static)\b', Keyword),
	    # ignored keywords, use a meaningfull token when available
	    (r'(?:private|protected|public)\b', Keyword),
            (r'(?:var|do|const|function|class)\b', Keyword.Declaration),
            (r'(?:true|false|nil|void)\b', Keyword.Constant),
	    (r'(?:Barrier|Binary|Boolean|CallMessage|Channel|Code|'
	     r'Comparable|Container|Control|Date|Dictionary|Directory|'
	     r'Duration|Enumeration|Event|Exception|Executable|File|Finalizable|'
	     r'Float|FormatInfo|Formatter|Global|Group|Hash|InputStream|'
	     r'IoService|Job|Kernel|Lazy|List|Loadable|Lobby|Location|Logger|Math|'
	     r'Mutex|nil|Object|Orderable|OutputStream|Pair|Path|Pattern|Position|'
	     r'Primitive|Process|Profile|PseudoLazy|PubSub|RangeIterable|Regexp|'
	     r'Semaphore|Server|Singleton|Socket|StackFrame|Stream|String|System|'
	     r'Tag|Timeout|Traceable|TrajectoryGenerator|Triplet|Tuple'
	     r'|UObject|UValue|UVar)\b', Name.Builtin),
	    (r'(?:this)\b', Name.Builtin.Pseudo),
	    (r'(?:[-=+*%/<>~^:]+|\.&?|\|\||&&)', Operator), # don't match single | and &
	    (r'(?:and_eq|and|bitand|bitor|in|not|not_eq|or_eq|or|xor_eq|xor)\b', Operator.Word),
            (r'[{}\[\]()]+', Punctuation),
            (r'(?:;|\||,|&|\?|!)+', Punctuation), 
            (r'[$a-zA-Z_][a-zA-Z0-9_]*', Name.Other),
	    (r'0x[0-9a-fA-F]+', Number.Hex), 
	    # Float, Integer, Angle and Duration
	    (r'(?:[0-9]+(?:(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)?'
	     r'((?:rad|deg|grad)|(?:ms|s|min|h|d))?)\b', Number.Float), 
	    # handle binary blob in strings
            (r'"', String.Double, "string.double"),
            (r"'", String.Single, "string.single"),
        ],
	'string.double': [
            (r'((?:\\\\|\\"|[^"])*?)(\\B\((\d+)\)\()', blob_callback),
            (r'(\\\\|\\"|[^"])*?"', String.Double, '#pop'),
	],
	'string.single': [
            (r"((?:\\\\|\\'|[^'])*?)(\\B\((\d+)\)\()", blob_callback),
            (r"(\\\\|\\'|[^'])*?'", String.Single, '#pop'),
	],
	# from http://pygments.org/docs/lexerdevelopment/#changing-states
	'comment': [
            (r'[^*/]', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline)
        ]		
    }


