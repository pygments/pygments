# -*- coding: utf-8 -*-
"""
    pygments.lexers.xquery
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for xquery language.

    :copyright: Copyright 2010 by Steve Spigarelli
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, ExtendedRegexLexer, bygroups, include, do_insertions
from pygments.token import Text, Comment, Operator, Keyword, Name, \
     String, Number, Punctuation, Literal, Generic
from pygments.lexers.web import XmlLexer


__all__ = ['XQueryLexer']


class XQueryLexer(ExtendedRegexLexer):
		"""
		An XQuery lexer, parsing a stream and outputting the tokens
		needed to highlight xquery code.
		"""
		name = 'XQuery'
		aliases = ['xquery', 'xqy']
		filenames = ['*.xqy', '*.xquery']
		mimetypes = ['text/xquery', 'application/xquery']

 		xquery_parse_state = []

# FIX UNICODE LATER
		#ncnamestartchar = ur"[A-Z]|_|[a-z]|[\u00C0-\u00D6]|[\u00D8-\u00F6]|[\u00F8-\u02FF]|[\u0370-\u037D]|[\u037F-\u1FFF]|[\u200C-\u200D]|[\u2070-\u218F]|[\u2C00-\u2FEF]|[\u3001-\uD7FF]|[\uF900-\uFDCF]|[\uFDF0-\uFFFD]|[\u10000-\uEFFFF]"
		ncnamestartchar = r"[A-Z]|_|[a-z]"
# FIX UNICODE LATER
		#ncnamechar = ncnamestartchar + ur"|-|\.|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]"
		ncnamechar = ncnamestartchar + r"|-|\.|[0-9]"
		ncname = "((%s)+(%s)*)" % (ncnamestartchar, ncnamechar)
		pitarget_namestartchar = r"[A-KN-WY-Z]|_|:|[a-kn-wy-z]"
		pitarget_namechar = pitarget_namestartchar + r"|-|\.|[0-9]"
		pitarget = "(%s)+(%s)*" % (pitarget_namestartchar, pitarget_namechar)
		prefixedname = "%s:%s" % (ncname, ncname)
		unprefixedname = ncname
		qname = "((%s)|(%s))" %(prefixedname, unprefixedname)

		entityref = r'&(lt|gt|amp|quot|apos);'
	 	charref = r'&#[0-9]+;|&#x[0-9a-fA-F]+;'

		stringdouble = r'("((' + entityref + r')|(' + charref + r')|("")|([^&"]))*")'
		stringsingle = r"('((" + entityref + r")|(" + charref + r")|('')|([^&']))*')"

		# FIX UNICODE LATER
		#elementcontentchar = ur'\t|\r|\n|[\u0020-\u0025]|[\u0028-\u003b]|[\u003d-\u007a]|\u007c|[\u007e-\u007F]'
		elementcontentchar = r'[A-Za-z]|\s|\d|[!"#$%\(\)\*\+,\-\./\:;=\?\@\[\\\]^_\'`\|~]'
		#quotattrcontentchar = ur'\t|\r|\n|[\u0020-\u0021]|[\u0023-\u0025]|[\u0027-\u003b]|[\u003d-\u007a]|\u007c|[\u007e-\u007F]'
		quotattrcontentchar = r'[A-Za-z]|\s|\d|[!#$%\(\)\*\+,\-\./\:;=\?\@\[\\\]^_\'`\|~]'
		#aposattrcontentchar = ur'\t|\r|\n|[\u0020-\u0025]|[\u0028-\u003b]|[\u003d-\u007a]|\u007c|[\u007e-\u007F]'
		aposattrcontentchar = r'[A-Za-z]|\s|\d|[!"#$%\(\)\*\+,\-\./\:;=\?\@\[\\\]^_`\|~]'


		# CHAR elements - fix the above elementcontentchar, quotattrcontentchar, aposattrcontentchar
		#x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]

		flags = re.DOTALL | re.MULTILINE | re.UNICODE

		def operator_root_callback(lexer, match, ctx):
			yield match.start(), Operator, match.group(1)
			# transition to root always - don't pop off stack
			ctx.stack = ['root']
			ctx.pos = match.end()

		def popstate_tag_callback(lexer, match, ctx):
			yield match.start(), Name.Tag, match.group(1)
			ctx.stack.append(lexer.xquery_parse_state.pop())
			ctx.pos = match.end()
		
		def popstate_kindtest_callback(lexer, match, ctx):
			yield match.start(), Punctuation, match.group(1)
			next_state = lexer.xquery_parse_state.pop()
			if next_state == 'occurrenceindicator':
				if re.match("[?*+]+", match.group(2)):
					yield match.start(), Punctuation, match.group(2)
					ctx.stack.append('operator')
					ctx.pos = match.end()
				else:
					ctx.stack.append('operator')
					ctx.pos = match.end(1)
			else:
				ctx.stack.append(next_state)
				ctx.pos = match.end()

		def popstate_callback(lexer, match, ctx):
			yield match.start(), Punctuation, match.group(1)
			# if we have run out of our state stack, pop whatever is on the pygments state stack
			if len(lexer.xquery_parse_state) == 0:
				ctx.stack.pop()
			elif len(ctx.stack) > 1:
				ctx.stack.append(lexer.xquery_parse_state.pop())
			else:
				# i don't know if i'll need this, but in case, default back to root
				ctx.stack = ['root']
			ctx.pos = match.end()

		def pushstate_element_content_starttag_callback(lexer, match, ctx):
			yield match.start(), Name.Tag, match.group(1)
			lexer.xquery_parse_state.append('element_content')
			ctx.stack.append('start_tag')
			ctx.pos = match.end()

		def pushstate_cdata_section_callback(lexer, match, ctx):
			yield match.start(), String.Doc, match.group(1)
			ctx.stack.append('cdata_section')
			lexer.xquery_parse_state.append(ctx.state.pop)
			ctx.pos = match.end()

		def pushstate_starttag_callback(lexer, match, ctx):
			yield match.start(), Name.Tag, match.group(1)
			lexer.xquery_parse_state.append(ctx.state.pop)
			ctx.stack.append('start_tag')
			ctx.pos = match.end()

		def pushstate_operator_order_callback(lexer, match, ctx):
			yield match.start(), Keyword, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Punctuation, match.group(3)
			ctx.stack = ['root']
			lexer.xquery_parse_state.append('operator')
			ctx.pos = match.end()

		def pushstate_operator_root_validate(lexer, match, ctx):
			yield match.start(), Keyword, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Punctuation, match.group(3)
			ctx.stack = ['root']
			lexer.xquery_parse_state.append('operator')
			ctx.pos = match.end()

		def pushstate_operator_root_validate_withmode(lexer, match, ctx):
			yield match.start(), Keyword, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Keyword, match.group(3)
			ctx.stack = ['root']
			lexer.xquery_parse_state.append('operator')
			ctx.pos = match.end()

		def pushstate_operator_processing_instruction_callback(lexer, match, ctx):
			yield match.start(), String.Doc, match.group(1)
			ctx.stack.append('processing_instruction')
			lexer.xquery_parse_state.append('operator')
			ctx.pos = match.end()

		def pushstate_operator_cdata_section_callback(lexer, match, ctx):
			yield match.start(), String.Doc, match.group(1)
			ctx.stack.append('cdata_section')
			lexer.xquery_parse_state.append('operator')
			ctx.pos = match.end()

		def pushstate_operator_xmlcomment_callback(lexer, match, ctx):
			yield match.start(), String.Doc, match.group(1)
			ctx.stack.append('xml_comment')
			lexer.xquery_parse_state.append('operator')
			ctx.pos = match.end()

		def pushstate_kindtest_callback(lexer, match, ctx):
			yield match.start(), Keyword, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Punctuation, match.group(3)
			lexer.xquery_parse_state.append('kindtest')
			ctx.stack.append('kindtest')
			ctx.pos = match.end()

		def pushstate_operator_kindtestforpi_callback(lexer, match, ctx):
			yield match.start(), Keyword, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Punctuation, match.group(3)
			lexer.xquery_parse_state.append('operator')
			ctx.stack.append('kindtestforpi')
			ctx.pos = match.end()


		def pushstate_operator_kindtest_callback(lexer, match, ctx):
			yield match.start(), Keyword, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Punctuation, match.group(3)
			lexer.xquery_parse_state.append('operator')
			ctx.stack.append('kindtest')
			ctx.pos = match.end()

		def pushstate_occurrenceindicator_kindtest_callback(lexer, match, ctx):
			yield match.start(), Name.Tag, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Punctuation, match.group(3)
			lexer.xquery_parse_state.append('occurrenceindicator')
			ctx.stack.append('kindtest')
			ctx.pos = match.end()

		def pushstate_operator_starttag_callback(lexer, match, ctx):
			yield match.start(), Name.Tag, match.group(1)
			lexer.xquery_parse_state.append('operator')
			ctx.stack.append('start_tag')
			ctx.pos = match.end()

		def pushstate_operator_root_callback(lexer, match, ctx):
			yield match.start(), Punctuation, match.group(1)
			lexer.xquery_parse_state.append('operator')
			ctx.stack = ['root']#.append('root')
			ctx.pos = match.end()

		def pushstate_operator_root_construct_callback(lexer, match, ctx):
			yield match.start(), Keyword, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Punctuation, match.group(3)
			lexer.xquery_parse_state.append('operator')
			ctx.stack = ['root']
			ctx.pos = match.end()

		def pushstate_root_callback(lexer, match, ctx):
			yield match.start(), Punctuation, match.group(1)
			cur_state = ctx.stack.pop()
			lexer.xquery_parse_state.append(cur_state)
			ctx.stack = ['root']#.append('root')
			ctx.pos = match.end()

		def pushstate_operator_callback(lexer, match, ctx):
			yield match.start(), Keyword, match.group(1)
			yield match.start(), Text, match.group(2)
			yield match.start(), Punctuation, match.group(3)
			lexer.xquery_parse_state.append('operator')
			ctx.pos = match.end()

		tokens = {
				'comment': [
            # xquery comments
						(r'(:\))', Comment, '#pop'),
						(r'(\(:)', Comment, '#push'),
						(r'[^:)]', Comment),
						(r'([^:)]|:|\))', Comment),
				],
				'whitespace': [
						(r'\s+', Text)
						],
				'operator': [
						include('whitespace'),
						(r'(\})', popstate_callback),
						(r'\(:', Comment, 'comment'),

						(r'(\{)', pushstate_root_callback),
						(r'then|else|external|at|div|except', Keyword, 'root'),
						(r'is|mod|order\s+by|stable\s+order\s+by', Keyword, 'root'),
						(r'and|or', Operator.Word, 'root'),
						(r'(eq|ge|gt|le|lt|ne|idiv|intersect|in)(?=\b)', Operator.Word, 'root'),
						(r'return|satisfies|to|union|where|preserve\s+strip', Keyword, 'root'),
						(r'(::|;|>=|>>|>|\[|<=|<<|<|-|\*|!=|\+|//|/|\||:=|,|=)', operator_root_callback),
						(r'(castable|cast)(\s+)(as)', bygroups(Keyword, Text, Keyword), 'singletype'),
						(r'(instance)(\s+)(of)|(treat)(\s+)(as)', bygroups(Keyword, Text, Keyword), 'itemtype'),
						(r'(case)|(as)', Keyword, 'itemtype'),
						(r'(\))(\s*)(as)', bygroups(Punctuation, Text, Keyword), 'itemtype'),
						(r'\$', Name.Variable, 'varname'),
						(r'(for|let)(\s+)(\$)', bygroups(Keyword, Text, Name.Variable), 'varname'),
						#(r'\)|\?|\]', Punctuation, '#push'),
						(r'\)|\?|\]', Punctuation),
						(r'(empty)(\s+)(greatest|least)', bygroups(Keyword, Text, Keyword)),
						(r'ascending|descending|default', Keyword, '#push'),
						(r'external', Keyword),
						(r'collation', Keyword, 'uritooperator'),
						# finally catch all string literals and stay in operator state
						(stringdouble, String.Double),
						(stringsingle, String.Single),

						(r'(catch)(\s*)', bygroups(Keyword, Text), 'root'),
						],
				'uritooperator': [
						(stringdouble, String.Double, '#pop'),
						(stringsingle, String.Single, '#pop')
						],
				'namespacedecl': [
						include('whitespace'),
						(r'\(:', Comment, 'comment'),
						(r'(at)(\s+)'+stringdouble, bygroups(Keyword, Text, String.Double)),
						(r"(at)(\s+)"+stringsingle, bygroups(Keyword, Text, String.Single)),
						(stringdouble, String.Double),
						(stringsingle, String.Single),
						(r',', Punctuation),
						(r'=', Operator),
						(r';', Punctuation, 'root'),
						(ncname, Name.Namespace),
						],
				'namespacekeyword': [
						include('whitespace'),
						(r'\(:', Comment, 'comment'),
						(stringdouble, String.Double, 'namespacedecl'),
						(stringsingle, String.Single, 'namespacedecl'),
						(r'inherit|no-inherit', Keyword, 'root'),
						(r'namespace', Keyword, 'namespacedecl'),
						(r'(default)(\s+)(element)', bygroups(Keyword, Text, Keyword)),
						(r'preserve|no-preserve', Keyword),
						(r',', Punctuation)
						],
				'varname': [
						(r'\(:', Comment, 'comment'),
						(qname, Name.Variable, 'operator'),
						],
				'singletype': [
						(r'\(:', Comment, 'comment'),
						(ncname + r'(:\*)', Name.Variable, 'operator'),
						(qname, Name.Variable, 'operator'),
						],
				'itemtype': [
						include('whitespace'),
						(r'\(:', Comment, 'comment'),
						(r'\$', Punctuation, 'varname'),
						(r'void\s*\(\s*\)', bygroups(Keyword, Text, Punctuation, Text, Punctuation), 'operator'),
						(r'(element|attribute|schema-element|schema-attribute|comment|text|node|binary|document-node)(\s*)(\()', pushstate_occurrenceindicator_kindtest_callback),
						# Marklogic specific type?
						(r'(processing-instruction)(\s*)(\()', bygroups(Keyword, Text, Punctuation), ('occurrenceindicator', 'kindtestforpi')),
						(r'(item)(\s*)(\()(\s*)(\))(?=[*+?])', bygroups(Keyword, Text, Punctuation, Text, Punctuation), 'occurrenceindicator'),
						(r'\(\#', Punctuation, 'pragma'),
						(r';', Punctuation, '#pop'),
						(r'then|else', Keyword, '#pop'),
						(r'(at)(\s+)' + stringdouble, bygroups(Keyword, Text, String.Double), 'namespacedecl'),
						(r'(at)(\s+)' + stringsingle, bygroups(Keyword, Text, String.Single), 'namespacedecl'),
						(r'except|intersect|in|is|return|satisfies|to|union|where', Keyword, 'root'),
						(r'and|div|eq|ge|gt|le|lt|ne|idiv|mod|or', Operator.Word, 'root'),
						(r':=|=|,|>=|>>|>|\[|\(|<=|<<|<|-|!=|\|', Operator, 'root'),
						(r'external|at', Keyword, 'root'),
						(r'(stable)(\s+)(order)(\s+)(by)', bygroups(Keyword, Text, Keyword, Text, Keyword), 'root'),
						(r'(castable|cast)(\s+)(as)', bygroups(Keyword, Text, Keyword), 'singletype'),
						(r'(instance)(\s+)(of)|(treat)(\s+)(as)', bygroups(Keyword, Text, Keyword)),
						(r'case|as', Keyword, 'itemtype'),
						(r'(\))(\s*)(as)', bygroups(Operator, Text, Keyword), 'itemtype'),
						(ncname + r'(:\*)', Keyword.Type, 'operator'),
						(qname, Keyword.Type, 'occurrenceindicator'),
						],
				'kindtest': [
						(r'\(:', Comment, 'comment'),
						(r'({)', Punctuation, 'root'),
						(r'(\))([*+?]?)', popstate_kindtest_callback),
						(r'\*', Name, 'closekindtest'),
						(qname, Name, 'closekindtest'),
						(r'(element|schema-element)(\s*)(\()', pushstate_kindtest_callback)
						],
				'kindtestforpi': [
						(r'\(:', Comment, 'comment'),
						(r'\)', Punctuation, '#pop'),
						(ncname, bygroups(Name.Variable, Name.Variable)),
						(stringdouble, String.Double),
						(stringsingle, String.Single)
						],
				'closekindtest': [
						(r'\(:', Comment, 'comment'),
						(r'(\))', popstate_callback),
						(r',', Punctuation),
						(r'(\{)', pushstate_operator_root_callback),
						(r'\?', Punctuation)
						],
				'xml_comment': [
						(r'-->', String.Doc, '#pop'),
						(r'[^-]{1,2}', Literal)
#						(r'\u009|\u00A|\u00D|[\u0020-\u00D7FF]|[\u00E000-\u00FFFD]|[\u0010000-\u0010FFFF]', Literal)
						],
				'processing_instruction': [
						(r'\s+', Text, 'processing_instruction_content'),
						(r'\?>', String.Doc, '#pop'),
						(pitarget, Name)
						],
				'processing_instruction_content': [
						(r'\?>', String.Doc, '#pop'),
						(r'\u009|\u00A|\u00D|[\u0020-\uD7FF]|[\uE000-\uFFFD]|[\u10000-\u10FFFF]', Literal)
						],
				'cdata_section': [
						(r']]>', String.Doc, '#pop'),
						(r'\u009|\u00A|\u00D|[\u0020-\uD7FF]|[\uE000-\uFFFD]|[\u10000-\u10FFFF]', Literal)
						],
				'start_tag': [
						include('whitespace'),
						(r'(/>)', popstate_tag_callback),
						(r'>', Name.Tag, 'element_content'),
						(r'"', Punctuation, 'quot_attribute_content'),
						(r"'", Punctuation, 'apos_attribute_content'),
						(r'=', Operator),
						(qname, Name.Tag),
						],
				'quot_attribute_content': [
						(r'"', Punctuation, 'start_tag'),
						(r'(\{)', pushstate_root_callback),
						(r'""', Name.Attribute),
						(quotattrcontentchar, Name.Attribute),
						(entityref, Name.Attribute),
						(charref, Name.Attribute),
						(r'\{\{|\}\}', Name.Attribute)
						],
				'apos_attribute_content': [
						(r"'", Punctuation, 'start_tag'),
						(r'\{', Punctuation, 'root'),
						(r"''", Name.Attribute),
						(aposattrcontentchar, Name.Attribute),
						(entityref, Name.Attribute),
						(charref, Name.Attribute),
						(r'\{\{|\}\}', Name.Attribute)
						],
				'element_content': [
						(r'</', Name.Tag, 'end_tag'),
						(r'(\{)', pushstate_root_callback),
						(r'(<!--)', pushstate_operator_xmlcomment_callback),
						(r'(<\?)', pushstate_operator_processing_instruction_callback),
						(r'(<!\[CDATA\[)', pushstate_operator_cdata_section_callback),
						(r'(<)', pushstate_element_content_starttag_callback),
						(elementcontentchar, Literal),
						(entityref, Literal),
						(charref, Literal),
						(r'\{\{|\}\}', Literal)
						],
				'end_tag': [
						include('whitespace'),
						(r'(>)', popstate_tag_callback),
						(qname, Name.Tag)
						],
				'xmlspace_decl': [
						(r'\(:', Comment, 'comment'),
						(r'preserve|strip', Keyword, '#pop')
						],
				'declareordering': [
						(r'\(:', Comment, 'comment'),
						include('whitespace'),
						(r'ordered|unordered', Keyword, '#pop')
						],
				'xqueryversion': [
						include('whitespace'),
						(r'\(:', Comment, 'comment'),
						(stringdouble, String.Double),
						(stringsingle, String.Single),
						(r'encoding', Keyword),
						(r';', Punctuation, '#pop')
						],
				'pragma': [
						(qname, Name.Variable, 'pragmacontents')
						],
				'pragmacontents': [
						(r'#\)', Punctuation, 'operator'),
						(r'\u009|\u00A|\u00D|[\u0020-\u00D7FF]|[\u00E000-\u00FFFD]|[\u0010000-\u0010FFFF]', Literal),
						(r'(\s*)', Text)
						],
				'occurrenceindicator': [
						include('whitespace'),
						(r'\(:', Comment, 'comment'),
						(r'\*|\?|\+', Operator, 'operator'),
						(r':=', Operator, 'root'),
						(r'', Text, 'operator'),
						],
				'option': [
						include('whitespace'),
						(qname, Name.Variable, '#pop')
						],
				'qname_braren': [
						include('whitespace'),
						(r'(\{)', pushstate_operator_root_callback),
						(r'(\()', Punctuation, 'root'),
						],
				'element_qname': [
						(qname, Name.Variable, 'root'),
						],
				'attribute_qname': [
						(qname, Name.Variable, 'root'),
						],
        'root': [
						include('whitespace'),
						(r'\(:', Comment, 'comment'),

						# handle operator state
						# order on numbers matters - handle most complex first
						(r'\d+(\.\d*)?[eE][\+\-]?\d+', Number.Double, 'operator'),
						(r'(\.\d+)[eE][\+\-]?\d+', Number.Double, 'operator'),
						(r'(\.\d+|\d+\.\d*)', Number, 'operator'),
						(r'(\d+)', Number.Integer, 'operator'),
						(r'(\.\.|\.|\)|\*)', Punctuation, 'operator'),
						(r'(declare)(\s+)(construction)', bygroups(Keyword, Text, Keyword), 'operator'),
						(r'(declare)(\s+)(default)(\s+)(order)', bygroups(Keyword, Text, Keyword, Text, Keyword), 'operator'),
						(ncname + ':\*', Name, 'operator'),
						(stringdouble, String.Double, 'operator'),
						(stringsingle, String.Single, 'operator'),


						(r'(\})', popstate_callback),

						#NAMESPACE DECL
						(r'(declare)(\s+)(default)(\s+)(collation)', bygroups(Keyword, Text, Keyword, Text, Keyword)),
						(r'(module|declare)(\s+)(namespace)', bygroups(Keyword, Text, Keyword), 'namespacedecl'),
						(r'(declare)(\s+)(base-uri)', bygroups(Keyword, Text, Keyword), 'namespacedecl'),

						#NAMESPACE KEYWORD
						(r'(declare)(\s+)(default)(\s+)(element|function)', bygroups(Keyword, Text, Keyword, Text, Keyword), 'namespacekeyword'),
						(r'(import)(\s+)(schema|module)', bygroups(Keyword.Psuedo, Text, Keyword.Psuedo), 'namespacekeyword'),
						(r'(declare)(\s+)(copy-namespaces)', bygroups(Keyword, Text, Keyword), 'namespacekeyword'),


						#VARNAMEs
						(r'(for|let|some|every)(\s+)(\$)', bygroups(Keyword, Text, Name.Variable), 'varname'),
						(r'\$', Name.Variable, 'varname'),
						(r'(declare)(\s+)(variable)(\s+)(\$)', bygroups(Keyword, Text, Keyword, Text, Name.Variable), 'varname'),

						#ITEMTYPE
						(r'(\))(\s+)(as)', bygroups(Operator, Text, Keyword), 'itemtype'),

						(r'(element|attribute|schema-element|schema-attribute|comment|text|node|document-node)(\s+)(\()', pushstate_operator_kindtest_callback),

						(r'(processing-instruction)(\s+)(\()', pushstate_operator_kindtestforpi_callback),

						(r'(<!--)', pushstate_operator_xmlcomment_callback),

						(r'(<\?)', pushstate_operator_processing_instruction_callback),

						(r'(<!\[CDATA\[)', pushstate_operator_cdata_section_callback),

				#		(r'</', Name.Tag, 'end_tag'),
						(r'(<)', pushstate_operator_starttag_callback),

						(r'(declare)(\s+)(boundary-space)', bygroups(Keyword, Text, Keyword), 'xmlspace_decl'),

						(r'(validate)(\s+)(lax|strict)', pushstate_operator_root_validate_withmode),
						(r'(validate)(\s*)(\{)', pushstate_operator_root_validate),
						(r'(typeswitch)(\s*)(\()', bygroups(Keyword, Text, Punctuation)),
						(r'(element|attribute)(\s*)(\{)', pushstate_operator_root_construct_callback),

						(r'(document|text|processing-instruction|comment)(\s*)(\{)', pushstate_operator_root_construct_callback),
						#ATTRIBUTE
						(r'(attribute)(\s+)(?=' + qname + r')', bygroups(Keyword, Text), 'attribute_qname'),
						#ELEMENT
						(r'(element)(\s+)(?=' +qname+ r')', bygroups(Keyword, Text), 'element_qname'),
						#PROCESSING_INSTRUCTION
						(r'(processing-instruction)(\s+)' + ncname + r'(\s*)(\{)', bygroups(Keyword, Text, Name.Variable, Text, Punctuation), 'operator'),

						(r'(declare|define)(\s+)(function)', bygroups(Keyword, Text, Keyword)),

						(r'(\{)', pushstate_operator_root_callback),

						(r'(unordered|ordered)(\s*)(\{)', pushstate_operator_order_callback),

						(r'(declare)(\s+)(ordering)', bygroups(Keyword, Text, Keyword), 'declareordering'),

						(r'(xquery)(\s+)(version)', bygroups(Keyword.Psuedo, Text, Keyword.Psuedo), 'xqueryversion'),

						(r'(\(#)', Punctuation, 'pragma'),

						# sometimes return can occur in root state
						(r'return', Keyword),

						(r'(declare)(\s+)(option)', bygroups(Keyword, Text, Keyword), 'option'),

						#URI LITERALS - single and double quoted
						(r'(at)(\s+)('+stringdouble+')', String.Double, 'namespacedecl'),
						(r'(at)(\s+)('+stringsingle+')', String.Single, 'namespacedecl'),


						(r'(ancestor-or-self|ancestor|attribute|child|descendant-or-self)(::)', bygroups(Keyword, Punctuation)),
						(r'(descendant|following-sibling|following|parent|preceding-sibling|preceding|self)(::)', bygroups(Keyword, Punctuation)),

						(r'(if)(\s*)(\()', bygroups(Keyword, Text, Punctuation)),

						(r'then|else', Keyword),

						# ML specific
						(r'(try)(\s*)', bygroups(Keyword, Text), 'root'),
						(r'(catch)(\s*)(\()(\$)', bygroups(Keyword, Text, Punctuation, Name.Variable), 'varname'),

						(r'@' + qname, Name.Attribute),
						(r'@\*', Name.Attribute),
						(r'@' + ncname, Name.Attribute),

						(r'//|/|\+|-|;|,|\(|\)', Punctuation),

						# STANDALONE QNAMES
						(qname + r'(?=\s*[{])', Name.Variable, 'qname_braren'),
						(qname + r'(?=\s*[(])', Name.Function, 'qname_braren'),
						(qname, Name.Variable, 'operator'),
        ]
    }
