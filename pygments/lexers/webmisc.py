"""
    pygments.lexers.webmisc
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for misc. web stuff.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, ExtendedRegexLexer, include, bygroups, \
    default, using
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Literal, Whitespace

from pygments.lexers.css import _indentation, _starts_block
from pygments.lexers.html import HtmlLexer
from pygments.lexers.javascript import JavascriptLexer
from pygments.lexers.ruby import RubyLexer

__all__ = ['DuelLexer', 'SlimLexer', 'XQueryLexer', 'QmlLexer', 'CirruLexer']


def _kindtest_name(match):
    """Yield the name, the optional whitespace and the '(' of a kind test."""
    yield match.start(1), Keyword.Type, match.group(1)
    if match.group(2):
        yield match.start(2), Whitespace, match.group(2)
    yield match.start(3), Punctuation, match.group(3)


class DuelLexer(RegexLexer):
    """
    Lexer for Duel Views Engine (formerly JBST) markup with JavaScript code blocks.
    """

    name = 'Duel'
    url = 'http://duelengine.org/'
    aliases = ['duel', 'jbst', 'jsonml+bst']
    filenames = ['*.duel', '*.jbst']
    mimetypes = ['text/x-duel', 'text/x-jbst']
    version_added = '1.4'

    flags = re.DOTALL

    tokens = {
        'root': [
            (r'(<%[@=#!:]?)(.*?)(%>)',
             bygroups(Name.Tag, using(JavascriptLexer), Name.Tag)),
            (r'(<%\$)(.*?)(:)(.*?)(%>)',
             bygroups(Name.Tag, Name.Function, Punctuation, String, Name.Tag)),
            (r'(<%--)(.*?)(--%>)',
             bygroups(Name.Tag, Comment.Multiline, Name.Tag)),
            (r'(<script.*?>)(.*?)(</script>)',
             bygroups(using(HtmlLexer),
                      using(JavascriptLexer), using(HtmlLexer))),
            (r'(.+?)(?=<)', using(HtmlLexer)),
            (r'.+', using(HtmlLexer)),
        ],
    }


class XQueryLexer(ExtendedRegexLexer):
    """
    An XQuery lexer, parsing a stream and outputting the tokens needed to
    highlight xquery code.
    """
    name = 'XQuery'
    url = 'https://www.w3.org/XML/Query/'
    aliases = ['xquery', 'xqy', 'xq', 'xql', 'xqm']
    filenames = ['*.xqy', '*.xquery', '*.xq', '*.xql', '*.xqm']
    mimetypes = ['text/xquery', 'application/xquery']
    version_added = '1.4'

    xquery_parse_state = []

    # NameStartChar and NameChar of XML 1.0 5th ed., without the colon
    namestart = (r"A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF"
                 r"\u0370-\u037D\u037F-\u1FFF\u200C-\u200D"
                 r"\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF"
                 r"\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF")
    namechar = namestart + r"\-.0-9\u00B7\u0300-\u036F\u203F-\u2040"
    ncnamestartchar = f"[{namestart}]"
    ncnamechar = f"[{namechar}]"
    ncname = f"(?:{ncnamestartchar}{ncnamechar}*)"
    # a processing instruction target is a name, but never "xml"
    pitarget = (f"(?![xX][mM][lL](?![{namechar}:]))"
                f"[{namestart}:][{namechar}:]*")
    prefixedname = f"{ncname}:{ncname}"
    unprefixedname = ncname
    # braced URI literal, e.g. Q{http://www.w3.org/2005/xpath-functions}name
    bracedurilit = r"(?:Q\{[^{}]*\})"
    qname = (f"(?:{bracedurilit}{ncname}|{prefixedname}|"
             f"(?!Q\\{{){unprefixedname})")

    # 4.0 allows underscores between the digits of any numeric literal
    digits = r'(?:[0-9]+(?:_+[0-9]+)*)'
    hexdigits = r'(?:[0-9a-fA-F]+(?:_+[0-9a-fA-F]+)*)'
    bindigits = r'(?:[01]+(?:_+[01]+)*)'

    entityref = r'(?:&(?:lt|gt|amp|quot|apos|nbsp);)'
    charref = r'(?:&#[0-9]+;|&#x[0-9a-fA-F]+;)'

    stringdouble = r'(?:"(?:' + entityref + r'|' + charref + r'|""|[^&"])*")'
    stringsingle = r"(?:'(?:" + entityref + r"|" + charref + r"|''|[^&'])*')"

    # content is any run of characters that does not end the construct
    elementcontentchar = r'[^{}<&]+'
    quotattrcontentchar = r'[^{}<&"]+'
    aposattrcontentchar = r"[^{}<&']+"

    flags = re.DOTALL | re.MULTILINE

    def punctuation_root_callback(lexer, match, ctx):
        yield match.start(), Punctuation, match.group(1)
        # transition to root always - don't pop off stack
        ctx.stack = ['root']
        ctx.pos = match.end()

    def operator_root_callback(lexer, match, ctx):
        yield match.start(), Operator, match.group(1)
        # transition to root always - don't pop off stack
        ctx.stack = ['root']
        ctx.pos = match.end()

    def popstate_tag_callback(lexer, match, ctx):
        yield match.start(), Name.Tag, match.group(1)
        if lexer.xquery_parse_state:
            ctx.stack.append(lexer.xquery_parse_state.pop())
        ctx.pos = match.end()

    def popstate_xmlcomment_callback(lexer, match, ctx):
        yield match.start(), String.Doc, match.group(1)
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
            ctx.pos = match.end(1)

    def popstate_callback(lexer, match, ctx):
        yield match.start(), Punctuation, match.group(1)
        # if we have run out of our state stack, pop whatever is on the pygments
        # state stack
        if len(lexer.xquery_parse_state) == 0:
            ctx.stack.pop()
            if not ctx.stack:
                # make sure we have at least the root state on invalid inputs
                ctx.stack = ['root']
        elif len(ctx.stack) > 1:
            ctx.stack.append(lexer.xquery_parse_state.pop())
        else:
            # an empty enclosed expression pushed no state of its own
            ctx.stack = ['root', lexer.xquery_parse_state.pop()]
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
        yield match.start(), Whitespace, match.group(2)
        yield match.start(), Punctuation, match.group(3)
        ctx.stack = ['root']
        lexer.xquery_parse_state.append('operator')
        ctx.pos = match.end()

    def pushstate_operator_map_callback(lexer, match, ctx):
        yield match.start(), Keyword, match.group(1)
        yield match.start(), Whitespace, match.group(2)
        yield match.start(), Punctuation, match.group(3)
        ctx.stack = ['root']
        lexer.xquery_parse_state.append('operator')
        ctx.pos = match.end()

    def pushstate_operator_root_validate(lexer, match, ctx):
        yield match.start(), Keyword, match.group(1)
        yield match.start(), Whitespace, match.group(2)
        yield match.start(), Punctuation, match.group(3)
        ctx.stack = ['root']
        lexer.xquery_parse_state.append('operator')
        ctx.pos = match.end()

    def pushstate_operator_root_validate_withmode(lexer, match, ctx):
        yield match.start(), Keyword, match.group(1)
        yield match.start(), Whitespace, match.group(2)
        yield match.start(), Keyword, match.group(3)
        # the '{' that follows the mode pushes the parse state
        ctx.stack = ['root']
        ctx.pos = match.end()

    def pushstate_operator_processing_instruction_callback(lexer, match, ctx):
        yield match.start(), String.Doc, match.group(1)
        ctx.stack.append('processing_instruction')
        lexer.xquery_parse_state.append('operator')
        ctx.pos = match.end()

    def pushstate_element_content_processing_instruction_callback(lexer, match, ctx):
        yield match.start(), String.Doc, match.group(1)
        ctx.stack.append('processing_instruction')
        lexer.xquery_parse_state.append('element_content')
        ctx.pos = match.end()

    def pushstate_element_content_cdata_section_callback(lexer, match, ctx):
        yield match.start(), String.Doc, match.group(1)
        ctx.stack.append('cdata_section')
        lexer.xquery_parse_state.append('element_content')
        ctx.pos = match.end()

    def pushstate_operator_cdata_section_callback(lexer, match, ctx):
        yield match.start(), String.Doc, match.group(1)
        ctx.stack.append('cdata_section')
        lexer.xquery_parse_state.append('operator')
        ctx.pos = match.end()

    def pushstate_element_content_xmlcomment_callback(lexer, match, ctx):
        yield match.start(), String.Doc, match.group(1)
        ctx.stack.append('xml_comment')
        lexer.xquery_parse_state.append('element_content')
        ctx.pos = match.end()

    def pushstate_operator_xmlcomment_callback(lexer, match, ctx):
        yield match.start(), String.Doc, match.group(1)
        ctx.stack.append('xml_comment')
        lexer.xquery_parse_state.append('operator')
        ctx.pos = match.end()

    def pushstate_kindtest_callback(lexer, match, ctx):
        yield from _kindtest_name(match)
        lexer.xquery_parse_state.append('kindtest')
        ctx.stack.append('kindtest')
        ctx.pos = match.end()

    def pushstate_operator_kindtestforpi_callback(lexer, match, ctx):
        yield from _kindtest_name(match)
        # the state is left with '#pop', so do not use the parse state here
        ctx.stack.append('operator')
        ctx.stack.append('kindtestforpi')
        ctx.pos = match.end()

    def pushstate_operator_kindtest_callback(lexer, match, ctx):
        yield from _kindtest_name(match)
        lexer.xquery_parse_state.append('operator')
        ctx.stack.append('kindtest')
        ctx.pos = match.end()

    def pushstate_occurrenceindicator_kindtest_callback(lexer, match, ctx):
        yield from _kindtest_name(match)
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
        ctx.stack = ['root']
        ctx.pos = match.end()

    def pushstate_operator_root_construct_callback(lexer, match, ctx):
        yield match.start(), Keyword, match.group(1)
        yield match.start(), Whitespace, match.group(2)
        yield match.start(), Punctuation, match.group(3)
        lexer.xquery_parse_state.append('operator')
        ctx.stack = ['root']
        ctx.pos = match.end()

    def pushstate_root_callback(lexer, match, ctx):
        yield match.start(), Punctuation, match.group(1)
        cur_state = ctx.stack.pop()
        lexer.xquery_parse_state.append(cur_state)
        ctx.stack = ['root']
        ctx.pos = match.end()

    def pushstate_operator_attribute_callback(lexer, match, ctx):
        yield match.start(), Name.Attribute, match.group(1)
        ctx.stack.append('operator')
        ctx.pos = match.end()

    def get_tokens_unprocessed(self, text=None, context=None):
        if context is None:
            # a truncated document must not leave entries behind for the next
            self.xquery_parse_state = []
        yield from super().get_tokens_unprocessed(text, context)

    tokens = {
        'comment': [
            # xquery comments
            (r'[^:()]+', Comment),
            (r'\(:', Comment, '#push'),
            (r':\)', Comment, '#pop'),
            (r'[:()]', Comment),
        ],
        'whitespace': [
            (r'\s+', Whitespace),
        ],
        # 4.0 lookup: ?name, ?"key", ?1, ?$k, ?(expr), ?*, ?.
        'lookup': [
            (r'(\?)(\s*)(\d+)',
             bygroups(Punctuation, Whitespace, Number.Integer), 'operator'),
            (r'(\?)(\s*)([*.])',
             bygroups(Punctuation, Whitespace, Operator), 'operator'),
            (r'(\?)(\s*)(' + ncname + r')',
             bygroups(Punctuation, Whitespace, Name), 'operator'),
            (r'(\?)(\s*)(' + stringdouble + ')',
             bygroups(Punctuation, Whitespace, String.Double), 'operator'),
            (r'(\?)(\s*)(' + stringsingle + ')',
             bygroups(Punctuation, Whitespace, String.Single), 'operator'),
            (r'(\?)(\s*)(\$)',
             bygroups(Punctuation, Whitespace, Name.Variable), 'varname'),
            (r'(\?)(\s*)(\()',
             bygroups(Punctuation, Whitespace, Punctuation), 'root'),
        ],
        'operator': [
            include('whitespace'),
            (r'(\})', popstate_callback),
            # a predicate or array constructor pushes a state, so this pops one
            (r'(\])', popstate_callback),
            (r'\(:', Comment, 'comment'),

            (r'(\{)', pushstate_root_callback),
            (r'then|else|external|at|div|except', Keyword, 'root'),
            (r'order by', Keyword, 'root'),
            (r'group by', Keyword, 'root'),
            # 4.0 node comparisons in word form
            (r'(is-not|precedes-or-is|follows-or-is|precedes|follows)\b',
             Operator.Word, 'root'),
            (r'is|mod|order\s+by|stable\s+order\s+by', Keyword, 'root'),
            (r'and|or', Operator.Word, 'root'),
            (r'(eq|ge|gt|le|lt|ne|idiv|intersect|in|otherwise)\b',
             Operator.Word, 'root'),
            (r'return|satisfies|to|union|where|count|preserve\s+strip',
             Keyword, 'root'),
            # 4.0 while and trace clauses
            (r'(while|trace)\b', Keyword, 'root'),
            (r'finally\b', Keyword),
            (r'(=!>|=>|->|>=|>>|>|<=|<<|<|-|\*|!=|\+|\|\||\||:=|=|!)',
             operator_root_callback),
            (r'(\[)', pushstate_operator_root_callback),
            (r'(::|:|;|//|/|,)',
             punctuation_root_callback),
            (r'(castable|cast)(\s+)(as)\b',
             bygroups(Keyword, Whitespace, Keyword), 'singletype'),
            (r'(instance)(\s+)(of)\b',
             bygroups(Keyword, Whitespace, Keyword), 'itemtype'),
            (r'(treat)(\s+)(as)\b',
             bygroups(Keyword, Whitespace, Keyword), 'itemtype'),
            (r'(case)(\s+)(' + stringdouble + ')',
             bygroups(Keyword, Whitespace, String.Double), 'itemtype'),
            (r'(case)(\s+)(' + stringsingle + ')',
             bygroups(Keyword, Whitespace, String.Single), 'itemtype'),
            # typeswitch: a parenthesized choice item type, not an expression
            (r'(case)(\s+)(?=\(\s*' + qname + r'\s*[|)])',
             bygroups(Keyword, Whitespace), 'itemtype'),
            # switch: a case clause is followed by an expression, not by a type
            (r'(case)(\s+)(?=[-+\d($])', bygroups(Keyword, Whitespace), 'root'),
            (r'(case|as)\b', Keyword, 'itemtype'),
            (r'(\))(\s*)(as)',
             bygroups(Punctuation, Whitespace, Keyword), 'itemtype'),
            (r'\$', Name.Variable, 'varname'),
            (r'(let)(\s+)(\$)(\s*)([(\[{])',
             bygroups(Keyword, Whitespace, Name.Variable, Whitespace,
                      Punctuation),
             ('operator', 'destructuring')),
            (r'(for|let|previous|next)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Name.Variable), 'varname'),
            (r'(for)(\s+)(tumbling|sliding)(\s+)(window)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Keyword,
                      Whitespace, Name.Variable),
             'varname'),
            (r'(for)(\s+)(member|key)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Name.Variable),
             'varname'),
            (r'(member|key|value)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Name.Variable), 'varname'),
            include('lookup'),
            (r'\)|\?', Punctuation),
            # argument list of a postfix call, e.g. $m?f(1), $a[1](2)
            (r'\(', Punctuation, 'root'),
            (r'(empty)(\s+)(greatest|least)',
             bygroups(Keyword, Whitespace, Keyword)),
            (r'ascending|descending|default', Keyword, '#push'),
            (r'(allowing)(\s+)(empty)',
             bygroups(Keyword, Whitespace, Keyword)),
            (r'external', Keyword),
            (r'(only)(\s+)(end)(\s+)(when)\b',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Keyword),
             'root'),
            (r'(start|end)(\s+)(when)\b',
             bygroups(Keyword, Whitespace, Keyword), 'root'),
            (r'(start|when|end)', Keyword, 'root'),
            (r'(only)(\s+)(end)', bygroups(Keyword, Whitespace, Keyword),
             'root'),
            (r'collation', Keyword, 'uritooperator'),

            # eXist specific XQUF
            (r'(into|following|preceding|with)', Keyword, 'root'),

            # support for current context on rhs of Simple Map Operator
            (r'\.', Operator),

            # finally catch all string literals and stay in operator state
            (stringdouble, String.Double),
            (stringsingle, String.Single),

            (r'(catch)(\s*)', bygroups(Keyword, Whitespace), 'root'),
        ],
        'uritooperator': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (stringdouble, String.Double, '#pop'),
            (stringsingle, String.Single, '#pop'),
        ],
        'namespacedecl': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'(at)(\s+)('+stringdouble+')',
             bygroups(Keyword, Whitespace, String.Double)),
            (r"(at)(\s+)("+stringsingle+')',
             bygroups(Keyword, Whitespace, String.Single)),
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
            (r',', Punctuation),
        ],
        'annotationname': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'\%', Name.Decorator),
            (r'(variable)(\s+)(\$)',
             bygroups(Keyword.Declaration, Whitespace, Name.Variable),
             'varname'),
            # 4.0 annotated item type and record declarations
            (r'(type)(\s+)(' + qname + r')(\s+)(as)\b',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Type,
                      Whitespace, Keyword),
             'itemtype'),
            (r'(record)(\s+)(' + qname + r')(\s*)(\()',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Type,
                      Whitespace, Punctuation),
             'recordtest'),
            # not the prefix of an annotation name such as %fn:x
            (r'(function|fn)(?![' + namechar + r':])',
             Keyword.Declaration, 'root'),
            # 4.0 annotation arguments are constants, not just string literals
            (r'[(),]', Punctuation),
            (stringdouble, String.Double),
            (stringsingle, String.Single),
            (r'0x' + hexdigits, Number.Hex),
            (r'0b' + bindigits, Number.Bin),
            (r'-?' + digits + r'\.' + digits + r'?|-?\.' + digits,
             Number.Float),
            (r'-?' + digits, Number.Integer),
            (r'(#)(' + qname + r')', bygroups(Punctuation, String.Symbol)),
            (r'(true|false)(\s*)(\()(\s*)(\))',
             bygroups(Keyword, Whitespace, Punctuation, Whitespace,
                      Punctuation)),
            (qname, Name.Decorator),
        ],
        'varname': [
            (r'\(:', Comment, 'comment'),
            # dynamic function call, e.g. $f(1)
            (r'(' + qname + r')(\()', bygroups(Name, Punctuation), 'root'),
            (qname, Name, 'operator'),
        ],
        # names bound by a destructuring let, e.g. let $( $a, $b ) := (1, 2)
        'destructuring': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'[)\]}]', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'(as)\b', Keyword, 'recordfieldtype'),
            (r'(\$)(' + qname + r')', bygroups(Name.Variable, Name)),
        ],
        'singletype': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            # 4.0 cast targets other than a plain type name
            (r'(record)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation),
             ('operator', 'recordtest')),
            (r'(enum|fn|function|map|array|gnode|jnode)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation),
             ('operator', 'typeargs')),
            (r'\(', Punctuation, ('operator', 'choiceitemtype')),
            (ncname + r':\*', Keyword.Type, 'operator'),
            # the optional occurrence indicator belongs to the single type
            (r'(' + qname + r')(\?)?', bygroups(Keyword.Type, Operator),
             'operator'),
        ],
        # 4.0 choice item type, e.g. (xs:date | xs:time)
        'choiceitemtype': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'\)', Punctuation, '#pop'),
            (r'\|', Operator),
            (r'(record)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation), 'recordtest'),
            (r'(' + qname + r')(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation), 'typeargs'),
            (r'[?*+]', Operator),
            (stringdouble, String.Double),
            (stringsingle, String.Single),
            (ncname + r':\*', Keyword.Type),
            (qname, Keyword.Type),
        ],
        'itemtype': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'\$', Name.Variable, 'varname'),
            (r'(void)(\s*)(\()(\s*)(\))',
             bygroups(Keyword, Text, Punctuation, Text, Punctuation), 'operator'),
            (r'(element|attribute|schema-element|schema-attribute|comment|text|'
             r'node|namespace-node|binary|document-node|empty-sequence)(\s*)(\()',
             pushstate_occurrenceindicator_kindtest_callback),
            # Marklogic specific type?
            (r'(processing-instruction)(\s*)(\()',
             bygroups(Keyword, Text, Punctuation),
             ('occurrenceindicator', 'kindtestforpi')),
            (r'(item)(\s*)(\()(\s*)(\))',
             bygroups(Keyword, Text, Punctuation, Text, Punctuation),
             'occurrenceindicator'),
            (r'(\(\#)(\s*)', bygroups(Punctuation, Text), 'pragma'),
            # 4.0 annotated function type, e.g. %updating fn(*)
            (r'(\%)(' + qname + r')',
             bygroups(Name.Decorator, Name.Decorator)),
            (r'\(', Punctuation, ('occurrenceindicator', 'choiceitemtype')),
            (r';', Punctuation, '#pop'),
            (r'then|else', Keyword, '#pop'),
            (r'(at)(\s+)(' + stringdouble + ')',
             bygroups(Keyword, Text, String.Double), 'namespacedecl'),
            (r'(at)(\s+)(' + stringsingle + ')',
             bygroups(Keyword, Text, String.Single), 'namespacedecl'),
            (r'except|intersect|in|is|return|satisfies|to|union|where|count',
             Keyword, 'root'),
            (r'and|div|eq|ge|gt|le|lt|ne|idiv|mod|or', Operator.Word, 'root'),
            (r':=|=|,|>=|>>|>|\[|\(|<=|<<|<|-|!=|\|\||\|', Operator, 'root'),
            (r'external|at', Keyword, 'root'),
            (r'(stable)(\s+)(order)(\s+)(by)',
             bygroups(Keyword, Text, Keyword, Text, Keyword), 'root'),
            (r'(castable|cast)(\s+)(as)',
             bygroups(Keyword, Text, Keyword), 'singletype'),
            (r'(treat)(\s+)(as)', bygroups(Keyword, Text, Keyword)),
            (r'(instance)(\s+)(of)', bygroups(Keyword, Text, Keyword)),
            (r'(case)(\s+)(' + stringdouble + ')',
             bygroups(Keyword, Text, String.Double), 'itemtype'),
            (r'(case)(\s+)(' + stringsingle + ')',
             bygroups(Keyword, Text, String.Single), 'itemtype'),
            (r'case|as', Keyword, 'itemtype'),
            (r'(\))(\s*)(as)', bygroups(Operator, Text, Keyword), 'itemtype'),
            (ncname + r':\*', Keyword.Type, 'operator'),
            (r'(record)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation),
             ('occurrenceindicator', 'recordtest')),
            (r'(enum|fn|function|map|array|gnode|jnode)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation),
             ('occurrenceindicator', 'typeargs')),
            (qname, Keyword.Type, 'occurrenceindicator'),
        ],
        # 4.0 record test: record(field as type, ...), record(*)
        'recordtest': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'\*', Punctuation),
            (stringdouble, String.Double),
            (stringsingle, String.Single),
            (r'(as)\b', Keyword, 'recordfieldtype'),
            # literal default value of a declared record field
            (r':=', Operator),
            (r'0x' + hexdigits, Number.Hex),
            (r'0b' + bindigits, Number.Bin),
            (r'-?' + digits + r'\.' + digits + r'?|-?\.' + digits,
             Number.Float),
            (r'-?' + digits, Number.Integer),
            (r'(\()(\s*)(\))', bygroups(Punctuation, Whitespace, Punctuation)),
            (ncname, Name.Variable),
        ],
        # sequence type of a record field, ending before the next ',' or ')'
        'recordfieldtype': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'(record)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation), 'recordtest'),
            (r'(' + qname + r')(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation), 'typeargs'),
            (r'\(', Punctuation, 'choiceitemtype'),
            (r'(\%)(' + qname + r')',
             bygroups(Name.Decorator, Name.Decorator)),
            (r'[?*+]', Operator),
            (ncname + r':\*', Keyword.Type),
            (qname, Keyword.Type),
            default('#pop'),
        ],
        # arguments of a parameterized type: map(...), fn(...), enum(...)
        'typeargs': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'\)', Punctuation, '#pop'),
            (r'(record)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation), 'recordtest'),
            (r'(' + qname + r')(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation), '#push'),
            (r'\(', Punctuation, 'choiceitemtype'),
            (r'(\%)(' + qname + r')',
             bygroups(Name.Decorator, Name.Decorator)),
            (r'(as)\b', Keyword),
            (r'(\$)(' + qname + r')', bygroups(Name.Variable, Name)),
            (r'[?*+]', Operator),
            (r',', Punctuation),
            (stringdouble, String.Double),
            (stringsingle, String.Single),
            (ncname + r':\*', Keyword.Type),
            (qname, Keyword.Type),
        ],
        'kindtest': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'\{', Punctuation, 'root'),
            (r'(\))([*+?]?)', popstate_kindtest_callback),
            # nested kind test, e.g. document-node(element(a))
            (r'(element|schema-element)(\s*)(\()', pushstate_kindtest_callback),
            (r'\*', Name, 'closekindtest'),
            (qname, Name, 'closekindtest'),
        ],
        'kindtestforpi': [
            (r'\(:', Comment, 'comment'),
            (r'\)', Punctuation, '#pop'),
            (ncname, Name.Variable),
            (stringdouble, String.Double),
            (stringsingle, String.Single),
        ],
        'closekindtest': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'(\))', popstate_callback),
            (r',', Punctuation),
            (r'(\{)', pushstate_operator_root_callback),
            (r'\?', Punctuation),
            # type name of a typed kind test, e.g. element(*, xs:string)
            (qname, Keyword.Type),
        ],
        'xml_comment': [
            (r'(-->)', popstate_xmlcomment_callback),
            (r'[^-]+', Literal),
            (r'-', Literal),
        ],
        'processing_instruction': [
            (r'\s+', Text, 'processing_instruction_content'),
            # the content state is pushed, so return via the parse state
            (r'(\?>)', popstate_xmlcomment_callback),
            (pitarget, Name),
        ],
        'processing_instruction_content': [
            (r'(\?>)', popstate_xmlcomment_callback),
            (r'[^?]+', Literal),
            (r'\?', Literal),
        ],
        'cdata_section': [
            (r'(]]>)', popstate_xmlcomment_callback),
            (r'[^\]]+', Literal),
            (r'\]', Literal),
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
            # escaped delimiters and braces, before the rules that end them
            (r'""', Name.Attribute),
            (r'\{\{|\}\}', Name.Attribute),
            (r'"', Punctuation, 'start_tag'),
            (r'(\{)', pushstate_root_callback),
            (quotattrcontentchar, Name.Attribute),
            (entityref, Name.Attribute),
            (charref, Name.Attribute),
        ],
        'apos_attribute_content': [
            # escaped delimiters and braces, before the rules that end them
            (r"''", Name.Attribute),
            (r'\{\{|\}\}', Name.Attribute),
            (r"'", Punctuation, 'start_tag'),
            (r'(\{)', pushstate_root_callback),
            (aposattrcontentchar, Name.Attribute),
            (entityref, Name.Attribute),
            (charref, Name.Attribute),
        ],
        'element_content': [
            (r'</', Name.Tag, 'end_tag'),
            # literal braces, before the enclosed-expression rule
            (r'\{\{|\}\}', Literal),
            (r'(\{)', pushstate_root_callback),
            (r'(<!--)', pushstate_element_content_xmlcomment_callback),
            (r'(<\?)', pushstate_element_content_processing_instruction_callback),
            (r'(<!\[CDATA\[)', pushstate_element_content_cdata_section_callback),
            (r'(<)', pushstate_element_content_starttag_callback),
            (elementcontentchar, Literal),
            (entityref, Literal),
            (charref, Literal),
        ],
        'end_tag': [
            include('whitespace'),
            (r'(>)', popstate_tag_callback),
            (qname, Name.Tag),
        ],
        'xmlspace_decl': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'preserve|strip', Keyword, '#pop'),
        ],
        'declareordering': [
            (r'\(:', Comment, 'comment'),
            include('whitespace'),
            (r'ordered|unordered', Keyword, '#pop'),
        ],
        'xqueryversion': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (stringdouble, String.Double),
            (stringsingle, String.Single),
            (r'encoding', Keyword),
            (r';', Punctuation, '#pop'),
        ],
        'pragma': [
            (qname, Name.Variable, 'pragmacontents'),
        ],
        'pragmacontents': [
            (r'#\)', Punctuation, 'operator'),
            (r'[^#]+', Literal),
            (r'#', Literal),
        ],
        'occurrenceindicator': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r'\*|\?|\+', Operator, 'operator'),
            # 4.0 sequence type union, e.g. case xs:date | xs:time
            (r'\|', Operator, 'itemtype'),
            (r':=', Operator, 'root'),
            default('operator'),
        ],
        'option': [
            include('whitespace'),
            (qname, Name.Variable, '#pop'),
        ],
        # declare decimal-format name property="value", ...;
        'decimalformat': [
            include('whitespace'),
            (r'\(:', Comment, 'comment'),
            (r';', Punctuation, 'root'),
            (r'=', Operator),
            (stringdouble, String.Double),
            (stringsingle, String.Single),
            (qname, Name.Variable),
        ],
        # 3.1 string constructor: ``[ text `{ expr }` text ]``
        'stringconstructor': [
            (r'\]``', String.Other, '#pop'),
            (r'(`\{)', pushstate_root_callback),
            (r'`', Punctuation),
            (r'[^`\]]+', String.Other),
            (r'\]', String.Other),
        ],
        # 4.0 string template: `text { expr } text`
        'stringtemplate': [
            (r'\{\{|\}\}|``', String.Other),
            (r'`', String.Other, '#pop'),
            (r'(\{)', pushstate_root_callback),
            (r'[^`{}]+', String.Other),
            (r'\}', String.Other),
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
            (r'0x' + hexdigits, Number.Hex, 'operator'),
            (r'0b' + bindigits, Number.Bin, 'operator'),
            (digits + r'(?:\.' + digits + r')?[eE][+-]?' + digits,
             Number.Float, 'operator'),
            (r'\.' + digits + r'[eE][+-]?' + digits, Number.Float, 'operator'),
            (r'\.' + digits + r'|' + digits + r'\.' + digits + r'?',
             Number.Float, 'operator'),
            (digits, Number.Integer, 'operator'),
            # 4.0 QName literal, e.g. #local:name
            (r'(#)(' + qname + r')',
             bygroups(Punctuation, String.Symbol), 'operator'),
            # context item and parent step, as in the operator state
            (r'\.\.|\.', Operator, 'operator'),
            (r'\)', Punctuation, 'operator'),
            (r'(declare)(\s+)(construction)',
             bygroups(Keyword.Declaration, Text, Keyword.Declaration),
             'xmlspace_decl'),
            (r'(declare)(\s+)(default)(\s+)(order)',
             bygroups(Keyword.Declaration, Text, Keyword.Declaration, Text, Keyword.Declaration), 'operator'),
            (r'(declare)(\s+)(context)(\s+)(item|value)',
             bygroups(Keyword.Declaration, Text, Keyword.Declaration, Text, Keyword.Declaration), 'operator'),
            (ncname + r':\*', Name, 'operator'),
            (bracedurilit + r'\*', Name.Tag, 'operator'),
            (r'\*:'+ncname, Name.Tag, 'operator'),
            (r'\*', Name.Tag, 'operator'),
            (stringdouble, String.Double, 'operator'),
            (stringsingle, String.Single, 'operator'),
            (r'``\[', String.Other, ('operator', 'stringconstructor')),
            (r'`', String.Other, ('operator', 'stringtemplate')),

            (r'(\}|\])', popstate_callback),

            # NAMESPACE DECL
            (r'(declare)(\s+)(default)(\s+)(collation)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Declaration)),
            (r'(module|declare)(\s+)(namespace)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
             'namespacedecl'),
            (r'(declare)(\s+)(base-uri)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
             'namespacedecl'),

            # NAMESPACE KEYWORD
            (r'(declare)(\s+)(default)(\s+)(element|function)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Declaration),
             'namespacekeyword'),
            (r'(import)(\s+)(schema|module)',
             bygroups(Keyword.Pseudo, Whitespace, Keyword.Pseudo),
             'namespacekeyword'),
            (r'(declare)(\s+)(copy-namespaces)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
             'namespacekeyword'),

            # VARNAMEs
            # 4.0 destructuring let, e.g. let $(a, b) := (1, 2)
            (r'(let)(\s+)(\$)(\s*)([(\[{])',
             bygroups(Keyword, Whitespace, Name.Variable, Whitespace,
                      Punctuation),
             ('operator', 'destructuring')),
            (r'(for|let|some|every|member|key|value)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Name.Variable), 'varname'),
            (r'(for)(\s+)(tumbling|sliding)(\s+)(window)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Keyword,
                      Whitespace, Name.Variable),
             'varname'),
            (r'(for)(\s+)(member|key)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Name.Variable),
             'varname'),
            (r'\$', Name.Variable, 'varname'),
            (r'(declare)(\s+)(variable)(\s+)(\$)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Name.Variable),
             'varname'),

            # DECIMAL FORMATS
            (r'(declare)(\s+)(default)(\s+)(decimal-format)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Declaration),
             'decimalformat'),
            (r'(declare)(\s+)(decimal-format)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
             'decimalformat'),

            # ANNOTATED GLOBAL VARIABLES AND FUNCTIONS
            (r'(declare)(\s+)(\%)', bygroups(Keyword.Declaration, Whitespace,
                                             Name.Decorator),
             'annotationname'),
            # annotated inline function, e.g. %updating function() { ... }
            (r'(\%)', Name.Decorator, 'annotationname'),

            # ITEMTYPE
            (r'(\))(\s+)(as)', bygroups(Operator, Whitespace, Keyword),
             'itemtype'),

            (r'(element|attribute|schema-element|schema-attribute|comment|'
             r'text|node|namespace-node|document-node|empty-sequence)(\s*)(\()',
             pushstate_operator_kindtest_callback),

            (r'(processing-instruction)(\s*)(\()',
             pushstate_operator_kindtestforpi_callback),

            (r'(<!--)', pushstate_operator_xmlcomment_callback),

            (r'(<\?)', pushstate_operator_processing_instruction_callback),

            (r'(<!\[CDATA\[)', pushstate_operator_cdata_section_callback),

            # (r'</', Name.Tag, 'end_tag'),
            (r'(<)', pushstate_operator_starttag_callback),

            (r'(declare)(\s+)(boundary-space)',
             bygroups(Keyword.Declaration, Text, Keyword.Declaration), 'xmlspace_decl'),

            (r'(validate)(\s+)(type)(\s+)(' + qname + r')',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Keyword.Type)),
            (r'(validate)(\s+)(lax|strict)',
             pushstate_operator_root_validate_withmode),
            (r'(validate)(\s*)(\{)', pushstate_operator_root_validate),
            (r'(typeswitch)(\s*)(\()', bygroups(Keyword, Whitespace,
                                                Punctuation)),
            (r'(switch)(\s*)(\()', bygroups(Keyword, Whitespace, Punctuation)),
            (r'(element|attribute|namespace)(\s*)(\{)',
             pushstate_operator_root_construct_callback),

            (r'(document|text|processing-instruction|comment)(\s*)(\{)',
             pushstate_operator_root_construct_callback),
            # 4.0 computed constructor named by a QName literal
            (r'(element|attribute|namespace)(\s+)(#)(' + qname + r')',
             bygroups(Keyword, Whitespace, Punctuation, String.Symbol)),
            # ATTRIBUTE
            (r'(attribute)(\s+)(?=' + qname + r')',
             bygroups(Keyword, Whitespace), 'attribute_qname'),
            # ELEMENT
            (r'(element)(\s+)(?=' + qname + r')',
             bygroups(Keyword, Whitespace), 'element_qname'),
            # PROCESSING_INSTRUCTION
            (r'(processing-instruction|namespace)(\s+)(' + ncname + r')(\s*)(\{)',
             bygroups(Keyword, Whitespace, Name.Variable, Whitespace,
                      Punctuation),
             'operator'),

            (r'(declare|define)(\s+)(function)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration)),

            # 4.0 item type and named record declarations
            (r'(declare)(\s+)(type)(\s+)(' + qname + r')(\s+)(as)\b',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Type, Whitespace, Keyword),
             'itemtype'),
            (r'(declare)(\s+)(record)(\s+)(' + qname + r')(\s*)(\()',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Type, Whitespace, Punctuation),
             'recordtest'),

            (r'(\{|\[)', pushstate_operator_root_callback),

            (r'(unordered|ordered)(\s*)(\{)',
             pushstate_operator_order_callback),

            (r'(map|array)(\s*)(\{)',
             pushstate_operator_map_callback),

            (r'(declare)(\s+)(ordering)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
             'declareordering'),

            (r'(xquery)(\s+)(version)',
             bygroups(Keyword.Pseudo, Whitespace, Keyword.Pseudo),
             'xqueryversion'),

            (r'(\(#)(\s*)', bygroups(Punctuation, Whitespace), 'pragma'),

            # sometimes return can occur in root state
            (r'return', Keyword),

            (r'(declare)(\s+)(option)', bygroups(Keyword.Declaration,
                                                 Whitespace,
                                                 Keyword.Declaration),
             'option'),

            # URI LITERALS - single and double quoted
            (r'(at)(\s+)('+stringdouble+')', String.Double, 'namespacedecl'),
            (r'(at)(\s+)('+stringsingle+')', String.Single, 'namespacedecl'),

            (r'(ancestor-or-self|ancestor|attribute|child|descendant-or-self)(::)',
             bygroups(Keyword, Punctuation)),
            (r'(descendant|following-sibling|following|parent|preceding-sibling'
             r'|preceding|self)(::)', bygroups(Keyword, Punctuation)),

            (r'(if)(\s*)(\()', bygroups(Keyword, Whitespace, Punctuation)),

            (r'then|else', Keyword),

            # 4.0 braced switch and typeswitch cases
            (r'(case)(\s+)(?=\(\s*' + qname + r'\s*[|)])',
             bygroups(Keyword, Whitespace), 'itemtype'),
            (r'(case)(\s+)(?=[-+\d("\'])', bygroups(Keyword, Whitespace)),
            (r'(case)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Name.Variable), 'varname'),
            (r'(case)\b', Keyword, 'itemtype'),
            (r'(default|finally)\b', Keyword),

            # eXist specific XQUF
            (r'(update)(\s*)(insert|delete|replace|value|rename)',
             bygroups(Keyword, Whitespace, Keyword)),
            (r'(into|following|preceding|with)', Keyword),

            # Marklogic specific
            (r'(try)(\s*)', bygroups(Keyword, Whitespace), 'root'),
            (r'(catch)(\s*)(\()(\$)',
             bygroups(Keyword, Whitespace, Punctuation, Name.Variable),
             'varname'),


            (r'(@'+qname+')', Name.Attribute, 'operator'),
            (r'(@'+ncname+')', Name.Attribute, 'operator'),
            (r'@\*:'+ncname, Name.Attribute, 'operator'),
            (r'@\*', Name.Attribute, 'operator'),
            (r'(@)', Name.Attribute, 'operator'),

            include('lookup'),

            (r':=', Operator),
            (r'//|/|\+|-|;|,|\(|\)|\?', Punctuation),

            # STANDALONE QNAMES
            # 4.0 inline function expression and keyword argument
            # the signature is optional, as in fn { ?height * ?width }
            (r'(function|fn)(?=\s*[({])', Keyword.Declaration),
            (r'(' + qname + r')(\s*)(:=)',
             bygroups(Name.Label, Whitespace, Operator)),
            (qname + r'(?=\s*\{)', Name.Tag, 'qname_braren'),
            (qname + r'(?=\s*\([^:])', Name.Function, 'qname_braren'),
            (r'(' + qname + ')(#)([0-9]+)', bygroups(Name.Function, Keyword.Type, Number.Integer)),
            (qname, Name.Tag, 'operator'),
        ]
    }


class QmlLexer(RegexLexer):
    """
    For QML files.
    """

    # QML is based on javascript, so much of this is taken from the
    # JavascriptLexer above.

    name = 'QML'
    url = 'https://doc.qt.io/qt-6/qmlapplications.html'
    aliases = ['qml', 'qbs']
    filenames = ['*.qml', '*.qbs']
    mimetypes = ['application/x-qml', 'application/x-qt.qbs+qml']
    version_added = '1.6'

    # pasted from JavascriptLexer, with some additions
    flags = re.DOTALL | re.MULTILINE

    tokens = {
        'commentsandwhitespace': [
            (r'\s+', Text),
            (r'<!--', Comment),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline)
        ],
        'slashstartsregex': [
            include('commentsandwhitespace'),
            (r'/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/'
             r'([gim]+\b|\B)', String.Regex, '#pop'),
            (r'(?=/)', Text, ('#pop', 'badregex')),
            default('#pop')
        ],
        'badregex': [
            (r'\n', Text, '#pop')
        ],
        'root': [
            (r'^(?=\s|/|<!--)', Text, 'slashstartsregex'),
            include('commentsandwhitespace'),
            (r'\+\+|--|~|&&|\?|:|\|\||\\(?=\n)|'
             r'(<<|>>>?|==?|!=?|[-<>+*%&|^/])=?', Operator, 'slashstartsregex'),
            (r'[{(\[;,]', Punctuation, 'slashstartsregex'),
            (r'[})\].]', Punctuation),

            # QML insertions
            (r'\bid\s*:\s*[A-Za-z][\w.]*', Keyword.Declaration,
             'slashstartsregex'),
            (r'\b[A-Za-z][\w.]*\s*:', Keyword, 'slashstartsregex'),

            # the rest from JavascriptLexer
            (r'(for|in|while|do|break|return|continue|switch|case|default|if|else|'
             r'throw|try|catch|finally|new|delete|typeof|instanceof|void|'
             r'this)\b', Keyword, 'slashstartsregex'),
            (r'(var|let|with|function)\b', Keyword.Declaration, 'slashstartsregex'),
            (r'(abstract|boolean|byte|char|class|const|debugger|double|enum|export|'
             r'extends|final|float|goto|implements|import|int|interface|long|native|'
             r'package|private|protected|public|short|static|super|synchronized|throws|'
             r'transient|volatile)\b', Keyword.Reserved),
            (r'(true|false|null|NaN|Infinity|undefined)\b', Keyword.Constant),
            (r'(Array|Boolean|Date|Error|Function|Math|netscape|'
             r'Number|Object|Packages|RegExp|String|sun|decodeURI|'
             r'decodeURIComponent|encodeURI|encodeURIComponent|'
             r'Error|eval|isFinite|isNaN|parseFloat|parseInt|document|this|'
             r'window)\b', Name.Builtin),
            (r'[$a-zA-Z_]\w*', Name.Other),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'"(\\\\|\\[^\\]|[^"\\])*"', String.Double),
            (r"'(\\\\|\\[^\\]|[^'\\])*'", String.Single),
        ]
    }


class CirruLexer(RegexLexer):
    r"""
    * using ``()`` for expressions, but restricted in a same line
    * using ``""`` for strings, with ``\`` for escaping chars
    * using ``$`` as folding operator
    * using ``,`` as unfolding operator
    * using indentations for nested blocks
    """

    name = 'Cirru'
    url = 'http://cirru.org/'
    aliases = ['cirru']
    filenames = ['*.cirru']
    mimetypes = ['text/x-cirru']
    version_added = '2.0'
    flags = re.MULTILINE

    tokens = {
        'string': [
            (r'[^"\\\n]+', String),
            (r'\\', String.Escape, 'escape'),
            (r'"', String, '#pop'),
        ],
        'escape': [
            (r'.', String.Escape, '#pop'),
        ],
        'function': [
            (r'\,', Operator, '#pop'),
            (r'[^\s"()]+', Name.Function, '#pop'),
            (r'\)', Operator, '#pop'),
            (r'(?=\n)', Text, '#pop'),
            (r'\(', Operator, '#push'),
            (r'"', String, ('#pop', 'string')),
            (r'[ ]+', Text.Whitespace),
        ],
        'line': [
            (r'(?<!\w)\$(?!\w)', Operator, 'function'),
            (r'\(', Operator, 'function'),
            (r'\)', Operator),
            (r'\n', Text, '#pop'),
            (r'"', String, 'string'),
            (r'[ ]+', Text.Whitespace),
            (r'[+-]?[\d.]+\b', Number),
            (r'[^\s"()]+', Name.Variable)
        ],
        'root': [
            (r'^\n+', Text.Whitespace),
            default(('line', 'function')),
        ]
    }


class SlimLexer(ExtendedRegexLexer):
    """
    For Slim markup.
    """

    name = 'Slim'
    aliases = ['slim']
    filenames = ['*.slim']
    mimetypes = ['text/x-slim']
    url = 'https://slim-template.github.io'
    version_added = '2.0'

    flags = re.IGNORECASE
    _dot = r'(?: \|\n(?=.* \|)|.)'
    tokens = {
        'root': [
            (r'[ \t]*\n', Text),
            (r'[ \t]*', _indentation),
        ],

        'css': [
            (r'\.[\w:-]+', Name.Class, 'tag'),
            (r'\#[\w:-]+', Name.Function, 'tag'),
        ],

        'eval-or-plain': [
            (r'([ \t]*==?)(.*\n)',
             bygroups(Punctuation, using(RubyLexer)),
             'root'),
            (r'[ \t]+[\w:-]+(?==)', Name.Attribute, 'html-attributes'),
            default('plain'),
        ],

        'content': [
            include('css'),
            (r'[\w:-]+:[ \t]*\n', Text, 'plain'),
            (r'(-)(.*\n)',
             bygroups(Punctuation, using(RubyLexer)),
             '#pop'),
            (r'\|' + _dot + r'*\n', _starts_block(Text, 'plain'), '#pop'),
            (r'/' + _dot + r'*\n', _starts_block(Comment.Preproc, 'slim-comment-block'), '#pop'),
            (r'[\w:-]+', Name.Tag, 'tag'),
            include('eval-or-plain'),
        ],

        'tag': [
            include('css'),
            (r'[<>]{1,2}(?=[ \t=])', Punctuation),
            (r'[ \t]+\n', Punctuation, '#pop:2'),
            include('eval-or-plain'),
        ],

        'plain': [
            (r'([^#\n]|#[^{\n]|(\\\\)*\\#\{)+', Text),
            (r'(#\{)(.*?)(\})',
             bygroups(String.Interpol, using(RubyLexer), String.Interpol)),
            (r'\n', Text, 'root'),
        ],

        'html-attributes': [
            (r'=', Punctuation),
            (r'"[^"]+"', using(RubyLexer), 'tag'),
            (r'\'[^\']+\'', using(RubyLexer), 'tag'),
            (r'\w+', Text, 'tag'),
        ],

        'slim-comment-block': [
            (_dot + '+', Comment.Preproc),
            (r'\n', Text, 'root'),
        ],
    }
