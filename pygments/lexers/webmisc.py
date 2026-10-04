"""
    pygments.lexers.webmisc
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for misc. web stuff.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, ExtendedRegexLexer, include, bygroups, \
    default, using, words, LexerContext
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Literal, Whitespace

from pygments.lexers.css import _indentation, _starts_block
from pygments.lexers.html import HtmlLexer
from pygments.lexers.javascript import JavascriptLexer
from pygments.lexers.ruby import RubyLexer

__all__ = ['DuelLexer', 'SlimLexer', 'XQueryLexer', 'QmlLexer', 'CirruLexer']


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


def _transition(action, save=None, push=(), reset=False):
    """Return a callback that yields tokens, saves a parse state and updates the state stack."""
    def callback(lexer, match, ctx):
        yield from action(lexer, match)
        if save == '#current':
            ctx.xquery_parse_state.append(ctx.stack.pop())
        elif save:
            ctx.xquery_parse_state.append(save)
        if reset:
            ctx.stack = ['root']
        ctx.stack.extend(push)
        ctx.pos = match.end()
    return callback


def _restore(action):
    """Return a callback that yields tokens and returns to the last saved parse state."""
    def callback(lexer, match, ctx):
        yield from action(lexer, match)
        if ctx.xquery_parse_state:
            ctx.stack.append(ctx.xquery_parse_state.pop())
        elif len(ctx.stack) > 1:
            ctx.stack.pop()
        ctx.pos = match.end()
    return callback


def _direct_constructors(save):
    """Return the rules for direct constructors that return to the given state."""
    return [
        (r'(<!--)', _transition(bygroups(String.Doc), save=save,
                                push=('xml_comment',))),
        (r'(<\?)', _transition(bygroups(String.Doc), save=save,
                               push=('processing_instruction',))),
        (r'(<!\[CDATA\[)', _transition(bygroups(String.Doc), save=save,
                                       push=('cdata_section',))),
        (r'(<)', _transition(bygroups(Name.Tag), save=save,
                             push=('start_tag',))),
    ]


class _XQueryLexerContext(LexerContext):
    """A lexer context with a stack of the states to return to."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.xquery_parse_state = []


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

    # NameStartChar and NameChar of XML 1.0 5th ed., without the colon
    namestart = (r"A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF"
                 r"\u0370-\u037D\u037F-\u1FFF\u200C-\u200D"
                 r"\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF"
                 r"\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF")
    namechar = namestart + r"\-.0-9\u00B7\u0300-\u036F\u203F-\u2040"
    ncnamestartchar = f"[{namestart}]"
    ncnamechar = f"[{namechar}]"
    ncname = f"(?:{ncnamestartchar}{ncnamechar}*)"
    # a keyword must not be the start of a longer name
    kwend = f"(?![{namechar}:])"
    # a processing instruction target is a name, but never "xml"
    pitarget = f"(?![xX][mM][lL]{kwend})[{namestart}:][{namechar}:]*"
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
    decimal = rf'(?:\.{digits}|{digits}\.{digits}?)'
    double = rf'(?:{decimal}|{digits})[eE][+-]?{digits}'

    entityref = r'(?:&(?:lt|gt|amp|quot|apos|nbsp);)'
    charref = r'(?:&#[0-9]+;|&#x[0-9a-fA-F]+;)'

    stringdouble = r'(?:"(?:' + entityref + r'|' + charref + r'|""|[^&"])*")'
    stringsingle = r"(?:'(?:" + entityref + r"|" + charref + r"|''|[^&'])*')"

    # content is any run of characters that does not end the construct
    elementcontentchar = r'[^{}<&]+'
    quotattrcontentchar = r'[^{}<&"]+'
    aposattrcontentchar = r"[^{}<&']+"

    # kind tests, followed by an optional occurrence indicator in sequence types
    kindtests = (r'element|attribute|schema-element|schema-attribute|comment|'
                 r'text|node|xnode|namespace-node|document-node|binary|'
                 r'empty-sequence')

    flags = re.DOTALL | re.MULTILINE

    def _seq(*words, token=Keyword, end=kwend):
        """Return the pattern and action of keywords separated by whitespace."""
        regex = r'(\s+)'.join(f'({word})' for word in words) + end
        return regex, bygroups(*[token, Whitespace] * (len(words) - 1), token)

    def popstate_kindtest_callback(lexer, match, ctx):
        yield match.start(1), Punctuation, match.group(1)
        next_state = ctx.xquery_parse_state.pop()
        if next_state == 'occurrenceindicator':
            if match.group(2):
                yield match.start(2), Operator, match.group(2)
            ctx.stack.append('operator')
            ctx.pos = match.end()
        else:
            ctx.stack.append(next_state)
            ctx.pos = match.end(1)

    # leave an enclosed expression or a nested state
    popstate_callback = _restore(bygroups(Punctuation))
    restore_tag = _restore(bygroups(Name.Tag))
    restore_literal = _restore(bygroups(String.Doc))
    # continue in the root state
    punctuation_root = _transition(bygroups(Punctuation), reset=True)
    operator_root = _transition(bygroups(Operator), reset=True)
    # enter an enclosed expression, and return to the current state
    enclosed_current = _transition(bygroups(Punctuation), save='#current',
                                   reset=True)
    # enter an expression, and continue with an operator
    enclosed_operator = _transition(bygroups(Punctuation), save='operator',
                                    reset=True)
    # enter the expression of a keyword, e.g. map { ... }
    construct_operator = _transition(
        bygroups(Keyword, Whitespace, Punctuation), save='operator', reset=True)
    # enter a kind test, and return to the given state
    kindtest_type = bygroups(Keyword.Type, Whitespace, Punctuation)
    kindtest_kindtest = _transition(kindtest_type, save='kindtest',
                                    push=('kindtest',))
    kindtest_operator = _transition(kindtest_type, save='operator',
                                    push=('kindtest',))
    kindtest_occurrence = _transition(kindtest_type, save='occurrenceindicator',
                                      push=('kindtest',))

    def get_tokens_unprocessed(self, text=None, context=None):
        ctx = context or _XQueryLexerContext(text, 0)
        yield from super().get_tokens_unprocessed(context=ctx)

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
            (r'\(:', Comment, 'comment'),
        ],
        'strings': [
            (stringdouble, String.Double),
            (stringsingle, String.Single),
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
        # a constant, e.g. an annotation argument or a record field default
        'constant': [
            include('strings'),
            (r'-?0x' + hexdigits, Number.Hex),
            (r'-?0b' + bindigits, Number.Bin),
            (r'-?' + double, Number.Float),
            (r'-?' + decimal, Number.Float),
            (r'-?' + digits, Number.Integer),
            (r'(#)(' + qname + r')', bygroups(Punctuation, String.Symbol)),
            (r'(true|false)(\s*)(\()(\s*)(\))',
             bygroups(Keyword, Whitespace, Punctuation, Whitespace,
                      Punctuation)),
        ],
        # variable bindings of FLWOR and quantified expressions
        'bindings': [
            # 4.0 destructuring let, e.g. let $(a, b) := (1, 2)
            (r'(let)(\s+)(\$)(\s*)([(\[{])',
             bygroups(Keyword, Whitespace, Name.Variable, Whitespace,
                      Punctuation),
             ('operator', 'destructuring')),
            (r'(for)(\s+)(tumbling|sliding)(\s+)(window)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Keyword,
                      Whitespace, Name.Variable),
             'varname'),
            (r'(for)(\s+)(member|key)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Name.Variable),
             'varname'),
            (r'(member|key|value)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Name.Variable), 'varname'),
        ],
        'operator': [
            include('whitespace'),
            # a predicate or array constructor pushes a state, so this pops one
            (r'([\]}])', popstate_callback),
            (r'(\{)', enclosed_current),

            # 4.0 record update, e.g. $r but with { 'x': 1 }
            (*_seq('but', 'with', token=Operator.Word), 'root'),
            (words(('and', 'or', 'div', 'idiv', 'mod', 'eq', 'ne', 'lt', 'le',
                    'gt', 'ge', 'is', 'is-not', 'precedes', 'follows',
                    'precedes-or-is', 'follows-or-is', 'union', 'intersect',
                    'except', 'to', 'otherwise'), suffix=kwend),
             Operator.Word, 'root'),
            (r'(=!>|=>|->|>=|>>|>|<=|<<|<|-|\*|!=|\+|\|\||\||:=|=|!)',
             operator_root),
            (r'(\[)', enclosed_operator),
            (r'(::|:|;|//|/|,)', punctuation_root),

            # XQuery Update Facility
            (*_seq('as', 'first|last', 'into'), 'root'),
            (*_seq('transform', 'with'), 'root'),

            (r'(?:(stable)(\s+))?(order|group)(\s+)(by)' + kwend,
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Keyword),
             'root'),
            (r'(only)(\s+)(end)(?:(\s+)(when))?' + kwend,
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Keyword),
             'root'),
            (*_seq('start|end', 'when'), 'root'),
            _seq('empty', 'greatest|least'),
            _seq('allowing', 'empty'),
            (words(('then', 'else', 'return', 'satisfies', 'where', 'count',
                    'while', 'trace', 'in', 'at', 'external', 'start', 'end',
                    'when', 'finally', 'catch', 'modify', 'into', 'after',
                    'before', 'with'), suffix=kwend), Keyword, 'root'),
            (words(('ascending', 'descending', 'default'), suffix=kwend),
             Keyword),
            (words(('collation',), suffix=kwend), Keyword, 'uritooperator'),

            (*_seq('castable|cast', 'as'), 'singletype'),
            (*_seq('instance', 'of'), 'itemtype'),
            (*_seq('treat', 'as'), 'itemtype'),
            # typeswitch: a parenthesized choice item type, not an expression
            (r'(case)(\s+)(?=\(\s*' + qname + r'\s*[|)])',
             bygroups(Keyword, Whitespace), 'itemtype'),
            # switch: a case clause is followed by an expression, not by a type
            (r'(case)(\s+)(?=[-+\d($"\'])', bygroups(Keyword, Whitespace),
             'root'),
            (words(('case', 'as'), suffix=kwend), Keyword, 'itemtype'),
            (r'(\))(\s*)(as)' + kwend,
             bygroups(Punctuation, Whitespace, Keyword), 'itemtype'),

            include('bindings'),
            (r'(for|let|previous|next)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Name.Variable), 'varname'),
            (r'\$', Name.Variable, 'varname'),
            include('lookup'),
            (r'\)|\?', Punctuation),
            # argument list of a postfix call, e.g. $m?f(1), $a[1](2)
            (r'\(', Punctuation, 'root'),

            # support for current context on rhs of Simple Map Operator
            (r'\.', Operator),

            # finally catch all string literals and stay in operator state
            include('strings'),
        ],
        'uritooperator': [
            include('whitespace'),
            (stringdouble, String.Double, '#pop'),
            (stringsingle, String.Single, '#pop'),
        ],
        'namespacedecl': [
            include('whitespace'),
            (r'(at)(\s+)(?=["\'])', bygroups(Keyword, Whitespace)),
            include('strings'),
            (r',', Punctuation),
            (r'=', Operator),
            (r';', Punctuation, 'root'),
            (ncname, Name.Namespace),
        ],
        'namespacekeyword': [
            include('whitespace'),
            (stringdouble, String.Double, 'namespacedecl'),
            (stringsingle, String.Single, 'namespacedecl'),
            (words(('inherit', 'no-inherit'), suffix=kwend), Keyword, 'root'),
            (words(('namespace',), suffix=kwend), Keyword, 'namespacedecl'),
            _seq('default', 'element'),
            (words(('preserve', 'no-preserve'), suffix=kwend), Keyword),
            (r',', Punctuation),
        ],
        'annotationname': [
            include('whitespace'),
            (r'\%', Name.Decorator),
            (r'(variable)(\s+)(\$)',
             bygroups(Keyword.Declaration, Whitespace, Name.Variable),
             'varname'),
            # 4.0 annotated item type and record declarations
            (r'(type)(\s+)(' + qname + r')(\s+)(as)' + kwend,
             bygroups(Keyword.Declaration, Whitespace, Keyword.Type,
                      Whitespace, Keyword),
             'itemtype'),
            (r'(record)(\s+)(' + qname + r')(\s*)(\()',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Type,
                      Whitespace, Punctuation),
             'recordtest'),
            # not the prefix of an annotation name such as %fn:x
            (words(('function', 'fn'), suffix=kwend), Keyword.Declaration,
             'root'),
            # 4.0 annotation arguments are constants, not just string literals
            (r'[(),]', Punctuation),
            include('constant'),
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
            (r'[)\]}]', Punctuation, '#pop'),
            (r',', Punctuation),
            (words(('as',), suffix=kwend), Keyword, 'recordfieldtype'),
            (r'(\$)(' + qname + r')', bygroups(Name.Variable, Name)),
        ],
        # the target type of a cast, followed by an optional '?'
        'singletype': [
            include('whitespace'),
            (r'(record)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation),
             ('#pop', 'castoptional', 'recordtest')),
            (r'(enum|map|array)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation),
             ('#pop', 'castoptional', 'typeargs')),
            (r'\(', Punctuation, ('#pop', 'castoptional', 'choiceitemtype')),
            (ncname + r':\*', Keyword.Type, ('#pop', 'castoptional')),
            (qname, Keyword.Type, ('#pop', 'castoptional')),
            default('#pop'),
        ],
        'castoptional': [
            (r'\?(?!\?)', Operator, '#pop'),
            default('#pop'),
        ],
        # a type nested in another type, e.g. in map(...) or (a | b)
        'nestedtype': [
            (r'(record)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation), 'recordtest'),
            (r'(' + qname + r')(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation), 'typeargs'),
            (r'\(', Punctuation, 'choiceitemtype'),
            (r'(\%)(' + qname + r')',
             bygroups(Name.Decorator, Name.Decorator)),
            (r'[?*+]', Operator),
            include('strings'),
            (ncname + r':\*', Keyword.Type),
            (qname, Keyword.Type),
        ],
        # 4.0 choice item type, e.g. (xs:date | xs:time)
        'choiceitemtype': [
            include('whitespace'),
            (r'\)', Punctuation, '#pop'),
            (r'\|', Operator),
            include('nestedtype'),
        ],
        # arguments of a parameterized type: map(...), fn(...), enum(...)
        'typeargs': [
            include('whitespace'),
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            (words(('as',), suffix=kwend), Keyword),
            (r'(\$)(' + qname + r')', bygroups(Name.Variable, Name)),
            include('nestedtype'),
        ],
        # sequence type of a record field, ending before the next ',' or ')'
        'recordfieldtype': [
            include('whitespace'),
            include('nestedtype'),
            default('#pop'),
        ],
        # 4.0 record test: record(field as type, ...), record(*)
        'recordtest': [
            include('whitespace'),
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'[?*]', Operator),
            (words(('as',), suffix=kwend), Keyword, 'recordfieldtype'),
            # literal default value of a declared record field
            (r':=', Operator),
            include('constant'),
            (r'(\()(\s*)(\))', bygroups(Punctuation, Whitespace, Punctuation)),
            (ncname, Name.Variable),
        ],
        # a sequence type, followed by an operator
        'itemtype': [
            include('whitespace'),
            (r'\$', Name.Variable, 'varname'),
            (r'(void)(\s*)(\()(\s*)(\))',
             bygroups(Keyword.Type, Whitespace, Punctuation, Whitespace,
                      Punctuation), 'operator'),
            (r'(' + kindtests + r')(\s*)(\()', kindtest_occurrence),
            (r'(processing-instruction)(\s*)(\()', kindtest_type,
             ('occurrenceindicator', 'kindtestforpi')),
            (r'(item)(\s*)(\()(\s*)(\))',
             bygroups(Keyword.Type, Whitespace, Punctuation, Whitespace,
                      Punctuation),
             'occurrenceindicator'),
            # 4.0 annotated function type, e.g. %updating fn(*)
            (r'(\%)(' + qname + r')',
             bygroups(Name.Decorator, Name.Decorator)),
            (r'(record)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation),
             ('occurrenceindicator', 'recordtest')),
            (r'(enum|fn|function|map|array|jnode)(\s*)(\()',
             bygroups(Keyword.Type, Whitespace, Punctuation),
             ('occurrenceindicator', 'typeargs')),
            (r'\(', Punctuation, ('occurrenceindicator', 'choiceitemtype')),
            (ncname + r':\*', Keyword.Type, 'occurrenceindicator'),
            (qname, Keyword.Type, 'occurrenceindicator'),
            default('operator'),
        ],
        'kindtest': [
            include('whitespace'),
            (r'\{', Punctuation, 'root'),
            (r'(\))([*+?]?)', popstate_kindtest_callback),
            # nested kind test, e.g. document-node(element(a))
            (r'(element|schema-element)(\s*)(\()', kindtest_kindtest),
            (r'\*', Name, 'closekindtest'),
            (qname, Name, 'closekindtest'),
        ],
        'kindtestforpi': [
            include('whitespace'),
            (r'\)', Punctuation, '#pop'),
            (ncname, Name.Variable),
            include('strings'),
        ],
        'closekindtest': [
            include('whitespace'),
            (r'(\))', popstate_callback),
            (r',', Punctuation),
            (r'(\{)', enclosed_operator),
            (r'\?', Punctuation),
            # type name of a typed kind test, e.g. element(*, xs:string)
            (qname, Keyword.Type),
        ],
        'xml_comment': [
            (r'(-->)', restore_literal),
            (r'[^-]+', Literal),
            (r'-', Literal),
        ],
        'processing_instruction': [
            (r'\s+', Whitespace, 'processing_instruction_content'),
            # the content state is pushed, so return via the parse state
            (r'(\?>)', restore_literal),
            (pitarget, Name),
        ],
        'processing_instruction_content': [
            (r'(\?>)', restore_literal),
            (r'[^?]+', Literal),
            (r'\?', Literal),
        ],
        'cdata_section': [
            (r'(]]>)', restore_literal),
            (r'[^\]]+', Literal),
            (r'\]', Literal),
        ],
        'start_tag': [
            (r'\s+', Whitespace),
            (r'(/>)', restore_tag),
            (r'>', Name.Tag, 'element_content'),
            (r'"', Punctuation, 'quot_attribute_content'),
            (r"'", Punctuation, 'apos_attribute_content'),
            (r'=', Operator),
            (qname, Name.Tag),
        ],
        # escaped delimiters, before the rules that end the attribute value
        'quot_attribute_content': [
            (r'""', Name.Attribute),
            (r'"', Punctuation, 'start_tag'),
            (quotattrcontentchar, Name.Attribute),
            include('attribute_content'),
        ],
        'apos_attribute_content': [
            (r"''", Name.Attribute),
            (r"'", Punctuation, 'start_tag'),
            (aposattrcontentchar, Name.Attribute),
            include('attribute_content'),
        ],
        'attribute_content': [
            # escaped braces, before the enclosed-expression rule
            (r'\{\{|\}\}', Name.Attribute),
            (r'(\{)', enclosed_current),
            (entityref, Name.Attribute),
            (charref, Name.Attribute),
        ],
        'element_content': [
            (r'</', Name.Tag, 'end_tag'),
            # literal braces, before the enclosed-expression rule
            (r'\{\{|\}\}', Literal),
            (r'(\{)', enclosed_current),
            *_direct_constructors('element_content'),
            (elementcontentchar, Literal),
            (entityref, Literal),
            (charref, Literal),
        ],
        'end_tag': [
            (r'\s+', Whitespace),
            (r'(>)', restore_tag),
            (qname, Name.Tag),
        ],
        # the value of a prolog declaration, e.g. declare ordering ordered;
        'declvalue': [
            include('whitespace'),
            (words(('preserve', 'strip', 'ordered', 'unordered', 'strict',
                    'lax', 'skip'), suffix=kwend), Keyword, '#pop'),
        ],
        'xqueryversion': [
            include('whitespace'),
            include('strings'),
            (words(('encoding',), suffix=kwend), Keyword),
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
            (r';', Punctuation, 'root'),
            (r'=', Operator),
            include('strings'),
            (qname, Name.Variable),
        ],
        # 3.1 string constructor: ``[ text `{ expr }` text ]``
        'stringconstructor': [
            (r'\]``', String.Other, '#pop'),
            (r'(`\{)', enclosed_current),
            (r'`', Punctuation),
            (r'[^`\]]+', String.Other),
            (r'\]', String.Other),
        ],
        # 4.0 string template: `text { expr } text`
        'stringtemplate': [
            (r'\{\{|\}\}|``', String.Other),
            (r'`', String.Other, '#pop'),
            (r'(\{)', enclosed_current),
            (r'[^`{}]+', String.Other),
            (r'\}', String.Other),
        ],
        'qname_braren': [
            include('whitespace'),
            (r'(\{)', enclosed_operator),
            (r'(\()', Punctuation, 'root'),
        ],
        # the name of a computed constructor, e.g. element name { ... }
        'constructorname': [
            (qname, Name.Variable, '#pop'),
        ],
        'root': [
            include('whitespace'),

            # handle operator state
            # order on numbers matters - handle most complex first
            (r'0x' + hexdigits, Number.Hex, 'operator'),
            (r'0b' + bindigits, Number.Bin, 'operator'),
            (double, Number.Float, 'operator'),
            (decimal, Number.Float, 'operator'),
            (digits, Number.Integer, 'operator'),
            # 4.0 QName literal, e.g. #local:name
            (r'(#)(' + qname + r')',
             bygroups(Punctuation, String.Symbol), 'operator'),
            # context item and parent step, as in the operator state
            (r'\.\.|\.', Operator, 'operator'),
            (r'\)', Punctuation, 'operator'),
            (ncname + r':\*', Name.Tag, 'operator'),
            (bracedurilit + r'\*', Name.Tag, 'operator'),
            (r'\*:' + ncname, Name.Tag, 'operator'),
            (r'\*', Name.Tag, 'operator'),
            (stringdouble, String.Double, 'operator'),
            (stringsingle, String.Single, 'operator'),
            (r'``\[', String.Other, ('operator', 'stringconstructor')),
            (r'`', String.Other, ('operator', 'stringtemplate')),

            (r'([\]}])', popstate_callback),

            # PROLOG
            (*_seq('xquery', 'version', token=Keyword.Pseudo), 'xqueryversion'),
            (*_seq('declare', 'boundary-space|construction|ordering|'
                   'revalidation', token=Keyword.Declaration), 'declvalue'),
            (*_seq('declare', 'default', 'order', token=Keyword.Declaration),
             'operator'),
            (*_seq('declare', 'context', 'item|value',
                   token=Keyword.Declaration), 'operator'),
            _seq('declare', 'default', 'collation', token=Keyword.Declaration),
            (*_seq('module|declare', 'namespace|base-uri',
                   token=Keyword.Declaration), 'namespacedecl'),
            (*_seq('declare', 'default', 'element|function',
                   token=Keyword.Declaration), 'namespacekeyword'),
            (*_seq('declare', 'copy-namespaces', token=Keyword.Declaration),
             'namespacekeyword'),
            (*_seq('import', 'schema|module', token=Keyword.Pseudo),
             'namespacekeyword'),
            (r'(declare)(\s+)(?:(default)(\s+))?(decimal-format)' + kwend,
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Declaration),
             'decimalformat'),
            (r'(declare)(\s+)(variable)(\s+)(\$)',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Name.Variable),
             'varname'),
            (r'(declare|define)(\s+)(?:(updating)(\s+))?(function)' + kwend,
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Declaration)),
            # 4.0 item type and named record declarations
            (r'(declare)(\s+)(type)(\s+)(' + qname + r')(\s+)(as)' + kwend,
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Type, Whitespace, Keyword),
             'itemtype'),
            (r'(declare)(\s+)(record)(\s+)(' + qname + r')(\s*)(\()',
             bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration,
                      Whitespace, Keyword.Type, Whitespace, Punctuation),
             'recordtest'),
            (*_seq('declare', 'option', token=Keyword.Declaration), 'option'),
            # annotated declarations
            (r'(declare)(\s+)(\%)',
             bygroups(Keyword.Declaration, Whitespace, Name.Decorator),
             'annotationname'),
            # annotated inline function, e.g. %updating function() { ... }
            (r'(\%)', Name.Decorator, 'annotationname'),

            # VARIABLES
            include('bindings'),
            (r'(for|let|some|every|copy)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Name.Variable), 'varname'),
            (r'\$', Name.Variable, 'varname'),

            # XQuery Update Facility
            _seq('insert|delete', 'nodes?'),
            _seq('replace', 'value', 'of', 'node'),
            _seq('replace|rename', 'node'),
            _seq('invoke', 'updating'),

            # KIND TESTS
            (r'(' + kindtests + r')(\s*)(\()', kindtest_operator),
            (r'(processing-instruction)(\s*)(\()', kindtest_type,
             ('operator', 'kindtestforpi')),

            # DIRECT CONSTRUCTORS
            *_direct_constructors('operator'),

            # COMPUTED CONSTRUCTORS AND OTHER ENCLOSED EXPRESSIONS
            (r'(validate)(\s+)(type)(\s+)(' + qname + r')',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Keyword.Type)),
            _seq('validate', 'lax|strict'),
            (r'(element|attribute|namespace|document|text|comment|'
             r'processing-instruction|map|array|ordered|unordered|validate)'
             r'(\s*)(\{)', construct_operator),
            # 4.0 computed constructor named by a QName literal
            (r'(element|attribute|namespace|processing-instruction)(\s+)(#)'
             r'(' + qname + r')',
             bygroups(Keyword, Whitespace, Punctuation, String.Symbol)),
            (r'(element|attribute|namespace|processing-instruction)(\s+)'
             r'(?=' + qname + r')',
             bygroups(Keyword, Whitespace), 'constructorname'),
            (r'(typeswitch|switch|if)(\s*)(\()',
             bygroups(Keyword, Whitespace, Punctuation)),
            (r'(try)(\s*)(?=\{)', bygroups(Keyword, Whitespace)),
            # Marklogic specific
            (r'(catch)(\s*)(\()(\$)',
             bygroups(Keyword, Whitespace, Punctuation, Name.Variable),
             'varname'),
            (r'(\{|\[)', enclosed_operator),
            (r'(\(#)(\s*)', bygroups(Punctuation, Whitespace), 'pragma'),

            # AXES
            (words(('ancestor', 'ancestor-or-self', 'attribute', 'child',
                    'descendant', 'descendant-or-self', 'following',
                    'following-or-self', 'following-sibling',
                    'following-sibling-or-self', 'item', 'namespace',
                    'parent', 'preceding', 'preceding-or-self',
                    'preceding-sibling', 'preceding-sibling-or-self', 'self'),
                   suffix=r'(::)'),
             bygroups(Keyword, Punctuation)),

            # KEYWORDS
            # 4.0 braced switch and typeswitch cases
            (r'(case)(\s+)(?=\(\s*' + qname + r'\s*[|)])',
             bygroups(Keyword, Whitespace), 'itemtype'),
            (r'(case)(\s+)(?=[-+\d("\'])', bygroups(Keyword, Whitespace)),
            (r'(case)(\s+)(\$)',
             bygroups(Keyword, Whitespace, Name.Variable), 'varname'),
            (words(('case',), suffix=kwend), Keyword, 'itemtype'),
            (words(('then', 'else', 'return', 'default', 'finally'),
                   suffix=kwend), Keyword),

            (r'(@' + qname + ')', Name.Attribute, 'operator'),
            (r'@\*:' + ncname, Name.Attribute, 'operator'),
            (r'@\*', Name.Attribute, 'operator'),
            (r'(@)', Name.Attribute, 'operator'),

            include('lookup'),

            (r':=', Operator),
            (r'\+|-', Operator),
            (r'//|/|;|,|\(|\?', Punctuation),

            # STANDALONE QNAMES
            # 4.0 inline function expression and keyword argument
            # the signature is optional, as in fn { ?height * ?width }
            (r'(function|fn)(?=\s*[({])', Keyword.Declaration),
            (r'(' + qname + r')(\s*)(:=)',
             bygroups(Name.Label, Whitespace, Operator)),
            (qname + r'(?=\s*\{)', Name.Tag, 'qname_braren'),
            (qname + r'(?=\s*\([^:])', Name.Function, 'qname_braren'),
            (r'(' + qname + r')(#)([0-9]+)',
             bygroups(Name.Function, Punctuation, Number.Integer), 'operator'),
            (qname, Name.Tag, 'operator'),
        ]
    }

    del _seq


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
