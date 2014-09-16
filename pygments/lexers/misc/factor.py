# -*- coding: utf-8 -*-
"""
    pygments.lexers.misc.factor
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for the Factor language.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, default
from pygments.token import Text, Comment, Keyword, Name, String, Number

__all__ = ['FactorLexer']


class FactorLexer(RegexLexer):
    """
    Lexer for the `Factor <http://factorcode.org>`_ language.

    .. versionadded:: 1.4
    """
    name = 'Factor'
    aliases = ['factor']
    filenames = ['*.factor']
    mimetypes = ['text/x-factor']

    flags = re.MULTILINE | re.UNICODE

    builtin_kernel = (
        r'(?:-rot|2bi|2bi@|2bi\*|2curry|2dip|2drop|2dup|2keep|2nip|'
        r'2over|2tri|2tri@|2tri\*|3bi|3curry|3dip|3drop|3dup|3keep|'
        r'3tri|4dip|4drop|4dup|4keep|<wrapper>|=|>boolean|\(clone\)|'
        r'\?|\?execute|\?if|and|assert|assert=|assert\?|bi|bi-curry|'
        r'bi-curry@|bi-curry\*|bi@|bi\*|boa|boolean|boolean\?|both\?|'
        r'build|call|callstack|callstack>array|callstack\?|clear|clone|'
        r'compose|compose\?|curry|curry\?|datastack|die|dip|do|drop|'
        r'dup|dupd|either\?|eq\?|equal\?|execute|hashcode|hashcode\*|'
        r'identity-hashcode|identity-tuple|identity-tuple\?|if|if\*|'
        r'keep|loop|most|new|nip|not|null|object|or|over|pick|prepose|'
        r'retainstack|rot|same\?|swap|swapd|throw|tri|tri-curry|'
        r'tri-curry@|tri-curry\*|tri@|tri\*|tuple|tuple\?|unless|'
        r'unless\*|until|when|when\*|while|with|wrapper|wrapper\?|xor)\s'
    )

    builtin_assocs = (
        r'(?:2cache|<enum>|>alist|\?at|\?of|assoc|assoc-all\?|'
        r'assoc-any\?|assoc-clone-like|assoc-combine|assoc-diff|'
        r'assoc-diff!|assoc-differ|assoc-each|assoc-empty\?|'
        r'assoc-filter|assoc-filter!|assoc-filter-as|assoc-find|'
        r'assoc-hashcode|assoc-intersect|assoc-like|assoc-map|'
        r'assoc-map-as|assoc-partition|assoc-refine|assoc-size|'
        r'assoc-stack|assoc-subset\?|assoc-union|assoc-union!|'
        r'assoc=|assoc>map|assoc\?|at|at+|at\*|cache|change-at|'
        r'clear-assoc|delete-at|delete-at\*|enum|enum\?|extract-keys|'
        r'inc-at|key\?|keys|map>assoc|maybe-set-at|new-assoc|of|'
        r'push-at|rename-at|set-at|sift-keys|sift-values|substitute|'
        r'unzip|value-at|value-at\*|value\?|values|zip)\s'
    )

    builtin_combinators = (
        r'(?:2cleave|2cleave>quot|3cleave|3cleave>quot|4cleave|'
        r'4cleave>quot|alist>quot|call-effect|case|case-find|'
        r'case>quot|cleave|cleave>quot|cond|cond>quot|deep-spread>quot|'
        r'execute-effect|linear-case-quot|no-case|no-case\?|no-cond|'
        r'no-cond\?|recursive-hashcode|shallow-spread>quot|spread|'
        r'to-fixed-point|wrong-values|wrong-values\?)\s'
    )

    builtin_math = (
        r'(?:-|/|/f|/i|/mod|2/|2\^|<|<=|<fp-nan>|>|>=|>bignum|'
        r'>fixnum|>float|>integer|\(all-integers\?\)|'
        r'\(each-integer\)|\(find-integer\)|\*|\+|\?1\+|'
        r'abs|align|all-integers\?|bignum|bignum\?|bit\?|bitand|'
        r'bitnot|bitor|bits>double|bits>float|bitxor|complex|'
        r'complex\?|denominator|double>bits|each-integer|even\?|'
        r'find-integer|find-last-integer|fixnum|fixnum\?|float|'
        r'float>bits|float\?|fp-bitwise=|fp-infinity\?|fp-nan-payload|'
        r'fp-nan\?|fp-qnan\?|fp-sign|fp-snan\?|fp-special\?|'
        r'if-zero|imaginary-part|integer|integer>fixnum|'
        r'integer>fixnum-strict|integer\?|log2|log2-expects-positive|'
        r'log2-expects-positive\?|mod|neg|neg\?|next-float|'
        r'next-power-of-2|number|number=|number\?|numerator|odd\?|'
        r'out-of-fixnum-range|out-of-fixnum-range\?|power-of-2\?|'
        r'prev-float|ratio|ratio\?|rational|rational\?|real|'
        r'real-part|real\?|recip|rem|sgn|shift|sq|times|u<|u<=|u>|'
        r'u>=|unless-zero|unordered\?|when-zero|zero\?)\s'
    )

    builtin_sequences = (
        r'(?:1sequence|2all\?|2each|2map|2map-as|2map-reduce|2reduce|'
        r'2selector|2sequence|3append|3append-as|3each|3map|3map-as|'
        r'3sequence|4sequence|<repetition>|<reversed>|<slice>|\?first|'
        r'\?last|\?nth|\?second|\?set-nth|accumulate|accumulate!|'
        r'accumulate-as|all\?|any\?|append|append!|append-as|'
        r'assert-sequence|assert-sequence=|assert-sequence\?|'
        r'binary-reduce|bounds-check|bounds-check\?|bounds-error|'
        r'bounds-error\?|but-last|but-last-slice|cartesian-each|'
        r'cartesian-map|cartesian-product|change-nth|check-slice|'
        r'check-slice-error|clone-like|collapse-slice|collector|'
        r'collector-for|concat|concat-as|copy|count|cut|cut-slice|'
        r'cut\*|delete-all|delete-slice|drop-prefix|each|each-from|'
        r'each-index|empty\?|exchange|filter|filter!|filter-as|find|'
        r'find-from|find-index|find-index-from|find-last|find-last-from|'
        r'first|first2|first3|first4|flip|follow|fourth|glue|halves|'
        r'harvest|head|head-slice|head-slice\*|head\*|head\?|'
        r'if-empty|immutable|immutable-sequence|immutable-sequence\?|'
        r'immutable\?|index|index-from|indices|infimum|infimum-by|'
        r'insert-nth|interleave|iota|iota-tuple|iota-tuple\?|join|'
        r'join-as|last|last-index|last-index-from|length|lengthen|'
        r'like|longer|longer\?|longest|map|map!|map-as|map-find|'
        r'map-find-last|map-index|map-integers|map-reduce|map-sum|'
        r'max-length|member-eq\?|member\?|midpoint@|min-length|'
        r'mismatch|move|new-like|new-resizable|new-sequence|'
        r'non-negative-integer-expected|non-negative-integer-expected\?|'
        r'nth|nths|pad-head|pad-tail|padding|partition|pop|pop\*|'
        r'prefix|prepend|prepend-as|produce|produce-as|product|push|'
        r'push-all|push-either|push-if|reduce|reduce-index|remove|'
        r'remove!|remove-eq|remove-eq!|remove-nth|remove-nth!|repetition|'
        r'repetition\?|replace-slice|replicate|replicate-as|rest|'
        r'rest-slice|reverse|reverse!|reversed|reversed\?|second|'
        r'selector|selector-for|sequence|sequence-hashcode|sequence=|'
        r'sequence\?|set-first|set-fourth|set-last|set-length|set-nth|'
        r'set-second|set-third|short|shorten|shorter|shorter\?|'
        r'shortest|sift|slice|slice-error|slice-error\?|slice\?|'
        r'snip|snip-slice|start|start\*|subseq|subseq\?|suffix|'
        r'suffix!|sum|sum-lengths|supremum|supremum-by|surround|tail|'
        r'tail-slice|tail-slice\*|tail\*|tail\?|third|trim|'
        r'trim-head|trim-head-slice|trim-slice|trim-tail|trim-tail-slice|'
        r'unclip|unclip-last|unclip-last-slice|unclip-slice|unless-empty|'
        r'virtual-exemplar|virtual-sequence|virtual-sequence\?|virtual@|'
        r'when-empty)\s'
    )

    builtin_namespaces = (
        r'(?:\+@|change|change-global|counter|dec|get|get-global|'
        r'global|inc|init-namespaces|initialize|is-global|make-assoc|'
        r'namespace|namestack|off|on|set|set-global|set-namestack|'
        r'toggle|with-global|with-scope|with-variable|with-variables)\s'
    )

    builtin_arrays = (
        r'(?:1array|2array|3array|4array|<array>|>array|array|array\?|'
        r'pair|pair\?|resize-array)\s'
    )

    builtin_io = (
        r'(?:\(each-stream-block-slice\)|\(each-stream-block\)|'
        r'\(stream-contents-by-block\)|\(stream-contents-by-element\)|'
        r'\(stream-contents-by-length-or-block\)|'
        r'\(stream-contents-by-length\)|\+byte\+|\+character\+|'
        r'bad-seek-type|bad-seek-type\?|bl|contents|each-block|'
        r'each-block-size|each-block-slice|each-line|each-morsel|'
        r'each-stream-block|each-stream-block-slice|each-stream-line|'
        r'error-stream|flush|input-stream|input-stream\?|'
        r'invalid-read-buffer|invalid-read-buffer\?|lines|nl|'
        r'output-stream|output-stream\?|print|read|read-into|'
        r'read-partial|read-partial-into|read-until|read1|readln|'
        r'seek-absolute|seek-absolute\?|seek-end|seek-end\?|'
        r'seek-input|seek-output|seek-relative|seek-relative\?|'
        r'stream-bl|stream-contents|stream-contents\*|stream-copy|'
        r'stream-copy\*|stream-element-type|stream-flush|'
        r'stream-length|stream-lines|stream-nl|stream-print|'
        r'stream-read|stream-read-into|stream-read-partial|'
        r'stream-read-partial-into|stream-read-partial-unsafe|'
        r'stream-read-unsafe|stream-read-until|stream-read1|'
        r'stream-readln|stream-seek|stream-seekable\?|stream-tell|'
        r'stream-write|stream-write1|tell-input|tell-output|'
        r'with-error-stream|with-error-stream\*|with-error>output|'
        r'with-input-output\+error-streams|'
        r'with-input-output\+error-streams\*|with-input-stream|'
        r'with-input-stream\*|with-output-stream|with-output-stream\*|'
        r'with-output>error|with-output\+error-stream|'
        r'with-output\+error-stream\*|with-streams|with-streams\*|'
        r'write|write1)\s'
    )

    builtin_strings = (
        r'(?:1string|<string>|>string|resize-string|string|string\?)\s'
    )

    builtin_vectors = (
        r'(?:1vector|<vector>|>vector|\?push|vector|vector\?)\s'
    )

    builtin_continuations = (
        r'(?:<condition>|<continuation>|<restart>|attempt-all|'
        r'attempt-all-error|attempt-all-error\?|callback-error-hook|'
        r'callcc0|callcc1|cleanup|compute-restarts|condition|'
        r'condition\?|continuation|continuation\?|continue|'
        r'continue-restart|continue-with|current-continuation|'
        r'error|error-continuation|error-in-thread|error-thread|'
        r'ifcc|ignore-errors|in-callback\?|original-error|recover|'
        r'restart|restart\?|restarts|rethrow|rethrow-restarts|'
        r'return|return-continuation|thread-error-hook|throw-continue|'
        r'throw-restarts|with-datastack|with-return)\s'
    )

    tokens = {
        'root': [
            # factor allows a file to start with a shebang
            (r'#!.*$', Comment.Preproc),
            default('base'),
        ],
        'base': [
            (r'\s+', Text),

            # defining words
            (r'((?:MACRO|MEMO|TYPED)?:[:]?)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Function)),
            (r'(M:[:]?)(\s+)(\S+)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Class, Text, Name.Function)),
            (r'(C:)(\s+)(\S+)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Function, Text, Name.Class)),
            (r'(GENERIC:)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Function)),
            (r'(HOOK:|GENERIC#)(\s+)(\S+)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Function, Text, Name.Function)),
            (r'\(\s', Name.Function, 'stackeffect'),
            (r';\s', Keyword),

            # imports and namespaces
            (r'(USING:)(\s+)',
             bygroups(Keyword.Namespace, Text), 'vocabs'),
            (r'(USE:|UNUSE:|IN:|QUALIFIED:)(\s+)(\S+)',
             bygroups(Keyword.Namespace, Text, Name.Namespace)),
            (r'(QUALIFIED-WITH:)(\s+)(\S+)(\s+)(\S+)',
             bygroups(Keyword.Namespace, Text, Name.Namespace, Text, Name.Namespace)),
            (r'(FROM:|EXCLUDE:)(\s+)(\S+)(\s+=>\s)',
             bygroups(Keyword.Namespace, Text, Name.Namespace, Text), 'words'),
            (r'(RENAME:)(\s+)(\S+)(\s+)(\S+)(\s+=>\s+)(\S+)',
             bygroups(Keyword.Namespace, Text, Name.Function, Text, Name.Namespace, Text, Name.Function)),
            (r'(ALIAS:|TYPEDEF:)(\s+)(\S+)(\s+)(\S+)',
             bygroups(Keyword.Namespace, Text, Name.Function, Text, Name.Function)),
            (r'(DEFER:|FORGET:|POSTPONE:)(\s+)(\S+)',
             bygroups(Keyword.Namespace, Text, Name.Function)),

            # tuples and classes
            (r'(TUPLE:|ERROR:)(\s+)(\S+)(\s+<\s+)(\S+)',
             bygroups(Keyword, Text, Name.Class, Text, Name.Class), 'slots'),
            (r'(TUPLE:|ERROR:|BUILTIN:)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Class), 'slots'),
            (r'(MIXIN:|UNION:|INTERSECTION:)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Class)),
            (r'(PREDICATE:)(\s+)(\S+)(\s+<\s+)(\S+)',
             bygroups(Keyword, Text, Name.Class, Text, Name.Class)),
            (r'(C:)(\s+)(\S+)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Function, Text, Name.Class)),
            (r'(INSTANCE:)(\s+)(\S+)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Class, Text, Name.Class)),
            (r'(SLOT:)(\s+)(\S+)', bygroups(Keyword, Text, Name.Function)),
            (r'(SINGLETON:)(\s+)(\S+)', bygroups(Keyword, Text, Name.Class)),
            (r'SINGLETONS:', Keyword, 'classes'),

            # other syntax
            (r'(CONSTANT:|SYMBOL:|MAIN:|HELP:)(\s+)(\S+)',
             bygroups(Keyword, Text, Name.Function)),
            (r'SYMBOLS:\s', Keyword, 'words'),
            (r'SYNTAX:\s', Keyword),
            (r'ALIEN:\s', Keyword),
            (r'(STRUCT:)(\s+)(\S+)', bygroups(Keyword, Text, Name.Class)),
            (r'(FUNCTION:)(\s+\S+\s+)(\S+)(\s+\(\s+[^\)]+\)\s)',
             bygroups(Keyword.Namespace, Text, Name.Function, Text)),
            (r'(FUNCTION-ALIAS:)(\s+)(\S+)(\s+\S+\s+)(\S+)(\s+\(\s+[^\)]+\)\s)',
             bygroups(Keyword.Namespace, Text, Name.Function, Text, Name.Function, Text)),

            # vocab.private
            (r'(?:<PRIVATE|PRIVATE>)\s', Keyword.Namespace),

            # strings
            (r'"""\s+(?:.|\n)*?\s+"""', String),
            (r'"(?:\\\\|\\"|[^"])*"', String),
            (r'\S+"\s+(?:\\\\|\\"|[^"])*"', String),
            (r'CHAR:\s+(?:\\[\\abfnrstv]|[^\\]\S*)\s', String.Char),

            # comments
            (r'!\s+.*$', Comment),
            (r'#!\s+.*$', Comment),
            (r'/\*\s+(?:.|\n)*?\s\*/\s', Comment),

            # boolean constants
            (r'[tf]\s', Name.Constant),

            # symbols and literals
            (r'[\\$]\s+\S+', Name.Constant),
            (r'M\\\s+\S+\s+\S+', Name.Constant),

            # numbers
            (r'[+-]?(?:[\d,]*\d)?\.(?:\d([\d,]*\d)?)?(?:[eE][+-]?\d+)?\s', Number),
            (r'[+-]?\d(?:[\d,]*\d)?(?:[eE][+-]?\d+)?\s', Number),
            (r'0x[a-fA-F\d](?:[a-fA-F\d,]*[a-fA-F\d])?(?:p\d([\d,]*\d)?)?\s', Number),
            (r'NAN:\s+[a-fA-F\d](?:[a-fA-F\d,]*[a-fA-F\d])?(?:p\d([\d,]*\d)?)?\s', Number),
            (r'0b[01]+\s', Number.Bin),
            (r'0o[0-7]+\s', Number.Oct),
            (r'(?:\d([\d,]*\d)?)?\+\d(?:[\d,]*\d)?/\d(?:[\d,]*\d)?\s', Number),
            (r'(?:\-\d([\d,]*\d)?)?\-\d(?:[\d,]*\d)?/\d(?:[\d,]*\d)?\s', Number),

            # keywords
            (r'(?:deprecated|final|foldable|flushable|inline|recursive)\s',
             Keyword),

            # builtins
            (builtin_kernel, Name.Builtin),
            (builtin_assocs, Name.Builtin),
            (builtin_combinators, Name.Builtin),
            (builtin_math, Name.Builtin),
            (builtin_sequences, Name.Builtin),
            (builtin_namespaces, Name.Builtin),
            (builtin_arrays, Name.Builtin),
            (builtin_io, Name.Builtin),
            (builtin_strings, Name.Builtin),
            (builtin_vectors, Name.Builtin),
            (builtin_continuations, Name.Builtin),

            # everything else is text
            (r'\S+', Text),
        ],
        'stackeffect': [
            (r'\s+', Text),
            (r'\(\s+', Name.Function, 'stackeffect'),
            (r'\)\s', Name.Function, '#pop'),
            (r'--\s', Name.Function),
            (r'\S+', Name.Variable),
        ],
        'slots': [
            (r'\s+', Text),
            (r';\s', Keyword, '#pop'),
            (r'({\s+)(\S+)(\s+[^}]+\s+}\s)',
             bygroups(Text, Name.Variable, Text)),
            (r'\S+', Name.Variable),
        ],
        'vocabs': [
            (r'\s+', Text),
            (r';\s', Keyword, '#pop'),
            (r'\S+', Name.Namespace),
        ],
        'classes': [
            (r'\s+', Text),
            (r';\s', Keyword, '#pop'),
            (r'\S+', Name.Class),
        ],
        'words': [
            (r'\s+', Text),
            (r';\s', Keyword, '#pop'),
            (r'\S+', Name.Function),
        ],
    }
