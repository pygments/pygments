# -*- coding: utf-8 -*-
"""
    pygments.filters
    ~~~~~~~~~~~~~~~~

    Module containing filter lookup functions and default
    filters.

    :copyright: 2006-2007 by Armin Ronacher, Georg Brandl.
    :license: BSD, see LICENSE for more details.
"""
try:
    set
except NameError:
    from sets import Set as set

import re
from pygments.token import String, Comment, Keyword, Name, string_to_tokentype
from pygments.filter import Filter
from pygments.util import get_list_opt, ClassNotFound
from pygments.plugin import find_plugin_filters


def find_filter_class(filtername):
    """
    Lookup a filter by name. Return None if not found.
    """
    if filtername in FILTERS:
        return FILTERS[filtername]
    for name, cls in find_plugin_filters():
        if name == filtername:
            return cls
    return None


def get_filter_by_name(filtername, **options):
    """
    Return an instantiated filter. Options are passed to the filter
    initializer if wanted. Raise a ClassNotFound if not found.
    """
    cls = find_filter_class(filtername)
    if cls:
        return cls(**options)
    else:
        raise ClassNotFound('filter %r not found' % filtername)


def get_all_filters():
    """
    Return a generator of all filter names.
    """
    for name in FILTERS:
        yield name
    for name, _ in find_plugin_filters():
        yield name


class CodeTagFilter(Filter):
    """
    Highlight special code tags in comments and docstrings.

    Per default, the list of highlighted tags is ``XXX``, ``TODO``, ``BUG`` and
    ``NOTE``. You can override this list by specifying a `codetags` parameter
    that takes a list of words.
    """
    def __init__(self, **options):
        Filter.__init__(self, **options)
        tags = get_list_opt(options, 'codetags',
                            ['XXX', 'TODO', 'BUG', 'NOTE'])
        self.tag_re = re.compile(r'(%s)' % '|'.join([
            re.escape(tag) for tag in tags if tag
        ]))

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if ttype in String.Doc or \
               ttype in Comment and \
               ttype not in Comment.Preproc:
                last = 0
                for match in self.tag_re.finditer(value):
                    start = match.start()
                    end = match.end()
                    if start != last:
                        yield ttype, value[last:start]
                    yield Comment.Special, value[start:end]
                    last = end
                if last != len(value):
                    yield ttype, value[last:]
                continue
            yield ttype, value


class KeywordCaseFilter(Filter):
    """
    Convert keywords to ``lower``, ``upper`` or ``capitalize`` which means
    first letter uppercase, rest lowercase.

    This can be useful e.g. if you highlight Pascal code and want to adapt the
    code to your styleguide. The default is ``lower``, override that by
    providing the `case` parameter.
    """

    def __init__(self, **options):
        Filter.__init__(self, **options)
        case = options.get('case', 'lower')
        if case not in ('lower', 'upper', 'capitalize'):
            raise TypeError('unknown conversion method %r' % case)
        self.convert = getattr(unicode, case)

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if ttype in Keyword:
                yield ttype, self.convert(value)
            else:
                yield ttype, value


class NameHighlightFilter(Filter):
    """
    Highlight a normal Name token with a different token type.

    Example::

        filter = NameHighlightFilter(
            names=['foo', 'bar', 'baz'],
            tokentype=Name.Function,
        )

    This would highlight the names "foo", "bar" and "baz"
    as functions. `Name.Function` is the default token type.
    """

    def __init__(self, **options):
        Filter.__init__(self, **options)
        self.names = set(get_list_opt(options, 'names', []))
        tokentype = options.get('tokentype')
        if tokentype:
            self.tokentype = string_to_tokentype(tokentype)
        else:
            self.tokentype = Name.Function

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if ttype is Name and value in self.names:
                yield self.tokentype, value
            else:
                yield ttype, value


FILTERS = {
    'codetagify':     CodeTagFilter,
    'keywordcase':    KeywordCaseFilter,
    'highlight':      NameHighlightFilter,
}
