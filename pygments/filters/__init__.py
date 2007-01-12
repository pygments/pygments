# -*- coding: utf-8 -*-
"""
    pygments.filters
    ~~~~~~~~~~~~~~~~

    Module containing filter lookup functions and default
    filters.

    :copyright: 2006 by Armin Ronacher.
    :license: BSD, see LICENSE for more details.
"""
import re
from pygments.token import String, Comment
from pygments.filter import Filter
from pygments.util import get_list_opt
from pygments.plugin import find_plugin_filters


def find_filter(filter, **options):
    """
    Lookup a builtin filter. Options are passed to the
    filter initialization if wanted.
    """
    if filter in FILTERS:
        return FILTERS[filter](**options)
    for name, cls in find_plugin_filters():
        if name == filter:
            return cls(**options)
    raise ValueError('filter %r not found' % filter)


def get_all_filters():
    """
    Return a generator for all filters by name.
    """
    for name in FILTERS:
        yield name
    for name, _ in find_plugin_filters():
        yield name


class CodeTagFilter(Filter):
    """
    Highlights special code tags in comments and docstrings. Per default, the
    list of highlighted tags is ``XXX``, ``TODO``, ``BUG`` and ``NOTE``. You can
    override this list by specifying a `codetags` parameter that takes a list of
    words.
    """
    def __init__(self, **options):
        Filter.__init__(self)
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
    Converts keywords to ``lower``, ``upper`` or ``capitalize`` which means
    first letter uppercase, rest lowercase. This can be useful e.g. if you
    highlight Pascal code and want to adapt the code to your styleguide. The
    default is ``lower``, override that by providing the `keywordcase`
    parameter.
    """

    def __init__(self, **options):
        Filter.__init__(self, **options)
        case = options.get('keywordcase', 'lower')
        if case not in ('lower', 'upper', 'capitalize'):
            raise TypeError
        self.convert = getattr(unicode, case)

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if ttype in Keyword:
                yield ttype, self.convert(value)
            else:
                yield ttype, value


FILTERS = {
    'codetagify':           CodeTagFilter,
    'keywordcase':          KeywordCaseFilter
}
