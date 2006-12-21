# -*- coding: utf-8 -*-
"""
    pygments.filter
    ~~~~~~~~~~~~~~~

    Module that implements the default filter.

    :copyright: 2006 by Armin Ronacher.
    :license: BSD, see LICENSE for more details.
"""


def apply_filters(stream, filters, lexer=None):
    """
    Use this method to apply an iterable of filters to
    a stream. If lexer is given it's forwarded to the
    filter, otherwise the filter receives `None`.
    """
    def _apply(filter, stream):
        for token in filter.filter(lexer, stream):
            yield token
    for filter in filters:
        stream = _apply(filter, stream)
    return stream


def simplefilter(f):
    """
    Decorator that converts a function into a filter::

        @simplefilter
        def lowercase(lexer, stream, options):
            for ttype, value in stream:
                yield ttype, value.lower()
    """
    return type('%sFilter' % f.__name__.title().replace('_', ''),
                (FunctionFilter,), {'function': f})


class Filter(object):
    """
    Default filter. Subclass this class or use the `simplefilter`
    decorator to create own filters.
    """

    def __init__(self, **options):
        self.options = options

    def filter(self, lexer, stream):
        raise NotImplementedError()


class FunctionFilter(Filter):
    """
    Abstract class used by `simplefilter` to create simple
    function filters on the fly. The `simplefilter` decorator
    automatically creates subclasses of this class for
    functions passed to it.
    """

    def __init__(self, **options):
        if not hasattr(self, 'function'):
            raise TypeError('%r used without bound function' %
                            self.__class__.__name__)
        Filter.__init__(self, **options)

    def filter(self, lexer, stream):
        for ttype, value in self.function(lexer, stream, self.options):
            yield ttype, value
