.. -*- mode: rst -*-

=======
Filters
=======

.. versionadded:: 0.7

Transforming a stream of tokens into another stream is called "filtering" and is
done by filters. The most common example of filters transform each token by
applying a simple rules such as highlighting the token if it is a TODO or
another special word, or converting keywords to uppercase to enforce a style
guide. More complex filters can transform the stream of tokens, such as removing
the line indentation or merging tokens together. It should be noted that pygments
filters are entirely unrelated to Python's `filter
<https://docs.python.org/3/library/functions.html#filter>`_.

An arbitrary number of filters can be applied to token streams coming from
lexers to improve or annotate the output. To apply a filter, you can use the
`add_filter()` method of a lexer:

.. sourcecode:: pycon

    >>> from pygments.lexers import PythonLexer
    >>> l = PythonLexer()
    >>> # add a filter given by a string and options
    >>> l.add_filter('codetagify', case='lower')
    >>> l.filters
    [<pygments.filters.CodeTagFilter object at 0xb785decc>]
    >>> from pygments.filters import KeywordCaseFilter
    >>> # or give an instance
    >>> l.add_filter(KeywordCaseFilter(case='lower'))

The `add_filter()` method takes keyword arguments which are forwarded to
the constructor of the filter.

To get a list of all registered filters by name, you can use the
`get_all_filters()` function from the `pygments.filters` module that returns an
iterable for all known filters.

If you want to write your own filter, have a look at :doc:`Write your own filter
<filterdevelopment>`.


Builtin Filters
===============

.. pygmentsdoc:: filters
