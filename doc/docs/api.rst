The full Pygments API
=====================

This page describes the Pygments API.

High-level API
==============

.. module:: pygments

Functions from the :mod:`pygments` module:

.. autofunction:: lex

.. autofunction:: format

.. autofunction:: highlight


.. module:: pygments.lexers

Functions from :mod:`pygments.lexers`:

.. autofunction:: get_lexer_by_name
.. autofunction:: get_lexer_for_filename
.. autofunction:: get_lexer_for_mimetype
.. autofunction:: load_lexer_from_file
.. autofunction:: guess_lexer
.. autofunction:: guess_lexer_for_filename
.. autofunction:: get_all_lexers
.. autofunction:: find_lexer_class_by_name
.. autofunction:: find_lexer_class


.. module:: pygments.formatters

Functions from :mod:`pygments.formatters`:

.. function:: get_formatter_by_name(alias, **options)

    Return an instance of a :class:`.Formatter` subclass that has `alias` in its
    aliases list. The formatter is given the `options` at its instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if no formatter with that
    alias is found.

.. function:: get_formatter_for_filename(fn, **options)

    Return a :class:`.Formatter` subclass instance that has a filename pattern
    matching `fn`. The formatter is given the `options` at its instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if no formatter for that filename
    is found.

.. function:: load_formatter_from_file(filename, formattername="CustomFormatter", **options)

    Return a `Formatter` subclass instance loaded from the provided file, relative
    to the current directory. The file is expected to contain a Formatter class
    named ``formattername`` (by default, CustomFormatter). Users should be very
    careful with the input, because this method is equivalent to running eval
    on the input file. The formatter is given the `options` at its instantiation.

    :exc:`pygments.util.ClassNotFound` is raised if there are any errors loading the Formatter

    .. versionadded:: 2.2

.. module:: pygments.styles

Functions from :mod:`pygments.styles`:

.. function:: get_style_by_name(name)

    Return a style class by its short name. The names of the builtin styles
    are listed in :data:`pygments.styles.STYLE_MAP`.

    Will raise :exc:`pygments.util.ClassNotFound` if no style of that name is
    found.

.. function:: get_all_styles()

    Return an iterable over all registered styles, yielding their names.

    .. versionadded:: 0.6


.. module:: pygments.lexer

Lexers
======

The base lexer class from which all lexers are derived is:

.. autoclass:: Lexer
   :members: __init__, get_tokens, get_tokens_unprocessed, analyse_text

There are several base class derived from ``Lexer`` you can use to build your lexer from:

.. autoclass:: pygments.lexer.RegexLexer
.. autoclass:: pygments.lexer.ExtendedRegexLexer
.. autoclass:: pygments.lexer.DelegatingLexer


.. module:: pygments.formatter

Formatters
==========

A formatter is derived from this class:


.. autoclass:: Formatter
   :members: __init__, get_style_defs, format


.. module:: pygments.util

Option processing
=================

The :mod:`pygments.util` module has some utility functions usable for processing
command line options. All of the following functions get values from a
dictionary of options. If the value is already in the type expected by the
option, it is returned as-is. Otherwise, if the value is a string, it is first
converted to the expected type if possible.

.. autoexception:: OptionError
.. autofunction:: get_bool_opt
.. autofunction:: get_int_opt
.. autofunction:: get_list_opt
.. autofunction:: get_choice_opt
