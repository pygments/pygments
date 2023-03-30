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

.. function:: get_lexer_by_name(alias, **options)

    Return an instance of a `Lexer` subclass that has `alias` in its
    aliases list. The lexer is given the `options` at its
    instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if no lexer with that alias is
    found.

.. function:: get_lexer_for_filename(fn, **options)

    Return a `Lexer` subclass instance that has a filename pattern
    matching `fn`. The lexer is given the `options` at its
    instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if no lexer for that filename
    is found.

.. function:: get_lexer_for_mimetype(mime, **options)

    Return a `Lexer` subclass instance that has `mime` in its mimetype
    list. The lexer is given the `options` at its instantiation.

    Will raise :exc:`pygments.util.ClassNotFound` if not lexer for that mimetype
    is found.

.. function:: load_lexer_from_file(filename, lexername="CustomLexer", **options)

    Return a `Lexer` subclass instance loaded from the provided file, relative
    to the current directory. The file is expected to contain a Lexer class
    named `lexername` (by default, CustomLexer). Users should be very careful with
    the input, because this method is equivalent to running eval on the input file.
    The lexer is given the `options` at its instantiation.

    :exc:`pygments.util.ClassNotFound` is raised if there are any errors loading the Lexer

    .. versionadded:: 2.2

.. function:: guess_lexer(text, **options)

    Return a `Lexer` subclass instance that's guessed from the text in
    `text`. For that, the :meth:`.analyse_text()` method of every known lexer
    class is called with the text as argument, and the lexer which returned the
    highest value will be instantiated and returned.

    :exc:`pygments.util.ClassNotFound` is raised if no lexer thinks it can
    handle the content.

.. function:: guess_lexer_for_filename(filename, text, **options)

    As :func:`guess_lexer()`, but only lexers which have a pattern in `filenames`
    or `alias_filenames` that matches `filename` are taken into consideration.

    :exc:`pygments.util.ClassNotFound` is raised if no lexer thinks it can
    handle the content.

.. function:: get_all_lexers()

    Return an iterable over all registered lexers, yielding tuples in the
    format::

    	(longname, tuple of aliases, tuple of filename patterns, tuple of mimetypes)

    .. versionadded:: 0.6

.. function:: find_lexer_class_by_name(alias)

    Return the `Lexer` subclass that has `alias` in its aliases list, without
    instantiating it.

    Will raise :exc:`pygments.util.ClassNotFound` if no lexer with that alias is
    found.

    .. versionadded:: 2.2

.. function:: find_lexer_class(name)

    Return the `Lexer` subclass that with the *name* attribute as given by
    the *name* argument.


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

.. exception:: OptionError

    This exception will be raised by all option processing functions if
    the type or value of the argument is not correct.

.. function:: get_bool_opt(options, optname, default=None)

    Intuitively, this is `options.get(optname, default)`, but restricted to
    Boolean value. The Booleans can be represented as string, in order to accept
    Boolean value from the command line arguments. If the key `optname` is
    present in the dictionary `options` and is not associated with a Boolean,
    raise an `OptionError`. If it is absent, `default` is returned instead.

    The valid string values for ``True`` are ``1``, ``yes``, ``true`` and
    ``on``, the ones for ``False`` are ``0``, ``no``, ``false`` and ``off``
    (matched case-insensitively).

.. function:: get_int_opt(options, optname, default=None)

    As :func:`get_bool_opt`, but interpret the value as an integer.

.. function:: get_list_opt(options, optname, default=None)

    If the key `optname` from the dictionary `options` is a string,
    split it at whitespace and return it. If it is already a list
    or a tuple, it is returned as a list.

.. function:: get_choice_opt(options, optname, allowed, default=None)

    If the key `optname` from the dictionary is not in the sequence
    `allowed`, raise an error, otherwise return it.

    .. versionadded:: 0.8
