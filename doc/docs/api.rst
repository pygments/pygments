=====================
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
.. autofunction:: find_lexer_class_for_filename

Choosing a lexer by filename
----------------------------

Use :func:`find_lexer_class_for_filename` when you need the lexer class
itself. It returns ``None`` when no lexer matches and accepts optional code
to help choose between matching lexers. Use :func:`get_lexer_for_filename`
when you want an instance; its optional ``code`` argument serves the same
purpose. Both functions match the lexer's ``filenames`` patterns and rank
matches using text analysis for nonempty code, or lexer priority otherwise.
Filename patterns without ``*`` receive a bonus in either case.

Use :func:`guess_lexer_for_filename` when you have the file contents and want
Pygments to compare matching lexers' ``analyse_text()`` results. It requires
the text and considers both ``filenames`` and ``alias_filenames`` patterns.
The analysis ranks matching lexers; it does not validate the text or require
a positive score. All three functions accept paths as well as basenames.

For example, these calls all select a lexer for ``example.py``; the first
returns its class and the other two return instances:

.. code-block:: pycon

   >>> from pygments.lexers import (
   ...     find_lexer_class_for_filename,
   ...     get_lexer_for_filename,
   ...     guess_lexer_for_filename,
   ... )
   >>> lexer_class = find_lexer_class_for_filename("example.py")
   >>> lexer_class.__name__
   'PythonLexer'
   >>> type(get_lexer_for_filename("example.py")).__name__
   'PythonLexer'
   >>> type(guess_lexer_for_filename("example.py", "print('hello')")).__name__
   'PythonLexer'


.. module:: pygments.formatters

Functions from :mod:`pygments.formatters`:

.. autofunction:: get_formatter_by_name
.. autofunction:: get_formatter_for_filename
.. autofunction:: load_formatter_from_file


.. module:: pygments.styles

Functions from :mod:`pygments.styles`:

.. autofunction:: get_style_by_name
.. autofunction:: get_all_styles
.. autodata:: STYLE_MAP

.. module:: pygments.lexer

Lexers
======

The base lexer class from which all lexers are derived is:

.. autoclass:: Lexer
   :members: __init__, add_filter, get_tokens, get_tokens_unprocessed, analyse_text

There are several base classes derived from ``Lexer`` you can use to build your
lexer from:

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

Utilities
=========

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

It also defines an exception:

.. autoexception:: ClassNotFound
