.. -*- mode: rst -*-

======================
Command Line Interface
======================

You can use Pygments from the shell, provided you installed the
:program:`pygmentize` script::

    $ pygmentize test.py
    print "Hello World"

will print the file test.py to standard output, using the Python lexer
(inferred from the file name extension) and the terminal formatter (because
you didn't give an explicit formatter name).

.. note::

   If you are on Windows, an extra tool may be needed for colored output to
   work in the terminal. You can make sure Pygments is installed with
   Windows console coloring support by installing Pygments with the ``windows-terminal``
   extra (e.g., ``pip install pygments[windows-terminal]``).

:program:`pygmentize` attempts to
detect the maximum number of colors that the terminal supports. The difference
between color formatters for 16 and 256 colors is immense, but there is a less
noticeable difference between color formatters for 256 and 16 million colors.

Here's the process of how it detects the maxiumum number of colors
supported by your terminal. If the ``COLORTERM`` environment variable is set to
either ``truecolor`` or ``24bit``, it will use a 16 million color representation
(like ``terminal16m``). Next, it will try to find ``256`` is anywhere in the
environment variable ``TERM``, which it will use a 256-color representaion
(such as ``terminal256``).  When neither of those are found, it falls back to a
the 16 color representation (like ``terminal``).

If you want HTML output::

    $ pygmentize -f html -l python -o test.html test.py

As you can see, the -l option explicitly selects a lexer. As seen above, if you
give an input file name and it has an extension that Pygments recognizes, you can
omit this option.

The ``-o`` option gives an output file name. If it is not given, output is
written to stdout.

The ``-f`` option selects a formatter (as with ``-l``, it can also be omitted
if an output file name is given and has a supported extension).
If no output file name is given and ``-f`` is omitted, the
:class:`.TerminalFormatter` is used.

The above command could therefore also be given as::

    $ pygmentize -o test.html test.py

To create a full HTML document, including line numbers and stylesheet (using the
"emacs" style), highlighting the Python file ``test.py`` to ``test.html``::

    $ pygmentize -O full,style=emacs,linenos=1 -o test.html test.py


Options and filters
-------------------

Lexer and formatter options can be given using the ``-O`` option::

    $ pygmentize -f html -O style=colorful,linenos=1 -l python test.py

Be sure to enclose the option string in quotes if it contains any special shell
characters, such as spaces or expansion wildcards like ``*``. If an option
expects a list value, separate the list entries with spaces (you'll have to
quote the option value in this case too, so that the shell doesn't split it).

Since the ``-O`` option argument is split at commas and expects the split values
to be of the form ``name=value``, you can't give an option value that contains
commas or equals signs.  Therefore, an option ``-P`` is provided (as of Pygments
0.9) that works like ``-O`` but can only pass one option per ``-P``. Its value
can then contain all characters::

    $ pygmentize -P "heading=Pygments, the Python highlighter" ...

Filters are added to the token stream using the ``-F`` option::

    $ pygmentize -f html -l pascal -F keywordcase:case=upper main.pas

As you see, options for the filter are given after a colon. As for ``-O``, the
filter name and options must be one shell word, so there may not be any spaces
around the colon.


Generating styles
-----------------

Formatters normally don't output full style information.  For example, the HTML
formatter by default only outputs ``<span>`` tags with ``class`` attributes.
Therefore, there's a special ``-S`` option for generating style definitions.
Usage is as follows::

    $ pygmentize -f html -S colorful -a .syntax

generates a CSS style sheet (because you selected the HTML formatter) for
the "colorful" style prepending a ".syntax" selector to all style rules.

For an explanation what ``-a`` means for :doc:`a particular formatter
<formatters>`, look for the `arg` argument for the formatter's
:meth:`.get_style_defs() <pygments.formatter.Formatter.get_style_defs()>` method.


Getting lexer names
-------------------

.. versionadded:: 1.0

The ``-N`` option guesses a lexer name for a given filename, so that ::

    $ pygmentize -N setup.py

will print out ``python``.  It won't highlight anything yet.  If no specific
lexer is known for that filename, ``text`` is printed.

Additionally, there is the ``-C`` option, which is just like like ``-N``, except
that it prints out a lexer name based solely on a given content from standard
input.


Guessing the lexer from the file contents
-----------------------------------------

The ``-g`` option will try to guess the correct lexer from the file contents,
or pass through as plain text if nothing can be guessed. This option also looks
for Vim modelines in the text, and for *some* languages, shebangs. Usage is as
follows::

    $ pygmentize -g setup.py

Note though, that this option is not very relaiable, and probably should be
used only if Pygments is not able to guess the correct lexer from the file's
extension.


Highlighting stdin until EOF
----------------------------

The ``-s`` option processes lines one at a time until EOF, rather than waiting
to process the entire file. This only works for stdin, only for lexers with no
line-spanning constructs, and is intended for streaming input such as you get
from `tail -f`. Usage is as follows::

    $ tail -f sql.log | pygmentize -s -l sql


Custom Lexers and Formatters
----------------------------

.. versionadded:: 2.2

The ``-x`` flag enables custom lexers and formatters to be loaded
from files relative to the current directory. Create a file with a class named
CustomLexer or CustomFormatter, then specify it on the command line::

    $ pygmentize -l your_lexer.py -f your_formatter.py -x

You can also specify the name of your class with a colon::

    $ pygmentize -l your_lexer.py:SomeLexer -x

For more information, see :doc:`the Pygments documentation on Lexer development
<lexerdevelopment>`.


Getting help
------------

The ``-L`` option lists lexers, formatters, along with their short
names and supported file name extensions, styles and filters. If you want to see
only one category, give it as an argument::

    $ pygmentize -L filters

will list only all installed filters.

.. versionadded:: 2.11

The ``--json`` option can be used in conjunction with the ``-L`` option to
output its contents as JSON. Thus, to print all the installed styles and their
description in JSON, use the command::

    $ pygmentize -L styles --json

The ``-H`` option will give you detailed information (the same that can be found
in this documentation) about a lexer, formatter or filter. Usage is as follows::

    $ pygmentize -H formatter html

will print the help for the HTML formatter, while ::

    $ pygmentize -H lexer python

will print the help for the Python lexer, etc.


A note on encodings
-------------------

.. versionadded:: 0.9

Pygments tries to be smart regarding encodings in the formatting process:

* If you give an ``encoding`` option, it will be used as the input and
  output encoding.

* If you give an ``outencoding`` option, it will override ``encoding``
  as the output encoding.

* If you give an ``inencoding`` option, it will override ``encoding``
  as the input encoding.

* If you don't give an encoding and have given an output file, the default
  encoding for lexer and formatter is the terminal encoding or the default
  locale encoding of the system.  As a last resort, ``latin1`` is used (which
  will pass through all non-ASCII characters).

* If you don't give an encoding and haven't given an output file (that means
  output is written to the console), the default encoding for lexer and
  formatter is the terminal encoding (``sys.stdout.encoding``).
