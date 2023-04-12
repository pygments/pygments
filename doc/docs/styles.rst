.. -*- mode: rst -*-

======
Styles
======

Pygments comes with :doc:`some builtin styles </styles/>` that work for both the
HTML and LaTeX formatter.

The builtin styles can be looked up with the `get_style_by_name` function:

.. sourcecode:: pycon

    >>> from pygments.styles import get_style_by_name
    >>> get_style_by_name('colorful')
    <class 'pygments.styles.colorful.ColorfulStyle'>

You can pass a instance of a `Style` class to a formatter as the `style`
option in form of a string:

.. sourcecode:: pycon

    >>> from pygments.styles import get_style_by_name
    >>> from pygments.formatters import HtmlFormatter
    >>> HtmlFormatter(style='colorful').style
    <class 'pygments.styles.colorful.ColorfulStyle'>

Or you can also import your own style (which must be a subclass of
`pygments.style.Style`) and pass it to the formatter:

.. sourcecode:: pycon

    >>> from yourapp.yourmodule import YourStyle
    >>> from pygments.formatters import HtmlFormatter
    >>> HtmlFormatter(style=YourStyle).style
    <class 'yourapp.yourmodule.YourStyle'>


Creating Own Styles
===================

See :doc:`styledevelopment`.


Builtin Styles
==============

Pygments ships some builtin styles which are maintained by the Pygments team.

To get a list of known styles you can use this snippet:

.. sourcecode:: pycon

    >>> from pygments.styles import STYLE_MAP
    >>> STYLE_MAP.keys()
    ['default', 'emacs', 'friendly', 'colorful']


Getting a list of available styles
==================================

.. versionadded:: 0.6

Because it could be that a plugin registered a style, there is
a way to iterate over all styles:

.. sourcecode:: pycon

    >>> from pygments.styles import get_all_styles
    >>> styles = list(get_all_styles())


.. _AnsiTerminalStyle:

Terminal Styles
===============

.. versionadded:: 2.2

Custom styles used with the 256-color terminal formatter can also map colors to
use the 8 default ANSI colors.  To do so, use ``ansigreen``, ``ansibrightred`` or
any other colors defined in :data:`pygments.style.ansicolors`.  Foreground ANSI
colors will be mapped to the corresponding `escape codes 30 to 37
<https://en.wikipedia.org/wiki/ANSI_escape_code#Colors>`_ thus respecting any
custom color mapping and themes provided by many terminal emulators.  Light
variants are treated as foreground color with and an added bold flag.
``bg:ansi<color>`` will also be respected, except the light variant will be the
same shade as their dark variant.

See the following example where the color of the string ``"hello world"`` is
governed by the escape sequence ``\x1b[34;01m`` (Ansi bright blue, Bold, 41 being red
background) instead of an extended foreground & background color.

.. sourcecode:: pycon

    >>> from pygments import highlight
    >>> from pygments.style import Style
    >>> from pygments.token import Token
    >>> from pygments.lexers import Python3Lexer
    >>> from pygments.formatters import Terminal256Formatter

    >>> class MyStyle(Style):
            styles = {
                Token.String:     'ansibrightblue bg:ansibrightred',
            }

    >>> code = 'print("Hello World")'
    >>> result = highlight(code, Python3Lexer(), Terminal256Formatter(style=MyStyle))
    >>> print(result.encode())
    b'\x1b[34;41;01m"\x1b[39;49;00m\x1b[34;41;01mHello World\x1b[39;49;00m\x1b[34;41;01m"\x1b[39;49;00m'

Colors specified using ``ansi*`` are converted to a default set of RGB colors
when used with formatters other than the terminal-256 formatter.

By definition of ANSI, the following colors are considered "light" colors, and
will be rendered by most terminals as bold:

- "brightblack" (darkgrey), "brightred", "brightgreen", "brightyellow", "brightblue",
  "brightmagenta", "brightcyan", "white"

The following are considered "dark" colors and will be rendered as non-bold:

- "black", "red", "green", "yellow", "blue", "magenta", "cyan",
  "gray"

Exact behavior might depends on the terminal emulator you are using, and its
settings.

.. _new-ansi-color-names:

.. versionchanged:: 2.4

The definition of the ANSI color names has changed.
New names are easier to understand and align to the colors used in other projects.

===================== ====================
New names             Pygments up to 2.3
===================== ====================
``ansiblack``         ``#ansiblack``
``ansired``           ``#ansidarkred``
``ansigreen``         ``#ansidarkgreen``
``ansiyellow``        ``#ansibrown``
``ansiblue``          ``#ansidarkblue``
``ansimagenta``       ``#ansipurple``
``ansicyan``          ``#ansiteal``
``ansigray``          ``#ansilightgray``
``ansibrightblack``   ``#ansidarkgray``
``ansibrightred``     ``#ansired``
``ansibrightgreen``   ``#ansigreen``
``ansibrightyellow``  ``#ansiyellow``
``ansibrightblue``    ``#ansiblue``
``ansibrightmagenta`` ``#ansifuchsia``
``ansibrightcyan``    ``#ansiturquoise``
``ansiwhite``         ``#ansiwhite``
===================== ====================

Old ANSI color names are deprecated but will still work.
