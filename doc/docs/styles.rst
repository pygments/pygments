.. -*- mode: rst -*-

======
Styles
======

Pygments comes with some builtin styles that work for both the HTML and
LaTeX formatter.

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

So, how to create a style? All you have to do is to subclass `Style` and
define some styles:

.. sourcecode:: python

    from pygments.style import Style
    from pygments.token import Keyword, Name, Comment, String, Error, \
         Number, Operator, Generic

    class YourStyle(Style):
        default_style = ""
        styles = {
            Comment:                'italic #888',
            Keyword:                'bold #005',
            Name:                   '#f00',
            Name.Function:          '#0f0',
            Name.Class:             'bold #0f0',
            String:                 'bg:#eee #111'
        }

That's it. There are just a few rules. When you define a style for `Name`
the style automatically also affects `Name.Function` and so on. If you
defined ``'bold'`` and you don't want boldface for a subtoken use ``'nobold'``.

(Philosophy: the styles aren't written in CSS syntax since this way
they can be used for a variety of formatters.)

`default_style` is the style inherited by all token types.

To make the style usable for Pygments, you must

* either register it as a plugin (see :doc:`the plugin docs <plugins>`)
* or drop it into the `styles` subpackage of your Pygments distribution one style
  class per style, where the file name is the style name and the class name is
  `StylenameClass`. For example, if your style should be called
  ``"mondrian"``, name the class `MondrianStyle`, put it into the file
  ``mondrian.py`` and this file into the ``pygments.styles`` subpackage
  directory.


Style Rules
===========

Here a small overview of all allowed styles:

``bold``
    render text as bold
``nobold``
    don't render text as bold (to prevent subtokens being highlighted bold)
``italic``
    render text italic
``noitalic``
    don't render text as italic
``underline``
    render text underlined
``nounderline``
    don't render text underlined
``bg:``
    transparent background
``bg:#000000``
    background color (black)
``border:``
    no border
``border:#ffffff``
    border color (white)
``#ff0000``
    text color (red)
``noinherit``
    don't inherit styles from supertoken

Note that there may not be a space between ``bg:`` and the color value
since the style definition string is split at whitespace.
Also, using named colors is not allowed since the supported color names
vary for different formatters.

Furthermore, not all lexers might support every style.


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

Custom styles used with `Terminal256` formatter can also defines foreground
colors using ansi-color. to do so use the `#ansigreen`, `#ansired` or any other
colors defined in ``pygments.style.ansilist``. Foreground ANSI colors will be
mapped to the corresponding `escape codes 30 to 37
<https://en.wikipedia.org/wiki/ANSI_escape_code#Colors>`_  thus respecting any
custom color mapping and themes provided by many terminal emulators. 

See following example where the color of the string `"hello world"` is governed
by the escape sequence `\x1b34;01m` (Ansi Blue) instead of an extended
foreground color.

.. sourcecode:: pycon

    >>> from pygments import highlight
    >>> from pygments.style import Style
    >>> from pygments.token import Token
    >>> from pygments.lexers import Python3Lexer
    >>> from pygments.formatters import Terminal256Formatter

    >>> class MyStyle(Style):
    >>>
    >>>     styles = {
    >>>         Token.String:     '#ansiblue',
    >>>     }

    >>> code = 'print("Hello World")'
    >>> result = highlight(code, Python3Lexer(), Terminal256Formatter(style=MyStyle))
    >>> print(result.encode())
    b'print(\x1b[34;01m"\x1b[39m\x1b[34;01mHello World\x1b[39m\x1b[34;01m"\x1b[39m)\n'

Style that use `#ansi*` foreground colors might not correctly work with
formatters others than ``Terminal256``. `HtmlFormatter` is capable of handling
some `#ansi*` code and will map to the corresponding HTML/CSS color. That is to
say, `#ansiblue` will be converted to `color:blue` , `#ansired` to `color:red`.
The behavior is undefined for argument like `#ansireset`, `#ansiunderline`,
`#ansibold`... etc.
