.. -*- mode: rst -*-

.. _creating-own-styles:

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
