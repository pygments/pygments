.. -*- mode: rst -*-

.. _creating-own-styles:

Creating Own Styles
===================

So, how to create a style? All you have to do is to subclass `Style` and
define some styles:

.. sourcecode:: python

    from pygments.style import Style
    from pygments.token import Token, Comment, Keyword, Name, String, \
         Error, Generic, Number, Operator


    class YourStyle(Style):

        styles = {
            Token:                  '',
            Comment:                'italic #888',
            Keyword:                'bold #005',
            Name:                   '#f00',
            Name.Class:             'bold #0f0',
            Name.Function:          '#0f0',
            String:                 'bg:#eee #111'
        }

That's it, save it as ``your.py``. There are just a few rules. When you define a style for `Name`
the style automatically also affects `Name.Function` and so on. If you
defined ``'bold'`` and you don't want boldface for a subtoken use ``'nobold'``.

(Philosophy: the styles aren't written in CSS syntax since this way
they can be used for a variety of formatters.)

``Token`` is the default style inherited by all token types.

To make the style usable for Pygments, you must

* either register it as a plugin (see :doc:`the plugin docs <plugins>`)
* or update the ``pygments.styles`` subpackage directory. For example:

  * add ``your.py`` file
  * register the new style by adding a line to the ``__init__.py`` file:
  
  .. sourcecode:: python
  
      STYLE_MAP = {
          'default':  'default::DefaultStyle',
          ...
          'your':  'your::YourStyle',
          ...


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
