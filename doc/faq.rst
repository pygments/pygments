:orphan:

Pygments FAQ
=============

What is Pygments?
-----------------

Pygments is a syntax highlighting engine written in Python. That means, it will
take source code (or other markup) in a supported language and output a
processed version (in different formats) containing syntax highlighting markup.

Its features include:

* a wide range of common :doc:`languages and markup formats <languages>` is supported
* new languages and formats are added easily
* a number of output formats is available, including:

  - HTML
  - ANSI sequences (console output)
  - LaTeX
  - RTF

* it is usable as a command-line tool and as a library
* parsing and formatting is fast

Pygments is licensed under the BSD license.

Where does the name Pygments come from?
---------------------------------------

*Py* of course stands for Python, while *pigments* are used for coloring paint,
and in this case, source code!

What are the system requirements?
---------------------------------

Pygments only needs a standard Python install, version 2.7 or higher or version
3.5 or higher for Python 3. No additional libraries are needed.

How can I use Pygments?
-----------------------

Pygments is usable as a command-line tool as well as a library.

From the command-line, usage looks like this (assuming the pygmentize script is
properly installed)::

    pygmentize -f html /path/to/file.py

This will print a HTML-highlighted version of /path/to/file.py to standard output.

For a complete help, please run ``pygmentize -h``.

Usage as a library is thoroughly demonstrated in the Documentation section.

How do I make a new style?
--------------------------

Please see the :doc:`documentation on styles <docs/styles>`.

How can I report a bug or suggest a feature?
--------------------------------------------

Please report bugs and feature wishes in the tracker at GitHub.

You can also e-mail the authors, see the contact details.

I want this support for this language!
--------------------------------------

Instead of waiting for others to include language support, why not write it
yourself? All you have to know is :doc:`outlined in the docs
<docs/lexerdevelopment>`.

Can I use Pygments for programming language processing?
-------------------------------------------------------

The Pygments lexing machinery is quite powerful can be used to build lexers for
basically all languages. However, parsing them is not possible, though some
lexers go some steps in this direction in order to e.g. highlight function names
differently.

Also, error reporting is not the scope of Pygments. It focuses on correctly
highlighting syntactically valid documents, not finding and compensating errors.

Who uses Pygments?
------------------

This is an (incomplete) list of projects and sites known to use the Pygments highlighter.

* `Wikipedia <https://en.wikipedia.org/>`_
* `BitBucket <https://bitbucket.org/>`_, a Mercurial and Git hosting site
* `The Sphinx documentation builder <https://sphinx-doc.org/>`_, for embedded source examples
* `rst2pdf <https://github.com/ralsina/rst2pdf>`_, a reStructuredText to PDF converter
* `Codecov <https://codecov.io/>`_, a code coverage CI service
* `Trac <https://trac.edgewall.org/>`_, the universal project management tool
* `AsciiDoc <https://www.methods.co.nz/asciidoc/>`_, a text-based documentation generator
* `ActiveState Code <https://code.activestate.com/>`_, the Python Cookbook successor
* `ViewVC <http://viewvc.org/>`_, a web-based version control repository browser
* `BzrFruit <https://repo.or.cz/w/bzrfruit.git>`_, a Bazaar branch viewer
* `QBzr <http://bazaar-vcs.org/QBzr>`_, a cross-platform Qt-based GUI front end for Bazaar
* `Review Board <https://www.reviewboard.org/>`_, a collaborative code reviewing tool
* `Diamanda <https://code.google.com/archive/p/diamanda/>`_, a Django powered wiki system with support for Pygments
* `Progopedia <http://progopedia.ru/>`_ (`English <http://progopedia.com/>`_),
  an encyclopedia of programming languages
* `Bruce <https://sites.google.com/site/r1chardj0n3s/bruce>`_, a reStructuredText presentation tool
* `PIDA <http://pida.co.uk/>`_, a universal IDE written in Python
* `BPython <https://bpython-interpreter.org/>`_, a curses-based intelligent Python shell
* `PuDB <https://pypi.org/project/pudb/>`_, a console Python debugger
* `XWiki <https://www.xwiki.org/>`_, a wiki-based development framework in Java, using Jython
* `roux <http://ananelson.com/software/roux/>`_, a script for running R scripts
  and creating beautiful output including graphs
* `hurl <http://hurl.it/>`_, a web service for making HTTP requests
* `wxHTMLPygmentizer <http://colinbarnette.net/projects/wxHTMLPygmentizer>`_ is
  a GUI utility, used to make code-colorization easier
* `Postmarkup <https://code.google.com/archive/p/postmarkup/>`_, a BBCode to XHTML generator
* `WpPygments <http://blog.mirotin.net/?page_id=49>`_, and `WPygments
  <https://github.com/capynet/WPygments>`_, highlighter plugins for WordPress
* `Siafoo <http://siafoo.net>`_, a tool for sharing and storing useful code and programming experience
* `D source <http://www.dsource.org/>`_, a community for the D programming language
* `dpaste.com <http://dpaste.com/>`_, another Django pastebin
* `Django snippets <https://djangosnippets.org/>`_, a pastebin for Django code
* `Fayaa <http://www.fayaa.com/code/>`_, a Chinese pastebin
* `Incollo.com <http://incollo.com>`_, a free collaborative debugging tool
* `PasteBox <https://p.boxnet.eu/>`_, a pastebin focused on privacy
* `hilite.me <http://www.hilite.me/>`_, a site to highlight code snippets
* `patx.me <http://patx.me/paste>`_, a pastebin
* `Fluidic <https://github.com/richsmith/fluidic>`_, an experiment in
  integrating shells with a GUI
* `pygments.rb <https://github.com/pygments/pygments.rb>`_, a pygments wrapper for Ruby
* `Clygments <https://github.com/bfontaine/clygments>`_, a pygments wrapper for
  Clojure
* `PHPygments <https://github.com/capynet/PHPygments>`_, a pygments wrapper for PHP
* `Spyder <https://www.spyder-ide.org/>`_, the Scientific Python Development
  Environment, uses pygments for the multi-language syntax highlighting in its
  `editor <https://docs.spyder-ide.org/editor.html>`_.
* `snippet.host <https://snippet.host>`_, minimal text and code snippet hosting

If you have a project or web site using Pygments, `open an issue or PR
<https://github.com/pygments/pygments>`_ and we'll add a line here.
