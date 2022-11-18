=======
Plugins
=======

If you want to extend Pygments without hacking the sources, but want to
use the lexer/formatter/style/filter lookup functions (`lexers.get_lexer_by_name`
et al.), you can use `setuptools`_ entrypoints to add new lexers, formatters
or styles as if they were in the Pygments core.

.. _setuptools: https://pypi.org/project/setuptools/

That means you can use your highlighter modules with the `pygmentize` script,
which relies on the mentioned functions.


Plugin discovery
================

At runtime, discovering plugins is preferentially done using Python's
standard library module `importlib.metadata`_, available in Python 3.8
and higher. In earlier Python versions, Pygments attempts to use the
`importlib_metadata`_ backport, if available. If not available, a
fallback is attempted on the older `pkg_resources`_ module. Finally, if
``pkg_resources`` is not available, no plugins will be loaded at
all. Note that ``pkg_resources`` is distributed with `setuptools`_, and
thus available on most Python environments. However, ``pkg_resources``
is considerably slower than ``importlib.metadata`` or its
``importlib_metadata`` backport. For this reason, if you run Pygments
under Python older than 3.8, it is recommended to install
``importlib-metadata``. Pygments defines a ``plugins`` packaging extra,
so you can ensure it is installed with best plugin support (i.e., that
``importlib-metadata`` is also installed in case you are running Python
earlier than 3.8) by specifying ``pygments[plugins]`` as the
requirement, for example, with ``pip``:

.. sourcecode:: shell

   $ python -m pip install --user pygments[plugins]

.. _importlib.metadata: https://docs.python.org/3.10/library/importlib.metadata.html
.. _importlib_metadata: https://pypi.org/project/importlib-metadata
.. _pkg_resources: https://setuptools.pypa.io/en/latest/pkg_resources.html


Defining plugins through entrypoints
====================================

Here is a list of setuptools entrypoints that Pygments understands:

`pygments.lexers`

    This entrypoint is used for adding new lexers to the Pygments core.
    The name of the entrypoint values doesn't really matter, Pygments extracts
    required metadata from the class definition:

    .. sourcecode:: ini

        [pygments.lexers]
        yourlexer = yourmodule:YourLexer

    Note that you have to define ``name``, ``aliases`` and ``filename``
    attributes so that you can use the highlighter from the command line:

    .. sourcecode:: python

        class YourLexer(...):
            name = 'Name Of Your Lexer'
            aliases = ['alias']
            filenames = ['*.ext']


`pygments.formatters`

    You can use this entrypoint to add new formatters to Pygments. The
    name of an entrypoint item is the name of the formatter. If you
    prefix the name with a slash it's used as a filename pattern:

    .. sourcecode:: ini

        [pygments.formatters]
        yourformatter = yourmodule:YourFormatter
        /.ext = yourmodule:YourFormatter


`pygments.styles`

    To add a new style you can use this entrypoint. The name of the entrypoint
    is the name of the style:

    .. sourcecode:: ini

        [pygments.styles]
        yourstyle = yourmodule:YourStyle


`pygments.filters`

    Use this entrypoint to register a new filter. The name of the
    entrypoint is the name of the filter:

    .. sourcecode:: ini

        [pygments.filters]
        yourfilter = yourmodule:YourFilter


How To Use Entrypoints
======================

This documentation doesn't explain how to use those entrypoints because this is
covered in the `setuptools documentation`_. That page should cover everything
you need to write a plugin.

.. _setuptools documentation: https://setuptools.readthedocs.io/en/latest/


Extending The Core
==================

If you have written a Pygments plugin that is open source, please inform us
about that. There is a high chance that we'll add it to the Pygments
distribution.
