=======
Plugins
=======

If you want to extend Pygments without hacking the sources, you can use
package `entry points`_ to add new lexers, formatters, styles or filters
as if they were in the Pygments core.

.. _entry points: https://packaging.python.org/en/latest/guides/creating-and-discovering-plugins/

The idea is to create a Python package, declare how extends Pygments,
and install it.

This will allow you to use your custom lexers/... with the
``pygmentize`` command. They will also be found by the lookup functions
(``lexers.get_lexer_by_name`` et al.), which makes them available to
tools such as Sphinx, mkdocs, ...


Plugin discovery
================

At runtime, discovering plugins is preferentially done using Python's
standard library module `importlib.metadata`_, available in Python 3.8
and higher. In earlier Python versions, Pygments attempts to use the
`importlib_metadata`_ backport, if available. If not available, a
fallback is attempted on the older `pkg_resources`_ module. Finally, if
``pkg_resources`` is not available, no plugins will be loaded at
all. Note that ``pkg_resources`` is distributed with setuptools, and
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


Defining plugins through entry points
=====================================

We have created a repository with a project template for defining your
own plugins.  It is available at

https://github.com/pygments/pygments-plugin-scaffolding


Extending The Core
==================

If you have written a Pygments plugin that is open source, please inform us
about that. There is a high chance that we'll add it to the Pygments
distribution.
