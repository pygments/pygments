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


Defining plugins through entry points
=====================================

We have created a repository with a project template for defining your
own plugins.  It is available at

https://github.com/pygments/pygments-plugin-scaffolding


Installing a custom style
=========================

Here is a complete style-only example using the same packaging tool as the
template. Create a directory for your plugin::

    mkdir pygments-yourstyle
    cd pygments-yourstyle

Save the following as ``your.py``, or use the ``your.py`` containing
``YourStyle`` from :doc:`styledevelopment`:

.. sourcecode:: python

    from pygments.style import Style
    from pygments.token import Keyword

    class YourStyle(Style):
        styles = {Keyword: 'bold #005'}

In the same directory, create ``pyproject.toml`` with this content:

.. sourcecode:: toml

    [build-system]
    requires = ["hatchling"]
    build-backend = "hatchling.build"

    [project]
    name = "pygments-yourstyle"
    version = "0.0.1"
    dependencies = ["pygments"]

    [project.entry-points."pygments.styles"]
    yourstyle = "your:YourStyle"

    [tool.hatch.build.targets.wheel]
    only-include = ["your.py"]

The entry point registers ``yourstyle`` as the name to pass to Pygments.
``your:YourStyle`` means the class ``YourStyle`` in the module ``your.py``;
change these names to match your own file and class. Choose a style name
that does not clash with a built-in style. The project name
``pygments-yourstyle`` is the package name used by pip, not the style name.

Create a virtual environment and install the package from this directory::

    python -m venv venv
    venv/bin/python -m pip install .

Installation registers the entry point automatically; you do not need to
edit Pygments or publish the package on PyPI. List the available styles,
then highlight ``your.py`` with your style::

    venv/bin/pygmentize -L styles
    venv/bin/pygmentize -l python -f html -O full,style=yourstyle -o example.html your.py

The list should include ``yourstyle``. Open ``example.html`` in a browser
to see Python keywords in bold dark blue.

On Windows, use ``venv\Scripts\python.exe`` and
``venv\Scripts\pygmentize.exe`` in place of the ``venv/bin/`` commands.
If another tool uses Pygments, install the plugin into that tool's Python
environment instead: the plugin and Pygments must be installed in the same
environment. After changing the style, run the installation command again.


Extending The Core
==================

If you have written a Pygments plugin that is open source, please inform us
about that. There is a high chance that we'll add it to the Pygments
distribution.
