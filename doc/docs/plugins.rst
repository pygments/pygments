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


Extending The Core
==================

If you have written a Pygments plugin that is open source, please inform us
about that. There is a high chance that we'll add it to the Pygments
distribution.
