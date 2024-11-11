========================
Contributing to Pygments
========================

Thanks for your interest in contributing! Please read the following
guidelines.


Licensing
=========

The code is distributed under the BSD 2-clause license. Contributors making pull
requests must agree that they are able and willing to put their contributions
under that license.


General contribution checklist
==============================

* Check the documentation for how to write
  :doc:`a new lexer <lexerdevelopment>`,
  :doc:`a new formatter <formatterdevelopment>`,
  :doc:`a new style <styledevelopment>` or
  :doc:`a new filter <filterdevelopment>`.
  If adding a lexer, please make sure you have
  read :ref:`lexer-pitfalls`.

* Run the test suite with ``tox``, and ensure it passes.

* Make sure to add a test for your new functionality, and where applicable,
  write documentation.

* Use the standard importing convention: ``from token import Punctuation``

Goals & non-goals of Pygments
=============================

Python support
--------------

Pygments supports all supported Python versions as per the `Python
Developer's Guide`_. Additionally, the default Python
version of the latest stable version of RHEL, Ubuntu LTS, and Debian
are supported, even if they're officially EOL. Supporting other
end-of-life versions is a non-goal of Pygments.

.. _Python Developer's Guide: https://devguide.python.org/versions/


Validation
----------

Pygments does not attempt to validate the input. Accepting code that
is not legal for a given language is acceptable if it simplifies the
codebase and does not result in surprising behavior. For instance, in
C89, accepting `//` based comments would be fine because de-facto all
compilers supported it, and having a separate lexer for it would not
be worth it.

Language support
----------------

While we strive for the broadest language support possible, we can't support
every programming language on the planet. Our minimum bar is fairly low, but to
avoid pet projects and other one-off languages, we expect any language that is
proposed for inclusion to have a reasonably sized community around it. If you
need a syntax highlighter for your in-house programming language or the brand
new language project you kicked off, consider writing a :doc:`plugin <plugins>`
until it gains enough popularity.
