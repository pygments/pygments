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
  write documentation. See below on how to test lexers.

* Use the standard importing convention: ``from token import Punctuation``


How to add a lexer
==================

To add a lexer, you have to perform the following steps:

* Select a matching module under ``pygments/lexers``, or create a new
  module for your lexer class.

  .. note::

     We encourage you to put your lexer class into its own module, unless it's a
     very small derivative of an already existing lexer.

* Next, make sure the lexer is known from outside the module. All modules
  in the ``pygments.lexers`` package specify ``__all__``. For example,
  ``esoteric.py`` sets::

     __all__ = ['BrainfuckLexer', 'BefungeLexer', ...]

  Add the name of your lexer class to this list (or create the list if your lexer
  is the only class in the module).

* Finally the lexer can be made publicly known by rebuilding the lexer mapping.

  .. code-block:: console

     $ tox -e mapfiles


How lexers are tested
=====================

To add a new lexer test, create a file with just your code snippet
under ``tests/snippets/<lexer_alias>/``. Then run
``tox -- --update-goldens <filename.txt>`` to auto-populate the
currently expected tokens. Check that they look good and check in the
file.

Lexer tests are run with ``tox``, like all other tests. While
working on a lexer, you can also run only the tests for that lexer
with ``tox -- tests/snippets/language-name/`` and/or
``tox -- tests/examplefiles/language-name/``.

Running the test suite with ``tox`` will run lexers on the test
inputs, and check that the output matches the expected tokens. If you
are improving a lexer, it is normal that the token output changes. To
update the expected token output for the tests, again use
``tox -- --update-goldens <filename.txt>``.  Review the changes and
check that they are as intended, then commit them along with your
proposed code change.

Large test files should go in ``tests/examplefiles``.  This works
similar to ``snippets``, but the token output is stored in a separate
file.  Output can also be regenerated with ``--update-goldens``.


Goals & non-goals of Pygments
=============================

Python support
--------------

Pygments supports all supported Python versions as per the `Python
Developer's Guide <devguide>`_. Additionally, the default Python
version of the latest stable version of RHEL, Ubuntu LTS, and Debian
are supported, even if they're officially EOL. Supporting other
end-of-life versions is a non-goal of Pygments.

.. _devguide: https://devguide.python.org/versions/


Validation
----------

Pygments does not attempt to validate the input. Accepting code that
is not legal for a given language is acceptable if it simplifies the
codebase and does not result in surprising behavior. For instance, in
C89, accepting `//` based comments would be fine because de-facto all
compilers supported it, and having a separate lexer for it would not
be worth it.
