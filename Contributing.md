Licensing
=========

The code is distributed under the BSD 2-clause license. Contributors making pull
requests must agree that they are able and willing to put their contributions
under that license.

Contribution checklist
======================

* Check the documentation for how to write
  [a new lexer](https://pygments.org/docs/lexerdevelopment/),
  [a new formatter](https://pygments.org/docs/formatterdevelopment/) or
  [a new filter](https://pygments.org/docs/filterdevelopment/)

* When writing rules, try to merge simple rules. For instance, combine:

  ```python
  _PUNCTUATION = [
    (r"\(", token.Punctuation),
    (r"\)", token.Punctuation),
    (r"\[", token.Punctuation),
    (r"\]", token.Punctuation),
    ("{", token.Punctuation),
    ("}", token.Punctuation),
  ]
  ```

  into:

  ```python
  (r"[\(\)\[\]{}]", token.Punctuation)
  ```

* Be careful with ``.*``. This matches greedily as much as it can. For instance,
  rule like ``@.*@`` will match the whole string ``@first@ second @third@``,
  instead of matching ``@first@`` and ``@second@``. You can use ``@.*?@`` in
  this case to stop early. The ``?`` tries to match _as few times_ as possible.

* Don't add imports of your lexer anywhere in the codebase. (In case you're
  curious about ``compiled.py`` -- this file exists for backwards compatibility
  reasons.)

* Use the standard importing convention: ``from token import Punctuation``

* For test cases that assert on the tokens produced by a lexer, use tools:

  * You can use the ``testcase`` formatter to produce a piece of code that
    can be pasted into a unittest file:
    ``python -m pygments -l lua -f testcase <<< "local a = 5"``

  * Most snippets should instead be put as a sample file under
    ``tests/snippets/<lexer_alias>/*.txt``. These files are automatically
    picked up as individual tests, asserting that the input produces the
    expected tokens.

    To add a new test, create a file with just your code snippet under a
    subdirectory based on your lexer's main alias. Then run
    ``pytest --update-goldens <filename.txt>`` to auto-populate the currently
    expected tokens. Check that they look good and check in the file.

    Also run the same command whenever you need to update the test if the
    actual produced tokens change (assuming the change is expected).

  * Large test files should go in ``tests/examplefiles``.  This works
    similar to ``snippets``, but the token output is stored in a separate
    file.  Output can also be regenerated with ``--update-goldens``.
