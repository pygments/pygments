Licensing
=========

The code is distributed under the BSD 2-clause license. Contributors making pull
requests must agree that they are able and willing to put their contributions
under that license.

Goals & non-goals of Pygments
=============================

Python support
--------------

Pygments supports all supported Python versions as per the [Python Developer's Guide](https://devguide.python.org/versions/). Additionally, the default Python version of the latest stable version of RHEL, Ubuntu LTS, and Debian are supported, even if they're officially EOL. Supporting other end-of-life versions is a non-goal of Pygments.

Validation
----------

Pygments does not attempt to validate the input. Accepting code that is not legal for a given language is acceptable if it simplifies the codebase and does not result in surprising behavior. For instance, in C89, accepting `//` based comments would be fine because de-facto all compilers supported it, and having a separate lexer for it would not be worth it.

Contribution checklist
======================

* Check the documentation for how to write
  [a new lexer](https://pygments.org/docs/lexerdevelopment/),
  [a new formatter](https://pygments.org/docs/formatterdevelopment/) or
  [a new filter](https://pygments.org/docs/filterdevelopment/)

* Make sure to add a test for your new functionality, and where applicable,
  write documentation.

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
  a rule like ``@.*@`` will match the whole string ``@first@ second @third@``,
  instead of matching ``@first@`` and ``@second@``. You can use ``@.*?@`` in
  this case to stop early. The ``?`` tries to match _as few times_ as possible.

* Beware of so-called "catastrophic backtracking".  As a first example, consider
  the regular expression ``(A+)*C``.  This is equivalent to ``A*B`` regarding
  what it matches, but *non*-matches will take very long.  This is because
  of the way the regular expression engine works.  Suppose you feed it 50
  'A's, and a 'C' at the end.  It first matches the 'A's greedily in ``A+``,
  but finds that it cannot match the end since 'B' is not the same as 'C'.
  Then it backtracks, removing one 'A' from the first ``A+`` and trying to
  match the rest as another ``(A+)*``.  This fails again, so it backtracks
  further left in the input string, etc.  In effect, it tries all combinations

  ```
  (AAAAAAAAAAAAAAAAA)
  (AAAAAAAAAAAAAAAA)(A)
  (AAAAAAAAAAAAAAA)(AA)
  (AAAAAAAAAAAAAAA)(A)(A)
  (AAAAAAAAAAAAAA)(AAA)
  (AAAAAAAAAAAAAA)(AA)(A)
  ...
  ```

  Thus, the matching has exponential complexity.  In a lexer, the
  effect is that Pygments will seemingly hang when parsing invalid
  input.

  ```python
  >>> import re
  >>> re.match('(A+)*B', 'A'*50 + 'C') # hangs
  ```

  As a more subtle and real-life example, here is a badly written
  regular expression to match strings:

  ```python
  r'"(\\?.)*?"'
  ```

  If the ending quote is missing, the regular expression engine will
  find that it cannot match at the end, and try to backtrack with less
  matches in the ``*?``.  When it finds a backslash, as it has already
  tried the possibility ``\\.``, it tries ``.`` (recognizing it as a
  simple character without meaning), which leads to the same
  exponential backtracking problem if there are lots of backslashes in
  the (invalid) input string.  A good way to write this would be
  ``r'"([^\\]|\\.)*?"'``, where the inner group can only match in one
  way.  Better yet is to use a dedicated state, which not only
  sidesteps the issue without headaches, but allows you to highlight
  string escapes.

  ```python
  'root': [
      ...,
      (r'"', String, 'string'),
      ...
  ],
  'string': [
      (r'\\.', String.Escape),
      (r'"', String, '#pop'),
      (r'[^\\"]+', String),
  ]
  ```

* When writing rules for patterns such as comments or strings, match as many
  characters as possible in each token.  This is an example of what not to
  do:

  ```python
  'comment': [
      (r'\*/', Comment.Multiline, '#pop'),
      (r'.', Comment.Multiline),
  ]
  ```

  This generates one token per character in the comment, which slows
  down the lexing process, and also makes the raw token output (and in
  particular the test output) hard to read.  Do this instead:

  ```python
  'comment': [
      (r'\*/', Comment.Multiline, '#pop'),
      (r'[^*]+', Comment.Multiline),
      (r'\*', Comment.Multiline),
  ]
  ```

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
