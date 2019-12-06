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
* If you have a tricky case, you can use the ``testcase`` formatter to produce
  an unit test quickly. Run 
  ``python -m pygments -l lua -f testcase <<< "local a = 5"``. This will
  produce a test case function skeleton.