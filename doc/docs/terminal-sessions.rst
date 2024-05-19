=================
Terminal sessions
=================

Pygments support the parsing and highlighting of terminal sessions, like
command-line shells, interactive consoles and language `REPL
<https://en.wikipedia.org/wiki/Read–eval–print_loop>`_.

They are typically command lines or code, mixed with generic output.

Examples for each can be found in the :doc:`lexer <lexers>` documentation.


Operating system shells
-----------------------

These lexers are expecting a prompt to identify user input. So to highlight a
shell session, prefix your code snippet with a specially formatted prompt.

They are typically named ``<shell> Session``.


Interactive consoles
--------------------

Similarly to systems shells, Pygments recognize a variety of interactive
language sessions.

Their IDs typically follow the ``<language>-console`` or
``<language>-repl`` pattern.


Generic output
--------------

To display standalone terminal output and keep styling consistent, you can use
the generic ``output`` lexer.


ANSI rendering
--------------

In all the lexers above, the command results are parsed as generic output.
Which means they are rendered as-is, without any styling applied, for example by ANSI codes.

Here is a couple of third-party projects covering this use-case:

- `pygments-ansi-color
  <https://github.com/chriskuehl/pygments-ansi-color>`_: implements
  a new lexer and formatter to parse and render pure ANSI content.
- `Click Extra <https://github.com/kdeldycke/click-extra>`_: adds
  `ANSI-capable lexers
  <https://kdeldycke.github.io/click-extra/pygments.html#ansi-language-lexers>`_
  for each language listed above.
