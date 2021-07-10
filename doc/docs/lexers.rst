.. -*- mode: rst -*-

================
Available lexers
================

This page lists all available builtin lexers and the options they take.

Currently, **all lexers** support these options:

`stripnl`
    Strip leading and trailing newlines from the input (default: ``True``)

`stripall`
    Strip all leading and trailing whitespace from the input (default:
    ``False``).

`ensurenl`
    Make sure that the input ends with a newline (default: ``True``).  This
    is required for some lexers that consume input linewise.

    .. versionadded:: 1.3

`tabsize`
    If given and greater than 0, expand tabs in the input (default: ``0``).

`encoding`
    If given, must be an encoding name (such as ``"utf-8"``). This encoding
    will be used to convert the input string to Unicode (if it is not already
    a Unicode string). The default is ``"guess"``.

    If this option is set to ``"guess"``, a simple UTF-8 vs. Latin-1
    detection is used, if it is set to ``"chardet"``, the
    `chardet library <https://chardet.github.io/>`_ is used to
    guess the encoding of the input.

    .. versionadded:: 0.6


The "Short Names" field lists the identifiers that can be used with the
`get_lexer_by_name()` function.

These lexers are builtin and can be imported from `pygments.lexers`:

.. pygmentsdoc:: lexers


Iterating over all lexers
-------------------------

.. versionadded:: 0.6

To get all lexers (both the builtin and the plugin ones), you can
use the `get_all_lexers()` function from the `pygments.lexers`
module:

.. sourcecode:: pycon

    >>> from pygments.lexers import get_all_lexers
    >>> i = get_all_lexers()
    >>> i.next()
    ('Diff', ('diff',), ('*.diff', '*.patch'), ('text/x-diff', 'text/x-patch'))
    >>> i.next()
    ('Delphi', ('delphi', 'objectpascal', 'pas', 'pascal'), ('*.pas',), ('text/x-pascal',))
    >>> i.next()
    ('XML+Ruby', ('xml+ruby', 'xml+erb'), (), ())

As you can see, the return value is an iterator which yields tuples
in the form ``(name, aliases, filetypes, mimetypes)``.
