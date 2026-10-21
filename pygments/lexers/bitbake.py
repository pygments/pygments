"""
    pygments.lexers.bitbake
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for BitBake recipes, classes, includes and configuration files
    used by the Yocto Project / OpenEmbedded build system.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include, using, words
from pygments.lexers.python import PythonLexer
from pygments.lexers.shell import BashLexer
from pygments.token import Comment, Error, Keyword, Name, Operator, \
    Punctuation, String, Text, Whitespace

__all__ = ['BitBakeLexer']


class _FragmentPythonLexer(PythonLexer):
    """Lex a ``${@...}`` fragment without reporting its incompleteness.

    An inline Python expression is handed over in pieces, split around any
    ``${VAR}`` expansions embedded in it, so a piece routinely begins or ends
    mid-string and is not valid Python on its own. The error would be an
    artefact of the splitting rather than a defect in the metadata.

    This is deliberately the only place errors are suppressed. Errors in
    BitBake's own syntax, and in a complete shell or Python function body,
    are reported.
    """

    def get_tokens_unprocessed(self, text, stack=('root',)):
        for index, token, value in super().get_tokens_unprocessed(text, stack):
            yield index, Text if token is Error else token, value


# ---------------------------------------------------------------------------
# Validity is decided by BitBake, never by this file.
#
# Each pattern is BitBake's own statement regex from ``bb.parse.parse_py``,
# mechanically adapted, and used below only as a zero-width guard: a rule
# fires when and only when BitBake's parser would accept the line. The token
# spans are then chosen separately, which is the part a lexer is for.
#
# Three adaptations are applied, each because BitBake matches one
# already-assembled logical line while a lexer sees raw file text:
#
#   \s   ->  [ \t]                 its \s can never meet a newline, because
#                                  the file has already been split into lines
#   .    ->  (?:[^\n]|\\[ \t]*\n)        continuations are joined before BitBake
#                                  sees them, and inline here instead
#   $    ->  [ \t]*$               the line is rstripped before matching, so
#                                  the anchors tolerate trailing whitespace
#
# The last applies to every anchor including the ones inside the assignment
# rule's negative lookaheads, which are what decide whether the quotes
# balance. Do not hand-edit these; regenerate them and check that the
# verdicts still agree with BitBake's on real metadata.
_BB = {
    'function start':
        '^(?:(?:(?:python(?=(?:(?:[ \\t]|\\\\[ \\t]*\\n)|\\()))|(?:fakeroot(?=(?:[ \\t]|\\\\[ \\t]*\\n))))(?:[ \\t]|\\\\[ \\t]*\\n)*)*(?:[\\w\\.\\-\\+\\{\\}\\$:]+)?(?:[ \\t]|\\\\[ \\t]*\\n)*\\((?:[ \\t]|\\\\[ \\t]*\\n)*\\)(?:[ \\t]|\\\\[ \\t]*\\n)*\\{[ \\t]*$',
    'def block':
        '^def(?:[ \\t]|\\\\[ \\t]*\\n)+(?:\\w+)(?:[^\\n]|\\\\[ \\t]*\\n)*:',
    'export':
        '^export(?:[ \\t]|\\\\[ \\t]*\\n)+(?:[a-zA-Z0-9\\-_+.${}/~]+)[ \\t]*$',
    'unset varflag':
        '^unset(?:[ \\t]|\\\\[ \\t]*\\n)+(?:[a-zA-Z0-9\\-_+.${}/~]+)\\[(?:[a-zA-Z0-9\\-_+.][a-zA-Z0-9\\-_+.@]+)\\][ \\t]*$',
    'unset':
        '^unset(?:[ \\t]|\\\\[ \\t]*\\n)+(?:[a-zA-Z0-9\\-_+.${}/~]+)[ \\t]*$',
    'addpylib':
        '^addpylib(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'addfragments':
        '^addfragments(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'inherit_defer':
        '^inherit_defer(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'inherit':
        '^inherit(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'include_all':
        '^include_all(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'include':
        '^include(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'require':
        '^require(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'EXPORT_FUNCTIONS':
        '^EXPORT_FUNCTIONS(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'addtask':
        '^addtask(?:[ \\t]|\\\\[ \\t]*\\n)+(?:[^#\\n]+)(?:#(?:[^\\n]|\\\\[ \\t]*\\n)*|(?:[^\\n]|\\\\[ \\t]*\\n)*?)',
    'deltask':
        '^deltask(?:[ \\t]|\\\\[ \\t]*\\n)+(?:[^#\\n]+)(?:#(?:[^\\n]|\\\\[ \\t]*\\n)*|(?:[^\\n]|\\\\[ \\t]*\\n)*?)',
    'addhandler':
        '^addhandler(?:[ \\t]|\\\\[ \\t]*\\n)+(?:(?:[^\\n]|\\\\[ \\t]*\\n)+)',
    'assignment':
        '^(?:export(?:[ \\t]|\\\\[ \\t]*\\n)+)?(?:[a-zA-Z0-9\\-_+.${}/~:]*?)(?:\\[(?:[a-zA-Z0-9\\-_+.][a-zA-Z0-9\\-_+.@/]*)\\])?(?:(?:[ \\t]|\\\\[ \\t]*\\n)*)(?:(?::=)|(?:\\?\\?=)|(?:\\?=)|(?:\\+=)|(?:=\\+)|(?:=\\.)|(?:\\.=)|=)(?:(?:[ \\t]|\\\\[ \\t]*\\n)*)(?!\'[^\'\\n]*\'[^\'\\n]*\'[ \\t]*$)(?!\\"[^\\"\\n]*\\"[^\\"\\n]*\\"[ \\t]*$)(?:\'(?:(?:[^\\n]|\\\\[ \\t]*\\n)*)\'|"(?:(?:[^\\n]|\\\\[ \\t]*\\n)*)")[ \\t]*$',
}


def _guard(*names):
    """A zero-width assertion that BitBake's parser would accept this line."""
    # The anchor is hoisted out of the alternation rather than repeated in
    # each branch, which regexlint flags as suspicious (W114).
    return r'(?=^(?:%s))' % '|'.join(_BB[name].lstrip('^') for name in names)


_G_FUNC = _guard('function start')
_G_DEF = _guard('def block')
_G_INCLUDE = _guard('inherit_defer', 'inherit', 'include_all', 'include',
                    'require')
_G_STATEMENT = _guard('addtask', 'deltask', 'addhandler', 'EXPORT_FUNCTIONS',
                      'addpylib', 'addfragments')
_G_ASSIGN = _guard('assignment')
_G_VALUELESS = _guard('export', 'unset varflag', 'unset')

# None of the guards capture, so they add nothing for ``bygroups`` to number.


# A bare BitBake identifier (variable, function or flag name). Allows the
# characters used by OE-Core variable names (digits, ``-``, ``.``, ``+``).
_IDENT = r'[A-Za-z_][A-Za-z0-9_\-.+]*'

# Optional OE override chain such as ``:append``, ``:remove``, ``:class-target``
# or ``:${PN}-doc``. Anchored so it only consumes ``:foo`` runs and never
# eats the leading ``:`` of the ``:=`` assignment operator.
_OVERRIDE = r'(?::[A-Za-z0-9_\-.+/~${}]+)*'

# All BitBake variable assignment operators, ordered so that the longer
# operators win the regex alternation.
_ASSIGN = r'(?:\?\?=|\?=|:=|\+=|=\+|\.=|=\.|=)'

# A variable or function name, matching what BitBake's own parser accepts
# (``[a-zA-Z0-9\-_+.${}/~:]`` for variables, ``[\w.\-+{}$:]`` for
# functions). Names may begin with a digit, may contain ``/`` as in
# ``PREFERRED_PROVIDER_virtual/kernel``, and may embed an expansion as in
# ``PREFERRED_PROVIDER_virtual/${SDK_PREFIX}libc``. The ``:`` introducing
# an override chain is excluded, since ``_OVERRIDE`` consumes that.
_NAME = r':?(?:\$\{[^{}\s]+\}|[A-Za-z0-9_\-.+/~${}])+'

# A varflag name, which may also begin with a digit, as in
# ``ESW_BRANCH[2024.1]``.
_FLAG = r'[A-Za-z0-9_\-.+][A-Za-z0-9_\-.+@/]*'


class BitBakeLexer(RegexLexer):
    """
    Lexer for BitBake recipes, classes, includes and configuration files
    used by the Yocto Project and OpenEmbedded build system.
    """

    name = 'BitBake'
    url = 'https://docs.yoctoproject.org/bitbake/'
    aliases = ['bitbake']
    filenames = ['*.bbclass', '*.bbappend']
    mimetypes = ['text/x-bitbake']
    version_added = '2.21'

    flags = re.MULTILINE

    tokens = {
        'root': [
            (r'[ \t]+$', Whitespace),
            (r'\n', Whitespace),

            # ``handle`` tests for a comment at column zero only, and only
            # after joining continuations, so an indented ``#`` is not a
            # comment and a comment ending in ``\`` swallows the line below.
            (r'^#(?:[^\n]|\\[ \t]*\n)*', Comment.Single),

            # ``python [name]() { ... }`` blocks (also ``fakeroot python``).
            # Must be tried before the generic shell function rule so the
            # ``python`` keyword is not mistaken for a shell function name.
            (_G_FUNC + r'(^(?:fakeroot[ \t]+)?)(python)((?:[ \t]+' + _NAME
             + _OVERRIDE + r')?)'
             r'([ \t]*\([ \t]*\)[ \t]*)(\{[ \t]*\n)'
             r'((?:.*\n)*?)'
             r'(^\}[ \t]*$)',
             bygroups(Keyword.Type, Keyword, Name.Function, Text,
                      Punctuation, using(PythonLexer), Punctuation)),

            # Shell task bodies: ``[fakeroot ]name[:override]() { ... }``.
            (_G_FUNC + r'(^(?:fakeroot[ \t]+)?)(' + _NAME + r')('
             + _OVERRIDE + r')'
             r'([ \t]*\([ \t]*\)[ \t]*)(\{[ \t]*\n)'
             r'((?:.*\n)*?)'
             r'(^\}[ \t]*$)',
             bygroups(Keyword.Type, Name.Function, Name.Decorator, Text,
                      Punctuation, using(BashLexer), Punctuation)),

            # Top-level python ``def`` blocks; the body is any run of
            # indented or blank lines following the signature.
            # BitBake's rule is ``def\s+(\w+).*:``, which validates little
            # and does not require a body, so neither may this. The guard
            # has already decided the line is a def.
            (_G_DEF + r'^def[ \t]+(?:[^\n]|\\[ \t]*\n)*:(?:[^\n]|\\[ \t]*\n)*(?:\n|$)'
             r'(?:[ \t]+.*\n|\n)*',
             using(PythonLexer)),

            # ``inherit`` / ``inherit_defer`` / ``include`` / ``include_all`` /
            # ``require`` directives. Longer keywords are listed first so the
            # regex alternation does not match the shorter prefix.
            (_G_INCLUDE
             + r'^(inherit_defer|inherit|include_all|include|require)\b',
             Keyword.Namespace, 'include-line'),

            # ``addtask`` / ``deltask`` / ``addhandler`` / ``EXPORT_FUNCTIONS``
            # / ``addpylib`` / ``addfragments``.
            (_G_STATEMENT
             + r'^(addtask|deltask|addhandler|EXPORT_FUNCTIONS|addpylib'
             r'|addfragments)\b',
             Keyword, 'statement'),

            # ``VAR[flag] = "value"`` (varflag assignment). The variable may
            # carry an override chain first, as in
            # ``GOARM:arm:class-target[export] = "1"``.
            (_G_ASSIGN + r'^(' + _NAME + r')(' + _OVERRIDE + r')(\[)('
             + _FLAG + r')(\])'
             r'([ \t]*)(' + _ASSIGN + r')',
             bygroups(Name.Variable, Name.Decorator, Punctuation,
                      Name.Attribute, Punctuation, Whitespace, Operator),
             'value'),

            # ``[export ]VAR[:override...] OP "value"`` assignments.
            (_G_ASSIGN + r'^(export[ \t]+)?(' + _NAME + r')('
             + _OVERRIDE + r')'
             r'([ \t]*)(' + _ASSIGN + r')',
             bygroups(Keyword.Type, Name.Variable, Name.Decorator,
                      Whitespace, Operator),
             'value'),

            # ``export VAR`` and ``unset VAR`` / ``unset VAR[flag]``, which
            # carry no value. These follow the assignment rules so that
            # ``export VAR = "value"`` is still matched as an assignment.
            (_G_VALUELESS + r'^(export|unset)([ \t]+)(' + _NAME + r')(\[)('
             + _FLAG + r')'
             r'(\])([ \t]*)$',
             bygroups(Keyword, Whitespace, Name.Variable, Punctuation,
                      Name.Attribute, Punctuation, Whitespace)),
            (_G_VALUELESS + r'^(export|unset)([ \t]+)(' + _NAME
             + r')([ \t]*)$',
             bygroups(Keyword, Whitespace, Name.Variable, Whitespace)),

            # Nothing above matched, so BitBake's parser would reject this
            # line. Report it. A documentation block that is wrong on
            # purpose carries ``:force:``, which skips Sphinx's filter for
            # that block and leaves the marking visible.
            (r'(?:[^\n]|\\[ \t]*\n)+', Error),
        ],

        'include-line': [
            (r'[ \t]+', Whitespace),
            (r'\\\n', Text),
            (r'\n', Whitespace, '#pop'),
            include('interp'),
            (r'\$', String),
            (r'[^\s$]+', String),
            (r'[^\n]', String),
        ],

        'statement': [
            (r'[ \t]+', Whitespace),
            (r'\\\n', Text),
            (r'\n', Whitespace, '#pop'),
            (words(('after', 'before'), suffix=r'\b'), Keyword),
            include('interp'),
            (r'\$', Name),
            (r'[^\s$\\]+', Name),
            (r'[^\n]', Name),
        ],

        'value': [
            (r'[ \t]+', Whitespace),
            # A backslash is special to BitBake only as the last
            # character of a line, where it continues the statement.
            # There are no escape sequences in a value, so treating
            # \\\\ as an escaped pair would swallow the backslash that
            # forms the continuation.
            (r'\\[ \t]*\n', String.Escape),
            (r'\n', Whitespace, '#pop'),
            (r'"', String.Double, 'string-double'),
            (r"'", String.Single, 'string-single'),
            include('interp'),
            (r'\$', String),
            (r'[^\s"\'$\\]+', String),
            (r'[^\n]', String),
        ],

        'string-double': [
            # A backslash is special to BitBake only as the last
            # character of a line, where it continues the statement.
            # There are no escape sequences in a value, so treating
            # \\\\ as an escaped pair would swallow the backslash that
            # forms the continuation.
            (r'\\[ \t]*\n', String.Escape),
            (r'"', String.Double, '#pop'),
            include('interp'),
            (r'\$', String.Double),
            (r'[^"\\$\n]+', String.Double),
            (r'[^\n]', String.Double),
        ],

        'string-single': [
            # A backslash is special to BitBake only as the last
            # character of a line, where it continues the statement.
            # There are no escape sequences in a value, so treating
            # \\\\ as an escaped pair would swallow the backslash that
            # forms the continuation.
            (r'\\[ \t]*\n', String.Escape),
            (r"'", String.Single, '#pop'),
            include('interp'),
            (r'\$', String.Single),
            (r"[^'\\$\n]+", String.Single),
            (r'[^\n]', String.Single),
        ],

        'interp': [
            # ``${@ python expression }`` evaluated by BitBake at parse time.
            (r'\$\{@', String.Interpol, 'py-interp'),
            # ``${VAR}`` variable expansion, which may itself contain further
            # expansions such as ``${TUNE_ARCH:tune-${DEFAULTTUNE}}``.
            (r'\$\{', String.Interpol, 'var-interp'),
        ],

        'var-interp': [
            (r'\}', String.Interpol, '#pop'),
            (r'\\[ \t]*\n', String.Escape),
            (r'\$\{@', String.Interpol, 'py-interp'),
            (r'\$\{', String.Interpol, '#push'),
            (r'[^${}\n\\]+', Name.Variable),
            (r'\$', Name.Variable),
            (r'[^\n]', Name.Variable),
        ],

        'py-interp': [
            (r'\}', String.Interpol, '#pop'),
            (r'\\[ \t]*\n', String.Escape),
            # ``${VAR}`` expansions embedded in the Python expression, as in
            # ``${@bb.utils.contains('F', 'x', '${A}', '', d)}``.
            (r'\$\{', String.Interpol, 'var-interp'),
            # The backslash is excluded so that the continuation rule above
            # gets the chance to match it. Swallowed here instead, the
            # newline ends up matching nothing, the state stack unwinds
            # mid-expression, and every following line of a multi-line
            # ``${@...}`` is lexed at the top level and reported as invalid.
            (r'[^${}\n\\]+', using(_FragmentPythonLexer)),
            # A ``$`` that does not open an expansion belongs to the Python
            # expression's own text, most often a regular-expression anchor
            # inside a quoted string.
            (r'\$', String),
            (r'\{', using(_FragmentPythonLexer)),
            (r'[^\n]', String),
        ],
    }
