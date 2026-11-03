"""
    Gnuplot lexer tests
    ~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.graphics import GnuplotLexer
from pygments.token import Error, Keyword, Name, Punctuation


@pytest.mark.parametrize('condition', [
    '1',
    '!exists("outfile")',
    '((x > 0) && (sqrt(abs(x)) > 1))',
    'f(g(h(x)), (x + 1)) > 0',
    '''strlen(")") + strlen('(') > 0''',
    'exists("x") && \\\n    (x > 0)',
])
def test_if_condition(condition):
    text = f'if ({condition}) print "ok" # ignored () {{}}\n'
    tokens = list(GnuplotLexer().get_tokens(text))
    assert not any(token is Error for token, _ in tokens)
    assert (Keyword, 'print') in tokens


def test_if_else_blocks():
    text = '''if (!exists("outfile")) outfile = "plot.eps"
if (!exists("imageName")) {
    set title "Comparison of histograms"
    if (sqrt(abs(x)) > 0) {
        print "positive"
    }
} else {
    set title imageName
}
'''
    tokens = list(GnuplotLexer().get_tokens(text))
    assert not any(token is Error for token, _ in tokens)
    assert (Name.Variable, 'outfile') in tokens
    assert tokens.count((Punctuation, '{')) == 3
    assert tokens.count((Punctuation, '}')) == 3
    assert tokens.count((Keyword, 'set')) == 2
    assert (Keyword, 'else') in tokens
    assert (Keyword, 'print') in tokens


def test_unbraced_if_else():
    tokens = list(GnuplotLexer().get_tokens('if (x) print x; else print 0\n'))
    assert not any(token is Error for token, _ in tokens)
    assert tokens.count((Keyword, 'print')) == 2
    assert (Keyword, 'else') in tokens
