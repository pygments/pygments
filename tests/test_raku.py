"""
    Raku lexer tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Error
from pygments.lexers import get_lexer_by_name, guess_lexer, \
    get_lexer_for_filename
from pygments.lexers.raku import RakuLexer


@pytest.mark.parametrize('alias', ['raku', 'perl6', 'pl6'])
def test_aliases(alias):
    assert isinstance(get_lexer_by_name(alias), RakuLexer)


def test_old_import_location():
    # Raku used to live in pygments.lexers.perl as Perl6Lexer
    from pygments.lexers.perl import Perl6Lexer
    from pygments.lexers.raku import Perl6Lexer as Perl6LexerFromRaku
    assert Perl6Lexer is RakuLexer
    assert Perl6LexerFromRaku is RakuLexer


def test_filenames():
    for name in ('a.raku', 'a.rakumod', 'a.rakutest', 'a.rakudoc',
                 # the Perl 6 era extensions keep working
                 'a.p6', 'a.pm6', 'a.pl6', 'a.6pl', 'a.p6l', 'a.6pm', 'a.p6m'):
        assert isinstance(get_lexer_for_filename(name), RakuLexer)


@pytest.mark.parametrize('code', [
    'use v6;\nsay "hi";\n',
    'use v6.d;\nsay "hi";\n',
    'use v6.c;\nsay "hi";\n',
    'use v6.*;\nsay "hi";\n',
    'use v6.e.PREVIEW;\nsay "hi";\n',
    'use v6.d+;\nsay "hi";\n',
    'unit module Foo;\nsub bar() { }\n',
    '#!/usr/bin/env perl6\nsay 1;\n',
    '#!/usr/bin/env raku\nsay 1;\n',
    '#!/usr/bin/raku\nsay 1;\n',
])
def test_guess(code):
    assert isinstance(guess_lexer(code), RakuLexer)


def test_every_character_is_preserved():
    # callbacks that split strings/heredocs must not drop or duplicate text
    code = (
        'my $t = qq:to/END/; # c\n  a $b {"x {1}"} c\n  END\nsay $t;\n'
        'say qq{a {$x} b}, "unterminated {', 'say q:to/X/;\nnever closed\n'
    )
    lexer = RakuLexer()
    for snippet in code:
        assert ''.join(v for _, v in lexer.get_tokens(snippet)) == snippet + \
            ('' if snippet.endswith('\n') else '\n')


def test_fuzz_never_loses_text():
    # the quote-like, heredoc, regex and pod callbacks all slice the source by
    # hand, so make sure nothing is dropped or duplicated on malformed input
    import random
    pieces = ['qq{', 'q:to/E/', 'qq:to/E/', '"{', '<<', '\u00ab', 's/a/', 's{x}{',
              'm/', 'tr/a/', '=begin pod\n', '=end pod', 'B<', '#`(', '$x<', '@a[',
              '{', '}', '(', ')', '\\', '/', '<[', '<?{', '\n', 'token T {', 'use ',
              'X::', '.', '-->', '\u00b2', 's:g/', 'OUTER:', 'Z+', '<1/3>', 'q =>',
              '>>', '>', '\u2018', '\u201c', '\uff62', '$(', 'rule ', '= /',
              "'/tmp/'", 'qq:to/E/, q:to/F/', 'E\n', 'F\n']
    rng = random.Random(1234)
    lexer = RakuLexer(stripnl=False, ensurenl=False)
    for _ in range(500):
        source = ''.join(rng.choice(pieces + list('ab $@%&;=:<>'))
                         for _ in range(rng.randint(1, 40)))
        tokens = list(lexer.get_tokens(source))
        assert ''.join(value for _, value in tokens) == source
        # every character must be matched by some rule
        assert not any(token is Error for token, _ in tokens), source


@pytest.mark.parametrize('code', [
    'use v5.36;\nsay "hi";\n',
    'use v7;\nsay "hi";\n',
    'use strict;\nuse warnings;\nprint "hi\\n";\n',
])
def test_not_guessed_from_other_versions(code):
    # only a v6 version statement marks a file as Raku
    assert RakuLexer.analyse_text(code) == 0.0
