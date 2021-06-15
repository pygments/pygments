# -*- coding: utf-8 -*-
"""
    pygments.lexers.lilypond
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for LilyPond.

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import inherit, words
from pygments.lexers.lisp import SchemeLexer
from pygments.lexers._lilypond_builtins import (keywords, clefs, scales,
                                                repeat_types, pitches,
                                                music_functions,
                                                dynamics, articulations,
                                                music_commands, markup_commands,
                                                grobs, translators, contexts,
                                                context_properties, grob_properties,
                                                scheme_functions, paper_variables,
                                                header_variables)
from pygments.token import Token

__all__ = ["LilyPondLexer"]

words_token_definitions = [
    (keywords, Token.Keyword),
    (clefs, Token.Name.Builtin.Clef),
    (scales, Token.Name.Builtin.Scale),
    (repeat_types, Token.Name.Builtin.RepeatType),
    (music_functions, Token.Name.Builtin.MusicFunction),
    (dynamics, Token.Name.Builtin.Dynamic),
    (articulations, Token.Name.Builtin.Articulation),
    (music_commands, Token.Name.Builtin.MusicCommand),
    (markup_commands, Token.Name.Builtin.MarkupCommand),
    (grobs, Token.Name.Builtin.Grob),
    (translators, Token.Name.Builtin.Translator),
    (contexts, Token.Name.Builtin.Context),
    (context_properties, Token.Name.Builtin.ContextProperty),
    (grob_properties, Token.Name.Builtin.GrobProperty),
    (paper_variables, Token.Name.Builtin.PaperVariable),
    (header_variables, Token.Name.Builtin.HeaderVariable),
]

word_token_dict = {}
# Walk in reversed order: keywords have priority above everything else.
for word_list, token in reversed(words_token_definitions):
    word_token_dict.update({w: token for w in word_list})

# Avoid highlighting, e.g., "sub" in lyrics as if it were
# a markup command.
need_backslash = [
    Token.Keyword,
    Token.Name.Builtin.MusicFunction,
    Token.Name.Builtin.Dynamic,
    Token.Name.Builtin.Articulation,
    Token.Name.Builtin.MusicCommand,
    Token.Name.Builtin.MarkupCommand,
]

class LilyPondLexer(SchemeLexer):
    """
    Lexer for input to `LilyPond <https://lilypond.org>`_, a text-based music typesetter.

    .. important::

       This lexer is meant to be used in conjuction with the ``lilypond`` style.

    .. versionadded:: 2.10
    """
    name = 'LilyPond'
    aliases = ['lilypond']
    filenames = ['*.ly']
    mimetypes = []

    flags = re.DOTALL | re.MULTILINE | re.UNICODE

    # Because parsing LilyPond input is very tricky (and in fact
    # impossible without executing LilyPond when there is Scheme
    # code in the file), this lexer does not try to recognize
    # lexical modes. Instead, it catches the most frequent pieces
    # of syntax, and, above all, knows about many kinds of builtins.

    # In order to parse embedded Scheme, this lexer subclasses the SchemeLexer.
    # It redefines the 'root' state entirely, and adds a rule for #{ #}
    # to the 'value' state. The latter is used to parse a Scheme expression
    # after #.

    def get_tokens_unprocessed(self, text):
        """Resolve all kinds of builtin names."""
        for index, token, value in super().get_tokens_unprocessed(text):
            if token is Token.Name:
                match = re.match(r"[\-^_]?(\\?)(.*)", value)
                word = match.group(2)
                try:
                    token = word_token_dict[word]
                except KeyError:
                    if match.group(1):
                        # \something, a variable or a call to a function
                        # of some kind (music, markup, void, etc.).
                        token = Token.Name.BackslashReference
                    else:
                        # Perhaps the left-hand-side of an assignment.
                        token = Token.Name
                else:
                    # Avoid highlighting something like "super" (without backslash)
                    # in lyrics as a markup command.
                    if not match.group(1) and token in need_backslash:
                        token = Token.Name
            elif token is Token.Name.Function or token is Token.Name.Variable:
                if value in scheme_functions:
                    token = Token.Name.Builtin.SchemeFunction
            elif token is Token.Name.Builtin:
                token = Token.Name.Builtin.SchemeBuiltin
            yield index, token, value

    tokens = {
        "root": [
            # Whitespace.
            (r"\s+", Token.Whitespace),

            # Multi-line comment. These are non-nestable.
            (r"%{.*?%}", Token.Comment.Multiline),

            # Simple comment.
            (r"%.*?$", Token.Comment),

            # End of embedded LilyPond in Scheme.
            (r"#}", Token.Punctuation, "#pop"),

            # Embedded Scheme, starting with # ("delayed"),
            # or $ (immediate). #@ and and $@ are the lesser known
            # "list splicing operators".
            (r"[#$]@?", Token.Punctuation, "value"),

            # Any kind of punctuation:
            # - sequential music: { },
            # - parallel music: << >>,
            # - voice separator: << \\ >>,
            # - chord: < >,
            # - bar check: |,
            # - dot in nested assignment: system-system-spacing.basic-distance,
            # - equals sign in assignemnts and lists for various commands:
            #   \override Stem.color = red,
            # - comma as alternative syntax for lists: \time 3,3,2 4/4,
            # - colon in tremolos: c:32,
            # - double hyphen in lyrics: li -- ly -- pond,
            (r"\\\\|--|[{}<>=.,:|]", Token.Punctuation),

            # Pitch, with optional octavation marks, octave check,
            # and forced or cautionary accidental.
            (words(pitches, suffix=r"=?[',]*!?\??(?=\d|\W)"), Token.Pitch),

            # String, optionally with direction specifier.
            (r'[\-_^]?"(\\"|[^"])*"', Token.String),

            # Integer, or duration with optional augmentation dots. We have no
            # way to distinguish these, so we highlight them all as numbers.
            (r"-?(\d+|\\longa|\\breve)\.*", Token.Number),

            # Other numbers.
            (r"-?\d+\.\d+", Token.Number.Float), # 5. and .5 are not allowed
            (r"-?\d+/\d+", Token.Number.Fraction),

            # Units.
            (r"\\(mm|cm|in|pt|staff-space)", Token.Number),

            # Separates duration and duration multiplier highlighted as fraction.
            (r"\*", Token.Number),

            # Chord modifiers. A bare 11 is not distinguishable from the
            # respective number, so we highlight as number too.
            (r"((m|dim|aug|maj)(?=\d|\W))|/|[+]", Token.Number),

            # Ties.
            (r"~", Token.Name.Builtin.Articulation),

            # Other common articulations: slurs, phrasing slurs, manual beams,
            # ligature brackets.
            (r"[\-_^]?\\?[()[\]]", Token.Name.Builtin.Articulation),

            # Predefined articulation shortcuts. A direction specifier is
            # required here.
            (r"[\-_^][>^_!.\-+]", Token.Name.Builtin.Articulation),

            # Other articulations whose name is not alphabetic.
            (r"[\-_^]?\\[\-^]", Token.Name.Builtin.Articulation),

            # One music function whose name is not alphabetic.
            (r"\\=", Token.Name.Builtin.MusicFunction),

            # Dynamics whose name is not alphabetic.
            (r"[\-_^]?\\[<>!]", Token.Name.Builtin.Dynamic),

            # Fingering numbers, string numbers.
            (r"[\-_^]?\\?\d+", Token.Name.Builtin.Articulation),

            # Any name or unquoted string, possibly referenced via the backslash,
            # possibly preceded with a direction specifier. Let
            # get_tokens_unprocessed() recognize builtins. [^\W\d_] with
            # re.UNICODE is a trick to match any Unicode letter or underscore.
            (r"[\-_^]?\\?(-|[^\W\d])*[^\W\d_]", Token.Name),

            # Note that the support for figured bass is limited. The double
            # flat -- is taken as a lyric hyphen, and the end of <5-> is
            # interpreted as -> (an accent with neutral direction). _+ and _!
            # are also misinterpreted as articulations. Finally, \\ is the voice
            # separator.

            # Virtually everything can appear in markup mode, so we highlight
            # as text.
            (r".", Token.Text),
        ],
        "value": [
            # Scan a LilyPond value, then pop back since we had a
            # complete expression.
            (r"#{", Token.Punctuation, ("#pop", "root")),
            inherit,
        ]
    }
