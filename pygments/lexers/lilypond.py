# -*- coding: utf-8 -*-
"""
    pygments.lexers.lilypond
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for LilyPond.

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
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


def builtin_words_with_prefix(names, prefix):
    return words(names, prefix, suffix=r"(?!-|[^\W\d])")

def builtin_backslashed_words(names):
    return builtin_words_with_prefix(names, prefix=r"[\-_^]?\\")

def builtin_words(names):
    # Backslashes are always allowed.  This is because you can often
    # find a context where the form with the backslash will do something
    # useful, for example:
    # \layout { \context { \Score ... } }
    return builtin_words_with_prefix(names, prefix=r"[\-_^]?\\?")

class LilyPondLexer(SchemeLexer):
    """
    Lexer for input to `LilyPond <https://lilypond.org>`_, a text-based music typesetter.

    .. important::

       This lexer is meant to be used in conjunction with the ``lilypond`` style.

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
        """Highlight Scheme variables as LilyPond builtins when applicable."""
        for index, token, value in super().get_tokens_unprocessed(text):
            if token is Token.Name.Function or token is Token.Name.Variable:
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
            (r"%\{.*?%\}", Token.Comment.Multiline),

            # Simple comment.
            (r"%.*?$", Token.Comment),

            # End of embedded LilyPond in Scheme.
            (r"#\}", Token.Punctuation, "#pop"),

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

            # Ties, slurs, manual beams.
            (r"[~()[\]]", Token.Name.Builtin.Articulation),

            # Predefined articulation shortcuts. A direction specifier is
            # required here.
            (r"[\-_^][>^_!.\-+]", Token.Name.Builtin.Articulation),

            # Fingering numbers, string numbers.
            (r"[\-_^]?\\?\d+", Token.Name.Builtin.Articulation),

            # Builtins.
            (builtin_backslashed_words(keywords), Token.Keyword),
            (builtin_words(clefs), Token.Name.Builtin.Clef),
            (builtin_words(scales), Token.Name.Builtin.Scale),
            (builtin_words(repeat_types), Token.Name.Builtin.RepeatType),
            (builtin_backslashed_words(music_functions), Token.Name.Builtin.MusicFunction),
            (builtin_backslashed_words(dynamics), Token.Name.Builtin.Dynamic),
            (builtin_backslashed_words(articulations), Token.Name.Builtin.Articulation),
            (builtin_backslashed_words(music_commands), Token.Name.Builtin.MusicCommand),
            (builtin_backslashed_words(markup_commands), Token.Name.Builtin.MarkupCommand),
            (builtin_words(grobs), Token.Name.Builtin.Grob),
            (builtin_words(translators), Token.Name.Builtin.Translator),
            (builtin_words(contexts), Token.Name.Builtin.Context),
            (builtin_words(context_properties), Token.Name.Builtin.ContextProperty),
            (builtin_words(grob_properties), Token.Name.Builtin.GrobProperty),
            (builtin_words(paper_variables), Token.Name.Builtin.PaperVariable),
            (builtin_words(header_variables), Token.Name.Builtin.HeaderVariable),

            # Other backslashed-escaped names (like dereferencing a
            # music variable.)  Dashes (as in markup commands) and underscores
            # (as in engravers) are allowed, but not at the end, to avoid
            # matching the direction specifier of the next element.
            (r"[\-_^]?\\(-|[^\W\d])*[^\W\d_]", Token.Name.BackslashReference),

            # Virtually everything can appear in markup mode, so we highlight
            # as text.
            (r".", Token.Text),
        ],
        "value": [
            # Scan a LilyPond value, then pop back since we had a
            # complete expression.
            (r"#\{", Token.Punctuation, ("#pop", "root")),
            inherit,
        ]
    }
