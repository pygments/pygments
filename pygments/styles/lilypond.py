# -*- coding: utf-8 -*-
"""
    pygments.styles.lilypond
    ~~~~~~~~~~~~~~~~~~~~~~~~

    LilyPond-specific style.

    :copyright: Copyright 2021-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Token

class LilypondStyle(Style):

    default_style = "#0000ff"

    styles = {
        Token.Whitespace: "",
        Token.Text: "",
        Token.Keyword: "bold",
        Token.Comment: "italic #A3AAB2",
        Token.String: "#C7290A",
        Token.Pitch: "#911520",
        Token.Number: "#976806", # also durations and chord modifiers
        Token.ChordModifier: "#C0870E",
        Token.Name.BackslashReference: "bold #08547A",
        Token.Name.Builtin.MusicCommand: "bold #08547A",
        Token.Name.Constant: "bold",
        Token.Name.Builtin.PaperVariable: "bold #6C5A05",
        Token.Name.Builtin.HeaderVariable: "bold #6C5A05",
        Token.Name.Builtin.MusicFunction: "bold #08547A",
        Token.Name.Builtin.Clef: "#08547A",
        Token.Name.Builtin.Scale: "#08547A",
        Token.Name.Builtin.RepeatType: "#08547A",
        Token.Name.Builtin.Dynamic: "#137607",
        Token.Name.Builtin.Articulation: "#137607",
        Token.Name.Builtin.SchemeFunction: "bold #A83401",
        Token.Name.Builtin.SchemeBuiltin: "bold",
        Token.Name.Builtin.MarkupCommand: "bold #831E71",
        Token.Name.Builtin.Context: "bold #038B7E",
        Token.Name.Builtin.ContextProperty: "#038B7E",
        Token.Name.Builtin.Grob: "bold #D94601",
        Token.Name.Builtin.GrobProperty: "#D94601",
        Token.Name.Builtin.Translator: "bold #6200A4",
    }
