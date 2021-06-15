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
        Token.Keyword: "bold", ## #8E2FCF",
        Token.Comment: "italic #A3AAB2", #4080B0",
        Token.String: "#C7290A",
        Token.Pitch: "#3A9097", ##"#0E916F",
        Token.Number: "#C0870E", # also durations and chord modifiers
        Token.ChordModifier: "#C0870E",
        Token.Name.BackslashReference: "#0C84C0",
        Token.Name.Constant: "bold",
        Token.Name.Builtin.PaperVariable: "bold #559100",
        Token.Name.Builtin.HeaderVariable: "bold #6C5A05",
        Token.Name.Builtin.MusicFunction: "bold #08547A",
        Token.Name.Builtin.Clef: "#08547A",
        Token.Name.Builtin.Scale: "#08547A",
        Token.Name.Builtin.RepeatType: "#08547A",
        Token.Name.Builtin.Dynamic: "#A60590",
        Token.Name.Builtin.Articulation: "#C75994",
        Token.Name.Builtin.MusicCommand: "bold #0C84C0",
        Token.Name.Builtin.SchemeFunction: "bold #B63700", ##"bold #1A5C46",
        Token.Name.Builtin.SchemeBuiltin: "bold",
        Token.Name.Builtin.MarkupCommand: "bold #019916", #"#3AB00B",
        Token.Name.Builtin.Context: "bold #029D7F",
        Token.Name.Builtin.ContextProperty: "#029D7F",
        Token.Name.Builtin.Grob: "bold #EC4D02",
        Token.Name.Builtin.GrobProperty: "#EC4D02",
        Token.Name.Builtin.Translator: "bold #5A029D",
    }
