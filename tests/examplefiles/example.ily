%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BACH		      	 %%%
%%% Goldberg Variations, BWV 988 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\include "lilypond-book-preamble.ly"

#(define-markup-command (fingerTieUp layout props f1 f2)
                          (string? string?)
    (interpret-markup layout props
      #{
         \markup {
           \override #'(baseline-skip . 1.5)
           \center-column {
             \rotate #180 \fontsize #1.25 \musicglyph #"ties.lyric.default"
             \line { \concat { #f1 \hspace #0.15 #f2 } }
           }
         }
      #}))

#(define-markup-command (fingerTieDown layout props f1 f2)
                          (string? string?)
    (interpret-markup layout props
      #{
         \markup {
           \override #'(baseline-skip . -1.5)
           \center-column {
             \line { \concat { #f1 \hspace #0.15 #f2 } }
             \fontsize #1.25 \musicglyph #"ties.lyric.default"
           }
         }
      #}))

RH-A = \relative c'' {
  \set Score.currentBarNumber = #5
  \bar ""
  d4 d(-1 e8.)\mordent f16 |
  e8 \appoggiatura { d16 } c8-3 \appoggiatura { b16 } a4. fis'!8\turn |
  g32(-3 fis16.) a32( g16.) fis32( e16.) d32(-3 c16.)
  \appoggiatura { c8-1 } a'8. c,16 |
  b32(-3 g16.) fis8 \appoggiatura { fis } g2\mordent |
  b4 b(-1 cis8.)\mordent d16 |
  \voiceOne
  s4 d2 |
  \set Staff.connectArpeggios = ##t
  g4\arpeggio
  \oneVoice
  4.\downprall fis16 g |
  g8-4 \appoggiatura { fis16 } e8 cis4.\lineprall e8 |
}

RH-B = \relative c'' {
  \voiceTwo
  s2.*5 |
  d16 cis b a~ 2 |
  g4\arpeggio
}

RH-C = \relative c'' {
  \voiceThree
  s2.*6 |
  \override NoteColumn.force-hshift = #0.6
  b4
}

RH-D = \relative c'' {
  \voiceFour
  s2.*6 |
  \stemUp
  \override NoteColumn.force-hshift = #0.3
  e4
}

LH-A = \relative c' {
  \clef bass
  \voiceOne
  \override Rest.staff-position = #6
  r4 r g |
  r r a |
  r8 c~ c b16 a g-1 fis e fis |
  g8 a b2 |
  \tweak staff-position #8 r4
  \tweak staff-position #9 r e  |
  a,\finger \markup \fingerTieUp "3" "1" s2 |
  r8 b e4. d8 |
  cis-3 d e2 |
}

LH-B = \relative c {
  \voiceTwo
  b2. |
  c2\finger \markup \fingerTieDown "4" "5"

  c8 d |
  e c d2-\tweak extra-offset #'(-0.65 . 1) \finger \markup \fingerTieDown "4 " "5" |
  g,4. d'8[-5 e8.\mordent fis16] |
  g2.~ |
  4 fis8^\prallprall e fis b |
  e,4.-5 e8 fis g-5 |
  a4.-4 b8 a g |
}

LH-C = \relative c {
  \voiceThree
  s4 d2 |
  s4 e4. s8 |
  s2.*2 |
  s4 b'2 |
}

global = {
  \time 3/4
  \key g\major
  s2.*4 \break
}

\score {

\new PianoStaff
  <<
    \new Staff \with {
      \consists "Span_arpeggio_engraver"
    }
    <<
      \global
      \new Voice { \RH-A }
      \new Voice { \RH-B }
      \new Voice { \RH-C }
      \new Voice { \RH-D }
    >>
    \new Staff
    <<
      \global
      \new Voice { \LH-A }
      \new Voice { \LH-B }
      \new Voice { \LH-C }
    >>
  >>

  \layout {
    indent = 0
    \context {
      \Staff
      \remove "Dot_column_engraver"
      \offset positions #'(-0.5 . 0.5) PianoStaff.Arpeggio
    }
    \context {
      \Voice
      \consists "Dot_column_engraver"
    }
  }
}
