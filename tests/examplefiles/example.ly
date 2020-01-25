#(set-global-staff-size 16)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BERG		 %%%
%%% String Quartet, Op.3 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\include "lilypond-book-preamble.ly"

pVoll = \markup {
  \dynamic p
  \hspace #0.25
  "(voll)"
}

dyn =
  #(define-music-function (parser location shift) (number-or-pair?)
     (_i "Shift dynamics.")
     (if (pair? shift)
         #{
           \once \override DynamicText.extra-offset = $shift
           \once \override DynamicTextSpanner.extra-offset = $shift
           \once \override Hairpin.extra-offset = $shift
         #}
         #{
           \once \override DynamicText.extra-offset = #(cons 0 shift)
           \once \override DynamicTextSpanner.extra-offset = #(cons 0 shift)
           \once \override Hairpin.extra-offset = #(cons 0 shift)
         #}))

violin-One = \relative c''' {

  \set Score.currentBarNumber = #173
  \bar ""
  \time 3/8

  \grace { \once \hideNotes
            \dyn #-0.85
            e8^(\< }
  d8)-.\!
  -\tweak X-offset #-4.5
  -\tweak extra-offset #'(0 . 2) ^"Klang:"
  << {
    <d, g\harmonic>4
      -.
      \upbow
      -\tweak X-offset #-5.5 ^"Flag."
      -\tweak X-offset #-3.5 \pppp
      -\tweak to-barline ##t \<
    \once \stemUp
    \tuplet 3/2 {
      es,16-.\pp\downbow
      -\tweak X-offset #-1 ^"gewöhnlich" 16-. 16~-- } 8
    \dyn #-0.5
    es''8 -\tweak minimum-length #6 ~
      -\tweak extra-offset #'(-0.25 . 2.75) --
      \upbow
      -\tweak X-offset #-2.5 \ppp\<
    8-.\!
      -\tweak X-offset #-4.5
      -\tweak extra-offset #'(0 . -1) ^"Klang:"
    <es, as\harmonic>4
      -.
      \upbow
      -\tweak X-offset #-5.5 ^"Flag."
      -\tweak X-offset #-3.5 \pppp
      -\tweak to-barline ##t
      -\tweak minimum-length #12 \<
    \tuplet 3/2 {
      e,!16
        \ppp
        --
        \downbow
        ^"col legno gestrichen"
      16-- 16~-- }
    \dyn #-0.5
    8\< f~-.--\upbow
    16\! r }
  \new Staff \with {
        \remove "Time_signature_engraver"
        alignAboveContext = #"violin-One"
        \magnifyStaff #3/4
        firstClef = ##f }
  { \override ParenthesesItem.font-size = #3
    \override ParenthesesItem.extra-offset = #'(0 . -0.5)
    \override Staff.OttavaBracket.stencil = #ly:line-spanner::print
    \override Staff.OttavaBracket.bound-details =
    #`((left . ((Y . 0) ; Change the integer here
                (attach-dir . ,LEFT)
                (padding . -5.5)
                (stencil-align-dir-y . ,CENTER)))
       (right . ((Y . 0) ; Change the integer here
                 (padding . -1)
                 (attach-dir . ,RIGHT)
                 (text . ,(make-draw-dashed-line-markup (cons 0 -1.2))))))
    \override Staff.OttavaBracket.left-bound-info =
       #ly:line-spanner::calc-left-bound-info-and-text
    \override Staff.OttavaBracket.right-bound-info =
       #ly:line-spanner::calc-right-bound-info
    \ottava#1
    <\parenthesize
     \tweak NoteHead.font-size #-4
     \tweak Accidental.font-size #-4
      g'!
    cis fis b! d!>4*1/2
    \ottava #0
    \stopStaff
    s8 s4. s8
    \startStaff
    \ottava#1
    <\parenthesize
     \tweak NoteHead.font-size #-4
     \tweak Accidental.font-size #-4
      g!
    cis fis b! d!>4*1/2
    \ottava #0
    \stopStaff
  } >>
  <b,! e!>8.
    -.
    --
    \upbow
    -\tweak X-offset #-1.5 \pp
    -\tweak minimum-length #8 \>
  r16\!
  \override Beam.positions = #'(2.75 . 2.75)
  \tempo "Ein wenig belebter"
  \override DynamicText.Y-offset = #-4
  \override Hairpin.Y-offset = #-3.5
  e,!8:64
    \pp
    -\tweak padding #1.82 ^"trem."
    ^"gewöhnlich, am Steg"
    ^\shape #'((1 . -3.75)(0.75 . -4)(-0.5 . -4)(-0.75 . -3.75)) (
    _\shape #'((1.6 . 0)(1 . 0)(0.25 . 0)(0 . 0)) (
  <d! f!>)
    -\tweak to-barline ##t \<
    -\tweak X-offset #1.25
    ^\markup \italic "espr."
  <cis fis>:
    ^\shape #'((1.5 . 1)(1.5 . 0.9)(0.25 . 0.5)(0 . 0.25)) (
    _\shape #'((2.45 . 0)(2.25 . 0)(0.25 . 0)(0 . 0)) (
  <c! g'!>))\! <cis fis>:\>
    ^\shape #'((1.25 . -3.75)(1 . -4)(-0.25 . -4)(-0.5 . -4)) (
    _\shape #'((1.75 . 0.15)(1.25 . 0.15)(0.25 . -0.15)(0 . -0.35)) (
  \override Score.MetronomeMark.padding = #4.25
  \tempo "rit."
  <d! f!>))\!
}

violin-Two = \relative c'' {

  \grace { \once \hideNotes
           \dyn #-0.85
           f8^(\<
           _ \shape #'((0.5 . -0.5)(0.5 . -0.25)(0 . -0.15)(0 . -0.4)) ( }
  <fis b>8)-.\!
  \dyn #1
  <b,! e!\harmonic>4
    -.
    \upbow
    -\tweak self-alignment-X #-0.2 ^"Flag."
    -\tweak X-offset #-3.5 \pppp
    -\tweak to-barline ##t \<
  \tuplet 3/2 {
    bes,16-.\pp\downbow
    -\tweak X-offset #-1 ^"gewöhnlich"
    16-. 16~-- } 8
  \dyn #-0.75
  <f''! bes>~
    --
    \upbow
    -\tweak X-offset #-2.5 \ppp\<
  q-.\!
  << { \voiceOne
       \stemDown
       \set fingeringOrientations = #'(right)
       <bes,-\tweak extra-offset #'(0 . 1)-2
        es-\tweak extra-offset #'(0 . 1.25)-4\harmonic>4
         -\tweak X-offset #-3.5 \pppp
         -\tweak to-barline ##t \<
       \tuplet 3/2 {
         \oneVoice
         b,!16
         \ppp
         --
         \downbow
         -\tweak whiteout ##t ^"col legno gestrichen"
      16-- 16~-- }
       \dyn #-0.2
       8\< bes~-.--\upbow
       16\! r
     } \\
     { \voiceTwo
       \once \override NoteColumn.force-hshift = #1
       \override ParenthesesItem.font-size = #3
       \override ParenthesesItem.extra-offset = #'(0 . -1)
       \set fingeringOrientations = #'(right)
      <des-\tweak extra-offset #'(0.75 . 0.5)-1
      \parenthesize
       as'-\tweak extra-offset #'(0.75 . 0.5)-4\harmonic>4 } >>
  es'8.-.--\upbow
    -\tweak X-offset #-1.5 \pp\>
  r16\!
  \override DynamicText.Y-offset = #-5.75
  \override Hairpin.Y-offset = #-5.25
  b,!8:64
    \pp
    ^"trem."
    -\tweak whiteout ##t ^"gewöhnlich, am Steg"
    -\shape #'((1.6 . 0)(1.25 . 0.15)(0.25 . 0.15)(0 . 0)) (
  bes)
    -\tweak to-barline ##t \<
    -\tweak X-offset #1.25
    -\tweak extra-offset #'(0 . -0.25)
    ^\markup \italic "espr."
  a!:
    -\shape #'((2.5 . -0.65)(2.25 . -0.5)(0.25 . -0.15)(0 . -0.15)) (
  gis)\! a!:\>
    -\shape #'((1.75 . 0.5)(1.25 . 0.5)(0.2 . 0.2)(0 . 0)) (
  bes)\!
}

viola = \relative c'' {
  \clef alto
  \grace { \once \hideNotes
            \dyn #-1.25
            a8^(\< }
  g8)\!-.
  << { \voiceOne
       \set fingeringOrientations = #'(right)
        <fis-\tweak extra-offset #'(0 . 0.5) -1
         b!-\tweak extra-offset #'(0 . 0.5) -4\harmonic>4
          -.
          \upbow
          -\tweak X-offset #-5.5 ^"Flag."
          -\tweak X-offset #-3.5 \pppp
          -\tweak Y-offset #0.75
          -\tweak to-barline ##t \<
        \tuplet 3/2 {
        \oneVoice \stemUp
        \dyn #1
        f,!16-.\pp\downbow
        -\tweak X-offset #-1 ^"gewöhnlich" f-. f~-- }
        f8
        \dyn #-0.75
        as'^~
          --
          \upbow
          -\tweak X-offset #-2.5 \ppp\<
        \stemNeutral
        as-.\!
     } \\
     { \voiceTwo
       \set fingeringOrientations = #'(right)
       \override ParenthesesItem.font-size = #3
       \override ParenthesesItem.extra-offset = #'(0 . -0.75)
       <g,! \parenthesize c!
         -\tweak extra-offset #'(0.75 . 0)-2\harmonic>4 } >>
  \dyn #-0.5
  <f'! bes\harmonic>4
    -.
    \upbow
    -\tweak X-offset #-5.5 ^"Flag."
    -\tweak X-offset #-3.5 \pppp
    -\tweak to-barline ##t \<
  \tuplet 3/2 {
    \dyn #-0.5
    <g, es'>16
      \ppp
      --
      \downbow
      -\tweak whiteout ##t ^"col legno gestrichen"
    q-- q~-- }
  q8\< <fis d'!>~-.---\upbow
  q16\! r
  \dyn #-0.5
  g'!8.-.--\upbow
  -\tweak X-offset #-1.5 \pp\> r16\!
  \override DynamicText.Y-offset = #-3.5
  \override Hairpin.Y-offset = #-3
  \override Beam.positions = #'(5 . 4.5)
  <g,! es'>8:64
    \pp
    ^"trem."
    -\tweak whiteout ##t ^"gewöhnlich, am Steg"
    ^\shape #'((1 . -4)(0.5 . -4.15)(-0.5 . -4)(-0.75 . -3.75)) (
    _\shape #'((1.5 . -0.2)(1.15 . -0.25)(0.25 . -0.25)(0 . -0.25)) (
  <fis d'!>))
    -\tweak to-barline ##t \<
    -\tweak X-offset #1.25
    -\tweak extra-offset #'(0 . -0.15)
    ^\markup \italic "espr."
  <f des'>:
    ^\shape #'((1.5 . 0)(1.25 . 0)(0.15 . 0.25)(0 . 0.25)) (
    _\shape #'((2.5 . -0.25)(2.25 . -0.25)(0 . -0.25)(0 . -0.25)) (
  \override Beam.positions = #'(4.5 . 5)
  <e! c'!>))\! <f! des'>:\>
    ^\shape #'((1.25 . -3.75)(1 . -4)(-0.25 . -4)(-0.5 . -4)) (
    _\shape #'((1.75 . -0.5)(1.5 . -0.25)(0.25 . -0.25)(0 . -0.5)) (
  <fis d'>))\!

}

cello = \relative c' {

  \clef bass
  \grace { \once \hideNotes
            \dyn #-0.75
            d8^(\< }
  cis8)\!-.
  \clef tenor
  \dyn #'(-1.5 . 2.5)
  <cis fis\harmonic>4
    -.
    \upbow
    -\tweak X-offset #-5.5 ^"Flag."
    \pppp -\tweak bound-padding #1 \<
  \clef bass
  \tuplet 3/2 {
    <c,,! as'>16-.\pp
    -\tweak X-offset #-1 ^"gewöhnlich"
    q-. q~--
  }
  q8
  \dyn #-2.4
  c''!~
    --
    \upbow
    -\tweak X-offset #-2.5 \ppp\<
  c8-.\!
  \clef tenor
  <c! f!\harmonic>4
    -.
    \upbow
    -\tweak X-offset #-5.5 ^"Flag."
    -\tweak X-offset #-3.5 \pppp
    -\tweak to-barline ##t \<
  \clef bass
  g,!8-.->\p ^"gewöhnlich"
  cis,4-.->
  g'!-.->
  cis,8~-.->
  8-!
  g'!4-\markup \pVoll ^"pizz."
  cis,8-. g'!-. cis,-.
}


\score {
  \new StaffGroup
  <<
    \new Staff = "violin-One" \violin-One
    \new Staff = "violin-Two" \violin-Two
    \new Staff = "viola" \viola
    \new Staff = "cello" \cello
  >>
  \layout {
    indent = 0
    ragged-right = ##f
    \context {
      \Score
      \omit TupletBracket
      \override TupletBracket.avoid-scripts = ##f
      \override Hairpin.to-barline = ##f
      \override Beam.damping = #2
      \override BarNumber.self-alignment-X = #LEFT
      \override BarNumber.padding = #2.25
    }
  }
}
