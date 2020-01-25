\version "2.19.15"
\language "english"

\include "dynamics-defs.ily"

altosaxINotes = \transpose c ef {
  \relative a'' {
    \transposition ef
    \clef "treble"
    R1*4 | % 5
    r2 a4 \p ( c8. [ bf16 ] | % 6
    a4 ) c,8. ( [ d16 ] ef4 bf8. [ c16 ] | % 7
    d4 ) r4 r2 | % 8
    R1*5 | % 13
    r2 \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      a8 \p -. [ a8 -. a8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      a8 -. [ a8 -. a8 -. ]
    }
    | % 14
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      d8 -. [ \cresc d8 -. d8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      d8 -. [ d8 -. d8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      a8 -. [ a8 -. a8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      a8 -. [ a8 -. a8 -. ]
    }
    | % 15
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      d8 -. [ d8 -. d8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      d8 -. [ d8 -. d8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 -. [ e8 -. e8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 -. [ e8 -. e8 -. ]
    }
    | % 16
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 -. [ e8 -. e8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 -. [ e8 -. e8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      g8 -. [ g8 -. g8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      g8 -. [ g8 -. g8 -. ]
    }
    | % 17
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      bf8 -. [ bf8 -. bf8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      bf8 -. [ bf8 -. bf8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      bf8 -. [ bf8 -. bf8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      bf8 -. [ bf8 -. bf8 -. ]
    }
    | % 18
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      bf8 -. [ bf8 -. bf8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      bf8 -. [ bf8 -. bf8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      bf8 -. [ bf8 -. bf8 -. ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      bf8 -. [ bf8 -. bf8 -. ]
    }
    | % 19
    bf8 r8 e,4 ~ -> e8 r8 e4 ~ -> | \barNumberCheck #20
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    | % 21
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      g8 [ e8 \pp e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    | % 22
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    | % 23
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    | % 24
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ \cresc e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    | % 25
    e2 \ff -> e4 -> e4 -> | % 26
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    \once \override TupletBracket #'stencil = ##f
    \times 2/3  {
      e8 [ e8 e8 ]
    }
    | % 27
    e2 -> e4 -> e4 -> | % 28
    e2 -> e4 -> e4 -> | % 29
    e4 -> e2 -> e4 -> | \barNumberCheck #30
    e4 -> e4 -> e4 -> e4 -> | % 31
    cs4 -> fs4 -> fs4 -> fs4 -> | % 32
    e2 -> fs4 gs4 | % 33
    a2 -> r2 | % 34
    R1*3
    \once \override MultiMeasureRest #'minimum-length = #8
    R1 \fermataMarkup
    R1*5
    \once \override MultiMeasureRest #'minimum-length = #8
    R1 \fermataMarkup
    R1*14 | % 58
    bf,4 ( ~ \p
    \times 2/3  {
      bf8 [ a8 g8 ]
    }
    c4 gs4 ) | % 59
    a8 ( [ c8 \< a'8 g8 ~ ] \! g8 [ f8 \> c8 a8 ) ] | \barNumberCheck
    #60
    bf4 \p ( ~
    \times 2/3  {
      bf8 [ a8 g8 ]
    }
    c4 g8 ) r8 | % 61
    c4 ( ~ \cresc
    \times 2/3  {
      c8 [ bf8 a8 ]
    }
    d4 a8 ) r8 | % 62
    d8. ( [ ef16 ) ] ef8. ( [ d16 ) ] d8. ( [ ef16 ) ] ef8. ( [ d16 ) ]
    | % 63
    ef8. ( -> [ d16 ) ] ef8. ( -> [ d16 ) ] ef8. ( -> [ d16 ) ] ef8. (
    -> [ d16 ) ] | % 64
    d'2 \ff -> bf8 -> [ g8 -> d8 -> bf8 -> ] | % 65
    a2 ( -> g4 ) r4 | % 66
    f'2 -> d8 -> [ bf'8 -> f8 -> d8 -> ] | % 67
    c2 ( -> bf8 ) r8 r4 | % 68
    R1*4
    \once \override MultiMeasureRest #'minimum-length = #8
    R1
    r2 r4 d'8 \f -> r8 | % 74
    \once \override MultiMeasureRest #'minimum-length = #8
    R1 | % 75
    r2 r4 d8 -> r8 | % 76
    \once \override MultiMeasureRest #'minimum-length = #8
    R1 | % 77
    r2 r4 c8 -> r8 | % 78
    r2 r4 bf8 -> r8 | % 79
    e2 \< -> c2 -> | \barNumberCheck #80
    r8 c,8 \pp [ r8 c8 ] r8 c8 [ r8 c8 ] | % 81
    r8 c8 [ r8 c8 ] r8 c8 [ r8 c8 ] | % 82
    r8 e8 [ r8 e8 ] r8 f8 [ r8 d8 ] | % 83
    r8 e8 [ r8 ef8 ] r8 d8 [ r8 df8 ] | % 84
    r8 c8 [ r8 c8 ] r8 c8 [ r8 c8 ] | % 85
    r8 c8 [ r8 c8 ] r8 c8 [ r8 f8 ] | % 86
    r8 f8 [ r8 f8 ] r8 f8 [ r8 e8 ] | % 87
    f8 r8 r4 r2 | % 88
    d2 \ff -> d4 -> df4 -> | % 89
    c4 -> c8. [ c16 ] f4 -> c4 -> | \barNumberCheck #90
    f2 -> f4 -> f4 -> | % 91
    cs4 -> d4 -> e4 -> f4 -> | % 92
    g4 -> gs4 -> a2 -> | % 93
    r8 f8 \pp [ r8 f8 ] r8 f8 [ r8 f8 ] | % 94
    r8 cs8 [ r8 cs8 ] r8 cs8 [ r8 cs8 ] | % 95
    r8 f8 [ r8 f8 ] r8 g8 [ r8 g8 ] | % 96
    r8 d8 [ r8 d8 ] r8 cs8 [ r8 cs8 ] | % 97
    r8 a'8 [ r8 a8 ] r8 a8 [ r8 fs8 ] | % 98
    r8 g8 -> [ r8 a8 -> ] r8 bf8 -> [ r8 a8 -> ] | % 99
    d,32 \pp \cresc ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ) ] d32 ( [ cs32 d32 cs32
    d32 cs32 d32 cs32 ) ] d32 ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ) ] d32
    ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ) ] | \barNumberCheck #100
    d32 ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ) ] d32 ( [ cs32 d32 cs32 d32
    cs32 d32 cs32 ) ] d32 ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ) ] d32 (
    [ cs32 d32 cs32 d32 f32 e32 cs32 ) ] | % 101
    d1 \ff | % 102
    d1 | % 103
    r8 a8 d4 -> r8 a8 d4 -> | % 104
    e4 -> ef4 -> d4 -> df4 -> | % 105
    c4 -> bf'4 -> f4 -> d'4 -> | % 106
    r4 ef,4 -> r2 | % 107
    \once \override MultiMeasureRest #'minimum-length = #8
    R1 \fermataMarkup | % 108
    \key d \major R1*20 | % 128
    d'2 \f -> d,4 -> fs4 -> | % 129
    a4.. ( -> b16 a2 ) | \barNumberCheck #130
    g,4 b8. [ d16 ] g4 -> b4 -> | % 131
    a2 -> fs2 -> | % 132
    R1*2 | % 134
    d16 \p [ d16 d16 d16 ] d16 [ d16 d16 d16 ] d16 [ d16 d16 d16 ] d16 [
    d16 d16 d16 ] | % 135
    d32 \ff ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ] d32 [ cs32 d32 cs32 d32
    cs32 d32 cs32 ] d32 [ cs32 d32 cs32 d32 cs32 d32 cs32 ] d32 [ cs32 d32
    cs32 d32 cs32 d32 cs32 ) ] | % 136
    d32 ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ] d32 [ cs32 d32 cs32 d32 cs32
    d32 cs32 ] d32 [ cs32 d32 cs32 d32 cs32 d32 cs32 ] d32 [ cs32 d32 cs32
    d32 cs32 d32 cs32 ) ] | % 137
    d32 ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ] d32 [ cs32 d32 cs32 d32 cs32
    d32 cs32 ] d32 [ cs32 d32 cs32 d32 cs32 d32 cs32 ] d32 [ cs32 d32 cs32
    d32 cs32 d32 cs32 ) ] | % 138
    d32 ( [ cs32 d32 cs32 d32 cs32 d32 cs32 ] d32 [ cs32 d32 cs32 d32 cs32
    d32 ds32 ) ] e32 ( [ ds32 e32 ds32 e32 ds32 e32 ds32 ] e32 [ ds32 e32
    ds32 e32 ds32 e32 ds32 ) ] | % 139
    e32 ( [ ds32 e32 ds32 e32 ds32 e32 ds32 ] e32 [ ds32 e32 ds32 e32 ds32
    e32 b'32 ) ] cs16 [ cs16 cs16 cs16 ] cs16 [ cs16 cs16 cs16 ] |
    \barNumberCheck #140
    c16 [ c16 c16 c16 ] cs16 [ cs16 cs16 cs16 ] d16 [ d16 d16 d16 ] e16
    [ e16 e16 e16 ] | % 141
    d2. \ff cs8. [ b16 ] | % 142
    a4 -> g4 -> fs4 -> d8. ( -> [ e16 ] | % 143
    fs4 ) r4 r2 | % 144
    \once \override MultiMeasureRest #'minimum-length = #8
    R1
    R1*9
    r2 a2 \pp ( | % 155
    bf1 ) | % 156
    g1 ( | % 157
    fs4 ~ fs8 ) r8 r2 | % 158
    r4 fs2 ~ fs8 r8 | % 159
    r4 e2 ~ e8 r8 | \barNumberCheck #160
    r4 a,2 ~ a8 r8 | % 161
    r4 a4 -- r4 bf4 -- | % 162
    r4 a4 -- r4 bf4 -- | % 163
    \once \override MultiMeasureRest #'minimum-length = #8
    R1
    \once \override MultiMeasureRest #'minimum-length = #8
    R1
    R1*4
    r2 d2 \pp | \barNumberCheck #170
    fs2 g2 ( | % 171
    d'1 \< ~ | % 172
    d1 \! \> ~ | % 173
    d8 \! ) r8 d8. \f [ d16 ] d4 d4 | % 174
    d,1 ~ -> | % 175
    d8 r8 r4 r2
  }
}