 \version "2.19.16"
\language "english"

\include "score-init.ily"
\include "../global-init.ily"
\include "../header-init.ily"

\include "../Notes/flute.ily"
\include "../Notes/oboe.ily"
\include "../Notes/clarinetI.ily"
\include "../Notes/clarinetII.ily"
\include "../Notes/clarinetIII.ily"
\include "../Notes/bassclarinet.ily"
\include "../Notes/bassoon.ily"
\include "../Notes/altosaxI.ily"
\include "../Notes/altosaxII.ily"
\include "../Notes/tenorsax.ily"
\include "../Notes/barisax.ily"
\include "../Notes/hornI.ily"
\include "../Notes/hornII.ily"
\include "../Notes/hornIII.ily"
\include "../Notes/hornIV.ily"
\include "../Notes/trumpetI.ily"
\include "../Notes/trumpetII.ily"
\include "../Notes/trumpetIII.ily"
\include "../Notes/tromboneI.ily"
\include "../Notes/tromboneII.ily"
\include "../Notes/tromboneIII.ily"
\include "../Notes/euphonium.ily"
\include "../Notes/tuba.ily"
\include "../Notes/timpani.ily"
\include "../Notes/snaredrum.ily"
\include "../Notes/bassdrum.ily"
\include "../Notes/cymbals.ily"
\include "../Notes/gong.ily"

\header {
  instrument = "Score"
  meter = "Duration: 8:40"
}

\score {
  <<
    \new StaffGroup = "flutes" <<
      \new Staff = "Flute" {
        \set Staff.instrumentName = #"Flute"
        \set Staff.shortInstrumentName = #"Fl."
        <<
          \global
          \fluteNotes
        >>
      }
      \new Staff = "Oboe" {
        \set Staff.instrumentName = #"Oboe"
        \set Staff.shortInstrumentName = "Ob."
        <<
          \global
          \oboeNotes
        >>
      }
    >>

    \new StaffGroup = "clarinets" <<
      \new Staff = "Clarinet 1" {
        \set Staff.instrumentName = #"Clarinet 1"
        \set Staff.shortInstrumentName = #"Cl. 1"
        \transpose bf c {
          <<
            \global
            \clarinetINotes
          >>
        }
      }
      \new Staff = "Clarinet 2" {
        \set Staff.instrumentName = #"Clarinet 2"
        \set Staff.shortInstrumentName = #"Cl. 2"
        \transpose bf c {
          <<
            \global
            \clarinetIINotes
          >>
        }
      }
      \new Staff = "Clarinet 3" {
        \set Staff.instrumentName = #"Clarinet 3"
        \set Staff.shortInstrumentName = #"Cl. 3"
        \transpose bf c {
          <<
            \global
            \clarinetIIINotes
          >>
        }
      }
      \new Staff = "Bass Clarinet" {
        \set Staff.instrumentName = #"Bass Clarinet"
        \set Staff.shortInstrumentName = #"Bs. Cl."
        \transpose bf, c {
          <<
            \global
            \bassclarinetNotes
          >>
        }
      }
    >>

\new StaffGroup = "Bassoons" <<
    \new Staff = "Bassoon" {
      \set Staff.instrumentName = #"Bassoon"
      \set Staff.shortInstrumentName = #"Bsn."
      <<
        \global
        \bassoonNotes
      >>
    }
>>

    \new StaffGroup = "Saxophones" <<
      \new Staff = "Alto Saxophone 1" {
        \set Staff.instrumentName = #"Alto Saxophone 1"
        \set Staff.shortInstrumentName = #"Alto Sax. 1"
        \transpose ef c {
          <<
            \global
            \altosaxINotes
          >>
        }
      }
       \new Staff = "Alto Saxophone 2" {
        \set Staff.instrumentName = #"Alto Saxophone 2"
        \set Staff.shortInstrumentName = #"Alto Sax. 2"
        \transpose ef c {
          <<
            \global
            \altosaxIINotes
          >>
        }
      }
      \new Staff = "Tenor Saxophone" {
        \set Staff.instrumentName = #"Tenor Saxophone"
        \set Staff.shortInstrumentName = #"Tenor Sax."
        \transpose bf, c {
          <<
            \global
            \tenorsaxNotes
          >>
        }
      }
      \new Staff = "Baritone Saxophone" {
        \set Staff.instrumentName = #"Baritone Saxophone"
        \set Staff.shortInstrumentName = #"Bari. Sax."
        \transpose ef, c {
          <<
            \global
            \barisaxNotes
          >>
        }
      }
    >>

    \new StaffGroup = "Horns" <<
      \new Staff = "F Horn 1" {
        \set Staff.instrumentName = #"F Horn 1"
        \set Staff.shortInstrumentName = #"Hn. 1"
        \transpose f c {
          <<
            \global
            \hornINotes
          >>
        }
      }
      \new Staff = "F Horn 2" {
        \set Staff.instrumentName = #"F Horn 2"
        \set Staff.shortInstrumentName = #"Hn. 2"
        \transpose f c {
          <<
            \global
            \hornIINotes
          >>
        }
      }
      \new Staff = "F Horn 3" {
        \set Staff.instrumentName = #"F Horn 3"
        \set Staff.shortInstrumentName = #"Hn. 3"
        \transpose f c {
          <<
            \global
            \hornIIINotes
          >>
        }
      }
      \new Staff = "F Horn 4" {
        \set Staff.instrumentName = #"F Horn 4"
        \set Staff.shortInstrumentName = #"Hn. 4"
        \transpose f c {
          <<
            \global
            \hornIVNotes
          >>
        }
      }
    >>

    \new StaffGroup = "Trumpets" <<
      \new Staff = "Trumpet 1" {
        \set Staff.instrumentName = #"Trumpet 1"
        \set Staff.shortInstrumentName = #"Tpt. 1"
        \transpose bf c {
          <<
            \global
            \trumpetINotes
          >>
        }
      }
      \new Staff = "Trumpet 2" {
        \set Staff.instrumentName = #"Trumpet 2"
        \set Staff.shortInstrumentName = #"Tpt. 2"
        \transpose bf c {
          <<
            \global
            \trumpetIINotes
          >>
        }
      }
      \new Staff = "Trumpet 3" {
        \set Staff.instrumentName = #"Trumpet 3"
        \set Staff.shortInstrumentName = #"Tpt. 3"
        \transpose bf c {
          <<
            \global
            \trumpetIIINotes
          >>
        }
      }
    >>

    \new StaffGroup = "Trombones" <<
      \new Staff = "Trombone 1" {
        \set Staff.instrumentName = #"Trombone 1"
        \set Staff.shortInstrumentName = #"Tbn. 1"
        <<
          \global
          \tromboneINotes
        >>
      }
      \new Staff = "Trombone 2" {
        \set Staff.instrumentName = #"Trombone 2"
        \set Staff.shortInstrumentName = #"Tbn. 2"
        <<
          \global
          \tromboneIINotes
        >>
      }
      \new Staff = "Trombone 3" {
        \set Staff.instrumentName = #"Trombone 3"
        \set Staff.shortInstrumentName = #"Tbn. 3"
        <<
          \global
          \tromboneIIINotes
        >>
      }
    >>

    \new StaffGroup = "Tubas" <<
      \new Staff = "Euphonium" {
        \set Staff.instrumentName = #"Euphonium"
        \set Staff.shortInstrumentName = #"Eupho"
        <<
          \global
          \euphoNotes
        >>
      }
      \new Staff = "Tuba" {
        \set Staff.instrumentName = #"Tuba"
        \set Staff.shortInstrumentName = #"Tba."
        <<
          \global
          \tubaNotes
        >>
      }
    >>
    

    \new StaffGroup = "Percussion" <<
      \new Staff = "Timpani" {
        \set Staff.instrumentName = #"Timpani"
        \set Staff.shortInstrumentName = #"Timp."
        <<
          \global
          \timpaniNotes
        >>
      }
      \new DrumStaff = "Snare Drum" {
        \snaredrumstaff
        <<
          \global
          \snaredrumNotes
        >>
      }

      \new DrumStaff = "Bass Drum" {
        \bassdrumstaff
        <<
          \global
          \bassdrumNotes
        >>
      }

      \new DrumStaff = "Cymbals" {
        \cymbalsstaff
        <<
          \global
          \cymbalsNotes
        >>
      }
      
       \new DrumStaff = "Gong" {
        \gongstaff
        <<
          \global
          \gongNotes
        >>
      }
    >>

    \new Dynamics {
      \startMeasureCount
      s1*175
      
      \stopMeasureCount
    }
  >>
}
