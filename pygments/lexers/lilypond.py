import re

from pygments.lexer import RegexLexer, include, words
from pygments.token import *

from pygments.lexers._lilypond_builtins import lilypond_keywords, lilypond_music_commands, dynamics, articulations, ornaments, fermatas, instrument_scripts, repeat_scripts, ancient_scripts, modes, markupcommands, markuplistcommands, contexts, midi_instruments, scheme_values, header_variables, paper_variables, layout_variables, repeat_types, accidental_styles, clefs, break_visibility, mark_formatters

__all__ = ['LilyPondLexer']

class LilyPondLexer(RegexLexer):

    name = 'LilyPond'
    aliases = ['lilypond', 'lily']
    filenames = ['*.ly', '*.ily']

    tokens = {
        
        'root': [
            include('keywords'),

            # articulation
            (r'[-_^][_.>|!+^-]', Name.Other),

            # dynamics -- moved to _lilypond_builtins.py
            #(r'\\[<!>]', Name.Pseudo),
            #(r'\\\!', Name.Entity),
            #(r'\\f{1,5}|p{1,5}', Name.Entity),
            #(r'mf|mp|fp|spp?|sff?|sfz|rfz', Name.Entity),
            #(r'cresc|decresc|dim|cr|decr', Name.Entity),
            #(r'\\\<|\\\>', Name.Entity),

            (r'\\f[\s|\\]', Name.Entity),
            (r'\\p[\s|\\]', Name.Entity),

            (r'\)(?![A-Za-z])', Name.Function),

            # duration
            (r'(\\(maxima|longa|breve)\b|(1|2|4|8|16|32|64|128|256|512|1024|2048)(?!\d))', Name.Function),
            # dot
            (r'\.', Name.Function),
            # scaling
            (r'\*[\t ]*\d+(/\d+)?', Name.Function),

            # an identifier allowing letters and single hyphens in between
            (r'[^\W\d_]+([_-][^\W\d_]+)*', Name.Function),

            # the lookahead pattern for the end of an identifier (ref)
            # (r'(?![_-]?[^\W\d])', Name.Function), #not working

            # decimal value
            (r'-?\d+(\.\d+)?', Number.Float),
            # number - integer
            (r'\d+', Number.Integer),
            # number - fraction
            (r'\d+/\d+', Number),

            # Dot Path(Delimiter): A dot in dotted path notation.
            (r'\.', Punctuation),

            # Comments
            (r'%{(.|[\r\n])*?%}', Comment.Multiline),
            (r'%.*?\n', Comment.Singleline),

            # String in quotation marks
            (r'"(.*?)"', String.Double),

            # String Quote Escape
            (r'\\[\\"]', String.Double),

            # Skip
            (r'\\skip', Name.Function),

            # Spacer
            (r's(?![A-Za-z])', Text),
    
            # Rest
            (r'[Rr](?![A-Za-z])', Text),
    
            # Note
            (r'[a-x]+(?![A-Za-z])', Text),

            # q - repeat a chord
            (r'q(?![A-Za-z])', Text),

            # drum notes
            (r'[a-z]+(?![A-Za-z])', Text),

            # Octave(_token.Token):
            (r'\,+|\'+', Text),

            # OctaveCheck(_token.Token):
            (r'=(\,+|\'+)?', Text),

            # Accidental Reminder(Accidental):
            (r'!', Text),

            # Accidental Cautionary(Accidental):
            (r'\?', Text),

            # open bracket
            (r"\{", Text), 

            # Close Bracket
            (r"\}", Text),

            # open simultaneous
            (r"<<", Text), 
    
            # Close Simultaneous
            (r">>", Text),

            # pipe symbol, bar check
            (r'\|', Text),

            # Dircetion
            (r'[-_^]', Text),

            # Articulation
            # ScriptAbbreviation
            (r'[+|!>._^-]', Text),

            # Fingering
            (r'\d+', Text),

            # StringNumber
            (r'\\\d+', Text),

            # Slur Start
            (r'\(', Text),   
            
            # Slur end
            (r"\)", Text),

            # Phrasing SlurS Start
            (r'\\\(', Text),  
            
            # Phrasing Slur End
            (r'\\\)', Text),  

            # Tie
            (r'~', Text), 

            # BeamStart(Beam, _token.MatchStart):
            (r'\[', Text),

            # BeamEnd(Beam, _token.MatchEnd):
            (r'\]', Text),

            # Ligature Start
            (r'\\\[', Text),
    
            # Ligature End
            (r'\\\]', Text),

            # Tremolo Colon
            (r':', Text), 
            # Tremolo Duration
            (r'\b(8|16|32|64|128|256|512|1024|2048)(?!\d)', Name.Function),

            # ChordModifier
            (r'((?<![a-z])|^)(aug|dim|sus|min|maj|m)(?![a-z])', Text),
            # ChordSeparator
            (r':|\^|/\+?', Text),
            # ChordStepNumber
            (r'\d+[-+]?', Text),
            # DotChord
            (r'\.', Text),
            # VoiceSeparator
            (r'\\\\', Text), 

            # time signatures
            (r'\d\/\d', Name.Function),

            # Whitespace
            (r'\s+', Text),
            

            # double angle brackets
            (r'<<|>>', Text),

            # equals sign
            (r'\=', Operator),

            # scheme start
            (r'[#$](?![{}])', Text),
            
        ],

            'keywords': [
            (words(lilypond_music_commands, prefix = r'\\'), Name.Builtin),
            (words(lilypond_keywords, prefix = r'\\'), Name.Builtin),
            (words(dynamics, prefix = r'\\'), Name.Entity),
            (words(articulations, prefix = r'\\'), Name.Function),
            (words(ornaments, prefix = r'\\'), Name.Function),
            (words(fermatas, prefix = r'\\'), Name.Function),
            (words(instrument_scripts, prefix = r'\\'), Name.Function),
            (words(repeat_scripts, prefix = r'\\'), Name.Function),
            (words(ancient_scripts, prefix = r'\\'), Name.Function),
            (words(modes, prefix = r'\\'), Name.Function),
            (words(markupcommands, prefix = r'\\'), Name.Function),
            (words(markuplistcommands, prefix = r'\\'), Name.Function),
            (words(contexts, prefix = r'\\'), Name.Function),
            (words(midi_instruments), Name.Variable),
            (words(scheme_values), Name.Variable),
            (words(header_variables), Name.Variable),
            (words(paper_variables), Name.Variable),
            (words(layout_variables), Name.Variable),
            #(words(midi_variables), Name.Variable),
            (words(repeat_types), Name.Function),
            (words(accidental_styles), Name.Variable),
            (words(clefs), Name.Function),
            (words(break_visibility), Name.Variable),
            (words(mark_formatters), Name.Variable),
        ],
    }
