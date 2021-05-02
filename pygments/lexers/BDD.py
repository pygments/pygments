from pygments.lexer import RegexLexer
from pygments.token import Text, Keyword, Punctuation


__all__ = ['BDDLexer']

class BDDLexer(RegexLexer):
    name = 'BDD'
    aliases = ['bdd']
    filenames = ['*.feature']

    tokens = {
        'root': [
            (r' .*\n', Text),
            (r'\|.*\n', Punctuation),
            : ""
     
            #keywords
            (words((
                'When', 'And', 'Then', 'Given', 'Scenario', 'Background', 'Feature',
              ), prefix=r'\b'),
            Keyword),

            # (words(('True', 'False', 'None'), suffix=r'\b'), 
            # Keyword),
        
        ],
       
    }