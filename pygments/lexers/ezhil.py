# -*- coding: utf-8 -*-
"""
    pygments.lexers.ezhil
    ~~~~~~~~~~~~~~~~~~~~~~

    Pygments lexers for Ezhil language.
    
    :copyright: Copyright 2015 Muthiah Annamalai
    :license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import RegexLexer, include, words
from pygments.token import Keyword, Text, Comment, Name
from pygments.token import String, Number, Punctuation, Operator

__all__ = ['EzhilLexer']

class EzhilLexer(RegexLexer):
    """
    *New in Pygments. Lexical analyzer for Tamil script based programming language, Ezhil*
    *See: http://ezhillang.org/
    """
    name = 'Ezhil'
    aliases = ['ezhil']
    filenames = ['*.n']
    mimetypes = ['text/x-ezhil']
    flags = re.MULTILINE | re.UNICODE
    # see constant from open-tamil package: tamil.utf8.tamil_letters
    _TALETTERS = u'a-zA-Z_|அ|ஆ|இ|ஈ|உ|ஊ|எ|ஏ|ஐ|ஒ|ஓ|ஔ|ஃ|க்|ச்|ட்|த்|ப்|ற்|ங்|ஞ்|ண்|ந்|ம்|ன்|ய்|ர்|ல்|வ்|ழ்|ள்|க|ச|ட|த|ப|ற|ஞ|ங|ண|ந|ம|ன|ய|ர|ல|வ|ழ|ள|ஜ|ஷ|ஸ|ஹ|க|கா|கி|கீ|கு|கூ|கெ|கே|கை|கொ|கோ|கௌ|ச|சா|சி|சீ|சு|சூ|செ|சே|சை|சொ|சோ|சௌ|ட|டா|டி|டீ|டு|டூ|டெ|டே|டை|டொ|டோ|டௌ|த|தா|தி|தீ|து|தூ|தெ|தே|தை|தொ|தோ|தௌ|ப|பா|பி|பீ|பு|பூ|பெ|பே|பை|பொ|போ|பௌ|ற|றா|றி|றீ|று|றூ|றெ|றே|றை|றொ|றோ|றௌ|ஞ|ஞா|ஞி|ஞீ|ஞு|ஞூ|ஞெ|ஞே|ஞை|ஞொ|ஞோ|ஞௌ|ங|ஙா|ஙி|ஙீ|ஙு|ஙூ|ஙெ|ஙே|ஙை|ஙொ|ஙோ|ஙௌ|ண|ணா|ணி|ணீ|ணு|ணூ|ணெ|ணே|ணை|ணொ|ணோ|ணௌ|ந|நா|நி|நீ|நு|நூ|நெ|நே|நை|நொ|நோ|நௌ|ம|மா|மி|மீ|மு|மூ|மெ|மே|மை|மொ|மோ|மௌ|ன|னா|னி|னீ|னு|னூ|னெ|னே|னை|னொ|னோ|னௌ|ய|யா|யி|யீ|யு|யூ|யெ|யே|யை|யொ|யோ|யௌ|ர|ரா|ரி|ரீ|ரு|ரூ|ரெ|ரே|ரை|ரொ|ரோ|ரௌ|ல|லா|லி|லீ|லு|லூ|லெ|லே|லை|லொ|லோ|லௌ|வ|வா|வி|வீ|வு|வூ|வெ|வே|வை|வொ|வோ|வௌ|ழ|ழா|ழி|ழீ|ழு|ழூ|ழெ|ழே|ழை|ழொ|ழோ|ழௌ|ள|ளா|ளி|ளீ|ளு|ளூ|ளெ|ளே|ளை|ளொ|ளோ|ளௌ|ௐ|ஜ|ஜா|ஜி|ஜீ|ஜு|ஜூ|ஜெ|ஜே|ஜை|ஜொ|ஜோ|ஜௌ|ஷ|ஷா|ஷி|ஷீ|ஷு|ஷூ|ஷெ|ஷே|ஷை|ஷொ|ஷோ|ஷௌ|ஸ|ஸா|ஸி|ஸீ|ஸு|ஸூ|ஸெ|ஸே|ஸை|ஸொ|ஸோ|ஸௌ|ஹ|ஹா|ஹி|ஹீ|ஹு|ஹூ|ஹெ|ஹே|ஹை|ஹொ|ஹோ|ஹௌ'
    tokens = {
        'root': [
            include('keywords'),
            (r'#.*\n', Comment.Single),
            (r'[@+/*,^\-%]|[!<>=]=?|&&?|\|\|?',Operator),
            (u'இல்',Operator.Word),
            (words(('assert', 'max', 'min',
                    'நீளம்','சரம்_இடமாற்று','சரம்_கண்டுபிடி',
                    'பட்டியல்','பின்இணை','வரிசைப்படுத்து',
                    'எடு','தலைகீழ்','நீட்டிக்க','நுழைக்க','வை',
                    'கோப்பை_திற','கோப்பை_எழுது','கோப்பை_மூடு',
                    'pi','sin','cos','tan','sqrt','hypot','pow','exp','log','log10'
                    'min','max','exit',
                    ), suffix=r'\b'),Name.Builtin),
            (r'(True|False)\b', Keyword.Constant),
            (r'[^\S\n]+', Text),
            include('identifier'),
            include('literal'),
            (r'[(){}\[\]:;.]', Punctuation),
            ],
        'keywords': [
            (u'பதிப்பி|தேர்ந்தெடு|தேர்வு|ஏதேனில்|ஆனால்|இல்லைஆனால்|இல்லை|ஆக|ஒவ்வொன்றாக|இல்|வரை|செய்|முடியேனில்|பின்கொடு|முடி|நிரல்பாகம்|தொடர்|நிறுத்து|நிரல்பாகம்', Keyword),
            ],
        'identifier': [
            (u'['+_TALETTERS+']['+u'0-9'+_TALETTERS+u']*',Name),
            ],
        'literal': [
            (r'".*?"', String),
            (r'(?u)\d+((\.\d*)?[eE][+-]?\d+|\.\d*)', Number.Float),
            (r'(?u)\d+',Number.Integer),
        ]}
    
    def __init__(self, **options):
        super(EzhilLexer, self).__init__(**options)
        self.encoding = options.get('encoding', 'utf-8')
