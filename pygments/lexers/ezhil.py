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
    *New in Pygments.*
    """
    name = 'Ezhil'
    aliases = ['ezhil']
    filenames = ['*.n']
    mimetypes = ['text/x-ezhil']

    tokens = {
        'root': [
            include('keywords'),
            (r'^#.*\n', Comment.Single),
            (r'#.*?\n', Comment.Single),
            (r'[@|+|/|*|!=|,|^|\-|%|<|>|<=|>=|=|==]',Operator),
            (r'(assert|min|max)',Name.Builtin),
            (r'(True|False)\b', Keyword.Constant),
            (r'[^\S\n]+', Text),
            include('identifier'),
            include('whitespace'),
            include('literal'),
            (r'[(){}\[\],:&%;.]', Punctuation),
            ],
        'keywords': [
            (u'பதிப்பி|தேர்ந்தெடு|தேர்வு|ஏதேனில்|ஆனால்|இல்லைஆனால்|இல்லை|ஆக|ஒவ்வொன்றாக|இல்|வரை|செய்|முடியேனில்|பின்கொடு|முடி|நிரல்பாகம்|தொடர்|நிறுத்து|நிரல்பாகம்', Keyword),
        ],
        'identifier': [
            (u'[a-zA-Z_|அ|ஆ|இ|ஈ|உ|ஊ|எ|ஏ|ஐ|ஒ|ஓ|ஔ|ஃ|க்|ச்|ட்|த்|ப்|ற்|ங்|ஞ்|ண்|ந்|ம்|ன்|ய்|ர்|ல்|வ்|ழ்|ள்|க|ச|ட|த|ப|ற|ஞ|ங|ண|ந|ம|ன|ய|ர|ல|வ|ழ|ள|ஜ|ஷ|ஸ|ஹ|க|கா|கி|கீ|கு|கூ|கெ|கே|கை|கொ|கோ|கௌ|ச|சா|சி|சீ|சு|சூ|செ|சே|சை|சொ|சோ|சௌ|ட|டா|டி|டீ|டு|டூ|டெ|டே|டை|டொ|டோ|டௌ|த|தா|தி|தீ|து|தூ|தெ|தே|தை|தொ|தோ|தௌ|ப|பா|பி|பீ|பு|பூ|பெ|பே|பை|பொ|போ|பௌ|ற|றா|றி|றீ|று|றூ|றெ|றே|றை|றொ|றோ|றௌ|ஞ|ஞா|ஞி|ஞீ|ஞு|ஞூ|ஞெ|ஞே|ஞை|ஞொ|ஞோ|ஞௌ|ங|ஙா|ஙி|ஙீ|ஙு|ஙூ|ஙெ|ஙே|ஙை|ஙொ|ஙோ|ஙௌ|ண|ணா|ணி|ணீ|ணு|ணூ|ணெ|ணே|ணை|ணொ|ணோ|ணௌ|ந|நா|நி|நீ|நு|நூ|நெ|நே|நை|நொ|நோ|நௌ|ம|மா|மி|மீ|மு|மூ|மெ|மே|மை|மொ|மோ|மௌ|ன|னா|னி|னீ|னு|னூ|னெ|னே|னை|னொ|னோ|னௌ|ய|யா|யி|யீ|யு|யூ|யெ|யே|யை|யொ|யோ|யௌ|ர|ரா|ரி|ரீ|ரு|ரூ|ரெ|ரே|ரை|ரொ|ரோ|ரௌ|ல|லா|லி|லீ|லு|லூ|லெ|லே|லை|லொ|லோ|லௌ|வ|வா|வி|வீ|வு|வூ|வெ|வே|வை|வொ|வோ|வௌ|ழ|ழா|ழி|ழீ|ழு|ழூ|ழெ|ழே|ழை|ழொ|ழோ|ழௌ|ள|ளா|ளி|ளீ|ளு|ளூ|ளெ|ளே|ளை|ளொ|ளோ|ளௌ][a-zA-Z0-9.]*',Name),
            ],
        'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),
        ],
        'literal': [
            (r'".*"', String),
            (r'0x[0-9A-Fa-f]+t?', Number.Hex),
            (r'[0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'[0-9]+L?', Number.Integer),
        ]
        }

    def __init__(self, **options):
        super(EzhilLexer, self).__init__(**options)
        self.encoding = options.get('encoding', 'utf-8')
