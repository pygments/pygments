"""
    Lexers for FASTA sequence files
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer
from pygments.token import Comment, Keyword, Name, Number, Punctuation, String, Text, Whitespace

__all__ = ['FastaLexer']


class FastaLexer(RegexLexer):
    """
    Lexer for FASTA sequence files.

    A FASTA file holds one or more sequences.  Every sequence starts with a
    header line that begins with ``>`` and carries the sequence identifier
    followed by an optional free-form description.  The sequence itself
    follows on the next lines, wrapped at an arbitrary width.
    """

    name = 'FASTA'
    aliases = ['fasta']
    filenames = [
        '*.fa',
        '*.fasta',
        '*.fas',
        '*.faa',
        '*.fna',
        '*.ffn',
        '*.frn',
        '*.pep',
        '*.seq',
    ]
    mimetypes = []
    url = 'https://en.wikipedia.org/wiki/FASTA_format'
    version_added = '2.22'

    tokens = {
        'root': [
            (r'^>', Punctuation, 'header'),
            (r'[-.]+', Comment),
            (r'[A-Za-z]+', Name.Constant),
            (r'\d+', Number.Integer),
            (r'\*', Keyword.Constant),
            (r'[ \t\r]+', Whitespace),
            (r'\n', Text),
        ],
        'header': [
            (r'\n', Text, '#pop'),
            (r'[ \t\r]+', Whitespace),
            (r'[^\s]+', Name.Tag, 'description'),
        ],
        'description': [
            (r'[ \t\r]+', Whitespace),
            (r'\n', Text, '#pop:2'),
            (r'[^\n\r]+', String),
        ],
    }

    def analyse_text(text):
        """Recognise a FASTA file by its header line and sequence lines."""
        lines = text.splitlines()
        if not lines or not lines[0].startswith('>'):
            return 0.0
        sequence = [
            line for line in lines[1:]
            if line.strip() and not line.startswith('>')
        ]
        if not sequence:
            return 0.0
        if not all(re.fullmatch(r'[A-Za-z*\-.\s]+', line) for line in sequence):
            return 0.0
        return 1.0
