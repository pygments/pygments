# -*- coding: utf-8 -*-
"""
    pygments.lexers.esoteric
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for esoteric languages.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error, Whitespace

__all__ = ['BrainfuckLexer', 'BefungeLexer', 'BoogieLexer', 'RedcodeLexer',
           'AheuiLexer']


class BrainfuckLexer(RegexLexer):
    """
    Lexer for the esoteric `BrainFuck <http://www.muppetlabs.com/~breadbox/bf/>`_
    language.
    """

    name = 'Brainfuck'
    aliases = ['brainfuck', 'bf']
    filenames = ['*.bf', '*.b']
    mimetypes = ['application/x-brainfuck']

    tokens = {
        'common': [
            # use different colors for different instruction types
            (r'[.,]+', Name.Tag),
            (r'[+-]+', Name.Builtin),
            (r'[<>]+', Name.Variable),
            (r'[^.,+\-<>\[\]]+', Comment),
        ],
        'root': [
            (r'\[', Keyword, 'loop'),
            (r'\]', Error),
            include('common'),
        ],
        'loop': [
            (r'\[', Keyword, '#push'),
            (r'\]', Keyword, '#pop'),
            include('common'),
        ]
    }


class BefungeLexer(RegexLexer):
    """
    Lexer for the esoteric `Befunge <http://en.wikipedia.org/wiki/Befunge>`_
    language.

    .. versionadded:: 0.7
    """
    name = 'Befunge'
    aliases = ['befunge']
    filenames = ['*.befunge']
    mimetypes = ['application/x-befunge']

    tokens = {
        'root': [
            (r'[0-9a-f]', Number),
            (r'[+*/%!`-]', Operator),             # Traditional math
            (r'[<>^v?\[\]rxjk]', Name.Variable),  # Move, imperatives
            (r'[:\\$.,n]', Name.Builtin),         # Stack ops, imperatives
            (r'[|_mw]', Keyword),
            (r'[{}]', Name.Tag),                  # Befunge-98 stack ops
            (r'".*?"', String.Double),            # Strings don't appear to allow escapes
            (r'\'.', String.Single),              # Single character
            (r'[#;]', Comment),                   # Trampoline... depends on direction hit
            (r'[pg&~=@iotsy]', Keyword),          # Misc
            (r'[()A-Z]', Comment),                # Fingerprints
            (r'\s+', Text),                       # Whitespace doesn't matter
        ],
    }


class RedcodeLexer(RegexLexer):
    """
    A simple Redcode lexer based on ICWS'94.
    Contributed by Adam Blinkinsop <blinks@acm.org>.

    .. versionadded:: 0.8
    """
    name = 'Redcode'
    aliases = ['redcode']
    filenames = ['*.cw']

    opcodes = ('DAT', 'MOV', 'ADD', 'SUB', 'MUL', 'DIV', 'MOD',
               'JMP', 'JMZ', 'JMN', 'DJN', 'CMP', 'SLT', 'SPL',
               'ORG', 'EQU', 'END')
    modifiers = ('A', 'B', 'AB', 'BA', 'F', 'X', 'I')

    tokens = {
        'root': [
            # Whitespace:
            (r'\s+', Text),
            (r';.*$', Comment.Single),
            # Lexemes:
            #  Identifiers
            (r'\b(%s)\b' % '|'.join(opcodes), Name.Function),
            (r'\b(%s)\b' % '|'.join(modifiers), Name.Decorator),
            (r'[A-Za-z_]\w+', Name),
            #  Operators
            (r'[-+*/%]', Operator),
            (r'[#$@<>]', Operator),  # mode
            (r'[.,]', Punctuation),  # mode
            #  Numbers
            (r'[-+]?\d+', Number.Integer),
        ],
    }


class BoogieLexer(RegexLexer):
    """
    For `Boogie <https://boogie.codeplex.com/>`_ source code.

    .. versionadded:: 2.1
    """
    name = 'Boogie'
    aliases = ['boogie']
    filenames = ['*.bpl']

    tokens = {
        'root': [
            # Whitespace and Comments
            (r'\n', Whitespace),
            (r'\s+', Whitespace),
            (r'//[/!](.*?)\n', Comment.Doc),
            (r'//(.*?)\n', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),

            (words((
                'axiom', 'break', 'call', 'ensures', 'else', 'exists', 'function',
                'forall', 'if', 'invariant', 'modifies', 'procedure',  'requires',
                'then', 'var', 'while'),
             suffix=r'\b'), Keyword),
            (words(('const',), suffix=r'\b'), Keyword.Reserved),

            (words(('bool', 'int', 'ref'), suffix=r'\b'), Keyword.Type),
            include('numbers'),
            (r"(>=|<=|:=|!=|==>|&&|\|\||[+/\-=>*<\[\]])", Operator),
            (r"([{}():;,.])", Punctuation),
            # Identifier
            (r'[a-zA-Z_]\w*', Name),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
        'numbers': [
            (r'[0-9]+', Number.Integer),
        ],
    }


class AheuiLexer(RegexLexer):
    """
    Aheui_ Lexer.

    Aheui_ is esoteric language based on Korean alphabets.

    .. _Aheui:: http://aheui.github.io/

    """

    name = u'Aheui'
    aliases = ['aheui']
    filenames = ['*.aheui']

    tokens = {
        'root': [
            (u'['
             u'가-갛갸-걓거-겋겨-곃고-곻교-궇규-긯'
             u'까-깧꺄-꺟꺼-껗껴-꼏꼬-꽇꾜-꿓뀨-끻'
             u'나-낳냐-냫너-넣녀-녛노-놓뇨-눟뉴-닇'
             u'다-닿댜-댷더-덯뎌-뎧도-돟됴-둫듀-딓'
             u'따-땋땨-떃떠-떻뗘-뗳또-똫뚀-뚷뜌-띟'
             u'라-랗랴-럏러-렇려-렿로-롷료-뤃류-릫'
             u'마-맣먀-먛머-멓며-몋모-뫃묘-뭏뮤-믷'
             u'바-밯뱌-뱧버-벟벼-볗보-봏뵤-붛뷰-빃'
             u'빠-빻뺘-뺳뻐-뻫뼈-뼣뽀-뽛뾰-뿧쀼-삏'
             u'사-샇샤-샿서-섷셔-셯소-솧쇼-숳슈-싛'
             u'싸-쌓쌰-썋써-쎃쎠-쎻쏘-쏳쑈-쑿쓔-씧'
             u'아-앟야-얗어-엏여-옇오-옿요-웋유-읳'
             u'자-잫쟈-쟣저-젛져-졓조-좋죠-줗쥬-즿'
             u'짜-짷쨔-쨯쩌-쩧쪄-쪟쪼-쫗쬬-쭣쮸-찋'
             u'차-챃챠-챻처-첳쳐-쳫초-촣쵸-춯츄-칗'
             u'카-캏캬-컇커-컿켜-켷코-콯쿄-쿻큐-킣'
             u'타-탛탸-턓터-텋텨-톃토-톻툐-퉇튜-틯'
             u'파-팧퍄-퍟퍼-펗펴-폏포-퐇표-풓퓨-픻'
             u'하-핳햐-햫허-헣혀-혛호-홓효-훟휴-힇'
             u']', Operator),
            ('.', Comment),
        ],
    }
