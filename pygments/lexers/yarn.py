# -*- coding: utf-8 -*-
"""
pygments module for YarnSpinner by Secret Lab
Written by Mars Geldard on 08/07/2019

Version: 1.0
"""

from pygments.lexer import RegexLexer, include
from pygments.token import *

__all__ = ['YarnLexer']

class YarnLexer(RegexLexer):
    name = 'YarnSpinner'
    aliases = ['yarn', 'yarnspinner', 'yarn-spinner']
    filenames = ['*.yarn', '*.yarn.txt']

    tokens = {
        'comments': [# only exists to import to other modes
            (r'//.*?\n', Comment.Single),
        ],
        'operators': [ # only exists to import to other modes
            (r'(\|\||<=|>=|==|!=|[=&<>!])', Operator),
            (r'\b(and|le|gt|or|leq|geq|eq|is|neq|not|to)\b', Operator.Word),
        ],
        'root': [
            include('comments'),                                      # if // ___ then comment
            (r'title:', Name.Attribute),                              # if title: ___ then highlight
            (r'(?<=title:)[^\n]+\n', Name.Function),                  # if preceded by 'title:' then highlight
            (r'^[\w|-]+:', Name.Attribute),                             # if _____: ___ then highlight
            (r'---', Keyword, 'node'),                                # if --- then switch mode to 'node'
            (r'.', Text),                                             # otherwise just text
        ],
        'node': [
            include('comments'),                                      # if // ___ then comment
            (r'===', Keyword, '#pop'),                                # if === then  switch mode back to 'root'
            (r'<<', Punctuation, 'expression'),                       # if << then switch mode to 'expression'
            (r'\[\[', Punctuation, 'goto'),                           # if [[ then switch mode to 'goto'
            (r'\s*[\w|-]+: ', Name.Class),                            # if <capitalized>: then highlight
            (r'#[^\n]+\n', Name.Tag),                                 # if #____\n then highlight
            (r'->', Operator),                                        # if -> then highlight
            (r'\s', Text),                                            # otherwise just split text into tokens
            (r'.', Text),                                             # otherwise just text
        ],
        'expression': [
            (r'>>', Punctuation, '#pop'),                             # if >> then switch mode back to 'node'
            ('"', String.Double, 'string'),                           # if " then switch mode to 'string'
            (r'[0-9]+\.?[0-9]*', Number.Float),                       # if <number> then highlight
            (r'\$[\w|-]+', Name.Variable),                            # if $<variable> then highlight
            (r'(true|false|null)\b', Keyword.Constant),               # if <boolean> or null then highlight
            (r'(if|elseif|else|endif|set|stop)\b', Keyword.Reserved), # if <keyword> then highlight
            include('operators'),                                     # if <operator> then highlight
            (r'.', Text),                                             # otherwise just text

        ],
        'goto': [
            (r'\]\]', Punctuation, '#pop'),                           # if ]] then switch mode back to 'node'
            (r'[^\]]+\|', Text),                                      # otherwise text if preceding a |
            (r'.', Name.Function),                                    # else it is the goto link, highlighted
        ],
        'string': [
            ('"', String.Double, '#pop'),                             # if " then switch mode back to 'expression'
            (r'.', String.Double),                                    # otherwise just String contents
        ],
    }
