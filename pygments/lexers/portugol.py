"""
    pygments.lexers.portugol
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Portugol language.
    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.

"""

import re

from pygments.lexer import Lexer
from pygments.token import Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error, Whitespace
from pygments.scanner import Scanner

# compatibility import
from pygments.lexers.modula2 import Modula2Lexer

__all__ = ['PortugolLexer']


class PortugolLexer(Lexer):
    """
    For Portugol source code.

    """
    name = 'Portugol'
    aliases = ['portugol',]
    filenames = ['*.alg', '*.portugol']

    KEYWORDS = (
        'aleatorio',
        'algoritmo',
        'arquivo',
        'ate',
        'caso',
        'cronometro',
        'debug',
        'e',
        'eco',
        'enquanto',
        'entao',
        'escolha',
        'escreva',
        'escreval',
        'faca',
        'falso',
        'fimalgoritmo',
        'fimenquanto',
        'fimescolha',
        'fimfuncao',
        'fimpara',
        'fimprocedimento',
        'fimrepita',
        'fimse',
        'funcao',
        'inicio',
        'int',
        'interrompa',
        'leia',
        'limpatela',
        'mod',
        'nao',
        'ou',
        'outrocaso',
        'para',
        'passo',
        'pausa',
        'procedimento',
        'repita',
        'retorne',
        'se',
        'senao',
        'timer',
        'var',
        'vetor',
        'verdadeiro',
        'xou',
        'div',
        'mod',
        'abs',
        'arccos',
        'arcsen',
        'arctan',
        'cos',
        'cotan',
        'Exp',
        'grauprad',
        'int',
        'log',
        'logn',
        'pi',
        'quad',
        'radpgrau',
        'raizq',
        'rand',
        'randi',
        'sen',
        'Tan',
        'asc',
        'carac',
        'caracpnum',
        'compr',
        'copia',
        'maiusc',
        'minusc',
        'numpcarac',
        'pos',
    )

    BUILTIN_TYPES = {
        'inteiro', 'real', 'caractere', 'logico'
    }

    def __init__(self, **options):
        Lexer.__init__(self, **options)
        self.keywords = set(self.KEYWORDS)

    def get_tokens_unprocessed(self, text):
        scanner = Scanner(text, re.DOTALL | re.MULTILINE | re.IGNORECASE)
        stack = ['initial']
        in_function_block = False
        in_property_block = False
        next_token_is_function = False
        block_labels = set()
        brace_balance = [0, 0]

        while not scanner.eos:
            token = Error
            if stack[-1] == 'initial':
                if scanner.scan(r'\s+'):
                    token = Whitespace
                elif scanner.scan(r'//.*?$'):
                    token = Comment.Single
                elif scanner.scan(r'(<\-)|(>=)|(<=)|%|<|>|-|\+|\*|\=|(<>)|\/|\.|:|,'):
                    token = Operator
                elif scanner.scan(r'[\(\)\[\]]+'):
                    token = Punctuation
                    # abort function naming ``foo = Function(...)``
                    next_token_is_function = False
                    # if we are in a function block we count the open
                    # braces because ootherwise it's impossible to
                    # determine the end of the modifier context
                    if in_function_block or in_property_block:
                        if scanner.match == '(':
                            brace_balance[0] += 1
                        elif scanner.match == ')':
                            brace_balance[0] -= 1
                        elif scanner.match == '[':
                            brace_balance[1] += 1
                        elif scanner.match == ']':
                            brace_balance[1] -= 1
                elif scanner.scan(r'[A-Za-z_][A-Za-z_0-9]*'):
                    lowercase_name = scanner.match.lower()
                    if lowercase_name in self.keywords:
                        token = Keyword
                        # if we are in a special block and a
                        # block ending keyword occurs (and the parenthesis
                        # is balanced) we end the current block context
                        if lowercase_name in ('funcao', 'procedimento'):
                            in_function_block = True
                            next_token_is_function = True
                    # if the last iteration set next_token_is_function
                    # to true we now want this name highlighted as
                    # function. so do that and reset the state
                    elif next_token_is_function:
                        token = Name.Function
                        next_token_is_function = False
                        block_labels.add(scanner.match.lower())

                    # name is in list of known labels
                    elif lowercase_name in block_labels:
                        token = Name.Label
                    elif lowercase_name in self.BUILTIN_TYPES:
                        token = Keyword.Type
                    else:
                        token = Name
                elif scanner.scan(r"\""):
                    token = String
                    stack.append('string')
                elif scanner.scan(r'\d+(?![eE]|\.[^.])'):
                    token = Number.Integer
                elif scanner.scan(r'\d+(\.\d+([eE][+-]?\d+)?|[eE][+-]?\d+)'):
                    token = Number.Float
                else:
                    # if the stack depth is deeper than once, pop
                    if len(stack) > 1:
                        stack.pop()
                    scanner.get_char()

            elif stack[-1] == 'string':
                if scanner.scan(r"''"):
                    token = String.Escape
                elif scanner.scan(r"\""):
                    token = String
                    stack.pop()
                elif scanner.scan(r"[^\"]*"):
                    token = String
                else:
                    scanner.get_char()
                    stack.pop()

            yield scanner.start_pos, token, scanner.match or ''
