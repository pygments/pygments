"""
    pygments.lexers.smtlib
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the SMT-LIB 2 language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, words
from pygments.token import Comment, Keyword, Name, Number, Punctuation, String, Whitespace

__all__ = ['SMTLibLexer']


class SMTLibLexer(RegexLexer):
    """For SMT-LIB 2 scripts, theory declarations and logic declarations."""

    name = 'SMT-LIB'
    url = 'https://smt-lib.org/'
    aliases = ['smtlib', 'smt-lib', 'smt2']
    filenames = ['*.smt2']
    version_added = '2.22'

    # Symbols may contain punctuation, so a word boundary is not sufficient.
    _symbol_start = r'a-zA-Z~!@$%^&*_\-+=<>.?/'
    _symbol_rest = _symbol_start + '0-9'
    _symbol = rf'[{_symbol_start}][{_symbol_rest}]*'
    _symbol_end = rf'(?![{_symbol_rest}])'

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r';[^\r\n]*', Comment.Single),
            (r'[()]+', Punctuation),
            (r'"', String, 'string'),
            (r'\|[^|\\]*\|', Name),
            (r'#b[01]+', Number.Bin),
            (r'#x[0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+\.[0-9]+', Number.Float),
            (r'[0-9]+', Number.Integer),
            (':' + _symbol, Name.Attribute),
            (words((
                'BINARY', 'DECIMAL', 'HEXADECIMAL', 'NUMERAL', 'STRING',
                '_', '!', 'as', 'lambda', 'let', 'exists', 'forall', 'match', 'par',
                'theory', 'logic',
                'assert', 'check-sat', 'check-sat-assuming', 'declare-const',
                'declare-datatype', 'declare-datatypes', 'declare-fun',
                'declare-sort', 'declare-sort-parameter', 'define-const',
                'define-fun', 'define-fun-rec', 'define-funs-rec', 'define-sort',
                'echo', 'exit', 'get-assertions', 'get-assignment', 'get-info',
                'get-model', 'get-option', 'get-proof', 'get-unsat-assumptions',
                'get-unsat-core', 'get-value', 'pop', 'push', 'reset',
                'reset-assertions', 'set-info', 'set-logic', 'set-option',
            ), suffix=_symbol_end), Keyword),
            (words(('true', 'false'), suffix=_symbol_end), Keyword.Constant),
            (words((
                'Bool', 'Int', 'Real', 'String', 'RegLan', 'Array', 'BitVec',
                'FloatingPoint', 'RoundingMode',
            ), suffix=_symbol_end), Keyword.Type),
            (words((
                'and', 'or', 'xor', 'not', '=>', '=', 'distinct', 'ite',
                '+', '-', '*', '/', '<', '<=', '>', '>=', 'div', 'mod', 'abs',
                'to_real', 'to_int', 'is_int', 'select', 'store', '@', '->',
            ), suffix=_symbol_end), Name.Builtin),
            (_symbol, Name),
        ],
        'string': [
            (r'""', String.Escape),
            (r'"', String, '#pop'),
            (r'[^"]+', String),
        ],
    }

    def analyse_text(text):
        if re.search(r'^\s*\((?:set-logic|declare-fun|check-sat(?:-assuming)?)(?=[\s)])', text, re.MULTILINE):
            return 0.8
