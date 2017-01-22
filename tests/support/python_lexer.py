# -*- coding: utf-8 -*-
# pygments.lexers.python (as CustomLexer) for test_cmdline.py

from pygments.lexers import PythonLexer


class CustomLexer(PythonLexer):
    name = 'PythonLexerWrapper'


class LexerWrapper(CustomLexer):
    name = 'PythonLexerWrapperWrapper'
