# -*- coding: utf-8 -*-
"""
    pygments.lexers.python (as CustomLexer)
    ~~~~~~~~~~~~~~~~~~~~~~

    For test_cmdline.py
"""

from pygments.lexers import PythonLexer

class CustomLexer(PythonLexer):
    name = 'PythonLexerWrapper'

class LexerWrapper(CustomLexer):
    name="PythonLexerWrapperWrapper"
