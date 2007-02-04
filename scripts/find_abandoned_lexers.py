#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Find Not Documented Lexers
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    Call it from the command line to find out which lexers arn't
    in the documentation right now. Maybe it would be a good idea
    to check the mimetypes, filename patterns and aliases too.

    :copyright: 2006-2007 by Armin Ronacher.
    :license: BSD, see LICENSE for more details.
"""
import re
import sys
import os


base_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
lexer_doc = os.path.join(base_path, 'docs', 'src', 'lexers.txt')
sys.path.insert(0, base_path)

from pygments.lexers import LEXERS

existing_lexers = set(LEXERS)
documented_lexers = set()
f = open(lexer_doc)
for line in f:
    m = re.search('^`([a-zA-Z0-9]+Lexer)`\s*$(?m)', line)
    if m:
        documented_lexers.add(m.group(1))
f.close()

just_documented = sorted(documented_lexers.difference(existing_lexers))
not_documented = sorted(existing_lexers.difference(documented_lexers))

if just_documented:
    print 'Lexers that just exist in the documentation:'
    for lexer in just_documented:
        print '   ', lexer

if not_documented:
    print 'Lexers that are not documented by now:'
    for lexer in not_documented:
        print '   ', lexer
