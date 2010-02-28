#!/usr/bin/python
# -*- coding: utf-8 -*-
"""
    Lexing error finder
    ~~~~~~~~~~~~~~~~~~~

    For the source files given on the command line, display
    the text where Error tokens are being generated, along
    with some context.

    :copyright: Copyright 2006-2010 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys, os

try:
    import pygments
except ImportError:
    # try parent path
    sys.path.append(os.path.join(os.path.dirname(__file__), '..'))


from pygments.lexer import RegexLexer
from pygments.lexers import get_lexer_for_filename, get_lexer_by_name
from pygments.token import Error, Text, _TokenType


class DebuggingRegexLexer(RegexLexer):
    """Make the state stack, position and current match instance attributes."""

    def get_tokens_unprocessed(self, text, stack=('root',)):
        """
        Split ``text`` into (tokentype, text) pairs.

        ``stack`` is the inital stack (default: ``['root']``)
        """
        self.pos = 0
        tokendefs = self._tokens
        self.statestack = list(stack)
        statetokens = tokendefs[self.statestack[-1]]
        while 1:
            for rexmatch, action, new_state in statetokens:
                self.m = m = rexmatch(text, self.pos)
                if m:
                    if type(action) is _TokenType:
                        yield self.pos, action, m.group()
                    else:
                        for item in action(self, m):
                            yield item
                    self.pos = m.end()
                    if new_state is not None:
                        # state transition
                        if isinstance(new_state, tuple):
                            for state in new_state:
                                if state == '#pop':
                                    self.statestack.pop()
                                elif state == '#push':
                                    self.statestack.append(self.statestack[-1])
                                else:
                                    self.statestack.append(state)
                        elif isinstance(new_state, int):
                            # pop
                            del self.statestack[new_state:]
                        elif new_state == '#push':
                            self.statestack.append(self.statestack[-1])
                        else:
                            assert False, 'wrong state def: %r' % new_state
                        statetokens = tokendefs[self.statestack[-1]]
                    break
            else:
                try:
                    if text[self.pos] == '\n':
                        # at EOL, reset state to 'root'
                        self.pos += 1
                        self.statestack = ['root']
                        statetokens = tokendefs['root']
                        yield self.pos, Text, u'\n'
                        continue
                    yield self.pos, Error, text[self.pos]
                    self.pos += 1
                except IndexError:
                    break


def main(fn):
    try:
        lx = get_lexer_for_filename(os.path.basename(fn))
    except ValueError:
        try:
            name, rest = fn.split('_', 1)
            lx = get_lexer_by_name(name)
        except ValueError:
            raise AssertionError('no lexer found for file %r' % fn)
    debug_lexer = False
    # does not work for e.g. ExtendedRegexLexers
    if lx.__class__.__bases__ == (RegexLexer,):
        lx.__class__.__bases__ = (DebuggingRegexLexer,)
        debug_lexer = True
    lno = 1
    text = file(fn, 'U').read()
    text = text.strip('\n') + '\n'
    text = text.decode('latin1')
    ntext = []
    states = []

    def show_token(tok):
        reprs = map(repr, tok)
        print '   ' + reprs[1] + ' ' + ' ' * (29-len(reprs[1])) + reprs[0],
        if debug_lexer:
            print ' ' + ' ' * (29-len(reprs[0])) + repr(states[i]),
        print

    for type, val in lx.get_tokens(text):
        lno += val.count('\n')
        if type == Error:
            print 'Error parsing', fn, 'on line', lno
            print 'Previous tokens' + (debug_lexer and ' and states' or '') + ':'
            for i in range(len(ntext) - num, len(ntext)):
                show_token(ntext[i])
            print 'Error token:'
            l = len(repr(val))
            print '   ' + repr(val),
            if debug_lexer:
                print ' ' * (60-l) + repr(lx.statestack),
            print
            print
            return 1
        ntext.append((type,val))
        if debug_lexer:
            states.append(lx.statestack[:])
    if showall:
        for tok in ntext:
            show_token(tok)
    return 0


num = 10
showall = False

if __name__ == '__main__':
    import getopt
    opts, args = getopt.getopt(sys.argv[1:], 'n:a')
    for opt, val in opts:
        if opt == '-n':
            num = int(val)
        elif opt == '-a':
            showall = True
    ret = 0
    for f in args:
        ret += main(f)
    sys.exit(bool(ret))
