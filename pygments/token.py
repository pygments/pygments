# -*- coding: utf-8 -*-
"""
    pygments.token
    ~~~~~~~~~~~~~~

    Basic token types and the standard tokens.

    :copyright: 2006 by Georg Brandl.
    :license: GNU LGPL, see LICENSE for more details.
"""

class _TokenType(tuple):
    parent = None

    def split(self):
        buf = []
        node = self
        while node is not None:
            buf.append(node)
            node = node.parent
        buf.reverse()
        return buf

    def __getattr__(self, val):
        if not val or not val[0].isupper():
            return tuple.__getattr__(self, val)
        new = _TokenType(self + (val,))
        setattr(self, val, new)
        new.parent = self
        return new

    def __hash__(self):
        return hash(tuple(self))

    def __repr__(self):
        return 'Token' + (self and '.' or '') + '.'.join(self)


Token     = _TokenType()

# Special token types
Text      = Token.Text
Error     = Token.Error
# Text that doesn't belong to this lexer (e.g. HTML in PHP)
Other     = Token.Other

# Common token types for source code
Keyword   = Token.Keyword
Name      = Token.Name
Literal   = Token.Literal
String    = Literal.String
Number    = Literal.Number
Operator  = Token.Operator
Comment   = Token.Comment

# Generic types for non-source code
Generic   = Token.Generic


def is_token_subtype(ttype, other):
    """Return True if ``ttype`` is a subtype of ``other``."""
    while ttype is not None:
        if ttype == other:
            return True
        ttype = ttype.parent
    return False


# Map standard token types to short names, used in CSS class naming.
# If you add a new item, please be sure to run this file to perform
# a consistency check for duplicate values.
STANDARD_TYPES = {
    Token:                         '',

    Text:                          '',
    Error:                         'err',
    Other:                         'x',

    Keyword:                       'k',
    Keyword.Constant:              'kc',
    Keyword.Declaration:           'kd',
    Keyword.Pseudo:                'kp',
    Keyword.Reserved:              'kr',
    Keyword.Type:                  'kt',

    Name:                          'n',
    Name.Attribute:                'na',
    Name.Builtin:                  'nb',
    Name.Builtin.Pseudo:           'bp',
    Name.Class:                    'nc',
    Name.Constant:                 'no',
    Name.Decorator:                'nd',
    Name.Entity:                   'ni',
    Name.Exception:                'ne',
    Name.Function:                 'nf',
    Name.Label:                    'nl',
    Name.Namespace:                'nn',
    Name.Other:                    'nx',
    Name.Tag:                      'nt',
    Name.Variable:                 'nv',
    Name.Variable.Class:           'vc',
    Name.Variable.Global:          'vg',
    Name.Variable.Instance:        'vi',

    Literal:                       'l',
    Literal.Date:                  'ld',

    String:                        's',
    String.Backtick:               'sb',
    String.Char:                   'sc',
    String.Doc:                    'sd',
    String.Double:                 's2',
    String.Escape:                 'se',
    String.Heredoc:                'sh',
    String.Interpol:               'si',
    String.Other:                  'sx',
    String.Regex:                  'sr',
    String.Single:                 's1',
    String.Symbol:                 'ss',

    Number:                        'm',
    Number.Float:                  'mf',
    Number.Hex:                    'mh',
    Number.Integer:                'mi',
    Number.Integer.Long:           'il',
    Number.Oct:                    'mo',

    Operator:                      'o',
    Operator.Word:                 'ow',

    Comment:                       'c',
    Comment.Multiline:             'cm',
    Comment.Preproc:               'cp',
    Comment.Single:                'c1',

    Generic:                       'g',
    Generic.Deleted:               'gd',
    Generic.Emph:                  'ge',
    Generic.Error:                 'gr',
    Generic.Heading:               'gh',
    Generic.Inserted:              'gi',
    Generic.Output:                'go',
    Generic.Prompt:                'gp',
    Generic.Strong:                'gs',
    Generic.Subheading:            'gu',
    Generic.Traceback:             'gt',
}



if __name__ == '__main__':
    import sys
    # sanity check for token name dict: no duplicate entries!
    s = STANDARD_TYPES.copy()
    s[Token] = '---' # Token and Text do conflict, that is okay
    t = {}
    for k, v in s.iteritems():
        t.setdefault(v, []).append(k)
    if len(t) == len(s):
        print 'Okay!'
        sys.exit()

    for k, v in t.iteritems():
        if len(v) > 1:
            print "%r has more than one key: %r" % (k, v)
