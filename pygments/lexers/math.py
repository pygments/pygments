# -*- coding: utf-8 -*-
"""
    pygments.lexers.math
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for math languages.

    :copyright: 2007 by Christopher Creutzig, Ken Schutte.
    :license: BSD, see LICENSE for more details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, bygroups
from pygments.token import Comment, String, Punctuation, Keyword, Name, \
    Operator, Number, Text, Generic

__all__ = ['MuPADLexer', 'MatlabLexer', 'MatlabSessionLexer']


class MuPADLexer(RegexLexer):
    """
    A `MuPAD <http://www.mupad.com>`_ lexer.
    Contributed by Christopher Creutzig <christopher@creutzig.de>.

    *New in Pygments 0.8.*
    """
    name = 'MuPAD'
    aliases = ['mupad']
    filenames = ['*.mu']

    tokens = {
      'root' : [
        (r'//.*?$', Comment.Single),
        (r'/\*', Comment.Multiline, 'comment'),
        (r'"(?:[^"\\]|\\.)*"', String),
        (r'\(|\)|\[|\]|\{|\}', Punctuation),
        (r'''(?x)\b(?:
            next|break|end|
            axiom|end_axiom|category|end_category|domain|end_domain|inherits|
            if|%if|then|elif|else|end_if|
            case|of|do|otherwise|end_case|
            while|end_while|
            repeat|until|end_repeat|
            for|from|to|downto|step|end_for|
            proc|local|option|save|begin|end_proc|
            delete|frame
          )\b''', Keyword),
        (r'''(?x)\b(?:
            DOM_ARRAY|DOM_BOOL|DOM_COMPLEX|DOM_DOMAIN|DOM_EXEC|DOM_EXPR|
            DOM_FAIL|DOM_FLOAT|DOM_FRAME|DOM_FUNC_ENV|DOM_HFARRAY|DOM_IDENT|
            DOM_INT|DOM_INTERVAL|DOM_LIST|DOM_NIL|DOM_NULL|DOM_POLY|DOM_PROC|
            DOM_PROC_ENV|DOM_RAT|DOM_SET|DOM_STRING|DOM_TABLE|DOM_VAR
          )\b''', Name.Class),
        (r'''(?x)\b(?:
            PI|EULER|E|CATALAN|
            NIL|FAIL|undefined|infinity|
            TRUE|FALSE|UNKNOWN
          )\b''',
          Name.Constant),
        (r'\b(?:dom|procname)\b', Name.Builtin.Pseudo),
        (r'\.|,|:|;|=|\+|-|\*|/|\^|@|>|<|\$|\||!|\'|%|~=', Operator),
        (r'''(?x)\b(?:
            and|or|not|xor|
            assuming|
            div|mod|
            union|minus|intersect|in|subset
          )\b''',
          Operator.Word),
        (r'\b(?:I|RDN_INF|RD_NINF|RD_NAN)\b', Number),
        #(r'\b(?:adt|linalg|newDomain|hold)\b', Name.Builtin),
        (r'''(?x)
          ((?:[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)
          (?:::[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)*)\s*([(])''',
          bygroups(Name.Function, Punctuation)),
        (r'''(?x)
          (?:[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)
          (?:::[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)*''', Name.Variable),
        (r'[0-9]+(?:\.[0-9]*)?(?:e[0-9]+)?', Number),
        (r'\.[0-9]+(?:e[0-9]+)?', Number),
        (r'.', Text)
      ],
      'comment' : [
        (r'[^*/]', Comment.Multiline),
        (r'/\*', Comment.Multiline, '#push'),
        (r'\*/', Comment.Multiline, '#pop'),
        (r'[*/]', Comment.Multiline)
      ]
    }


class MatlabLexer(RegexLexer):
    """
    For Matlab (or GNU Octave) source code.
    Contributed by Ken Schutte <kschutte@csail.mit.edu>.

    *New in Pygments 1.0.*
    """
    name = 'Matlab'
    aliases = ['matlab', 'octave']
    filenames = ['*.m']
    mimetypes = ['text/matlab']

    #
    # These lists are generated automatically.
    # Run the following in bash shell:
    #
    # for f in elfun specfun elmat; do
    #   echo -n "$f = "
    #   matlab -nojvm -r "help $f;exit;"|perl -ne 'push(@c,$1) if /^    (\w+)\s+-/; END {print q{["}.join(q{","},@c).qq{"]\n};}'
    # done
    #
    # elfun: Elementary math functions
    # specfun: Special Math functions
    # elmat: Elementary matrices and matrix manipulation
    #
    # taken from Matlab version 7.4.0.336 (R2007a)
    #
    elfun = ["sin","sind","sinh","asin","asind","asinh","cos","cosd","cosh",
             "acos","acosd","acosh","tan","tand","tanh","atan","atand","atan2",
             "atanh","sec","secd","sech","asec","asecd","asech","csc","cscd",
             "csch","acsc","acscd","acsch","cot","cotd","coth","acot","acotd",
             "acoth","hypot","exp","expm1","log","log1p","log10","log2","pow2",
             "realpow","reallog","realsqrt","sqrt","nthroot","nextpow2","abs",
             "angle","complex","conj","imag","real","unwrap","isreal","cplxpair",
             "fix","floor","ceil","round","mod","rem","sign"]
    specfun = ["airy","besselj","bessely","besselh","besseli","besselk","beta",
               "betainc","betaln","ellipj","ellipke","erf","erfc","erfcx",
               "erfinv","expint","gamma","gammainc","gammaln","psi","legendre",
               "cross","dot","factor","isprime","primes","gcd","lcm","rat",
               "rats","perms","nchoosek","factorial","cart2sph","cart2pol",
               "pol2cart","sph2cart","hsv2rgb","rgb2hsv"]
    elmat = ["zeros","ones","eye","repmat","rand","randn","linspace","logspace",
             "freqspace","meshgrid","accumarray","size","length","ndims","numel",
             "disp","isempty","isequal","isequalwithequalnans","cat","reshape",
             "diag","blkdiag","tril","triu","fliplr","flipud","flipdim","rot90",
             "find","end","sub2ind","ind2sub","bsxfun","ndgrid","permute",
             "ipermute","shiftdim","circshift","squeeze","isscalar","isvector",
             "ans","eps","realmax","realmin","pi","i","inf","nan","isnan",
             "isinf","isfinite","j","why","compan","gallery","hadamard","hankel",
             "hilb","invhilb","magic","pascal","rosser","toeplitz","vander",
             "wilkinson"]

    tokens = {
        'root': [
            # line starting with '!' is sent as a system command.  not sure what
            # label to use...
            (r'^!.*', String.Other),
            (r'%.*$', Comment),
            (r'^\s*function', Keyword, 'deffunc'),

            # from 'iskeyword' on version 7.4.0.336 (R2007a):
            (r'break|case|catch|classdef|continue|else|elseif|end|for|function|'
             r'global|if|otherwise|parfor|persistent|return|switch|try|while',
             Keyword),

            ("|".join(elfun+specfun+elmat), Name.Builtin),

            # operators:
            (r'-|==|~=|<|>|<=|>=|&&|&|~', Operator),
            # operators requiring escape for re:
            (r'\.\*|\*|\+|\.\^|\.\\|\.\/|\/|\\', Operator),

            # punctuation:
            (r'\[|\]|\(|\)|\{|\}|:|@|\.|,', Punctuation),
            (r'=|:|;', Punctuation),

            # quote can be transpose, instead of string:
            (r'(\w+)(\')', bygroups(Text, Operator)),

            (r'\'', String, 'string'),
            (r'.', Text),
        ],
        'string': [
            (r'[^\']*\'', String, '#pop')
        ],
        'deffunc': [
            (r'(\s*)(.+)(\s*)(=)(\s*)(.+)(\()(.*)(\))(\s*)',
             bygroups(Text.Whitespace, Text, Text.Whitespace, Punctuation,
                      Text.Whitespace, Name.Function, Punctuation, Text,
                      Punctuation, Text.Whitespace), '#pop'),
        ],
    }


line_re  = re.compile('.*?\n')

class MatlabSessionLexer(Lexer):
    """
    For Matlab (or GNU Octave) sessions.  Modeled after PythonConsoleLexer.
    Contributed by Ken Schutte <kschutte@csail.mit.edu>.

    *New in Pygments 1.0.*
    """
    name = 'Matlab session'
    aliases = ['matlabsession']

    def get_tokens_unprocessed(self, text):
        mlexer = MatlabLexer(**self.options)

        curcode = ''
        insertions = []

        for match in line_re.finditer(text):
            line = match.group()

            if line.startswith('>>'):
                insertions.append((len(curcode),
                                   [(0, Generic.Prompt, line[:3])]))
                curcode += line[3:]

            elif line.startswith('???'):

                idx = len(curcode)

                # without is showing error on same line as before...?
                line = "\n" + line
                token = (0, Generic.Traceback, line)
                insertions.append(  (idx, [token,]) )

            else:
                if curcode:
                    for item in do_insertions(
                        insertions, mlexer.get_tokens_unprocessed(curcode)):
                        yield item
                    curcode = ''
                    insertions = []

                yield match.start(), Generic.Output, line

        if curcode: # or item:
            for item in do_insertions(
                insertions, mlexer.get_tokens_unprocessed(curcode)):
                yield item
