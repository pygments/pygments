# -*- coding: utf-8 -*-
"""
    pygments.lexers.modeling
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for modeling languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, using
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

from pygments.lexers.html import HtmlLexer
from pygments.lexers import _stan_builtins

__all__ = ['ModelicaLexer', 'BugsLexer', 'JagsLexer', 'StanLexer']


class ModelicaLexer(RegexLexer):
    """
    For `Modelica <http://www.modelica.org/>`_ source code.

    .. versionadded:: 1.1
    """
    name = 'Modelica'
    aliases = ['modelica']
    filenames = ['*.mo']
    mimetypes = ['text/x-modelica']

    flags = re.IGNORECASE | re.DOTALL

    tokens = {
        'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment),
            (r'/(\\\n)?\*(.|\n)*?\*(\\\n)?/', Comment),
        ],
        'statements': [
            (r'"', String, 'string'),
            (r'(\d+\.\d*|\.\d+|\d+|\d.)[eE][+-]?\d+[lL]?', Number.Float),
            (r'(\d+\.\d*|\.\d+)', Number.Float),
            (r'\d+[Ll]?', Number.Integer),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'(true|false|NULL|Real|Integer|Boolean)\b', Name.Builtin),
            (r'([a-z_][\w]*|\'[^\']+\')'
             r'([\[\d,:\]]*)'
             r'(\.([a-z_][\w]*|\'[^\']+\'))+'
             r'([\[\d,:\]]*)', Name.Class),
            (r'(\'[\w\+\-\*\/\^]+\'|\w+)', Name),
            (r'[()\[\]{},.;]', Punctuation),
            (r'\'', Name, 'quoted_ident'),
        ],
        'root': [
            include('whitespace'),
            include('classes'),
            include('functions'),
            include('keywords'),
            include('operators'),
            (r'("<html>|<html>)', Name.Tag, 'html-content'),
            include('statements'),
        ],
        'keywords': [
            (r'(algorithm|annotation|break|connect|constant|constrainedby|'
            r'discrete|each|end|else|elseif|elsewhen|encapsulated|enumeration|'
            r'equation|exit|expandable|extends|'
            r'external|false|final|flow|for|if|import|impure|in|initial\sequation|'
            r'inner|input|loop|nondiscrete|outer|output|parameter|partial|'
            r'protected|public|pure|redeclare|replaceable|stream|time|then|true|'
            r'when|while|within)\b', Keyword),
        ],
        'functions': [
            (r'(abs|acos|acosh|asin|asinh|atan|atan2|atan3|ceil|cos|cosh|'
             r'cross|diagonal|div|exp|fill|floor|getInstanceName|identity|'
             r'linspace|log|log10|matrix|mod|max|min|ndims|ones|outerProduct|'
             r'product|rem|scalar|semiLinear|skew|sign|sin|sinh|size|'
             r'spatialDistribution|sum|sqrt|symmetric|tan|tanh|transpose|'
             r'vector|zeros)\b', Name.Function),
        ],
        'operators': [
            (r'(actualStream|and|assert|backSample|cardinality|change|Clock|'
             r'delay|der|edge|hold|homotopy|initial|inStream|noClock|noEvent|'
             r'not|or|pre|previous|reinit|return|sample|smooth|'
             r'spatialDistribution|shiftSample|subSample|superSample|terminal|'
             r'terminate)\b', Name.Builtin),
        ],
        'classes': [
            (r'(operator)?(\s+)?(block|class|connector|end|function|model|'
             r'operator|package|record|type)(\s+)'
             r'((?!if|for|when|while)[a-z_]\w*|\'[^\']+\')([;]?)',
             bygroups(Keyword, Text, Keyword, Text, Name.Class, Text))
        ],
        'quoted_ident': [
            (r'\'', Name, '#pop'),
            (r'[^\']+', Name), # all other characters
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-f0-9]{2,4}|[0-7]{1,3})',
             String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'html-content': [
            (r'<\s*/\s*html\s*>"', Name.Tag, '#pop'),
            (r'.+?(?=<\s*/\s*html\s*>)', using(HtmlLexer)),
        ]
    }


class BugsLexer(RegexLexer):
    """
    Pygments Lexer for `OpenBugs <http://www.openbugs.info/w/>`_ and WinBugs
    models.

    .. versionadded:: 1.6
    """

    name = 'BUGS'
    aliases = ['bugs', 'winbugs', 'openbugs']
    filenames = ['*.bug']

    _FUNCTIONS = (
        # Scalar functions
        'abs', 'arccos', 'arccosh', 'arcsin', 'arcsinh', 'arctan', 'arctanh',
        'cloglog', 'cos', 'cosh', 'cumulative', 'cut', 'density', 'deviance',
        'equals', 'expr', 'gammap', 'ilogit', 'icloglog', 'integral', 'log',
        'logfact', 'loggam', 'logit', 'max', 'min', 'phi', 'post.p.value',
        'pow', 'prior.p.value', 'probit', 'replicate.post', 'replicate.prior',
        'round', 'sin', 'sinh', 'solution', 'sqrt', 'step', 'tan', 'tanh',
        'trunc',
        # Vector functions
        'inprod', 'interp.lin', 'inverse', 'logdet', 'mean', 'eigen.vals',
        'ode', 'prod', 'p.valueM', 'rank', 'ranked', 'replicate.postM',
        'sd', 'sort', 'sum',
        # Special
        'D', 'I', 'F', 'T', 'C')
    """ OpenBUGS built-in functions

    From http://www.openbugs.info/Manuals/ModelSpecification.html#ContentsAII

    This also includes

    - T, C, I : Truncation and censoring.
      ``T`` and ``C`` are in OpenBUGS. ``I`` in WinBUGS.
    - D : ODE
    - F : Functional http://www.openbugs.info/Examples/Functionals.html

    """

    _DISTRIBUTIONS = ('dbern', 'dbin', 'dcat', 'dnegbin', 'dpois',
                      'dhyper', 'dbeta', 'dchisqr', 'ddexp', 'dexp',
                      'dflat', 'dgamma', 'dgev', 'df', 'dggamma', 'dgpar',
                      'dloglik', 'dlnorm', 'dlogis', 'dnorm', 'dpar',
                      'dt', 'dunif', 'dweib', 'dmulti', 'ddirch', 'dmnorm',
                      'dmt', 'dwish')
    """ OpenBUGS built-in distributions

    Functions from
    http://www.openbugs.info/Manuals/ModelSpecification.html#ContentsAI
    """

    tokens = {
        'whitespace': [
            (r"\s+", Text),
        ],
        'comments': [
            # Comments
            (r'#.*$', Comment.Single),
        ],
        'root': [
            # Comments
            include('comments'),
            include('whitespace'),
            # Block start
            (r'(model)(\s+)({)',
             bygroups(Keyword.Namespace, Text, Punctuation)),
            # Reserved Words
            (r'(for|in)(?![0-9a-zA-Z\._])', Keyword.Reserved),
            # Built-in Functions
            (r'(%s)(?=\s*\()'
             % r'|'.join(_FUNCTIONS + _DISTRIBUTIONS),
             Name.Builtin),
            # Regular variable names
            (r'[A-Za-z][\w.]*', Name),
            # Number Literals
            (r'[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?', Number),
            # Punctuation
            (r'\[|\]|\(|\)|:|,|;', Punctuation),
            # Assignment operators
            # SLexer makes these tokens Operators.
            (r'<-|~', Operator),
            # Infix and prefix operators
            (r'\+|-|\*|/', Operator),
            # Block
            (r'[{}]', Punctuation),
        ]
    }

    def analyse_text(text):
        if re.search(r"^\s*model\s*{", text, re.M):
            return 0.7
        else:
            return 0.0


class JagsLexer(RegexLexer):
    """
    Pygments Lexer for JAGS.

    .. versionadded:: 1.6
    """

    name = 'JAGS'
    aliases = ['jags']
    filenames = ['*.jag', '*.bug']

    # JAGS
    _FUNCTIONS = (
        'abs', 'arccos', 'arccosh', 'arcsin', 'arcsinh', 'arctan', 'arctanh',
        'cos', 'cosh', 'cloglog',
        'equals', 'exp', 'icloglog', 'ifelse', 'ilogit', 'log', 'logfact',
        'loggam', 'logit', 'phi', 'pow', 'probit', 'round', 'sin', 'sinh',
        'sqrt', 'step', 'tan', 'tanh', 'trunc', 'inprod', 'interp.lin',
        'logdet', 'max', 'mean', 'min', 'prod', 'sum', 'sd', 'inverse',
        'rank', 'sort', 't', 'acos', 'acosh', 'asin', 'asinh', 'atan',
        # Truncation/Censoring (should I include)
        'T', 'I')
    # Distributions with density, probability and quartile functions
    _DISTRIBUTIONS = tuple('[dpq]%s' % x for x in
                           ('bern', 'beta', 'dchiqsqr', 'ddexp', 'dexp',
                            'df', 'gamma', 'gen.gamma', 'logis', 'lnorm',
                            'negbin', 'nchisqr', 'norm', 'par', 'pois', 'weib'))
    # Other distributions without density and probability
    _OTHER_DISTRIBUTIONS = (
        'dt', 'dunif', 'dbetabin', 'dbern', 'dbin', 'dcat', 'dhyper',
        'ddirch', 'dmnorm', 'dwish', 'dmt', 'dmulti', 'dbinom', 'dchisq',
        'dnbinom', 'dweibull', 'ddirich')

    tokens = {
        'whitespace': [
            (r"\s+", Text),
        ],
        'names': [
            # Regular variable names
            (r'[a-zA-Z][\w.]*\b', Name),
        ],
        'comments': [
            # do not use stateful comments
            (r'(?s)/\*.*?\*/', Comment.Multiline),
            # Comments
            (r'#.*$', Comment.Single),
        ],
        'root': [
            # Comments
            include('comments'),
            include('whitespace'),
            # Block start
            (r'(model|data)(\s+)({)',
             bygroups(Keyword.Namespace, Text, Punctuation)),
            (r'var(?![0-9a-zA-Z\._])', Keyword.Declaration),
            # Reserved Words
            (r'(for|in)(?![0-9a-zA-Z\._])', Keyword.Reserved),
            # Builtins
            # Need to use lookahead because . is a valid char
            (r'(%s)(?=\s*\()' % r'|'.join(_FUNCTIONS
                                          + _DISTRIBUTIONS
                                          + _OTHER_DISTRIBUTIONS),
             Name.Builtin),
            # Names
            include('names'),
            # Number Literals
            (r'[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?', Number),
            (r'\[|\]|\(|\)|:|,|;', Punctuation),
            # Assignment operators
            (r'<-|~', Operator),
            # # JAGS includes many more than OpenBUGS
            (r'\+|-|\*|\/|\|\|[&]{2}|[<>=]=?|\^|%.*?%', Operator),
            (r'[{}]', Punctuation),
        ]
    }

    def analyse_text(text):
        if re.search(r'^\s*model\s*\{', text, re.M):
            if re.search(r'^\s*data\s*\{', text, re.M):
                return 0.9
            elif re.search(r'^\s*var', text, re.M):
                return 0.9
            else:
                return 0.3
        else:
            return 0


class StanLexer(RegexLexer):
    """Pygments Lexer for Stan models.

    The Stan modeling language is specified in the *Stan Modeling Language User's Guide and Reference Manual, v2.4.0*,
    `pdf <https://github.com/stan-dev/stan/releases/download/v2.4.0/stan-reference-2.4.0.pdf>`__.

    .. versionadded:: 1.6
    """

    name = 'Stan'
    aliases = ['stan']
    filenames = ['*.stan']

    tokens = {
        'whitespace': [
            (r"\s+", Text),
        ],
        'comments': [
            (r'(?s)/\*.*?\*/', Comment.Multiline),
            # Comments
            (r'(//|#).*$', Comment.Single),
        ],
        'root': [
            # Stan is more restrictive on strings than this regex
            (r'"[^"]*"', String),
            # Comments
            include('comments'),
            # block start
            include('whitespace'),
            # Block start
            (r'(%s)(\s*)({)' %
             r'|'.join(('functions', 'data', r'transformed\s+?data',
                        'parameters', r'transformed\s+parameters',
                        'model', r'generated\s+quantities')),
             bygroups(Keyword.Namespace, Text, Punctuation)),
            # Reserved Words
            (r'(%s)\b' % r'|'.join(_stan_builtins.KEYWORDS), Keyword),
            # Truncation
            (r'T(?=\s*\[)', Keyword),
            # Data types
            (r'(%s)\b' % r'|'.join(_stan_builtins.TYPES), Keyword.Type),
            # Punctuation
            (r"[;:,\[\]()]", Punctuation),
            # Builtin
            (r'(%s)(?=\s*\()'
             % r'|'.join(_stan_builtins.FUNCTIONS
                         + _stan_builtins.DISTRIBUTIONS),
             Name.Builtin),
            # Special names ending in __, like lp__
            (r'[A-Za-z]\w*__\b', Name.Builtin.Pseudo),
            (r'(%s)\b' % r'|'.join(_stan_builtins.RESERVED), Keyword.Reserved),
            # Regular variable names
            (r'[A-Za-z]\w*\b', Name),
            # Real Literals
            (r'-?[0-9]+(\.[0-9]+)?[eE]-?[0-9]+', Number.Float),
            (r'-?[0-9]*\.[0-9]*', Number.Float),
            # Integer Literals
            (r'-?[0-9]+', Number.Integer),
            # Assignment operators
            # SLexer makes these tokens Operators.
            (r'<-|~', Operator),
            # Infix, prefix and postfix operators (and = )
            (r"\+|-|\.?\*|\.?/|\\|'|\^|==?|!=?|<=?|>=?|\|\||&&", Operator),
            # Block delimiters
            (r'[{}]', Punctuation),
        ]
    }

    def analyse_text(text):
        if re.search(r'^\s*parameters\s*\{', text, re.M):
            return 1.0
        else:
            return 0.0
