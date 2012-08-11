# -*- coding: utf-8 -*-
"""
    pygments.lexers.bugs
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for BUGS-like languages for Bayesian statistical models.

    :copyright: Copyright 2012 by the Pygments team, 
    :license: BSD, see LICENSE for details.
"""
from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Comment, String, Punctuation, Keyword, Name, \
    Operator, Number, Text

__all__ = ['JagsLexer', 'BugsLexer', 'StanLexer']

class BugsLexer(RegexLexer):
    """ Pygments Lexer for Stan models """

    name = 'BUGS'
    aliases = ['bugs', 'winbugs', 'openbugs']
    filenames = ['*.bug']

    _FUNCTIONS = [
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
        ## Special
        'D', 'I', 'F', 'T', 'C']
    """ OpenBUGS built-in functions 

    From http://www.openbugs.info/Manuals/ModelSpecification.html#ContentsAII

    This also includes

    - T, C, I : Truncation and censoring. ``T`` and ``C`` are in OpenBUGS. ``I`` in WinBUGS.
    - D : ODE 
    - F : Functional http://www.openbugs.info/Examples/Functionals.html

    """

    _DISTRIBUTIONS = ['dbern', 'dbin', 'dcat', 'dnegbin', 'dpois',
                           'dhyper', 'dbeta', 'dchisqr', 'ddexp', 'dexp',
                           'dflat', 'dgamma', 'dgev', 'df', 'dggamma', 'dgpar',
                           'dloglik', 'dlnorm', 'dlogis', 'dnorm', 'dpar',
                           'dt', 'dunif', 'dweib', 'dmulti', 'ddirch', 'dmnorm',
                           'dmt', 'dwish']
    """ OpenBUGS built-in distributions

    Functions From http://www.openbugs.info/Manuals/ModelSpecification.html#ContentsAI
    """


    tokens = {
        'whitespace' : [
            (r"\s+", Text),
            ],
        'comments' : [
            # Comments
            (r'#.*$', Comment.Single),
            ],
        'root': [
            # Comments
            include('comments'),
            include('whitespace'),
            # Block start
            (r'(?s)(model)(\s|\n)+({)',
             bygroups(Keyword.Namespace, Text, Punctuation), 'block')
        ],
        'block' : [
            include('comments'),
            include('whitespace'),
            # Reserved Words
            (r'(for|in)\b', Keyword.Reserved),
            # Built-in Functions
            (r'(%s)(?=\s*\()'
             % r'|'.join(_FUNCTIONS + _DISTRIBUTIONS),
             Name.Builtin),
            # Regular variable names
            (r'[A-Za-z][A-Za-z0-9_.]*', Name),
            # Number Literals
            (r'[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?', Number),
            # Punctuation
            (r'(\[|\]|\(|\)|:|,)', Punctuation),
            # Assignment operators
            # SLexer makes these tokens Operators. 
            (r'(<-|~)', Operator),
            # Infix and prefix operators
            (r'(\+|-|\*|/)', Operator),
            # Block
            (r'{', Punctuation, '#push'),
            (r'}', Punctuation, '#pop'),
            ]
        }

class JagsLexer(RegexLexer):
    """ Pygments Lexer for JAGS """
    name = 'JAGS'
    aliases = ['jags']
    filenames = ['*.jags']
    
    ## JAGS
    _FUNCTIONS = [
        'abs', 'arccos', 'arccosh', 'arcsin', 'arcsinh', 'arctan', 'arctanh',
        'cos', 'cosh', 'cloglog',
        'equals', 'exp', 'icloglog', 'ifelse', 'ilogit', 'log', 'logfact',
        'loggam', 'logit', 'phi', 'pow', 'probit', 'round', 'sin', 'sinh',
        'sqrt', 'step', 'tan', 'tanh', 'trunc', 'inprod', 'interp.lin',
        'logdet', 'max', 'mean', 'min', 'prod', 'sum', 'sd', 'inverse', 'rank', 'sort', 't',
        'acos', 'acosh', 'asin', 'asinh', 'atan',
        # Truncation/Censoring (should I include)
        'T', 'I']
    # Distributions with density, probability and quartile functions
    _DISTRIBUTIONS = ['[dpq]%s' % x for x in
                           ['bern', 'beta', 'dchiqsqr', 'ddexp', 'dexp',
                            'df', 'gamma', 'gen.gamma', 'logis', 'lnorm',
                            'negbin', 'nchisqr', 'norm', 'par', 'pois', 'weib']]
    # Other distributions without density and probability 
    _OTHER_DISTRIBUTIONS = [
        'dt', 'dunif', 'dbetabin', 'dbern', 'dbin', 'dcat', 'dhyper',
        'ddirch', 'dmnorm', 'dwish', 'dmt', 'dmulti', 'dbinom', 'dchisq',
        'dnbinom', 'dweibull', 'ddirich']

    tokens = {
        'whitespace' : [
            (r"\s+", Text),
            ],
        'names' : [
            # Regular variable names
            (r'\b[A-Za-z][A-Za-z0-9_.]*\b', Name),
            ],
        'comments' : [
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
            (r'(?s)(model|data)(\s|\n)+({)',
             bygroups(Keyword.Namespace, Text, Punctuation), 'block'),
            # Variable declaration (TODO: improve)
            (r'var\b', Keyword.Declaration, 'var')
        ],
        'statements': [
            include('comments'),
            include('whitespace'),
            # Reserved Words
            (r'(for|in)\b', Keyword.Reserved),
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
            (r'(\[|\]|\(|\)|:|,)', Punctuation),
            # Assignment operators
            (r'(<-|~)', Operator),
            # # JAGS includes many more than OpenBUGS
            # |/|\|\||\&\&|>=?|<=?|[=!]?=|!|%.*?%|^)'
            (r'(\+|-|\*|\/|\|\|[&]{2}|[<>=]=?|\^|%.*?%)', Operator),
            ],
        'block' : [
            include('statements'),
            (r';', Punctuation),
            (r'{', Punctuation, '#push'),
            (r'}', Punctuation, '#pop'),
            ],
        'var' : [
            include('statements'),
            (r';', Punctuation, '#pop'),
            ]
        }

class StanLexer(RegexLexer):
    """ Pygments Lexer for Stan models """
    name = 'Stan'
    aliases = ['stan']
    filenames = ['*.stan']

    _RESERVED = ('for', 'in', 'while', 'repeat', 'until', 'if',
                'then', 'else', 'true', 'false', 'T')

    _TYPES = ('int', 'real', 'vector', 'simplex', 'ordered', 'row_vector', 'matrix',
              'corr_matrix', 'cov_matrix')

    # STAN 1.0 Manual, Chapter 20
    _CONSTANTS = ['pi', 'e', 'sqrt2', 'log2', 'log10', 'nan', 'infinity',
                  'epsilon', 'negative_epsilon']
    _FUNCTIONS = ['abs', 'int_step', 'min', 'max',
                  'if_else', 'step',
                  'fabs', 'fdim',
                  'fmin', 'fmax',
                  'fmod',
                  'floor', 'ceil', 'round', 'trunc',
                  'sqrt', 'cbrt', 'square', 'exp', 'exp2', 'expm1',
                  'log', 'log2', 'log10', 'pow', 'logit', 'inv_logit',
                  'inv_cloglog', 'hypot', 'cos', 'sin', 'tan', 'acos',
                  'asin', 'atan', 'atan2', 'cosh', 'sinh', 'tanh',
                  'acosh', 'asinh', 'atanh', 'erf', 'erfc', 'Phi',
                  'log_loss', 'tgamma', 'lgamma', 'lmgamma', 'lbeta',
                  'binomial_coefficient_log',
                  'fma', 'multiply_log', 'log1p', 'log1m', 'log1p_exp',
                  'log_sum_exp',
                  'rows', 'cols',
                  'dot_product', 'prod', 'mean', 'variance', 'sd',
                  'diagonal', 'diag_matrix', 'col', 'row',
                  'softmax', 'trace', 'determinant', 'inverse', 'eigenvalue',
                  'eigenvalues_sym', 'cholesky', 'singular_values',
                  '(log)?normal_p', 'exponential_p', 'gamma_p', 'weibull_p']
    _DISTRIBUTIONS = ['bernoulli', 'bernoulli_logit', 'binomial',
                      'beta_binomial', 'hypergeometric', 'categorical',
                      'ordered_logistic', 'negative_binomial', 'poisson',
                      'multinomial', 'normal', 'student_t',
                      'cauchy', 'double_exponential', 'logistic',
                      'lognormal', 'chi_square', 'inv_chi_square',
                      'scaled_inv_chi_square', 'exponential',
                      'gamma', 'inv_gamma', 'weibull', 'pareto',
                      'beta', 'uniform', 'dirichlet', 'multi_normal',
                      'multi_normal_cholesky', 'multi_student_t',
                      'wishart', 'inv_wishart', 'lkj_cov',
                      'lkj_corr_cholesky']

    tokens = {
        'whitespace' : [
            (r"\s+", Text),
            ],
        'comments' : [
            # do not use stateful comments
            (r'(?s)/\*.*?\*/', Comment.Multiline),
            # Comments
            (r'(//|#).*$', Comment.Single),
            ],
        'root': [
            # Comments
            include('comments'),
            # block start
            include('whitespace'),
            # Block start
            (r'(?s)(%s)(\s*)({)' %
             r'|'.join(('data', r'transformed\s+?data',
                        'parameters', r'transformed\s+parameters',
                        'model', r'generated\s+quantities')),
             bygroups(Keyword.Namespace, Text, Punctuation), 'block')
        ],
        'block' : [
            include('comments'),
            include('whitespace'),
            # Reserved Words
            (r'(%s)\b' % r'|'.join(_RESERVED), Keyword.Reserved),
            # Data types
            (r'(%s)\b' % r'|'.join(_TYPES), Keyword.Type),
            # Punctuation
            (r"[;:,\[\]()]", Punctuation),
            # Builtin
            (r'(%s)(?=\s*\()'
             % r'|'.join(_FUNCTIONS
                         + _DISTRIBUTIONS 
                         + ['%s_log' % x for x in _DISTRIBUTIONS]),
             Name.Builtin),
            (r'(%s)(?=\s*\()'
             % r'|'.join(_CONSTANTS),
             Keyword.Constant),
            # Special names ending in __, like lp__
            (r'\b[A-Za-z][A-Za-z0-9_]*__\b', Name.Builtin.Pseudo),
            # Regular variable names
            (r'\b[A-Za-z][A-Za-z0-9_]*\b', Name),
            # Real Literals
            (r'-?[0-9]+(\.[0-9]+)?[eE]-?[0-9]+', Number.Float),
            (r'-?[0-9]*\.[0-9]*', Number.Float),
            # Integer Literals
            (r'-?[0-9]+', Number.Integer),
            # Assignment operators
            # SLexer makes these tokens Operators. 
            (r'(<-|~)', Operator),
            # Infix and prefix operators
            (r"(\+|-|\.?\*|\.?/|\\|')", Operator),
            # Block
            (r'{', Punctuation, '#push'),
            (r'}', Punctuation, '#pop'),
            ]
        }
