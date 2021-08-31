"""
    pygments.lexers.modeling
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for modeling languages.

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, using, default, this, inherit, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

from pygments.lexers.html import HtmlLexer
from pygments.lexers.jvm import JavaLexer
from pygments.lexers import _stan_builtins

__all__ = ['ModelicaLexer', 'BugsLexer', 'JagsLexer', 'StanLexer', 'UmpleLexer']


class ModelicaLexer(RegexLexer):
    """
    For `Modelica <http://www.modelica.org/>`_ source code.

    .. versionadded:: 1.1
    """
    name = 'Modelica'
    aliases = ['modelica']
    filenames = ['*.mo']
    mimetypes = ['text/x-modelica']

    flags = re.DOTALL | re.MULTILINE

    _name = r"(?:'(?:[^\\']|\\.)+'|[a-zA-Z_]\w*)"

    tokens = {
        'whitespace': [
            (r'[\s\ufeff]+', Text),
            (r'//[^\n]*\n?', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline)
        ],
        'root': [
            include('whitespace'),
            (r'"', String.Double, 'string'),
            (r'[()\[\]{},;]+', Punctuation),
            (r'\.?[*^/+-]|\.|<>|[<>:=]=?', Operator),
            (r'\d+(\.?\d*[eE][-+]?\d+|\.\d*)', Number.Float),
            (r'\d+', Number.Integer),
            (r'(abs|acos|actualStream|array|asin|assert|AssertionLevel|atan|'
             r'atan2|backSample|Boolean|cardinality|cat|ceil|change|Clock|'
             r'Connections|cos|cosh|cross|delay|diagonal|div|edge|exp|'
             r'ExternalObject|fill|floor|getInstanceName|hold|homotopy|'
             r'identity|inStream|integer|Integer|interval|inverse|isPresent|'
             r'linspace|log|log10|matrix|max|min|mod|ndims|noClock|noEvent|'
             r'ones|outerProduct|pre|previous|product|Real|reinit|rem|rooted|'
             r'sample|scalar|semiLinear|shiftSample|sign|sin|sinh|size|skew|'
             r'smooth|spatialDistribution|sqrt|StateSelect|String|subSample|'
             r'sum|superSample|symmetric|tan|tanh|terminal|terminate|time|'
             r'transpose|vector|zeros)\b', Name.Builtin),
            (r'(algorithm|annotation|break|connect|constant|constrainedby|der|'
             r'discrete|each|else|elseif|elsewhen|encapsulated|enumeration|'
             r'equation|exit|expandable|extends|external|firstTick|final|flow|for|if|'
             r'import|impure|in|initial|inner|input|interval|loop|nondiscrete|outer|'
             r'output|parameter|partial|protected|public|pure|redeclare|'
             r'replaceable|return|stream|then|when|while)\b',
             Keyword.Reserved),
            (r'(and|not|or)\b', Operator.Word),
            (r'(block|class|connector|end|function|model|operator|package|'
             r'record|type)\b', Keyword.Reserved, 'class'),
            (r'(false|true)\b', Keyword.Constant),
            (r'within\b', Keyword.Reserved, 'package-prefix'),
            (_name, Name)
        ],
        'class': [
            include('whitespace'),
            (r'(function|record)\b', Keyword.Reserved),
            (r'(if|for|when|while)\b', Keyword.Reserved, '#pop'),
            (_name, Name.Class, '#pop'),
            default('#pop')
        ],
        'package-prefix': [
            include('whitespace'),
            (_name, Name.Namespace, '#pop'),
            default('#pop')
        ],
        'string': [
            (r'"', String.Double, '#pop'),
            (r'\\[\'"?\\abfnrtv]', String.Escape),
            (r'(?i)<\s*html\s*>([^\\"]|\\.)+?(<\s*/\s*html\s*>|(?="))',
             using(HtmlLexer)),
            (r'<|\\?[^"\\<]+', String.Double)
        ]
    }


class BugsLexer(RegexLexer):
    """
    Pygments Lexer for `OpenBugs <http://www.openbugs.net/>`_ and WinBugs
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
            (r'(model)(\s+)(\{)',
             bygroups(Keyword.Namespace, Text, Punctuation)),
            # Reserved Words
            (r'(for|in)(?![\w.])', Keyword.Reserved),
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
            (r'(model|data)(\s+)(\{)',
             bygroups(Keyword.Namespace, Text, Punctuation)),
            (r'var(?![\w.])', Keyword.Declaration),
            # Reserved Words
            (r'(for|in)(?![\w.])', Keyword.Reserved),
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

    The Stan modeling language is specified in the *Stan Modeling Language
    User's Guide and Reference Manual, v2.17.0*,
    `pdf <https://github.com/stan-dev/stan/releases/download/v2.17.0/stan-reference-2.17.0.pdf>`__.

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
            (r'(%s)(\s*)(\{)' %
             r'|'.join(('functions', 'data', r'transformed\s+?data',
                        'parameters', r'transformed\s+parameters',
                        'model', r'generated\s+quantities')),
             bygroups(Keyword.Namespace, Text, Punctuation)),
            # target keyword
            (r'target\s*\+=', Keyword),
            # Reserved Words
            (r'(%s)\b' % r'|'.join(_stan_builtins.KEYWORDS), Keyword),
            # Truncation
            (r'T(?=\s*\[)', Keyword),
            # Data types
            (r'(%s)\b' % r'|'.join(_stan_builtins.TYPES), Keyword.Type),
             # < should be punctuation, but elsewhere I can't tell if it is in
             # a range constraint
            (r'(<)(\s*)(upper|lower)(\s*)(=)',
             bygroups(Operator, Whitespace, Keyword, Whitespace, Punctuation)),
            (r'(,)(\s*)(upper)(\s*)(=)',
             bygroups(Punctuation, Whitespace, Keyword, Whitespace, Punctuation)),
            # Punctuation
            (r"[;,\[\]()]", Punctuation),
            # Builtin
            (r'(%s)(?=\s*\()' % '|'.join(_stan_builtins.FUNCTIONS), Name.Builtin),
            (r'(~)(\s*)(%s)(?=\s*\()' % '|'.join(_stan_builtins.DISTRIBUTIONS),
                bygroups(Operator, Whitespace, Name.Builtin)),
            # Special names ending in __, like lp__
            (r'[A-Za-z]\w*__\b', Name.Builtin.Pseudo),
            (r'(%s)\b' % r'|'.join(_stan_builtins.RESERVED), Keyword.Reserved),
            # user-defined functions
            (r'[A-Za-z]\w*(?=\s*\()]', Name.Function),
            # Regular variable names
            (r'[A-Za-z]\w*\b', Name),
            # Real Literals
            (r'[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?', Number.Float),
            (r'\.[0-9]+([eE][+-]?[0-9]+)?', Number.Float),
            # Integer Literals
            (r'[0-9]+', Number.Integer),
            # Assignment operators
            (r'<-|(?:\+|-|\.?/|\.?\*|=)?=|~', Operator),
            # Infix, prefix and postfix operators (and = )
            (r"\+|-|\.?\*|\.?/|\\|'|\^|!=?|<=?|>=?|\|\||&&|%|\?|:", Operator),
            # Block delimiters
            (r'[{}]', Punctuation),
            # Distribution |
            (r'\|', Punctuation)
        ]
    }

    def analyse_text(text):
        if re.search(r'^\s*parameters\s*\{', text, re.M):
            return 1.0
        else:
            return 0.0
            
class UmpleLexer(JavaLexer):
  """
    Pygments lexer for Umple <https://cruise.umple.org/umple/> source code

    .. versionadded::2.11 
  """


  name="Umple"
  aliases=['umple']
  filenames = ['*.ump', '*.umpt']
  
  tokens = {
    'root': [
        (words(('activate', 'active', 'after', 'afterEvery', 'all', 'around', 'atomic', 'attr', 'attribute', 'before', 'custom', 'deactivate',
            'debug', 'displayColor','displayColour', 'during', 'emit','entry', 'execute', 'exit', 'generate', 'generated', 'hops', 'include',
            'includeFilter','inner','isA', 'isFeature', 'key', 'model', 'off', 'Off', 'on', 'On', 'pooled', 'position','position.association',
            'post', 'prefix', 'primitive', 'queued', 'regex', 'require', 'singleton', 'sorted', 'sub', 'subclass', 'suffix', 'super', 'superclass',
            'top', 'trace', 'unspecified'),
            suffix=r'\b'),
        Keyword),
      # method names
      (r'((?:(?:[^\W\d]|\$)[\w.\[\]$<>]*\s+)+?)'  # return arguments
      r'((?:[^\W\d]|\$)[\w$]*)'                  # method name
      r'(\s*)(\()',                              # signature start
      bygroups(using(this), Name.Function, Text.Whitespace, Punctuation)),
      
      (words(('around_proceed', 'assertAttribute', 'assertEqual', 'assertFalse',
            'assertMethod', 'assertNull', 'assertTrue'), suffix=r'\b'), Name.Function),
      (words(('autounique', 'const', 'defaulted', 'external', 'filter', 'final', 'Final', 'fixml', 'generic',
            'immutable', 'inner', 'internal', 'ivar', 'lazy', 'mixset', 'pre', 'settable', 'statemachine',
            'template', 'test', 'tracer', 'trait', 'unique'), suffix=r'\b'),
       Keyword.Declaration),
      (r'((queued|pooled)?sm)\b', Keyword),
      (words(('association', 'associationClass'), suffix=r'\b'), Keyword.Declaration, 'associations'),
      (r'(distributable)\b(\s+)(RMI|WS|off|forced)', bygroups(Keyword.Reserved, Text.Whitespace, Keyword.Reserved)),
      (r'(depend)(\s+)([\w.]+\*?)', bygroups(Keyword.Reserved, Text.Whitespace, Keyword.Namespace)),
      (r'(namespace)\b(\s+)([\w.]+\*?|-)(\s+)?(--redefine)?',
       bygroups(Keyword.Namespace, Text.Whitespace, Name.Namespace, Text.Whitespace, Keyword)),
      (r'(strictness)\b(\s+)', bygroups(Keyword.Reserved, Text.Whitespace),'strictness'),
      (r'(use)\b(\s+)(\w+.ump)\b', bygroups(Keyword.Reserved, Text.Whitespace, Name.Other)),
      #Attribute types
      (words(('Boolean', 'Double', 'Float', 'Integer', 'String'), suffix=r'\b'), Keyword.Type),
      #Alternate Language
      (words(('Java', 'Php', 'RTCpp', 'SimpleCpp', 'Ruby', 'Cpp', 'Json', 'StructureDiagram', 'Yuml', 'Violet', 'Umlet', 
            'Simulate', 'TextUml', 'Scxml', 'GvStateDiagram', 'GvClassDiagram', 'GvFeatureDiagram', 'GvClassTrait', 
            'GvEntityRelationshipDiagram', 'Alloy', 'NuSMV', 'NuSMVOptimizer', 'Papyrus', 'Ecore', 'Xmi', 'Xtext', 'Sql', 
            'Umple', 'UmpleSelf', 'USE', 'Test', 'SimpleMetrics', 'Uigu2'), suffix=r'\b'), Keyword),
       #operators
       (words(('and', 'cardinality', 'giving', 'has', 'not', 'or', 'until', 'where', 'xor'), suffix=r'\b'), Operator.Word),
       #association
       (r'([0-9]+[.]+[0-9\*]+)|<@>|->', Operator),
       inherit
     ],
     'associations': [
       (r'(\s+)([^\W][\w$]+)', bygroups(Text.Whitespace, Name.Class), "#pop"),
       (r'(\s*[{}])', bygroups(Text.Whitespace), "#pop"),
     ],
    'strictness': [
      (words(('modelOnly', 'noExtraCode', 'none'), suffix=r'\b'), Keyword, "#pop"),
      (words(('allow', 'ignore', 'expect', 'disallow'), suffix=r'\b'), Keyword, '#pop'),
    ],

  }
 
