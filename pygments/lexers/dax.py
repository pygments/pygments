"""
    pygments.lexers.dax
    ~~~~~~~~~~~~~~~~~~~

    Lexer for LilyPond.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import Comment, Punctuation, Whitespace,\
    Name, Operator, String, Number, Text

__all__ = ['DaxLexer']

class DaxLexer(RegexLexer):
    """
    Lexer for Power BI DAX
    Referenced from: https://github.com/sql-bi/SyntaxHighlighterBrushDax

    """
    name = 'Dax'
    aliases = ['dax']
    filenames = ['*.dax']
    url = 'https://learn.microsoft.com/en-us/dax/dax-function-reference'
    mimetypes = []

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r' .*\n', Text),
            (r"--.*\n?", Comment.Single),	# Comment: Double dash comment
            (r"//.*\n?", Comment.Single),	# Comment: Double backslash comment
            (r'/\*', Comment.Multiline, 'multiline-comments'),
            (words(('abs','acos','acosh','acot','acoth','addcolumns','addmissingitems','all',
                'allcrossfiltered','allexcept','allnoblankrow','allselected','and',
                'approximatedistinctcount','asin','asinh','atan','atanh','average',
                'averagea','averagex','beta.dist','beta.inv','blank','calculate',
                'calculatetable','calendar','calendarauto','ceiling','chisq.dist',
                'chisq.dist.rt','chisq.inv','chisq.inv.rt','closingbalancemonth',
                'closingbalancequarter','closingbalanceyear','coalesce','combin',
                'combina','combinevalues','concatenate','concatenatex','confidence.norm',
                'confidence.t','contains','containsrow','containsstring','containsstringexact',
                'convert','cos','cosh','cot','coth','count','counta','countax','countblank',
                'countrows','countx','crossfilter','crossjoin','currency','currentgroup',
                'customdata','datatable','date','dateadd','datediff','datesbetween',
                'datesinperiod','datesmtd','datesqtd','datesytd','datevalue','day','degrees',
                'detailrows','distinct','distinctcount','distinctcountnoblank','divide',
                'earlier','earliest','edate','endofmonth','endofquarter','endofyear',
                'eomonth','error','even','exact','except','exp','expon.dist','fact',
                'false','filter','filters','find','firstdate','firstnonblank','firstnonblankvalue',
                'fixed','floor','format','gcd','generate','generateall','generateseries','geomean',
                'geomeanx','groupby','hasonefilter','hasonevalue','hour','if','if.eager','iferror',
                'ignore','int','intersect','isblank','iscrossfiltered','isempty','iserror','iseven',
                'isfiltered','isinscope','islogical','isnontext','isnumber','iso.ceiling','isodd',
                'isonorafter','isselectedmeasure','issubtotal','istext','keepfilters',
                'keywordmatch','lastdate','lastnonblank','lastnonblankvalue','lcm','left',
                'len','ln','log','log10','lookupvalue','lower','max','maxa','maxx',
                'median','medianx','mid','min','mina','minute','minx','mod','month',
                'mround','naturalinnerjoin','naturalleftouterjoin',
                'nextday','nextmonth','nextquarter','nextyear','nonvisual','norm.dist','norm.inv',
                'norm.s.dist','norm.s.inv','not','now','odd','openingbalancemonth',
                'openingbalancequarter','openingbalanceyear','or','parallelperiod',
                'path','pathcontains','pathitem','pathitemreverse','pathlength','percentile.exc',
                'percentile.inc','percentilex.exc','percentilex.inc','permut','pi',
                'poisson.dist','power','previousday','previousmonth','previousquarter',
                'previousyear','product','productx','quarter','quotient','radians',
                'rand','randbetween','rank.eq','rankx','related','relatedtable','removefilters',
                'replace','rept','right','rollup','rollupaddissubtotal','rollupgroup',
                'rollupissubtotal','round','rounddown','roundup','row','sameperiodlastyear',
                'sample','search','second','selectcolumns','selectedmeasure',
                'selectedmeasureformatstring','selectedmeasurename','selectedvalue',
                'sign','sin','sinh','sqrt','sqrtpi','startofmonth','startofquarter',
                'startofyear','stdev.p','stdev.s''stdevx.p','stdevx.s','substitute',
                'substitutewithindex','sum','summarize','summarizecolumns','sumx',
                'switch','t.dist','t.dist.2t','t.dist.rt','t.inv','t.inv.2t','tan',
                'tanh','time','timevalue','today','topn','topnskip','totalmtd',
                'totalqtd','totalytd','treatas','trim','true','trunc','unichar','unicode',
                'union','upper','userelationship','username','userobjectid','userprincipalname',
                'utcnow','utctoday','value','values','var.p','var.s','varx.p','varx.s',
                'weekday','weeknum','xirr','xnpv','year','yearfrac'),
                 prefix=r'(?i)', suffix=r'\b'), Name.Function), #Functions

            (words(('at','asc','boolean','both','by','create','currency',
                'datetime','day','define','desc','double',
                'evaluate','false','integer','measure',
                'month','none','order','return','single','start','string',
                'table','true','var','year'),
                prefix=r'(?i)', suffix=r'\b'), Name.Builtin), # Keyword

            (r':=|[-+*\/=^]', Operator),
            (r'\b(IN|NOT)\b', Operator.Word),
            (r'"', String, 'string'), #StringLiteral
            (r"'(?:[^']|'')*'(?!')(?:\[[ \w]+\])?|\w+\[[ \w]+\]",
                Name.Attribute),	# Column reference
            (r"\[[ \w]+\]", Name.Attribute), #Measure reference
            (r'\b\d+\.?\d*|\B\.\d+\b', Number),# Number
            (r'[\[\](){}`,.]', Punctuation), #Parenthesis
            (r'.*\n', Text),

        ],
        'multiline-comments': [
            (r'/\*', Comment.Multiline, 'multiline-comments'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[^/*]+', Comment.Multiline),
            (r'[/*]', Comment.Multiline)
        ],
        'string': [
            (r'""', String.Escape),
            (r'"', String, '#pop'),
            (r'[^"]+', String),
        ]
    }
