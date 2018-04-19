# -*- coding: utf-8 -*-
"""
    pygments.lexers.mosel
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for the mosel language.
    http://www.fico.com/en/products/fico-xpress-optimization

"""

from pygments.lexer import RegexLexer, bygroups, using, this, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

__all__ = ['MoselLexer']


class MoselLexer(RegexLexer):
    name = 'Mosel'
    aliases = ['mosel']
    filenames = ['*.mos']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text.Whitespace),
            (r'!.*?\n', Comment.Single),
            (r'\(!(.|\n)*?!\)', Comment.Multiline),
            (words((
                'and', 'array', 'as',
                'boolean', 'break',
                'case', 'count', 'counter',
                'declarations', 'div', 'do', 'dynamic',
                'elif', 'else', 'end-', 'end', 'evaluation', 'exit',
                'false', 'forall', 'forward', 'from', 'function',
                'if', 'imports', 'in', 'include', 'initialisations', 'initializations', 'integer', 'inter', 'is_binary', 'is_continuous', 'is_free', 'is_integer', 'is_partint', 'is_semcont', 'is_semint', 'is_sos1', 'is_sos2',
                'linctr', 'list',
                'max', 'min', 'mod', 'model', 'mpvar',
                'next', 'not', 'of', 'options', 'or',
                'package', 'parameters', 'procedure',
                'public', 'prod', 'range', 'real', 'record', 'repeat', 'requirements',
                'set', 'string', 'sum',
                'then', 'to', 'true',
                'union', 'until', 'uses',
                'version',
                'while', 'with'),  prefix=r'\b', suffix=r'\b'), Keyword.Builtin),
            (r'(integer|is_binary|real)', Keyword.Type),
            (r'(\+|\-|\*|/|=|<=|>=|==|\||\^|<|>|<>|\.\.|:=|:)', Operator),
            (r'[()\[\]{},;:]+', Punctuation),
            (words((
                # core functions
                'abs', 'arctan', 'assert',
                'bitflip', 'bitneg', 'bitset', 'bitshift', 'bittest', 'bitval',
                'ceil', 'cos', 'create', 'currentdate', 'currenttime', 'cuthead', 'cuttail',
                'delcell', 'exists', 'exit', 'exp', 'exportprob',
                'fclose', 'fflush', 'finalize', 'findfirst', 'findlast', 'floor', 'fopen', 'fselect', 'fskipline',
                'getact', 'getcoeff', 'getcoeffs', 'getdual', 'getfid', 'getfirst', 'gethead', 'getfname', 'getlast', 'getobjval', 'getparam', 'getrcost', 'getreadcnt', 'getreverse', 'getsize', 'getslack', 'getsol', 'gettail', 'gettype', 'getvars',
                'iseof', 'ishidden', 'isodd', 'ln', 'log',
                'makesos1', 'makesos2', 'maxlist', 'minlist',
                'publish',
                'random', 'read', 'readln', 'reset', 'reverse', 'round',
                'setcoeff', 'sethidden', 'setioerr', 'setname', 'setparam', 'setrandseed', 'settype', 'sin', 'splithead', 'splittail', 'sqrt', 'strfmt', 'substr',
                'timestamp',
                'unpublish',
                'write', 'writeln'

                # mmxpress_functions
                'addmipsol',
                'basisstability',
                'calcsolinfo', 'clearmipdir', 'clearmodcut', 'command', 'copysoltoinit',
                'defdelayedrows', 'defsecurevecs',
                'estimatemarginals',
                'fixglobal',
                'getbstat', 'getdualray', 'getiis', 'getiissense', 'getiistype', 'getinfcause', 'getinfeas', 'getlb', 'getloadedlinctrs', 'getloadedmpvars', 'getname', 'getprimalray', 'getprobstat', 'getrange', 'getsensrng', 'getsize', 'getsol', 'getub', 'getvars',
                'implies', 'indicator', 'isiisvalid', 'isintegral', 'loadbasis',
                'loadmipsol', 'loadprob',
                'maximize', 'minimize',
                'postsolve',
                'readbasis', 'readdirs', 'readsol', 'refinemipsol', 'rejectintsol', 'repairinfeas', 'resetbasis', 'resetiis', 'resetsol',
                'savebasis', 'savemipsol', 'savesol', 'savestate', 'selectsol', 'setbstat', 'setcallback', 'setcbcutoff', 'setgndata', 'setlb', 'setmipdir', 'setmodcut', 'setsol', 'setub', 'setucbdata', 'stopoptimize',
                'unloadprob',
                'writebasis', 'writedirs', 'writeprob', 'writesol',
                'xor',

                # mmsystem_functions
                'addmonths',
                'copytext', 'cuttext',
                'deltext',
                'endswith', 'expandpath',
                'fcopy', 'fdelete', 'findfiles', 'findtext', 'fmove',
                'getasnumber', 'getchar', 'getcwd', 'getdate', 'getday', 'getdaynum', 'getdays', 'getdirsep',
                'getendparse', 'setendparse',
                'getenv', 'getfsize', 'getfstat', 'getftime', 'gethour', 'getminute', 'getmonth', 'getmsec', 'getpathsep',
                'getqtype', 'setqtype',
                'getsecond',
                'getsepchar', 'setsepchar',
                'getsize',
                'getstart', 'setstart',
                'getsucc', 'setsucc',
                'getsysinfo', 'getsysstat', 'gettime',
                'gettmpdir',
                'gettrim', 'settrim',
                'getweekday', 'getyear',
                'inserttext', 'isvalid',
                'makedir', 'makepath', 'newtar',
                'newzip', 'nextfield',
                'openpipe',
                'parseextn', 'parseint', 'parsereal', 'parsetext', 'pastetext', 'pathmatch', 'pathsplit',
                'qsort', 'quote',
                'readtextline', 'regmatch', 'regreplace', 'removedir', 'removefiles',
                'setchar', 'setdate', 'setday', 'setenv', 'sethour',
                'setminute', 'setmonth', 'setmsec', 'setsecond', 'settime', 'setyear', 'sleep', 'startswith', 'system',
                'tarlist', 'textfmt', 'tolower', 'toupper', 'trim',
                'untar', 'unzip',
                'ziplist',

                 # mmsystem_parameters
                'datefmt', 'datetimefmt', 'monthnames', 'sys_endparse', 'sys_fillchar', 'sys_pid', 'sys_qtype', 'sys_regcache', 'sys_sepchar',
    
                # mmjobs_instance_mgmt_functions
                'clearaliases', 'connect',
                'disconnect',
                'findxsrvs',
                'getaliases', 'getbanner', 'gethostalias',
                'sethostalias',

                # mmjobs_model_mgmt_functions
                'compile',
                'detach',
                'getannidents', 'getannotations', 'getexitcode', 'getgid', 'getid', 'getnode', 'getrmtid', 'getstatus', 'getuid',
                'load',
                'reset', 'resetmodpar', 'run',
                'setcontrol', 'setdefstream', 'setmodpar', 'setworkdir', 'stop',
                'unload',
    
                # mmjobs_synchornization_functions
                'dropnextevent',
                'getclass', 'getfromgid', 'getfromid', 'getfromuid', 'getnextevent', 'getvalue',
                'isqueueempty',
                'nullevent',
                'peeknextevent',
                'send', 'setgid', 'setuid',
                'wait', 'waitfor',

                # 'mmjobs_parameters', 
                'conntmpl', 'defaultnode', 'fsrvdelay', 'fsrvnbiter', 'fsrvport', 'jobid', 'keepalive', 'nodenumber', 'parentnumber'
            ),  prefix=r'\b', suffix=r'\b'), Name.Function),
            (r'(\d+\.(?!\.)\d*|\.(?!.)\d+)([eE][+-]?\d+)?', Number.Float),
            (r'\d+([eE][+-]?\d+)?', Number.Integer),
            (r'[+-]?Infinity', Number.Integer),
            (r'0[xX][0-9a-fA-F]+', Number),
            (r'\"', String.Double, 'double_quote'),
            (r'\'', String.Single, 'single_quote'),
            (r'(\w+|(\.(?!\.)))', Text)
        ],
        'single_quote' : [
            (r'\'', String.Single, '#pop'),
            (r'(.|\n)', String.Single)
        ],
        'double_quote' : [
            (r'(\\"|\\[0-7]{1,3}\D|\\[abfnrtv]|\\\\)', String.Escape),
            (r'\"', String.Double, '#pop'),
            (r'(.|\n)', String.Double)
        ]
    }
