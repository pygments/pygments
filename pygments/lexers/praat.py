# -*- coding: utf-8 -*-
"""
    pygments.lexers.praat
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for Praat

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
#from __future__ import print_function

import re

from pygments.lexer import ExtendedRegexLexer, words, bygroups, default, include
from pygments.token import *

__all__ = ['PraatLexer']


class PraatLexer(ExtendedRegexLexer):
    """
    For `Praat <http://www.praat.org>`_ scripts.
    """

    # The comma list is a comma-delimited list of arguments
    # Every time a comma is found, comma_list is recuersively put
    # back on the stack. To exit, we need to go all the way up to the
    # first caller and then pop again if that caller was not root.
    def exit_comma_list(lexer, match, ctx):
        yield match.start(), Text, match.group(0)
        ctx.pos = match.end()
        while ctx.stack:
            if ctx.stack[-1] == 'comma_list':
                ctx.stack.pop()
            elif ctx.stack[-1] != 'root':
                ctx.stack.pop()
            else:
                break

    name = 'Praat'
    aliases = ['praat']
    filenames = ['*.praat', '*.proc', '*.psc']

    keywords = [
        'if', 'then', 'else', 'elsif', 'elif', 'endif', 'fi', 'for', 'from', 'to', 'endfor', 'endproc',
        'while', 'endwhile', 'repeat', 'until', 'select', 'plus', 'minus', 'demo', 'assert', 'stopwatch',
        'nocheck', 'nowarn', 'noprogress', 'editor', 'endeditor', 'clearinfo'
    ]

    functions_string = [
        'backslashTrigraphsToUnicode', 'chooseDirectory', 'chooseReadFile',
        'chooseWriteFile', 'date', 'demoKey', 'do', 'environment', 'extractLine', 'extractWord',
        'fixed', 'info', 'left', 'mid', 'percent', 'readFile', 'replace', 'replace_regex', 'right',
        'selected', 'string', 'unicodeToBackslashTrigraphs',
    ]

    functions_numeric = [
        'abs', 'appendFile', 'appendFileLine', 'appendInfo', 'appendInfoLine', 'arccos', 'arccosh',
        'arcsin', 'arcsinh', 'arctan', 'arctan2', 'arctanh', 'barkToHertz', 'beginPause',
        'beginSendPraat', 'besselI', 'besselK', 'beta', 'beta2', 'binomialP', 'binomialQ', 'boolean',
        'ceiling', 'chiSquareP', 'chiSquareQ', 'choice', 'comment', 'cos', 'cosh', 'createDirectory',
        'deleteFile', 'demoClicked', 'demoClickedIn', 'demoCommandKeyPressed',
        'demoExtraControlKeyPressed', 'demoInput', 'demoKeyPressed',
        'demoOptionKeyPressed', 'demoShiftKeyPressed', 'demoShow', 'demoWaitForInput',
        'demoWindowTitle', 'demoX', 'demoY', 'differenceLimensToPhon', 'do', 'editor', 'endPause',
        'endSendPraat', 'endsWith', 'erb', 'erbToHertz', 'erf', 'erfc', 'exitScript', 'exp',
        'extractNumber', 'fileReadable', 'fisherP', 'fisherQ', 'floor', 'gaussP', 'gaussQ',
        'hertzToBark', 'hertzToErb', 'hertzToMel', 'hertzToSemitones', 'imax', 'imin',
        'incompleteBeta', 'incompleteGammaP', 'index', 'index_regex', 'invBinomialP',
        'invBinomialQ', 'invChiSquareQ', 'invFisherQ', 'invGaussQ', 'invSigmoid', 'invStudentQ',
        'length', 'ln', 'lnBeta', 'lnGamma', 'log10', 'log2', 'max', 'melToHertz', 'min', 'minusObject',
        'natural', 'number', 'numberOfColumns', 'numberOfRows', 'numberOfSelected',
        'objectsAreIdentical', 'option', 'optionMenu', 'pauseScript',
        'phonToDifferenceLimens', 'plusObject', 'positive', 'randomBinomial', 'randomGauss',
        'randomInteger', 'randomPoisson', 'randomUniform', 'real', 'readFile', 'removeObject',
        'rindex', 'rindex_regex', 'round', 'runScript', 'runSystem', 'runSystem_nocheck',
        'selectObject', 'selected', 'semitonesToHertz', 'sentencetext', 'sigmoid', 'sin', 'sinc',
        'sincpi', 'sinh', 'soundPressureToPhon', 'sqrt', 'startsWith', 'studentP', 'studentQ', 'tan',
        'tanh', 'variableExists', 'word', 'writeFile', 'writeFileLine', 'writeInfo',
        'writeInfoLine',
    ]

    functions_array = [
        'linear', 'randomGauss', 'randomInteger', 'randomUniform', 'zero',
    ]

    objects = [
        'Activation', 'AffineTransform', 'AmplitudeTier', 'Art', 'Artword', 'Autosegment',
        'BarkFilter', 'BarkSpectrogram', 'CCA', 'Categories', 'Cepstrogram', 'Cepstrum',
        'Cepstrumc', 'ChebyshevSeries', 'ClassificationTable', 'Cochleagram', 'Collection',
        'ComplexSpectrogram', 'Configuration', 'Confusion', 'ContingencyTable', 'Corpus',
        'Correlation', 'Covariance', 'CrossCorrelationTable', 'CrossCorrelationTables', 'DTW',
        'DataModeler', 'Diagonalizer', 'Discriminant', 'Dissimilarity', 'Distance',
        'Distributions', 'DurationTier', 'EEG', 'ERP', 'ERPTier', 'EditCostsTable',
        'EditDistanceTable', 'Eigen', 'Excitation', 'Excitations', 'ExperimentMFC', 'FFNet',
        'FeatureWeights', 'FileInMemory', 'FilesInMemory', 'Formant', 'FormantFilter',
        'FormantGrid', 'FormantModeler', 'FormantPoint', 'FormantTier', 'GaussianMixture', 'HMM',
        'HMM_Observation', 'HMM_ObservationSequence', 'HMM_State', 'HMM_StateSequence',
        'Harmonicity', 'ISpline', 'Index', 'Intensity', 'IntensityTier', 'IntervalTier', 'KNN',
        'KlattGrid', 'KlattTable', 'LFCC', 'LPC', 'Label', 'LegendreSeries', 'LinearRegression',
        'LogisticRegression', 'LongSound', 'Ltas', 'MFCC', 'MSpline', 'ManPages', 'Manipulation',
        'Matrix', 'MelFilter', 'MelSpectrogram', 'MixingMatrix', 'Movie', 'Network', 'OTGrammar',
        'OTHistory', 'OTMulti', 'PCA', 'PairDistribution', 'ParamCurve', 'Pattern', 'Permutation',
        'Photo', 'Pitch', 'PitchModeler', 'PitchTier', 'PointProcess', 'Polygon', 'Polynomial',
        'PowerCepstrogram', 'PowerCepstrum', 'Procrustes', 'RealPoint', 'RealTier', 'ResultsMFC',
        'Roots', 'SPINET', 'SSCP', 'SVD', 'Salience', 'ScalarProduct', 'Similarity', 'SimpleString',
        'SortedSetOfString', 'Sound', 'Speaker', 'Spectrogram', 'Spectrum', 'SpectrumTier',
        'SpeechSynthesizer', 'SpellingChecker', 'Strings', 'StringsIndex', 'Table',
        'TableOfReal', 'TextGrid', 'TextInterval', 'TextPoint', 'TextTier', 'Tier', 'Transition',
        'VocalTract', 'VocalTractTier', 'Weight', 'WordList',
    ]

    variables_numeric = [
        'macintosh', 'windows', 'unix', 'praatVersion', 'pi', 'e', 'undefined',
    ]

    variables_string = [
        'praatVersion', 'tab', 'shellDirectory', 'homeDirectory',
        'preferencesDirectory', 'newline', 'temporaryDirectory',
        'defaultDirectory',
    ]

    tokens = {
        'root': [
            (r'(\s+)(#.*?$)',  bygroups(Text, Comment.Single)),
            (r'^#.*?$',        Comment.Single),
            (r';[^\n]*',       Comment.Single),
            (r'\s+',           Text),

            (r'\bprocedure\b', Keyword,       'procedure_definition'),
            (r'\bcall\b',      Keyword,       'procedure_call'),
            (r'@',             Name.Function, 'procedure_call'),

            include('function_call'),

            (words(keywords, suffix=r'\b'), Keyword),

            (r'(\bform\b)(\s+)([^\n]+)',
                bygroups(Keyword, Text, String), 'old_form'),

            (r'(print(?:line|tab)?|echo|exit|asserterror|pause|send(?:praat|socket)|include|execute|system(?:_nocheck)?)(\s+)',
                bygroups(Keyword, Text), 'string_unquoted'),

            (r'(goto|label)(\s+)(\w+)', bygroups(Keyword, Text, Name.Label)),

            include('variable_name'),
            include('number'),

            (r'"', String, 'string'),

            (words((objects), suffix=r'(?=\s+\S+\n)'), Name.Class, 'string_unquoted'),

            (r'(\b[A-Z][^.:\n"]+\.{3})', Keyword, 'old_arguments'),
            (r'\b[A-Z][^.:\n"]+:',       Keyword, 'comma_list'),
            (r'\b[A-Z][^\n]+',           Keyword),
            (r'(\.{3}|\)|\(|,|\$)',      Text),
        ],
        'procedure_call': [
            (r'\s+', Text),
            (r'([\w.]+)(:|\s*\()',
                bygroups(Name.Function, Text), '#pop'),
            (r'([\w.]+)', Name.Function, ('#pop', 'old_arguments')),
        ],
        'procedure_definition': [
            (r'\s', Text),
            (r'([\w.]+)(\s*?[(:])',
                bygroups(Name.Function, Text), '#pop'),
            (r'([\w.]+)([^\n]*)',
                bygroups(Name.Function, Text), '#pop'),
        ],
        'function_call': [
            (words(functions_string , suffix=r'\$(?=\s*[:(])'), Name.Function, 'function'),
            (words(functions_array  , suffix=r'#(?=\s*[:(])'),  Name.Function, 'function'),
            (words(functions_numeric, suffix=r'(?=\s*[:(])'),   Name.Function, 'function'),
        ],
        'function': [
            (r'\s+',   Text),
            (r':',     Text, 'comma_list'),
            (r'\s*\(', Text, ('#pop', 'comma_list')),
            (r'\)',    Text, '#pop'),
        ],
        'comma_list': [
            (r'\n\s*\.{3}', Text),

            (r'\n', exit_comma_list),

            (r'\s+', Text),
            (r'"',   String, 'string'),
            (r'\b(if|then|else|fi|endif)\b', Keyword),

            include('function_call'),
            include('variable_name'),
            include('operator'),
            include('number'),

            (r',', Text, 'comma_list'),
            (r'(\)|\]|^)', Text, '#pop'),
        ],
        'old_arguments': [
            (r'\n', Text, '#pop'),

            include('variable_name'),
            include('operator'),
            include('number'),

            (r'"',     String, 'string'),
            (r'[A-Z]', Text),
            (r'\s',    Text),
        ],
        'number': [
            (r'\b\d+(\.\d*)?([eE][-+]?\d+)?%?', Number),
        ],
        'object_attributes': [
            (r'\.?(n(col|row)|[xy]min|[xy]max|[nd][xy])\b', Name.Builtin, '#pop'),
            (r'(\.?(?:col|row)\$)(\[)',
                bygroups(Name.Builtin, Text), 'variable_name'),
            (r'(\$?)(\[)',
                bygroups(Name.Builtin, Text), 'comma_list'),
        ],
        'variable_name': [
            include('operator'),
            include('number'),

            (r'\b_', Generic.Error),

            (words(variables_string,  suffix=r'\$'), Name.Variable.Global),
            (words(variables_numeric, suffix=r'\b'), Name.Variable.Global),

            (r'\bObject_\w+', Name.Builtin, 'object_attributes'),

            (r"\b(Object_)(')",
                bygroups(Name.Builtin, String.Interpol),
                ('object_attributes', 'string_interpolated')),

            (r'\.?[a-z][a-zA-Z0-9_.]*(\$|#)?', Text),
            (r'\[',                            Text,            'comma_list'),
            (r"'(?=.*')",                      String.Interpol, 'string_interpolated'),
            (r'\]',                            Text),
        ],
        'operator': [
            (r'([+\/*<>=!-]=?|[&*|][&*|]?|\^|<>)', Operator),
            (r'\b(and|or|not|div|mod)\b',          Operator.Word),
        ],
        'string_interpolated': [
            (r'\.?[_a-z][a-zA-Z0-9_.]*(?:\$|#|:[0-9]+)?', String.Interpol),
            (r"'",          String.Interpol, '#pop'),
        ],
        'string_unquoted': [
            (r'\n\s*\.{3}', Text),
            (r'\n',         Text,            '#pop'),
            (r'\s',         Text),
            (r"'(?=.*')",   String.Interpol, 'string_interpolated'),
            (r"'",          String),
            (r"[^'\n]+",    String),
        ],
        'string': [
            (r'\n\s*\.{3}', Text),
            (r'"',          String,          '#pop'),
            (r"'(?=.*')",   String.Interpol, 'string_interpolated'),
            (r"'",          String),
            (r'[^\'"\n]+',  String),
        ],
        'old_form': [
            (r'\s+', Text),

            (r'(optionmenu|choice)([ \t]+\S+:[ \t]+)',
              bygroups(Keyword, Text), 'number'),

            (r'(option|button)([ \t]+)',
              bygroups(Keyword, Text), 'number'),

            (r'(option|button)([ \t]+)',
              bygroups(Keyword, Text), 'string_unquoted'),

            (r'(sentence|text)([ \t]+\S+)',
              bygroups(Keyword, Text), 'string_unquoted'),

            (r'(word)([ \t]+\S+[ \t]*)(\S+)?([ \t]+.*)?',
              bygroups(Keyword, Text, String, Generic.Error)),

            (r'(boolean)(\s+\S+\s*)(0|1|"?(?:yes|no)"?)',
              bygroups(Keyword, Text, Name.Variable)),

            # Ideally processing of the number would happend in the 'number'
            # but that doesn't seem to work
            (r'(real|natural|positive|integer)([ \t]+\S+[ \t]*)([+-]?\d+(\.\d*)?([eE][-+]?\d+)?%?)',
              bygroups(Keyword, Text, Operator, Number)),

            (r'(comment)(\s+)',
              bygroups(Keyword, Text), 'string_unquoted'),

            (r'\bendform\b', Keyword, '#pop'),
        ]
    }
