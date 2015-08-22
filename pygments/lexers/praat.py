# -*- coding: utf-8 -*-
"""
    pygments.lexers.praat
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for Praat

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, words, bygroups, default, include
from pygments.token import *

__all__ = ['PraatLexer']


class PraatLexer(RegexLexer):
    """
    For `Praat <http://www.praat.org>` scripts.
    """

    name = 'Praat'
    aliases = ['praat']
    filenames = ['*.praat', '*.proc', '*.psc']

    tokens = {
        'root': [
            (r'\A\#!.+?$', Comment.Hashbang),
            (r'\#.*?$', Comment.Single),
            (r';[^\n]*', Comment.Single),
            (r'^\.\.\.', Text),
            (r'^\s+\.\.\.', Text),
            (r'\s+', Text),
            (r'\bprocedure\b', Keyword, 'procedure-definition'),
            (r'\bcall\b', Keyword, 'procedure-call'),
            (r'@', Name.Function, 'procedure-call'),
            (words((
                'writeInfo', 'writeInfoLine', 'appendInfo', 'appendInfoLine', 
                'info$', 'writeFile', 'writeFileLine', 'appendFile', 
                'appendFileLine', 'abs', 'round', 'floor', 'ceiling', 'min', 
                'max', 'imin', 'imax', 'sqrt', 'sin', 'cos', 'tan', 'arcsin', 
                'arccos', 'arctan', 'arctan2', 'sinc', 'sincpi', 'exp', 'ln', 
                'lnBeta', 'lnGamma', 'log10', 'log2', 'sinh', 'cosh', 'tanh', 
                'arcsinh', 'arccosh', 'arctanh', 'sigmoid', 'invSigmoid', 
                'erf', 'erfc', 'randomUniform', 'randomInteger', 'randomGauss', 
                'randomPoisson', 'randomBinomial)', 'gaussP', 'gaussQ', 
                'invGaussQ', 'incompleteGammaP', 'incompleteBeta', 
                'chiSquareP', 'chiSquareQ', 'invChiSquareQ', 'studentP', 
                'studentQ', 'invStudentQ', 'fisherP', 'fisherQ', 'invFisherQ', 
                'binomialP', 'binomialQ', 'invBinomialP', 'invBinomialQ', 
                'hertzToBark', 'barkToHerz', 'hertzToMel', 'melToHertz', 
                'hertzToSemitones', 'semitonesToHerz', 'erb', 'hertzToErb', 
                'erbToHertz', 'phonToDifferenceLimens', 'string$',
                'differenceLimensToPhon', 'soundPressureToPhon', 'beta', 
                'beta2', 'besselI', 'besselK', 'numberOfColumns', 'number',
                'numberOfRows', 'selected$', 'selected', 'numberOfSelected', 
                'variableExists', 'index', 'rindex', 'startsWith', 'endsWith', 
                'index_regex', 'rindex_regex', 'replace_regex$', 'length', 
                'extractWord$', 'extractLine$', 'extractNumber', 'left$',  
                'right$', 'mid$', 'replace$', 'date$', 'fixed$', 'percent$', 
                'zero#', 'linear#', 'randomUniform#', 'randomInteger#', 
                'randomGauss#', 'beginPause', 'endPause', 'do', 'do$', 'editor', 
                'demoShow', 'demoWindowTitle', 'demoInput', 'demoWaitForInput', 
                'demoClicked', 'demoClickedIn', 'demoX', 'demoY', 
                'demoKeyPressed', 'demoKey$', 'demoExtraControlKeyPressed', 
                'demoShiftKeyPressed', 'demoCommandKeyPressed', 
                'demoOptionKeyPressed', 'environment$', 'chooseReadFile$', 
                'chooseDirectory$', 'createDirectory', 'fileReadable', 
                'deleteFile', 'selectObject', 'removeObject', 'plusObject', 
                'minusObject', 'runScript', 'exitScript', 'beginSendPraat', 
                'endSendPraat', 'objectsAreIdentical',
                # These functions belong to the new-style forms, but so far they
                # only exist for pause menus, not initial forms. Placed here
                # while a better solution comes along.
                'comment', 'natural', 'real', 'positive', 'word', 'sentence'
                'text', 'boolean', 'choice', 'option', 'optionMenu'),
                prefix=r'\b', suffix=r'(?=\s*[:(])'),
                Name.Function, 'function'),
            (words((
                'if', 'then', 'else', 'elsif', 'elif', 'endif', 'fi', 'for',
                'from', 'to', 'endfor', 'endproc', 'while', 'endwhile', 'repeat',
                'until', 'select', 'plus', 'minus', 'demo', 'assert',
                'asserterror', 'stopwatch', 'nocheck', 'nowarn', 'noprogress',
                'editor', 'endeditor', 'clearinfo'),
                prefix=r'\b', suffix=r'\b'),
                Keyword),
            (r'(\bform\b)(\s+)([^\n]+)', bygroups(Keyword, Text, String), 'old-form'),
            (r'(print(?:line|tab)?|echo|exit|pause|send(?:praat|socket)|include|execute|system(?:_nocheck)?)(\s+)',
                bygroups(Keyword, Text), 'string-unquoted'),
            (r'(goto|label)(\s+)(\w+)', bygroups(Keyword, Text, Name.Label)),  
            include('operator'),
            include('variable-name'),
            include('number'),
            (r'"', String, 'string'),
            (words((
                "Activation","AffineTransform","AmplitudeTier","Art","Artword",
                "Autosegment","BarkFilter","CCA","Categories","Cepstrum",
                "Cepstrumc","ChebyshevSeries","ClassificationTable","Cochleagram",
                "Collection","Configuration","Confusion","ContingencyTable",
                "Corpus","Correlation","Covariance","CrossCorrelationTable",
                "CrossCorrelationTables","DTW","Diagonalizer","Discriminant",
                "Dissimilarity","Distance","Distributions","DurationTier","EEG",
                "ERP","ERPTier","Eigen","Excitation","Excitations",
                "ExperimentMFC","FFNet","FeatureWeights","Formant",
                "FormantFilter","FormantGrid","FormantPoint","FormantTier",
                "GaussianMixture","HMM","HMM_Observation",
                "HMM_ObservationSequence","HMM_State","HMM_StateSequence",
                "Harmonicity","ISpline","Index","Intensity","IntensityTier",
                "IntervalTier","KNN","KlattGrid","KlattTable","LFCC","LPC",
                "Label","LegendreSeries","LinearRegression","LogisticRegression",
                "LongSound","Ltas","MFCC","MSpline","ManPages","Manipulation",
                "Matrix","MelFilter","MixingMatrix","Movie","Network","OTGrammar",
                "OTHistory","OTMulti","PCA","PairDistribution","ParamCurve",
                "Pattern","Permutation","Pitch","PitchTier","PointProcess",
                "Polygon","Polynomial","Procrustes","RealPoint","RealTier",
                "ResultsMFC","Roots","SPINET","SSCP","SVD","Salience",
                "ScalarProduct","Similarity","SimpleString","SortedSetOfString",
                "Sound","Speaker","Spectrogram","Spectrum","SpectrumTier",
                "SpeechSynthesizer","SpellingChecker","Strings","StringsIndex",
                "Table","TableOfReal","TextGrid","TextInterval","TextPoint",
                "TextTier","Tier","Transition","VocalTract","Weight","WordList"),
                suffix=r'(?=\s+\S+\n)'),
                Name.Class, 'string-unquoted'),
            (r'\bObject_', Name.Builtin),
            (r'(\b[A-Z][^.:\n"]+\.{3})', Keyword, 'old-arguments'),
            (r'\b[A-Z][^.:\n"]+:', Keyword, 'comma-list'),
            (r'\b[A-Z][^\n]+', Keyword),
            (r'(\.{3}|\)|\(|,|\$)', Text),
        ],
        'old-arguments': [
            include('variable-name'),
            include('operator'),
            include('number'),
            (r'"', String, 'string'),
            (r'^', String, '#pop'),
            (r'[A-Z]', Text),
            (r'\s', Text),
        ],
        'function': [
            (r'\s+', Text),
            (r':', Text, ('comma-list', '#pop')),
            (r'\s*\(', Text, ('comma-list', '#pop')),
            (r'\)', Text, '#pop'),
            default('#pop'),
        ],
        'procedure-call': [
            (r'\s+', Text),
            (r'[\w.]+', Name.Function),
            (r':', Text, '#pop'),
            (r'\s*\(', Text, '#pop'),
            default('#pop'),
        ],
        'procedure-definition': [
            (r'\s', Text),
            (r'([\w.]+)(\s*?[(:])', bygroups(Name.Function, Text), '#pop'),
            (r'([\w.]+)([^\n]*)', bygroups(Name.Function, Text), '#pop'),
            default('#pop'),
        ],
        'comma-list': [
            (r'\s+', Text),
            (r',', Text, '#push'),
            (r'(\)|^)', Text, '#pop'),
            default('#pop'),
        ],
        'number': [
            (r'[+-]?\d+', Number),
            (r'(?<=\d)(\.\d+)', Number),
            (r'[eE][+-]?\d+', Number),
        ],
        'operator': [
            (r'([+/*<>=!-]=?|[%^|]|<>)', Operator),
            (r'\b(and|or|not)\b', Operator.Word),
        ],
        'variable-name': [
            (words((
                "macintosh","windows","unix","praatVersion","praatVersion$","pi",
                "undefined","newline$","tab$","shellDirectory$","homeDirectory$",
                "preferencesDirectory$","temporaryDirectory$","defaultDirectory$"),
                prefix=r'\b', suffix=r'\b',),
                Name.Builtin),
            (r"\.?[a-z][a-zA-Z0-9_.]*\$?", Text),
            (r"\[", Text, '#push'),
            (r"'(?=.*')", String.Interpol, 'string-interpolated'),
            (r"\]", Text, '#pop'),
        ],
        'string-interpolated': [
            (r'\.?[_a-z][a-zA-Z0-9_.]*(?:\$|#|:[0-9]+)?', String.Interpol),
            (r"'", String.Interpol, '#pop'),
        ],
        'string-unquoted': [
            (r'\s', Text),
            (r"'(?=.*')", String.Interpol, ('#pop', 'string-interpolated')),
            (r"'", String),
            (r'\S+(?=\n)', String, '#pop'),
            default('#pop'),
        ],
        'string': [
            (r"[^'\"]+", String),
            (r"'(?=.*')", String.Interpol, 'string-interpolated'),
            (r"'", String),
            (r'"', String, '#pop'),
        ],
        'old-form': [
            (r'\s+', Text),
            (r'(optionmenu|choice)(\s+\S+:\s+)([0-9]+)',
              bygroups(Keyword, Text, Number)),
            (r'(option|button)(\s+)([+-]?\d+(?:(?:\.\d*)?(?:[eE][+-]?\d+)?)?\b)',
              bygroups(Keyword, Text, Number)),
            (r'(option|button)(\s+)',
              bygroups(Keyword, Text), 'string-unquoted'),
            (r'(sentence|text)(\s+\S+)',
              bygroups(Keyword, Text), 'string-unquoted'),
            (r'(word)(\s+\S+\s*)(\S+)?(\s.*)?',
              bygroups(Keyword, Text, String, Text)),
            (r'(boolean)(\s+\S+\s*)(0|1|"?(?:yes|no)"?)',
              bygroups(Keyword, Text, Name.Variable)),
            (r'(real|natural|positive|integer)(\s+\S+\s*)([+-]?\d+(?:(?:\.\d*)?(?:[eE][+-]?\d+)?)?\b)', 
              bygroups(Keyword, Text, Number)),
            (r'(comment)(\s+)(.*)', 
              bygroups(Keyword, Text, String)),
            (r'(endform)', 
              bygroups(Keyword), "#pop"),
        ],
    }
