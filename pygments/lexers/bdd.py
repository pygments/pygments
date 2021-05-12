"""
    pygments.lexers.bdd
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for BDD language.

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, bygroups
from pygments.token import Comment, Keyword, Name, String, Number, Generic, Text

__all__ = ['BddLexer']

class BddLexer(RegexLexer):
    name = 'BDD'
    aliases = ['bdd']
    filenames = ['*.feature']
    mimetypes = ['text/x-bdd']

    feature_keywords = '^()(:)(.*)$'
    feature_element_keywords = '^(\\s*)()(:)(.*)$'
    examples_keywords = '^(\\s*)()(:)(.*)$'
    step_keywords = '^(\\s*)(Given|When|Then|Add|And|Feature|Scenario Outline|Scenario|Background|Examples|But|\\* )'

    tokens = {
        'comments': [
            (r'^\s*#.*$', Comment),
        ],
        'feature_elements': [
            (step_keywords, Keyword, "step_content_stack"),
            include('comments'),
            (r"(\s|.)", Name.Function),
        ],
        'feature_elements_on_stack': [
            (step_keywords, Keyword, "#pop:2"),
            include('comments'),
            (r"(\s|.)", Name.Function),
        ],
        'examples_table': [
            (r"\s+\|", Keyword, 'examples_table_header'),
            include('comments'),
            (r"(\s|.)", Name.Function),
        ],
        'examples_table_header': [
            (r"\s+\|\s*$", Keyword, "#pop:2"),
            include('comments'),
            (r"\\\|", Name.Variable),
            (r"\s*\|", Keyword),
            (r"[^|]", Name.Variable),
        ],
        'scenario_sections_on_stack': [
            (feature_element_keywords,
             bygroups(Name.Function, Keyword, Keyword, Name.Function),
             "feature_elements_on_stack"),
        ],
        'narrative': [
            include('scenario_sections_on_stack'),
            include('comments'),
            (r"(\s|.)", Name.Function),
        ],
        'table_vars': [
            (r'(<|>)', Keyword.Type),
            (r'(\|)', Keyword.Type),
            (r'(:)', Keyword.Type),

            (r'((?<=\<)[^\\>]+(?=\>))', Name.Variable),
            #(r'(["][\w\s]+["])', Name.Exception),
            #Quotes recognition
            (r'"([^\"]*)"', Name.Exception),
        ],
        'numbers': [
            (r'(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', String),
        ],
        'string': [
            include('table_vars'),
            (r'(\s|.)', String),
        ],
        'py_string': [
            (r'"""', Keyword, "#pop"),
            include('string'),
        ],
        'step_content_root': [
            (r"$", Keyword, "#pop"),
            include('step_content'),
        ],
        'step_content_stack': [
            (r"$", Keyword, "#pop:2"),
            include('step_content'),
        ],
        'step_content': [
            include('table_vars'),
            include('numbers'),
            include('comments'),
            (r'(\s|.)', Name.Function),
        ],
        'table_content': [
            (r"\s+\|\s*$", Keyword, "#pop"),
            include('comments'),
            (r"\\\|", String),
            (r"\s*\|", Keyword),
            include('string'),
        ],
        'double_string': [
            (r'"', Name.Function, "#pop"),
            include('string'),
        ],
        'root': [
            (r'\n', Name.Function),
            include('comments'),
            (r'"""', Keyword, "py_string"),
            include('table_vars'),
            include('numbers'),
            (r'(\s*)(@[^@\r\n\t\f\v]+)', bygroups(Name.Function, Name.Label)),
            (step_keywords, bygroups(Name.Function, Keyword),
             'step_content_root'),
            (feature_keywords, bygroups(Keyword, Keyword, Name.Function),
             'narrative'),
            (feature_element_keywords,
             bygroups(Name.Function, Keyword, Keyword, Name.Function),
             'feature_elements'),
            (examples_keywords,
             bygroups(Name.Function, Keyword, Keyword, Name.Function),
             'examples_table'),
            (r'(\s|.)', Name.Function),
        ]
    }
