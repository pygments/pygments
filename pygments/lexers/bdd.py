"""
    pygments.lexers.bdd
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for BDD language.

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, bygroups
from pygments.token import Comment, Keyword, Name, String, Number, Generic, Text, Punctuation

__all__ = ['BddLexer']

class BddLexer(RegexLexer):
    """
    Lexer for Bdd(Behavior-driven development).
    .. versionadded:: 2.10
    """

    name = 'Bdd'
    aliases = ['bdd']
    filenames = ['*.feature']
    mimetypes = ['text/x-bdd']
    
    # pre-defined keywords
    feature_keywords = '^()(:)(.*)$'
    feature_element_keywords = '^(\\s*)()(:)(.*)$'
    examples_keywords = '^(\\s*)()(:)(.*)$'
    step_keywords = '^(\\s*)(Given|When|Then|Add|And|Feature|Scenario Outline|Scenario|Background|Examples|But)'

    tokens = {
        'comments': [
            (r'^\s*#.*$', Comment),
        ],
        'feature_elements': [
            (step_keywords, Keyword, "step_content_stack"),
            include('comments'),
            (r"(\s)", Text),
        ],
        'feature_elements_on_stack': [
            (step_keywords, Keyword, "#pop:2"),
            include('comments'),
            (r"(\s)", Text),
        ],
        'examples_table': [
            (r"\s+\|", Keyword, 'examples_table_header'),
            include('comments'),
            (r"(\s)", Text),
        ],
        'examples_table_header': [
            (r"\s+\|\s*$", Keyword, "#pop:2"),
            include('comments'),
            (r"\\\|", Punctuation),
            (r"\s*\|", Punctuation),
            (r"[^|]", Name.Variable),
        ],
        'scenario_sections_on_stack': [
            (feature_element_keywords,
             bygroups(Text, Keyword, Keyword, Text),
             "feature_elements_on_stack"),
        ],
        'narrative': [
            include('scenario_sections_on_stack'),
            include('comments'),
            (r"(\s|.)", Text),
        ],
        'table_vars': [
            (r'(<|>)', Punctuation),
            (r'(\|)', Punctuation),
            (r'(:)', Punctuation),
            (r'((?<=\<)[^\\>]+(?=\>))', Name.Variable),
            (r'"([^\"]*)"', String),
        ],
        'numbers': [
            (r'(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number),
        ],
        'string': [
            include('table_vars'),
            (r'(\s|.)', Text),
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
            (r'(\s|.)', Text),
        ],
        'table_content': [
            (r"\s+\|\s*$", Keyword, "#pop"),
            include('comments'),
            (r"\\\|", Punctuation),
            (r"\s*\|", Punctuation),
            include('string'),
        ],
        'double_string': [
            (r'"', Text, "#pop"),
            include('string'),
        ],
        'root': [
            (r'\n', Text),
            include('comments'),
            (r'"""', Keyword, "py_string"),
            include('table_vars'),
            include('numbers'),
            (r'(\s*)(@[^@\r\n\t\f\v]+)', bygroups(Text, Name.Label)),
            (step_keywords, bygroups(Text, Keyword),
             'step_content_root'),
            (feature_keywords, bygroups(Keyword, Text),
             'narrative'),
            (feature_element_keywords,
             bygroups(Text, Keyword),
             'feature_elements'),
            (examples_keywords,
             bygroups(Text, Keyword),
             'examples_table'),
            (r'(\s|.)', Text),
        ]
    }
