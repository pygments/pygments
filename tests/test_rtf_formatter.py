"""
    Pygments RTF formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from io import StringIO
import itertools
import re
import pytest

from pygments.formatters import RtfFormatter
from pygments.lexers import CppLexer, PythonLexer
from pygments.lexers.special import TextLexer
from pygments.style import _ansimap, Style
from pygments.token import Name, String, Token

foot = r'\par' '\n' r'}' + '\n'


def _escape(string):
    return string.replace("\n", r"\n")


def _build_message(*args, **kwargs):
    string = kwargs.get('string', None)
    t = _escape(kwargs.get('t', ''))
    expected = _escape(kwargs.get('expected', ''))
    result = _escape(kwargs.get('result', ''))

    if string is None:
        string = ("The expected output of '{t}'\n\n"
                  "\t\tShould be '{expected}'\n\n"
                  "\t\tActually outputs '{result}'\n\n"
                  "\t(WARNING: Partial Output of Result!)")

    end = -len(_escape(foot))
    start = end - len(expected)

    return string.format(t=t,
                         result = result[start:end],
                         expected = expected)


def format_rtf(t, options=None, lexer=TextLexer):
    if options is None:
        options = {}
    tokensource = lexer().get_tokens(t)
    fmt = RtfFormatter(**options)
    buf = StringIO()
    fmt.format(tokensource, buf)
    result = buf.getvalue()
    buf.close()
    return result


def extract_color_table(rtf):
    r"""
    Return af list of \redR\greenG\blueB; color definitions
    extracted from the input (the color table).
    """
    return re.findall((r"\\red[0-9]{1,3}"
                       r"\\green[0-9]{1,3}"
                       r"\\blue[0-9]{1,3};"),
                       rtf)


def test_rtf_header():
    t = ''
    result = format_rtf(t)
    expected = r'{\rtf1\ansi\uc0'
    msg = (f"RTF documents are expected to start with '{expected}'\n"
           f"\t\tStarts intead with '{result[:len(expected)]}'\n"
           "\t(WARNING: Partial Output of Result!)")
    assert result.startswith(expected), msg


def test_rtf_footer():
    t = ''
    result = format_rtf(t)
    expected = ''
    msg = (f"RTF documents are expected to end with '{_escape(expected)}'\n"
           f"\t\tEnds intead with '{_escape(result[-len(expected):])}'\n"
           "\t(WARNING: Partial Output of Result!)")
    assert result.endswith(expected+foot), msg


def test_ascii_characters():
    t = 'a b c d ~'
    result = format_rtf(t)
    expected = r'a b c d ~'
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected+foot), msg


def test_escape_characters():
    t = '\\ {{'
    result = format_rtf(t)
    expected = r'\\ \{\{'
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected+foot), msg


def test_single_characters():
    t = 'â € ¤ каждой'
    result = format_rtf(t)
    expected = (r'{\u226} {\u8364} {\u164} '
                r'{\u1082}{\u1072}{\u1078}{\u1076}{\u1086}{\u1081}')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected+foot), msg


def test_double_characters():
    t = 'က 힣 ↕ ↕︎ 鼖'
    result = format_rtf(t)
    expected = (r'{\u4096} {\u55203} {\u8597} '
                r'{\u8597}{\u65038} {\u55422}{\u56859}')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected+foot), msg


def test_linenos_all_defaults():
    t = 'line1\nline2\n'
    options = {}
    result = format_rtf(t, options)
    expected = (r'line1\par' + '\n'
                r'line2\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenos_text():
    t = 'line1\nline2\n'
    options = dict(linenos=True, lineno_padding=2)
    result = format_rtf(t, options)
    expected = (r'{\cf1 1  }line1\par' + '\n'
                r'{\cf1 2  }line2\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenos_newline_characters():
    t = r'line1\nline2' + '\n'
    options = dict(linenos=True, lineno_padding=2)
    result = format_rtf(t, options)
    expected = (r'{\cf1 1  }line1\\nline2\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenos_python():
    class TestStyle(Style):
        name = 'rtf_formatter_test'
        line_number_color = "#ff0000"
        styles = {Token: '', String: '#00ff00', Name: '#0000ff'}

    t = r's = "line1\nline2"' + '\n'
    options = dict(linenos=True, lineno_padding=2, style=TestStyle)
    result = format_rtf(t, options, PythonLexer)
    expected = (r'{\rtf1\ansi\uc0\deff0{\fonttbl{\f0\fmodern\fprq1\fcharset0;}}' + '\n'
                r'{\colortbl;' + '\n'
                r'\red255\green0\blue0;' + '\n'
                r'\red0\green255\blue0;' + '\n'
                r'\red0\green0\blue255;' + '\n'
                r'}' + '\n'
                r'\f0\sa0' + '\n'
                r'\dntblnsbdb' + '\n'
                r'{\cf1 1  }{\cf3 s} = {\cf2 "}{\cf2 line1}{\cf2 \\n}{\cf2 line2}{\cf2 "}\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenos_left_padding():
    t = '0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n'
    options = dict(linenos=True, lineno_padding=2)
    result = format_rtf(t, options)
    expected = (r'{\cf1  9  }8\par' + '\n'
                r'{\cf1 10  }9\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_lineno_padding():
    t = 'line1\nline2\n'
    options = dict(linenos=True, lineno_padding=3)
    result = format_rtf(t, options)
    expected = (r'{\cf1 1   }line1\par' + '\n'
                r'{\cf1 2   }line2\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenostep():
    t = 'line1\nline2\nline3\nline4\n'
    options = dict(linenos=True,
                   linenostep=2,
                   lineno_padding=2)
    result = format_rtf(t, options)
    expected = (r'{\cf1    }line1\par' + '\n'
                r'{\cf1 2  }line2\par' + '\n'
                r'{\cf1    }line3\par' + '\n'
                r'{\cf1 4  }line4\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenostart():
    t = 'line1\nline2\nline3\nline4\n'
    options = dict(linenos=True,
                   linenostart=3,
                   lineno_padding=2)
    result = format_rtf(t, options)
    expected = (r'{\cf1 3  }line1\par' + '\n'
                r'{\cf1 4  }line2\par' + '\n'
                r'{\cf1 5  }line3\par' + '\n'
                r'{\cf1 6  }line4\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenostart_left_padding():
    t = 'line1\nline2\nline3\n'
    options = dict(linenos=True,
                   linenostart=98,
                   lineno_padding=2)
    result = format_rtf(t, options)
    expected = (r'{\cf1  98  }line1\par' + '\n'
                r'{\cf1  99  }line2\par' + '\n'
                r'{\cf1 100  }line3\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenos_hl_lines():
    t = 'line1\nline2\nline3\n'
    options = dict(linenos=True,
                   hl_lines="2 3",
                   lineno_padding=2)
    result = format_rtf(t, options)
    expected = (r'{\cf1 1  }line1\par' + '\n'
                r'{\highlight2 {\cf1 2  }line2\par}' + '\n'
                r'{\highlight2 {\cf1 3  }line3\par}' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_linenos_off_hl_lines():
    t = 'line1\nline2\nline3\n'
    options = dict(linenos=False,
                   hl_lines="2 3")
    result = format_rtf(t, options)
    expected = (r'line1\par' + '\n'
                r'{\highlight1 line2\par}' + '\n'
                r'{\highlight1 line3\par}' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_hl_linenostart_no_lines_highlighted():
    t = 'line11\nline12\nline13\n'
    options = dict(linenos=False,
                   hl_lines="2 3",
                   hl_linenostart=True,
                   linenostart=11)
    result = format_rtf(t, options)
    expected = (r'line11\par' + '\n'
                r'line12\par' + '\n'
                r'line13\par' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_hl_linenostart_lines_highlighted():
    t = 'line11\nline12\nline13\n'
    options = dict(linenos=False,
                   hl_lines="12 13",
                   hl_linenostart=True,
                   linenostart=11)
    result = format_rtf(t, options)
    expected = (r'line11\par' + '\n'
                r'{\highlight1 line12\par}' + '\n'
                r'{\highlight1 line13\par}' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)
    assert result.endswith(expected), msg


def test_lineno_color_style_specify_hex():
    class TestStyle(Style):
        name = 'rtf_formatter_test'
        line_number_color = "#123456"

    t = 'line1\nline2\n'
    options = dict(linenos=True,
                   style=TestStyle)
    result = format_rtf(t, options)
    rtf_color_str = RtfFormatter.hex_to_rtf_color(TestStyle.line_number_color)
    color_tbl = extract_color_table(result)
    msg = (f"Color table {color_tbl} "
           f"should have '{rtf_color_str}' "
            "as first entry")

    # With linenos=True the color table should contain:
    #  1st entry: line number color (hence \cf1)
    assert color_tbl[0] == rtf_color_str, msg


def test_lineno_color_style_specify_inherit():
    class TestStyle(Style):
        name = 'rtf_formatter_test'
        line_number_color = "inherit" # Default from pygments/style.py

    t = 'line1\nline2\n'
    options = dict(linenos=True,
                   style=TestStyle)
    result = format_rtf(t, options)
    rtf_color_str = RtfFormatter.hex_to_rtf_color(_ansimap['ansibrightblack'])
    color_tbl = extract_color_table(result)
    msg = (f"Color table {color_tbl} "
           f"should have '{rtf_color_str}' "
            "as first entry")

    # With linenos=True the color table should contain:
    #  1st entry: line number color (hence \cf1)
    assert color_tbl[0] == rtf_color_str, msg


def test_lineno_color_from_cli_option():
    class TestStyle(Style):
        name = 'rtf_formatter_test'
        line_number_color = "#123456" # Default from pygments/style.py

    option_color = "112233"
    t = 'line1\nline2\n'
    options = dict(linenos=True,
                   style=TestStyle,
                   lineno_color=option_color)
    result = format_rtf(t, options)
    rtf_color_str = RtfFormatter.hex_to_rtf_color(option_color)
    color_tbl = extract_color_table(result)
    msg = (f"Color table {color_tbl} "
           f"should have '{rtf_color_str}' "
            "as first entry")

    # With linenos=True the color table should contain:
    #  1st entry: line number color (hence \cf1)
    assert color_tbl[0] == rtf_color_str, msg


def test_hl_color_style():
    class TestStyle(Style):
        name = 'rtf_formatter_test'
        line_number_color = "#123456"
        highlight_color = "#abcdef"

    t = 'line1\nline2\n'
    options = dict(linenos=True,
                   lineno_padding=2,
                   style=TestStyle,
                   hl_lines="1 2")
    result = format_rtf(t, options)

    rtf_color = RtfFormatter.hex_to_rtf_color(TestStyle.highlight_color)

    color_tbl = extract_color_table(result)
    msg = (f"Color table {color_tbl} "
           f"should have '{rtf_color}' "
            "as second entry")

    # With linenos=True and hl_lines="1 2" the color table should contain:
    #  1st entry: line number color (hence \cf1)
    #  2nd entry: highlight color (hence \highlight2)
    assert color_tbl[1] == rtf_color, msg

    expected = (r'{\highlight2 {\cf1 1  }line1\par}' + '\n'
                r'{\highlight2 {\cf1 2  }line2\par}' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)

    assert result.endswith(expected), msg


def test_hl_color_style_no_linenos():
    class TestStyle(Style):
        name = 'rtf_formatter_test'
        line_number_color = "#123456"
        highlight_color = "#abcdef"

    t = 'line1\nline2\n'
    options = dict(linenos=False,
                   style=TestStyle,
                   hl_lines="1 2")
    result = format_rtf(t, options)

    rtf_color = RtfFormatter.hex_to_rtf_color(TestStyle.highlight_color)

    color_tbl = extract_color_table(result)
    msg = (f"Color table {color_tbl} "
           f"should have '{rtf_color}' "
            "as second entry")

    # With linenos=False and hl_lines="1 2" the color table should contain:
    #  1st entry: highlight color (hence \highlight1)
    assert rtf_color in color_tbl and color_tbl[0] == rtf_color, msg

    expected = (r'{\highlight1 line1\par}' + '\n'
                r'{\highlight1 line2\par}' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)

    assert result.endswith(expected), msg


def test_hl_color_option():
    class TestStyle(Style):
        name = 'rtf_formatter_test'
        line_number_color = "#123456"
        highlight_color = "#abcdef"

    t = 'line1\nline2\n'
    hl_color = "aabbcc"
    options = dict(linenos=False,
                   style=TestStyle,
                   hl_lines="1 2",
                   hl_color=hl_color)
    result = format_rtf(t, options)

    rtf_color = RtfFormatter.hex_to_rtf_color(hl_color)

    color_tbl = extract_color_table(result)
    msg = (f"Color table {color_tbl} "
           f"should have '{rtf_color}' "
            "as second entry")

    # With linenos=False and hl_lines="1 2" the color table should contain:
    #  1st entry: highlight color (hence \highlight1)
    assert rtf_color in color_tbl and color_tbl[0] == rtf_color, msg

    expected = (r'{\highlight1 line1\par}' + '\n'
                r'{\highlight1 line2\par}' + '\n'
                r'}' + '\n')
    msg = _build_message(t=t, result=result, expected=expected)

    assert result.endswith(expected), msg


def test_all_options():
    # Test if all combinations of options (given values and defaults)
    # produce output:
    #
    # - No uncaught exceptions
    # - Output contains one \par control word per input line

    def get_option_combinations(options):
        for _, values in options.items():
            values.append('default')
        # https://stackoverflow.com/a/40623158
        combinations =  (dict(zip(options.keys(), x))
                         for x in itertools.product(*options.values()))
        for c in combinations:
            yield {opt:val for opt,val in c.items() if val!='default'}

    options = {'linenos': [True],
    		   'lineno_fontsize': [36],
    		   'fontsize': [36],
    		   'lineno_padding': [4],
    		   'linenostart': [10],
    		   'linenostep': [3],
    		   'lineno_color': ['ff0000'],
    		   'hl_lines': ['2'],
    		   'hl_linenostart': [True],
    		   'hl_color': ['00ff00']
    		   }

    t_cpp = [r'#include <iostream>',
             r'int main(int argc, char** argv) {',
             r'    /* Multi-line comment',
             r'       with \n escape sequence */'
             r'    for (int i = 0; i < argc; i++){',
             r'        std::cout << i << ": " << argv[i] << "\n";',
             r'    }',
             r'    return 0;',
             r'}'
             ]

    t_python = [r'# Description of program',
                r'def add(a, b):',
                r'    """ Add numbers a and b.',
                r'        Newline \n in docstring."""',
                r'    return a+b',
                r'if __name__ == "__main__":',
                r'result = add(2,2)',
                r'print(f"Result:\n{result}")'
                ]

    t_text = [r'Header1;"Long',
              r'Header2";Header3',
              r'1,2;Single Line;20/02/2024',
              r'1,3;"Multiple',
              r'Lines";21/02/2024']


    for opts in get_option_combinations(options):

        opt_strs = '\n'.join([f"{k}: {v}" for k,v in opts.items()])
        opt_str_for_copying = "-O " + ",".join([f"{k}={v}" for k,v in opts.items()])

        for t, lexer in [(t_cpp, CppLexer),
                         (t_python, PythonLexer),
                         (t_text, TextLexer)]:

            input_text = '\n'.join(t) + '\n' # Last line should end in \n

            try:
                result = format_rtf(input_text, opts,lexer)
            except Exception as e:
                msg = (f"RTF-formatting caused an exception with options\n\n"
                       f"{opt_strs}\n\n"
                       f"{opt_str_for_copying}\n\n"
                       f"Lexer: {lexer.__name__}\n\n"
                       f"Input:\n"
                       f"{input_text}\n"
                       f"{type(e)}: {e}\n")

                pytest.fail(msg)

            num_input_lines = len(t)
            num_of_pars = result.count(r'\par')

            msg = (f"Different of number of input lines and formatted lines:\n"
                   f"{opt_strs}\n\n"
                   f"{opt_str_for_copying}\n\n"
                   f"\\par control words: {num_of_pars}\n"
                   f"Input lines: {num_input_lines}\n\n"
                   f"Input:\n"
                   f"{input_text}\n")

            assert num_of_pars == num_input_lines, msg
