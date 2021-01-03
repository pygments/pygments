# -*- coding: utf-8 -*-
"""
    Pygments RTF formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from io import StringIO

from pygments.formatters import RtfFormatter
from pygments.lexers.special import TextLexer


foot = (r'\par' '\n' r'}')


def _escape(string):
    return string.replace("\n", r"\n")


def _build_message(*args, **kwargs):
    string = kwargs.get('string', None)
    t = _escape(kwargs.get('t', ''))
    expected = _escape(kwargs.get('expected', ''))
    result = _escape(kwargs.get('result', ''))

    if string is None:
        string = ("The expected output of '{t}'\n"
                  "\t\tShould be '{expected}'\n"
                  "\t\tActually outputs '{result}'\n"
                  "\t(WARNING: Partial Output of Result!)")

    end = -len(_escape(foot))
    start = end - len(expected)

    return string.format(t=t,
                         result = result[start:end],
                         expected = expected)


def format_rtf(t):
    tokensource = list(TextLexer().get_tokens(t))
    fmt = RtfFormatter()
    buf = StringIO()
    fmt.format(tokensource, buf)
    result = buf.getvalue()
    buf.close()
    return result


def test_rtf_header():
    t = ''
    result = format_rtf(t)
    expected = r'{\rtf1\ansi\uc0'
    msg = ("RTF documents are expected to start with '{expected}'\n"
           "\t\tStarts intead with '{result}'\n"
           "\t(WARNING: Partial Output of Result!)".format(
               expected=expected,
               result=result[:len(expected)]))
    assert result.startswith(expected), msg


def test_rtf_footer():
    t = ''
    result = format_rtf(t)
    expected = ''
    msg = ("RTF documents are expected to end with '{expected}'\n"
           "\t\tEnds intead with '{result}'\n"
           "\t(WARNING: Partial Output of Result!)".format(
               expected=_escape(expected),
               result=_escape(result[-len(expected):])))
    assert result.endswith(expected+foot), msg


def test_ascii_characters():
    t = 'a b c d ~'
    result = format_rtf(t)
    expected = (r'a b c d ~')
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
