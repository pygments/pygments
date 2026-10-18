"""
    Tests for the vim modeline feature
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments import modeline


def test_modelines():
    for buf in [
            'vi: ft=python' + '\n' * 8,
            'vi: ft=python' + '\n' * 8,
            '\n\n\n\nvi=8: syntax=python' + '\n' * 8,
            '\n' * 8 + 'ex: filetype=python',
            '\n' * 8 + 'vim: some,other,syn=python\n\n\n\n'
    ]:
        assert modeline.get_filetype_from_buffer(buf) == 'python'


def test_modeline_later_start():
    buf = 'vim: set ts=4 ex: ft=python'
    assert modeline.get_filetype_from_buffer(buf) == 'python'


def test_modeline_no_quadratic_backtracking():
    # used to take about a minute
    assert modeline.get_filetype_from_buffer('vi:' * 32000) is None
