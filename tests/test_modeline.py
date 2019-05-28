# -*- coding: utf-8 -*-
"""
    Tests for the vim modeline feature
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

from pygments import modeline


def test_lexer_classes():
    def verify(buf):
        assert modeline.get_filetype_from_buffer(buf) == 'python'

    for buf in [
            'vi: ft=python' + '\n' * 8,
            'vi: ft=python' + '\n' * 8,
            '\n\n\n\nvi=8: syntax=python' + '\n' * 8,
            '\n' * 8 + 'ex: filetype=python',
            '\n' * 8 + 'vim: some,other,syn=python\n\n\n\n'
    ]:
        yield verify, buf
