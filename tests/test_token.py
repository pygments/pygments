# -*- coding: utf-8 -*-
"""
    Test suite for the token module
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import copy

import pytest

from pygments import token


def test_tokentype():
    t = token.String
    assert t.split() == [token.Token, token.Literal, token.String]
    assert t.__class__ is token._TokenType


def test_functions():
    assert token.is_token_subtype(token.String, token.String)
    assert token.is_token_subtype(token.String, token.Literal)
    assert not token.is_token_subtype(token.Literal, token.String)

    assert token.string_to_tokentype(token.String) is token.String
    assert token.string_to_tokentype('') is token.Token
    assert token.string_to_tokentype('String') is token.String


def test_sanity_check():
    stp = token.STANDARD_TYPES.copy()
    stp[token.Token] = '---'  # Token and Text do conflict, that is okay
    t = {}
    for k, v in stp.items():
        t.setdefault(v, []).append(k)
    if len(t) == len(stp):
        return  # Okay

    for k, v in t.items():
        if len(v) > 1:
            pytest.fail("%r has more than one key: %r" % (k, v))


def test_copying():
    # Token instances are supposed to be singletons, so copying or even
    # deepcopying should return themselves
    t = token.String
    assert t is copy.copy(t)
    assert t is copy.deepcopy(t)
