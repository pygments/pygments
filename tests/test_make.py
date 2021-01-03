# -*- coding: utf-8 -*-
"""
    CMake Tests
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexers import CMakeLexer, guess_lexer


def test_guess_cmake_lexer_from_header():
    headers = [
        "CMAKE_MINIMUM_REQUIRED(VERSION 2.6 FATAL_ERROR)",
        "cmake_minimum_required(version 3.13)  # CMake version check",
        " CMAKE_MINIMUM_REQUIRED\t( VERSION 2.6 FATAL_ERROR ) ",
    ]
    for header in headers:
        code = '\n'.join([
            header,
            'project(example)',
            'set(CMAKE_CXX_STANDARD 14)',
            'set(SOURCE_FILES main.cpp)',
            'add_executable(example ${SOURCE_FILES})',
        ])
        lexer = guess_lexer(code)
        assert isinstance(lexer, CMakeLexer), \
            "header must be detected as CMake: %r" % header
