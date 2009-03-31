#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Pygments
    ~~~~~~~~

    Pygments is a syntax highlighting package written in Python.

    It is a generic syntax highlighter for general use in all kinds of software
    such as forum systems, wikis or other applications that need to prettify
    source code. Highlights are:

    * a wide range of common languages and markup formats is supported
    * special attention is paid to details, increasing quality by a fair amount
    * support for new languages and formats are added easily
    * a number of output formats, presently HTML, LaTeX, RTF, SVG and ANSI sequences
    * it is usable as a command-line tool and as a library
    * ... and it highlights even Brainfuck!

    The `Pygments tip`_ is installable with ``easy_install Pygments==dev``.

    .. _Pygments tip: http://dev.pocoo.org/hg/pygments-main/archive/tip.tar.gz#egg=Pygments-dev

    :copyright: Copyright 2006-2009 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

try:
    from setuptools import setup, find_packages
except ImportError:
    from distutils.core import setup
    def find_packages():
        return [
            'pygments',
            'pygments.lexers',
            'pygments.formatters',
            'pygments.styles',
            'pygments.filters',
        ]

try:
    from distutils.command.build_py import build_py_2to3 as build_py
except ImportError:
    from distutils.command.build_py import build_py

setup(
    name = 'Pygments',
    version = '1.1',
    url = 'http://pygments.org/',
    license = 'BSD License',
    author = 'Georg Brandl',
    author_email = 'georg@python.org',
    description = 'Pygments is a syntax highlighting package written in Python.',
    long_description = __doc__,
    keywords = 'syntax highlighting',
    packages = find_packages(),
    scripts = ['pygmentize'],
    platforms = 'any',
    zip_safe = False,
    include_package_data = True,
    classifiers = [
        'License :: OSI Approved :: BSD License',
        'Intended Audience :: Developers',
        'Intended Audience :: End Users/Desktop',
        'Intended Audience :: System Administrators',
        'Development Status :: 5 - Production/Stable',
        'Programming Language :: Python',
        'Operating System :: OS Independent',
    ],
    cmdclass = {'build_py': build_py},
)
