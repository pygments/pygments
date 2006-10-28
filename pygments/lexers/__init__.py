# -*- coding: utf-8 -*-
"""
    pygments.lexers
    ~~~~~~~~~~~~~~~

    Pygments lexers.

    :copyright: 2006 by Georg Brandl.
    :license: GNU LGPL, see LICENSE for more details.
"""
import fnmatch
import types
from os.path import basename

from pygments.lexers._mapping import LEXERS
from pygments.plugin import find_plugin_lexers


__all__ = ['get_lexer_by_name', 'get_lexer_for_filename',
           'guess_lexer'] + LEXERS.keys()

_lexer_cache = {}


def _load_lexers(module_name):
    """
    Loads a lexer (and all others in the module too)
    """
    mod = __import__(module_name, None, None, ['__all__'])
    for lexer_name in mod.__all__:
        cls = getattr(mod, lexer_name)
        _lexer_cache[cls.name] = cls


def get_lexer_by_name(alias, **options):
    """
    Get a lexer by an alias
    """
    # lookup builtin lexers
    for module_name, name, aliases, _ in LEXERS.itervalues():
        if alias in aliases:
            if name not in _lexer_cache:
                _load_lexers(module_name)
            return _lexer_cache[name](**options)
    # continue with lexers from setuptools entrypoints
    for cls in find_plugin_lexers():
        if alias in cls.aliases:
            return cls(**options)
    raise ValueError('no lexer for alias %r found' % alias)


def get_lexer_for_filename(fn, **options):
    """
    Guess a lexer by a filename
    """
    fn = basename(fn)
    # lookup builtin lexers
    for module_name, name, _, filenames in LEXERS.itervalues():
        for filename in filenames:
            if fnmatch.fnmatch(fn, filename):
                if name not in _lexer_cache:
                    _load_lexers(module_name)
                return _lexer_cache[name](**options)
    # continue with lexers from setuptools entrypoints
    for cls in find_plugin_lexers():
        for filename in cls.filenames:
            if fnmatch.fnmatch(fn, filename):
                return cls(**options)
    raise ValueError('no lexer for filename %r found' % fn)


def guess_lexer(text, **options):
    """
    Guess a lexer by strong distinctions in the text (eg, shebang).
    """
    best_lexer = [0.0, None]
    # builtin lexers
    for module_name, name, _, _ in LEXERS.itervalues():
        if name not in _lexer_cache:
            _load_lexers(module_name)
        lexer = _lexer_cache[name]
        rv = lexer.analyse_text(text)
        if rv == 1.0:
            return lexer(**options)
        if rv > best_lexer[0]:
            best_lexer[:] = (rv, lexer)
    # plugin lexers
    for lexer in find_plugin_lexers():
        rv = lexer.analyse_text(text)
        if rv == 1.0:
            return lexer(**options)
        if rv > best_lexer[0]:
            best_lexer[:] = (rv, lexer)
    if best_lexer[0] == 0.0 or best_lexer[1] is None:
        from pygments.lexers.special import TextLexer
        return TextLexer(**options)
    return best_lexer[1](**options)


class _automodule(types.ModuleType):
    """Automatically import lexers."""

    def __getattr__(self, name):
        info = LEXERS.get(name)
        if info:
            _load_lexers(info[0])
            cls = _lexer_cache[info[1]]
            setattr(self, name, cls)
            return cls
        raise AttributeError(name)


import sys
oldmod = sys.modules['pygments.lexers']
newmod = _automodule('pygments.lexers')
newmod.__dict__.update(oldmod.__dict__)
sys.modules['pygments.lexers'] = newmod
del newmod.newmod, newmod.oldmod, newmod.sys, newmod.types
