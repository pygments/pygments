# -*- coding: utf-8 -*-
"""
    pygments.lexers
    ~~~~~~~~~~~~~~

    Pygments lexers.

    :copyright: 2006 by Georg Brandl.
    :license: GNU LGPL, see LICENSE for more details.
"""
import fnmatch
import types
from os.path import basename

from pygments.lexers._mapping import LEXERS


__all__ = ['get_lexer_by_name', 'get_lexer_for_filename'] + LEXERS.keys()

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
    for module_name, name, aliases, _ in LEXERS.itervalues():
        if alias in aliases:
            if name not in _lexer_cache:
                _load_lexers(module_name)
            return _lexer_cache[name](**options)
    raise ValueError('no lexer for alias %r found' % alias)


def get_lexer_for_filename(fn, **options):
    """
    Guess a lexer by a filename
    """
    fn = basename(fn)
    for module_name, name, _, filenames in LEXERS.itervalues():
        for filename in filenames:
            if fnmatch.fnmatch(fn, filename):
                if name not in _lexer_cache:
                    _load_lexers(module_name)
                return _lexer_cache[name](**options)
    raise ValueError('no lexer for filename %r found' % fn)


class _automodule(types.ModuleType):

    def __getattr__(self, name):
        """Automatically import lexers."""
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
