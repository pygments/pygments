# -*- coding: utf-8 -*-
"""
    pygments.formatters
    ~~~~~~~~~~~~~~~~~~~

    Pygments formatters.

    :copyright: 2006 by Georg Brandl, Armin Ronacher.
    :license: BSD, see LICENSE for more details.
"""
import os.path
from pygments.formatters.html import HtmlFormatter
from pygments.formatters.terminal import TerminalFormatter
from pygments.formatters.latex import LatexFormatter
from pygments.formatters.rtf import RtfFormatter
from pygments.formatters.bbcode import BBCodeFormatter
from pygments.formatters.other import NullFormatter, RawTokenFormatter
from pygments.plugin import find_plugin_formatters


def _doc_desc(obj):
    if not obj.__doc__:
        return ''
    res = []
    for line in obj.__doc__.strip().splitlines():
        if line.strip():
            res.append(line.strip())
        else:
            break
    return ''.join(res)


#: Map formatter classes to ``(longname, names, file extensions, descr)``.
FORMATTERS = {
    HtmlFormatter:        ('HTML', ('html',), ('.htm', '.html'),
                           _doc_desc(HtmlFormatter)),
    TerminalFormatter:    ('Terminal', ('terminal', 'console'), (),
                           _doc_desc(TerminalFormatter)),
    LatexFormatter:       ('LaTeX', ('latex', 'tex'), ('.tex',),
                           _doc_desc(LatexFormatter)),
    RtfFormatter:         ('RTF', ('rtf',), ('.rtf',),
                           _doc_desc(RtfFormatter)),
    RawTokenFormatter:    ('Raw tokens', ('raw', 'tokens'), ('.raw',),
                           _doc_desc(RawTokenFormatter)),
    NullFormatter:        ('Text only', ('text', 'null'), ('.txt',),
                           _doc_desc(NullFormatter)),
    BBCodeFormatter:      ('BBcode', ('bbcode', 'bb'), (),
                           _doc_desc(BBCodeFormatter))
}


_formatter_cache = {}

def _init_formatter_cache():
    if _formatter_cache:
        return
    for cls, info in FORMATTERS.iteritems():
        for alias in info[1]:
            _formatter_cache[alias] = cls
        for ext in info[2]:
            _formatter_cache["/"+ext] = cls
    for name, cls in find_plugin_formatters():
        _formatter_cache[name] = cls


def get_formatter_by_name(name, **options):
    _init_formatter_cache()
    cls = _formatter_cache.get(name, None)
    if not cls:
        raise ValueError("No formatter found for name %r" % name)
    return cls(**options)


def get_formatter_for_filename(fn, **options):
    _init_formatter_cache()
    # try by filename extension
    cls = _formatter_cache.get("/"+os.path.splitext(fn)[1], None)
    if cls:
        return cls(**options)
    # try by whole file name
    cls = _formatter_cache.get("/"+os.path.basename(fn), None)
    if not cls:
        raise ValueError("No formatter found for file name %r" % fn)
    return cls(**options)


def get_all_formatters():
    """Return a generator for all formatters."""
    for formatter in FORMATTERS:
        yield formatter
    for _, formatter in find_plugin_formatters():
        yield formatter
