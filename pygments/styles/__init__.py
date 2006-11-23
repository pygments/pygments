# -*- coding: utf-8 -*-
"""
    pygments.styles
    ~~~~~~~~~~~~~~~

    Contains built-in styles.

    :copyright: 2006 by Georg Brandl.
    :license: GNU LGPL, see LICENSE for more details.
"""
from pygments.plugin import find_plugin_styles


#: Maps style names to 'submodule::classname'.
STYLE_MAP = {
    'default':  'default::DefaultStyle',
    'emacs':    'default::DefaultStyle',
    'friendly': 'friendly::FriendlyStyle',
    'colorful': 'colorful::ColorfulStyle',
    'autumn':   'autumn::AutumnStyle',
    'murphy':   'murphy::MurphyStyle',
    'manni':    'manni::ManniStyle',
    'perldoc':  'perldoc::PerldocStyle',
    'pastie':   'pastie::PastieStyle',
    'borland':  'borland::BorlandStyle',
    'trac':     'trac::TracStyle',
    'native':   'native::NativeStyle'
}


def get_style_by_name(name):
    if name not in STYLE_MAP:
        for found_name, style in find_plugin_styles():
            if name == found_name:
                return style
        raise ValueError("Style %r not found" % name)

    mod, cls = STYLE_MAP[name].split('::')
    mod = __import__('pygments.styles.' + mod, None, None, [cls])
    return getattr(mod, cls)


def get_all_styles():
    """Return an generator for all styles by name.
    Both builtin and plugin."""
    for name in STYLE_MAP:
        yield name
    for name, _ in find_plugin_styles():
        yield name
