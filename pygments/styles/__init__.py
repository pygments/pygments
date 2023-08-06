"""
    pygments.styles
    ~~~~~~~~~~~~~~~

    Contains built-in styles.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.plugin import find_plugin_styles
from pygments.util import ClassNotFound
from pygments.styles._mapping import STYLES

__all__ = ['get_style_by_name', 'get_all_styles']

#: A dictionary of built-in styles, mapping style names to
#: ``'submodule::classname'`` strings.
STYLE_MAP = {k[1]: k[3] for k in STYLES.values()}


def get_style_by_name(name):
    """
    Return a style class by its short name. The names of the builtin styles
    are listed in :data:`pygments.styles.STYLE_MAP`.

    Will raise :exc:`pygments.util.ClassNotFound` if no style of that name is
    found.
    """
    if name in STYLE_MAP:
        mod, cls = STYLE_MAP[name].split('::')
        builtin = "yes"
    else:
        for found_name, style in find_plugin_styles():
            if name == found_name:
                return style
        # perhaps it got dropped into our styles package
        builtin = ""
        mod = name
        cls = name.title() + "Style"

    try:
        mod = __import__('pygments.styles.' + mod, None, None, [cls])
    except ImportError:
        raise ClassNotFound("Could not find style module %r" % mod +
                         (builtin and ", though it should be builtin") + ".")
    try:
        return getattr(mod, cls)
    except AttributeError:
        raise ClassNotFound("Could not find style class %r in style module." % cls)


def get_all_styles():
    """Return a generator for all styles by name, both builtin and plugin."""
    yield from STYLE_MAP
    for name, _ in find_plugin_styles():
        yield name
