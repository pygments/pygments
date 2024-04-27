"""
    pygments.styles
    ~~~~~~~~~~~~~~~

    Contains built-in styles.

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.plugin import find_plugin_styles
from pygments.util import ClassNotFound
from pygments.styles._mapping import STYLES

#: A dictionary of built-in styles, mapping style names to
#: ``'submodule::classname'`` strings.
#: This list is deprecated. Use `pygments.styles.STYLES` instead
STYLE_MAP = {v[1]: v[0].split('.')[-1] + '::' + k for k, v in STYLES.items()}

#: Internal reverse mapping to make `get_style_by_name` more efficient
_STYLE_NAME_TO_MODULE_MAP = {v[1]: (v[0], k) for k, v in STYLES.items()}


def get_style_by_name(name):
    """
    Return a style class by its short name. The names of the builtin styles
    are listed in :data:`pygments.styles.STYLE_MAP`.

    Will raise :exc:`pygments.util.ClassNotFound` if no style of that name is
    found.
    """
    if name in _STYLE_NAME_TO_MODULE_MAP:
        mod, cls = _STYLE_NAME_TO_MODULE_MAP[name]
        builtin = "yes"
    else:
        for found_name, style in find_plugin_styles():
            if name == found_name:
                return style
        # perhaps it got dropped into our styles package
        builtin = ""
        mod = 'pygments.styles.' + name
        cls = name.title() + "Style"

    try:
        mod = __import__(mod, None, None, [cls])
    except ImportError as e:
        raise ClassNotFound("Could not find style module %r" % mod +
                            (builtin and ", though it should be builtin")
                            + ".") from e
    try:
        return getattr(mod, cls)
    except AttributeError as e:
        raise ClassNotFound("Could not find style class %r in style module." % cls) from e


def get_all_styles():
    """Return a generator for all styles by name, both builtin and plugin."""
    for v in STYLES.values():
        yield v[1]
    for name, _ in find_plugin_styles():
        yield name
