"""
    pygments.lexers._luajit_builtins
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    LuaJIT built-in modules: ffi, bit, jit.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

MODULES = {
    'ffi': (
        'ffi.C', 'ffi.abi', 'ffi.alignof', 'ffi.cast', 'ffi.cdef',
        'ffi.copy', 'ffi.errno', 'ffi.fill', 'ffi.gc', 'ffi.istype',
        'ffi.load', 'ffi.metatype', 'ffi.new', 'ffi.offsetof',
        'ffi.os', 'ffi.arch', 'ffi.sizeof', 'ffi.string', 'ffi.typeof',
    ),
    'bit': (
        'bit.arshift', 'bit.band', 'bit.bnot', 'bit.bor', 'bit.bswap',
        'bit.bxor', 'bit.lshift', 'bit.rol', 'bit.ror', 'bit.rshift',
        'bit.tobit', 'bit.tohex',
    ),
    'jit': (
        'jit.arch', 'jit.flush', 'jit.off', 'jit.on', 'jit.opt',
        'jit.os', 'jit.prngstate', 'jit.security', 'jit.status',
        'jit.version', 'jit.version_num',
    ),
}

def all_luajit_builtins():
    return tuple(name for names in MODULES.values() for name in names)
