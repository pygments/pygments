"""
    pygments._firstchar
    ~~~~~~~~~~~~~~~~~~~

    First-character dispatch for :class:`~pygments.lexer.RegexLexer`.

    A regex lexer tries every rule of the current state, in order, until one
    matches, but most attempts fail on the very first character.  For each
    state we therefore precompute a ``char -> rules`` table so the lexer only
    tries the rules that can possibly match the character at the current
    position.

    A rule is bucketed only when its pattern *provably* cannot match unless it
    starts with one of a known, finite set of characters, found by inspecting
    the compiled pattern's syntax tree.  Anything uncertain is "broad" and
    tried for every character, so first-match-wins behaviour is unchanged.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

try:
    from re import _parser as sre_parse, _constants as sre
except ImportError:  # Python < 3.11
    import sre_parse
    import sre_constants as sre


def _firsts(seq):
    """Return ``(chars, nullable)`` for a parsed pattern sequence.

    ``chars`` is the set of possible first characters, or ``None`` if it
    cannot be bounded.  ``nullable`` is True if the sequence can match empty.
    """
    chars = set()
    for op, av in seq:
        c, nullable = _node(op, av)
        if c is None:
            return None, True
        chars |= c
        if not nullable:
            return chars, False
    return chars, True


def _node(op, av):
    """Return ``(chars, nullable)`` for a single syntax-tree node."""
    if op is sre.LITERAL:
        return {chr(av)}, False
    elif op is sre.IN:
        chars = set()
        for sop, sav in av:
            if sop is sre.LITERAL:
                chars.add(chr(sav))
            # Only expand small ranges; large (mostly Unicode) ranges bloat
            # the bucket table for no real dispatch benefit, so treat as broad.
            elif sop is sre.RANGE and sav[1] - sav[0] <= 512:
                chars.update(map(chr, range(sav[0], sav[1] + 1)))
            else:  # NEGATE, CATEGORY, huge range, … give up
                return None, False
        return chars, False
    elif op is sre.BRANCH:
        chars, nullable = set(), False
        for branch in av[1]:
            c, n = _firsts(branch)
            if c is None:
                return None, True
            chars |= c
            nullable |= n
        return chars, nullable
    elif op is sre.SUBPATTERN:
        if av[1] or av[2]:  # inline flags, e.g. (?i:...)
            return None, True
        return _firsts(av[3])
    elif op is sre.MAX_REPEAT or op is sre.MIN_REPEAT:
        c, n = _firsts(av[2])
        return (None, True) if c is None else (c, n or av[0] == 0)
    elif op is sre.AT:  # ^ $ \b: zero-width, keep looking at the next node
        return set(), True
    return None, True  # ANY, NOT_LITERAL, ASSERT, group references, ...


def _pattern_firsts(match):
    """First characters of a compiled rule, or None if unknown/nullable."""
    try:
        pattern = match.__self__
        if pattern.flags & re.IGNORECASE:
            # re case-folds beyond str.lower/upper (dotless i, long s, ...).
            return None
        chars, nullable = _firsts(sre_parse.parse(pattern.pattern,
                                                   pattern.flags))
    except Exception:
        # we're doing an optional optimization, so don't let it break lexing
        return None
    return None if nullable or not chars else chars


def _build(rules):
    """Build ``(buckets, broad)`` for one state's ``(match, ...)`` rules.

    ``buckets`` maps a character to the ordered rules to try for it; ``broad``
    is the rules that must be tried for any character (and at end of input).
    Returns ``(None, rules)`` when no rule can be bucketed.
    """
    # Pair each rule with the characters it can start with (None = any char).
    firsts = [(rule, _pattern_firsts(rule[0])) for rule in rules]
    broad = [rule for rule, chars in firsts if chars is None]
    if len(broad) == len(rules):
        return None, rules

    # Collect every character that some rule specifically starts with.
    starts = set()
    for rule, chars in firsts:
        if chars is not None:
            starts |= chars

    # A rule belongs in a character's bucket if it is broad (matches any char)
    # or explicitly starts with that character; keep the original rule order.
    buckets = {}
    for ch in starts:
        buckets[ch] = [rule for rule, chars in firsts
                       if chars is None or ch in chars]
    return buckets, broad


# Cache keyed by the identity of the processed-tokendefs dict.  id() values can
# be reused after garbage collection, so the entry is validated by identity.
_cache = {}


def state_dispatch(tokendefs):
    """Return ``{state: (buckets, broad)}`` for a processed tokendefs dict."""
    entry = _cache.get(id(tokendefs))
    if entry is not None and entry[0] is tokendefs:
        return entry[1]
    try:
        dispatch = {state: _build(rules) for state, rules in tokendefs.items()}
    except Exception:  # never let dispatch break lexing
        dispatch = {state: (None, rules) for state, rules in tokendefs.items()}
    _cache[id(tokendefs)] = (tokendefs, dispatch)
    return dispatch
