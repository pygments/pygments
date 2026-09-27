"""
    pygments.regexopt
    ~~~~~~~~~~~~~~~~~

    An algorithm that generates optimized regexes for matching long lists of
    literal strings.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
from re import escape
from operator import itemgetter

CS_ESCAPE = re.compile(r'[\[\^\\\-\]]')
FIRST_ELEMENT = itemgetter(0)


def _common_leading(s1, s2):
    """Return the longest common leading substring of two strings."""
    for i, c in enumerate(s1):
        if c != s2[i]:
            return s1[:i]
    return s1


def commonprefix(m):
    """Given an iterable of strings, returns the longest common leading substring"""
    if not m:
        return ""
    return _common_leading(min(m), max(m))


def make_charset(letters):
    return '[' + CS_ESCAPE.sub(r'\\\g<0>', ''.join(letters)) + ']'


def regex_opt_inner(strings, open_paren):
    """Return a regex that matches any string in the sorted list of strings."""
    close_paren = open_paren and ')' or ''
    # print strings, repr(open_paren)
    if not strings:
        # print '-> nothing left'
        return ''
    first = strings[0]
    if len(strings) == 1:
        # print '-> only 1 string'
        return open_paren + escape(first) + close_paren
    if not first:
        # print '-> first string empty'
        return open_paren + regex_opt_inner(strings[1:], '(?:') \
            + '?' + close_paren
    if len(first) == 1:
        # multiple one-char strings? make a charset
        oneletter = []
        rest = []
        for s in strings:
            if len(s) == 1:
                oneletter.append(s)
            else:
                rest.append(s)
        if len(oneletter) > 1:  # do we have more than one oneletter string?
            if rest:
                # print '-> 1-character + rest'
                return open_paren + regex_opt_inner(rest, '') + '|' \
                    + make_charset(oneletter) + close_paren
            # print '-> only 1-character'
            return open_paren + make_charset(oneletter) + close_paren
    # `strings` is sorted, so the longest common prefix of all of them is the
    # common prefix of the first and last one -- no need to rescan for min/max.
    prefix = _common_leading(first, strings[-1])
    if prefix:
        plen = len(prefix)
        # we have a prefix for all strings
        # print '-> prefix:', prefix
        return open_paren + escape(prefix) \
            + regex_opt_inner([s[plen:] for s in strings], '(?:') \
            + close_paren
    # is there a suffix?
    strings_rev = [s[::-1] for s in strings]
    suffix = commonprefix(strings_rev)
    if suffix:
        slen = len(suffix)
        # print '-> suffix:', suffix[::-1]
        return open_paren \
            + regex_opt_inner(sorted(s[:-slen] for s in strings), '(?:') \
            + escape(suffix[::-1]) + close_paren
    # recurse on common 1-string prefixes
    # print '-> last resort'
    # `strings` is sorted and `first` is its smallest element, so the strings
    # sharing their first character with `first` form a contiguous block at the
    # front.  Find that boundary directly instead of running groupby with a
    # per-element key function.
    c0 = first[0]
    split = 1
    n = len(strings)
    while split < n and strings[split][0] == c0:
        split += 1
    return open_paren \
        + regex_opt_inner(strings[:split], '') + '|' \
        + regex_opt_inner(strings[split:], '') \
        + close_paren


def regex_opt(strings, prefix='', suffix=''):
    """Return a compiled regex that matches any string in the given list.

    The strings to match must be literal strings, not regexes.  They will be
    regex-escaped.

    *prefix* and *suffix* are pre- and appended to the final regex.
    """
    strings = sorted(set(strings))
    return prefix + regex_opt_inner(strings, '(') + suffix
