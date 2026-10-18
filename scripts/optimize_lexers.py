"""
    scripts/optimize_lexers.py
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    Rewrite lexer modules so that every maximal run of consecutive *simple*
    rules in a state is merged into one rule with a combined regex.

    ``RegexLexer.get_tokens_unprocessed`` tries the rules of the current
    state in order at every position, so a state with N rules costs up to N
    ``re.match()`` calls per token.  A run of simple rules ::

        (regex0, token0),
        (regex1, token1),
        ...

    is equivalent to the single rule ::

        ('(?P<g0>regex0)|(?P<g1>regex1)|...', merged(token0, token1, ...))

    because the alternatives of a regex are tried in order and the first one
    that matches wins, exactly like the rules in the rule loop.  A rule is
    simple when it consists of a regex and a plain token type only (no state
    transition, no callback) and its regex is foldable (no named group,
    backreference, conditional or global inline flag).  All other rules stay
    where they are as barriers, so the relative order of the rules and
    therefore the lexer output is unchanged.

    The build hook in ``hatch_build.py`` runs this script so that the lexers
    in the wheel are optimized while the sources in the repository (and
    editable installs) stay readable.  To see what it does::

        python scripts/optimize_lexers.py --stats
        python scripts/optimize_lexers.py --diff pygments/lexers/python.py
        python scripts/optimize_lexers.py --out build/optimized

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import argparse
import ast
import difflib
import functools
import importlib
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# Always optimize the lexers of this source tree, not an installed Pygments.
sys.path.insert(0, ROOT)

from pygments.lexer import Future, RegexLexer  # noqa: E402
from pygments.token import _TokenType  # noqa: E402

LEXERS_DIR = os.path.join(ROOT, 'pygments', 'lexers')
IMPORT_LINE = 'from pygments.lexer import merged  # added by scripts/optimize_lexers.py\n'

# ``SomeLexer.tokens`` used in any way other than reusing a whole state
# (``SomeLexer.tokens['state']`` as a value).
_INTROSPECTION_RE = re.compile(
    r'\b([A-Z]\w*)\.tokens\b(?!\[[\'"][^\'"]*[\'"]\]\s*(?:[,)\]}]|$))',
    re.MULTILINE)


def can_merge_pattern(pattern):
    """Whether a regex can be folded into a merged alternation.

    It must not use a named group (which could collide with the name of
    another rule's group or with the wrapper group), a backreference or
    conditional (the group numbers shift once the regex is one alternative
    among many), or a global inline flag like ``(?i)`` (which is illegal
    when it is no longer at the start of the regex, and would apply to the
    other alternatives too).
    """
    return ('(?P<' not in pattern
            and '(?(' not in pattern
            and not re.search(r'\\[1-9]', pattern)
            and not re.search(r'\(\?[aiLmsux]+\)', pattern))


def merge_patterns(patterns):
    """Combine the regexes of a run of simple rules into one regex."""
    return '|'.join(f'(?P<g{i}>{pattern})' for i, pattern in enumerate(patterns))


def regex_literal(pattern):
    """A Python literal for *pattern*, as a raw string whenever possible."""
    if pattern.isprintable():
        for quote in ("'", '"'):
            if quote not in pattern:
                literal = f'r{quote}{pattern}{quote}'
                try:
                    if ast.literal_eval(literal) == pattern:
                        return literal
                except (SyntaxError, ValueError):
                    pass
    return repr(pattern)


def _evaluate(node, namespace):
    expression = ast.Expression(body=node)
    ast.fix_missing_locations(expression)
    return eval(compile(expression, '<rule>', 'eval'), namespace)


def simple_rule_pattern(node, namespace):
    """Return the regex of the rule *node* if it is a simple, foldable rule.

    *node* is the AST of one entry of a state's rule list, and *namespace*
    the names visible in the class body that defines the rules.  Returns
    ``None`` for anything else.
    """
    if not isinstance(node, ast.Tuple) or len(node.elts) != 2:
        return None
    try:
        regex = _evaluate(node.elts[0], namespace)
        token = _evaluate(node.elts[1], namespace)
    except Exception:
        return None
    if isinstance(regex, Future):
        regex = regex.get()
    if not isinstance(regex, str) or type(token) is not _TokenType:
        return None
    if not can_merge_pattern(regex):
        return None
    return regex


def simple_runs(nodes, namespace):
    """Yield the maximal runs of at least two consecutive simple rules in
    *nodes* as lists of ``(node, regex)`` pairs."""
    run = []
    for node in nodes:
        pattern = simple_rule_pattern(node, namespace)
        if pattern is None:
            if len(run) > 1:
                yield run
            run = []
        else:
            run.append((node, pattern))
    if len(run) > 1:
        yield run


def _state_lists(dict_node):
    """Yield ``(state name, list node)`` for the rule lists of a ``tokens``
    dict, including the nested dicts of lexers with token variants."""
    for key, value in zip(dict_node.keys, dict_node.values):
        name = key.value if isinstance(key, ast.Constant) else '?'
        if isinstance(value, ast.List):
            yield name, value
        elif isinstance(value, ast.Dict):
            yield from _state_lists(value)


def _line_starts(source):
    starts = [0]
    for line in source.splitlines(keepends=True):
        starts.append(starts[-1] + len(line))
    return starts


def _offset(source, line_starts, lineno, col_offset):
    """Character offset in *source* of an AST position (whose column is a
    UTF-8 byte offset)."""
    start = line_starts[lineno - 1]
    line = source[start:line_starts[lineno]]
    return start + len(line.encode('utf-8')[:col_offset].decode('utf-8'))


@functools.cache
def introspected_classes():
    """The names of the lexer classes whose rules are inspected by other code
    in the lexers package, like ``SomeLexer.tokens.items()`` or
    ``SomeLexer.tokens['root'][0][0]``.  Their rules are left as they are,
    since that code may rely on finding particular rules.  Reusing a whole
    state as in ``'state': SomeLexer.tokens['state'],`` is fine and does not
    count.
    """
    names = set()
    for path in lexer_module_paths():
        with open(path, encoding='utf-8') as f:
            names.update(_INTROSPECTION_RE.findall(f.read()))
    return frozenset(names)


def optimize_source(source, module, filename='<lexer module>', skip=()):
    """Merge the runs of simple rules in the lexer classes of *source*.

    *module* is the imported module the source belongs to; it is used to
    find the lexer classes and to evaluate the rules.  The classes named in
    *skip* are left alone.  Returns the rewritten source and a list of
    ``(class name, state, number of rules)`` for every merged run.
    Everything but the merged rules (comments, formatting) is kept as it is.
    """
    tree = ast.parse(source, filename)
    line_starts = _line_starts(source)
    edits = []
    merges = []
    import_end = None
    seen_class = False

    for node in tree.body:
        if isinstance(node, (ast.Import, ast.ImportFrom)) and not seen_class:
            import_end = node.end_lineno
        if not isinstance(node, ast.ClassDef):
            continue
        seen_class = True
        cls = getattr(module, node.name, None)
        if not (isinstance(cls, type) and issubclass(cls, RegexLexer)):
            continue
        if node.name in skip:
            continue
        if cls.flags & re.VERBOSE:
            # A '#' comment in one regex would run into the next alternative.
            continue
        namespace = {**vars(module), **vars(cls)}
        for stmt in node.body:
            if not (isinstance(stmt, ast.Assign) and len(stmt.targets) == 1
                    and isinstance(stmt.targets[0], ast.Name)
                    and stmt.targets[0].id == 'tokens'
                    and isinstance(stmt.value, ast.Dict)):
                continue
            for state, rules in _state_lists(stmt.value):
                for run in simple_runs(rules.elts, namespace):
                    combined = merge_patterns([pattern for _, pattern in run])
                    try:
                        compiled = re.compile(combined, cls.flags)
                    except re.error:
                        continue
                    assert [compiled.groupindex[f'g{i}'] for i in range(len(run))] == \
                        sorted(compiled.groupindex.values())
                    edits.append(_merged_rule_edit(source, line_starts, run, combined))
                    merges.append((node.name, state, len(run)))

    if not edits:
        return source, merges

    if import_end is None:
        # No imports before the first class: put ours after the docstring.
        first = tree.body[0]
        import_end = first.end_lineno if isinstance(first, ast.Expr) else 0
    edits.append((line_starts[import_end], line_starts[import_end], IMPORT_LINE))

    # Apply the edits back to front so that the offsets stay valid.
    for start, end, text in sorted(edits, reverse=True):
        source = source[:start] + text + source[end:]
    return source, merges


def _merged_rule_edit(source, line_starts, run, combined):
    """The ``(start, end, text)`` edit replacing the rules of *run* with
    the merged rule."""
    first, last = run[0][0], run[-1][0]
    start = _offset(source, line_starts, first.lineno, first.col_offset)
    end = _offset(source, line_starts, last.end_lineno, last.end_col_offset)
    column = start - line_starts[first.lineno - 1]
    tokens = ', '.join(ast.get_source_segment(source, node.elts[1])
                       for node, _ in run)
    text = f'({regex_literal(combined)},\n{" " * (column + 1)}merged({tokens}))'
    if not source[line_starts[first.lineno - 1]:start].strip():
        # The run starts a line: explain where the rule comes from.
        text = (f'# The next rule merges {len(run)} simple rules '
                f'(scripts/optimize_lexers.py).\n{" " * column}{text}')
    return start, end, text


def lexer_module_paths():
    """The paths of all lexer modules."""
    return [os.path.join(LEXERS_DIR, name)
            for name in sorted(os.listdir(LEXERS_DIR))
            if name.endswith('.py') and name != '__init__.py']


def optimize_module(path):
    """Optimize the lexer module at *path*.

    Returns the original source, the optimized source and the list of merges
    (see :func:`optimize_source`).
    """
    with open(path, encoding='utf-8') as f:
        source = f.read()
    name = os.path.splitext(os.path.basename(path))[0]
    module = importlib.import_module(f'pygments.lexers.{name}')
    optimized, merges = optimize_source(source, module, path,
                                        skip=introspected_classes())
    return source, optimized, merges


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split('\n\n')[1],
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('paths', nargs='*',
                        help='lexer modules to process (default: all)')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--out', metavar='DIR',
                       help='write the optimized modules to '
                            'DIR/pygments/lexers/ (only the ones that changed)')
    group.add_argument('--diff', action='store_true',
                       help='print a unified diff of the changes')
    group.add_argument('--stats', action='store_true',
                       help='print the merged runs per module (default)')
    args = parser.parse_args(argv)

    paths = [os.path.abspath(p) for p in args.paths] or lexer_module_paths()
    outdir = os.path.join(args.out, 'pygments', 'lexers') if args.out else None
    if outdir:
        os.makedirs(outdir, exist_ok=True)

    total_runs = total_rules = changed = 0
    for path in paths:
        source, optimized, merges = optimize_module(path)
        if not merges:
            continue
        changed += 1
        total_runs += len(merges)
        total_rules += sum(n for _, _, n in merges)
        name = os.path.basename(path)
        if outdir:
            with open(os.path.join(outdir, name), 'w', encoding='utf-8',
                      newline='\n') as f:
                f.write(optimized)
        elif args.diff:
            relpath = os.path.relpath(path, ROOT).replace(os.sep, '/')
            sys.stdout.writelines(difflib.unified_diff(
                source.splitlines(keepends=True),
                optimized.splitlines(keepends=True),
                'a/' + relpath, 'b/' + relpath))
        else:
            print(f'{name}: {len(merges)} runs, '
                  f'{sum(n for _, _, n in merges)} rules merged')
            for clsname, state, n in merges:
                print(f'    {clsname}.{state}: {n} rules')
    print(f'{changed} modules changed: {total_rules} rules merged into '
          f'{total_runs} regexes', file=sys.stderr)
    if args.stats:
        print('classes left alone because their rules are inspected elsewhere: '
              + ', '.join(sorted(introspected_classes())), file=sys.stderr)
    return 0


if __name__ == '__main__':
    sys.exit(main())
