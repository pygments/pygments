"""
    pygments.lexers.raku
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for Raku.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import ExtendedRegexLexer, LexerContext, include, \
    bygroups, using, this, default
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace, Generic
from pygments.util import shebang_matches

__all__ = ['RakuLexer']


# Superscript digits and signs. \w matches the digits, but in Raku they are
# exponents ("$x²"), so they must not be part of an identifier.
_SUPERSCRIPTS = '²³¹⁰-⁻'
# One identifier character, for lookbehinds.
_IDENT_CHAR = r"(?:(?![" + _SUPERSCRIPTS + r"])\w|['\-:])"
# What may follow a word for it to still be part of a longer identifier. A
# single colon does not continue an identifier ("@a.push: 1", "Blob:D:"),
# and neither does a hyphen that is not followed by a letter ("$x-1").
_IDENT_END = r"(?:(?![" + _SUPERSCRIPTS + r"])\w|['\-](?=[^\W\d])|::)"


def _sublex(lexer, text, offset, state='root'):
    """Lex `text` on its own in `state`, reporting positions shifted by `offset`."""
    context = LexerContext(text, 0, ['root'] if state == 'root' else ['root', state])
    for pos, token, value in lexer.get_tokens_unprocessed(context=context):
        yield offset + pos, token, value


def _find_unescaped(text, needle, start):
    """Like ``text.find(needle, start)`` but skips backslash-escaped matches."""
    while True:
        pos = text.find(needle, start)
        if pos <= 0:
            return pos
        backslashes = 0
        while pos - 1 - backslashes >= 0 and text[pos - 1 - backslashes] == '\\':
            backslashes += 1
        if backslashes % 2 == 0:
            return pos
        start = pos + 1


def _scan_regex_end(text, pos, opening, closing, skip_match_variable=False):
    """Find where a regex (or the replacement of a substitution) that starts
    at `pos` ends, i.e. the index of its closing delimiter, or ``len(text)``.

    Unlike a plain search this steps over backslash escapes and over quoted
    strings, so the delimiter may appear in them: ``rx/ '/tmp/' .* /``."""
    n_chars = len(closing)
    mirrored = opening != closing
    depth = 1
    end = len(text)
    while pos < end:
        char = text[pos]
        if char == '\\':
            pos += 2
            continue
        if char in '\'"':
            close_quote = _find_unescaped(text, char, pos + 1)
            # only a quote that is closed on the same line counts
            if close_quote != -1 and '\n' not in text[pos:close_quote]:
                pos = close_quote + 1
                continue
        if skip_match_variable and char == '$' and text.startswith(closing, pos + 1):
            # "$/" in the replacement of s/.../.../ is the match variable,
            # provided the real delimiter still follows on this line
            line_end = text.find('\n', pos)
            if text.find(closing, pos + 1 + n_chars,
                         end if line_end == -1 else line_end) != -1:
                pos += 1 + n_chars
                continue
        if text.startswith(closing, pos):
            depth -= 1
            if depth == 0:
                return pos
            pos += n_chars
        elif mirrored and text.startswith(opening, pos):
            depth += 1
            pos += n_chars
        else:
            pos += 1
    return end


def _closing_position(text, opening_chars, delimiter_start, escapes=True, regex=False):
    """Return the index of the closing delimiter for a quote-like construct
    whose opening delimiter `opening_chars` starts at `delimiter_start`, or
    ``len(text)`` if it is never closed. Mirrored delimiters nest."""
    n_chars = len(opening_chars)
    closer = RakuLexer.RAKU_BRACKETS.get(opening_chars[0])
    if regex:
        return _scan_regex_end(text, delimiter_start + n_chars, opening_chars,
                               closer * n_chars if closer else opening_chars)

    find = _find_unescaped if escapes else (lambda t, n, i: t.find(n, i))
    if closer is None:  # not a mirrored character: look for the next occurrence
        end_pos = find(text, opening_chars, delimiter_start + n_chars)
        return len(text) if end_pos < 0 else end_pos

    closing_chars = closer * n_chars
    nesting_level = 1
    search_pos = delimiter_start
    while nesting_level > 0:
        next_open_pos = find(text, opening_chars, search_pos + n_chars)
        next_close_pos = find(text, closing_chars, search_pos + n_chars)

        if next_close_pos == -1:
            return len(text)
        elif next_open_pos != -1 and next_open_pos < next_close_pos:
            nesting_level += 1
            search_pos = next_open_pos
        else:
            nesting_level -= 1
            search_pos = next_close_pos
    return next_close_pos


def _extra_heredocs(line):
    """Heredocs opened later on the same line: (terminator, interpolate)."""
    found = []
    opener = re.compile(r"(?<![\w'-])(qq|q|Q)[a-zA-Z]?\s*((?::\w+\s*)+)([^\w\s:])")
    for match in opener.finditer(line):
        if not re.search(r':to\b', match.group(2)):
            continue
        closer = RakuLexer.RAKU_BRACKETS.get(match.group(3), match.group(3))
        end = line.find(closer, match.end())
        if end != -1:
            found.append((line[match.end():end],
                          match.group(1) == 'qq' or ':qq' in match.group(2)))
    return found


class RakuLexer(ExtendedRegexLexer):
    """
    For Raku source code.
    """

    name = 'Raku'
    url = 'https://www.raku.org'
    aliases = ['raku', 'perl6', 'pl6']
    filenames = ['*.pl', '*.pm', '*.nqp', '*.p6', '*.6pl', '*.p6l', '*.pl6',
                 '*.6pm', '*.p6m', '*.pm6', '*.t', '*.raku', '*.rakumod',
                 '*.rakutest', '*.rakudoc']
    mimetypes = ['text/x-raku', 'application/x-raku', 'text/x-perl6',
                 'application/x-perl6']
    version_added = '2.0'
    flags = re.MULTILINE | re.DOTALL

    # Kept for backwards compatibility: one identifier character.
    RAKU_IDENTIFIER_RANGE = _IDENT_CHAR

    # words that declare something: scope, routine and package declarators
    RAKU_DECLARATORS = (
        'anon', 'augment', 'class', 'constant', 'enum', 'grammar',
        'has', 'knowhow', 'macro', 'method', 'module', 'multi', 'my',
        'only', 'our', 'package', 'proto', 'regex', 'role', 'rule',
        'state', 'sub', 'submethod', 'subset', 'supersede', 'token',
        'unit',
    )

    RAKU_NAMESPACE_KEYWORDS = (
        'import', 'need', 'no', 'require', 'use',
    )

    RAKU_KEYWORDS = (
        # Phasers
        'BEGIN', 'CATCH', 'CHECK', 'CLOSE', 'CONTROL', 'DOC', 'END',
        'ENTER', 'FIRST', 'INIT', 'KEEP', 'LAST', 'LEAVE', 'NEXT',
        'POST', 'PRE', 'QUIT', 'UNDO',
        # Control flow, traits and other reserved words
        'also', 'default', 'do', 'does', 'eager', 'else', 'elsif',
        'export', 'for', 'gather', 'given', 'handles', 'hides',
        'hyper', 'if', 'is', 'last', 'lazy', 'let', 'loop', 'made',
        'make', 'native', 'next', 'of', 'once', 'orwith', 'proceed',
        'quietly', 'race', 'react', 'redo', 'repeat', 'repr',
        'required', 'return', 'return-rw', 'returns', 'rw', 'sink',
        'start', 'succeed', 'supply', 'symbol', 'temp', 'trusts',
        'try', 'unless', 'until', 'when', 'whenever', 'where', 'while',
        'will', 'with', 'without',
    )

    # names of the traits that can follow "is"
    RAKU_TRAITS = (
        'DEPRECATED', 'assoc', 'built', 'cached', 'copy', 'default', 'dynamic',
        'equiv', 'export', 'hidden-from-USAGE', 'hidden-from-backtrace',
        'implementation-detail', 'looser', 'native', 'nodal', 'pure', 'raw',
        'readonly', 'repr', 'required', 'rw', 'symbol', 'test-assertion',
        'tighter',
    )

    # Routines that can be called as a plain word: say "hi", map(...). These are
    # builtins wherever they appear. Routines of the Test module are included,
    # as a lexer cannot know whether it has been loaded.
    RAKU_BUILTINS = (
        'abs', 'acos', 'acosec', 'acosech', 'acosh', 'acotan',
        'acotanh', 'all', 'any', 'append', 'asec', 'asech', 'asin',
        'asinh', 'atan', 'atan2', 'atanh', 'atomic-assign',
        'atomic-dec-fetch', 'atomic-fetch', 'atomic-fetch-add',
        'atomic-fetch-dec', 'atomic-fetch-inc', 'atomic-fetch-sub',
        'atomic-inc-fetch', 'await', 'bag', 'bail-out', 'callframe',
        'callsame', 'callwith', 'can-ok', 'cas', 'categorize',
        'ceiling', 'cglobal', 'chars', 'chdir', 'chmod', 'chomp',
        'chop', 'chown', 'chr', 'chrs', 'cis', 'classify', 'close',
        'cmp-ok', 'comb', 'combinations', 'copy', 'cos', 'cosec',
        'cosech', 'cosh', 'cotan', 'cotanh', 'cross', 'dd', 'deepmap',
        'defined', 'diag', 'die', 'dies-ok', 'dir', 'does-ok', 'done',
        'done-testing', 'duckmap', 'elems', 'emit', 'end', 'EVAL',
        'eval-dies-ok', 'eval-lives-ok', 'EVALFILE', 'exit',
        'exits-ok', 'exp', 'explicitly-manage', 'expmod', 'fail',
        'fails-like', 'fc', 'first', 'flat', 'flip', 'floor', 'flunk',
        'full-barrier', 'get', 'getc', 'gist', 'grep', 'hash', 'head',
        'index', 'indir', 'is-approx', 'is-deeply', 'is-prime',
        'isa-ok', 'isnt', 'item', 'join', 'keys', 'kv', 'lastcall',
        'lc', 'like', 'lines', 'link', 'list', 'lives-ok', 'log',
        'log10', 'log2', 'lsb', 'MAIN', 'map', 'max', 'min', 'minmax',
        'mix', 'mkdir', 'move', 'msb', 'nano', 'nativecast',
        'nativesizeof', 'nextcallee', 'nextsame', 'nextwith',
        'nodemap', 'nok', 'none', 'not', 'note', 'now', 'ok', 'one',
        'open', 'ord', 'ords', 'pack', 'pairs', 'parse-base',
        'parse-names', 'pass', 'periods', 'permutations', 'pick',
        'plan', 'pop', 'prepend', 'print', 'printf', 'produce',
        'prompt', 'push', 'put', 'rand', 'reduce', 'rename',
        'repeated', 'repl', 'report', 'reverse', 'rindex', 'rmdir',
        'roll', 'roots', 'rotate', 'round', 'roundrobin', 'run',
        'samecase', 'samemark', 'samewith', 'say', 'sec', 'sech',
        'set', 'shell', 'shift', 'sign', 'signal', 'sin', 'sinh',
        'skip', 'skip-rest', 'sleep', 'sleep-timer', 'sleep-until',
        'slip', 'slurp', 'snap', 'snapper', 'snip', 'snitch', 'so',
        'sort', 'splice', 'split', 'sprintf', 'spurt', 'sqrt',
        'squish', 'srand', 'subbuf-rw', 'substr', 'substr-rw',
        'subtest', 'sum', 'symlink', 'tail', 'take', 'take-rw', 'tan',
        'tanh', 'tc', 'tclc', 'throws-like', 'time', 'todo', 'trim',
        'trim-leading', 'trim-trailing', 'truncate', 'uc', 'undefine',
        'unimatch', 'uniname', 'uninames', 'uniparse', 'uniprop',
        'uniprops', 'unique', 'unival', 'unlike', 'unlink', 'unpack',
        'unpolar', 'unshift', 'USAGE', 'use-ok', 'val', 'values',
        'warn', 'wordcase', 'words', 'zip',
    )

    # Routines that only exist as methods. "name" is the builtin in "$x.name",
    # but on its own it is a word the user chose ("name => 1", "sub name"), so
    # these are only builtins after a dot. Some are also reserved words
    # ("start", "last"): those are keywords on their own and methods after a dot.
    RAKU_METHODS = (
        'abs2rel', 'absolute', 'accept', 'ACCEPTS', 'accessed',
        'acquire', 'act', 'action', 'actions', 'add', 'add_attribute',
        'add_enum_value', 'add_fallback', 'add_method', 'add_parent',
        'add_private_method', 'add_role', 'add_trustee', 'adverb',
        'after', 'allocate', 'allof', 'alternative-names',
        'annotations', 'antipair', 'antipairs', 'anyof',
        'app_lifetime', 'arch', 'archname', 'are', 'args', 'arity',
        'ASSIGN-KEY', 'ASSIGN-POS', 'assuming', 'ast', 'at', 'AT-KEY',
        'AT-POS', 'attributes', 'auth', 'backtrace', 'base',
        'base-repeating', 'basename', 'batch', 'before', 'BIND-KEY',
        'BIND-POS', 'bind-stderr', 'bind-stdin', 'bind-stdout',
        'bind-udp', 'bits', 'bless', 'block', 'bool-only', 'bounds',
        'break', 'Bridge', 'broken', 'BUILD', 'build-date',
        'bytecode-size', 'bytes', 'cache', 'CALL-ME',
        'calling-package', 'can', 'cancel', 'candidates', 'cando',
        'canonpath', 'caps', 'caption', 'capture', 'catdir',
        'categorize-list', 'catfile', 'catpath', 'cause', 'changed',
        'child', 'child-name', 'child-typename', 'chunks',
        'classify-list', 'cleanup', 'clone', 'close-stdin', 'closed',
        'code', 'codes', 'collate', 'column', 'command', 'comment',
        'compiler', 'compose', 'compose_type', 'composer', 'concise',
        'condition', 'config', 'configure_destroy',
        'configure_type_checking', 'conj', 'connect', 'constraints',
        'construct', 'contains', 'contents', 'count', 'count-only',
        'cpu-cores', 'cpu-usage', 'CREATE', 'create_type', 'created',
        'cue', 'curdir', 'curupdir', 'd', 'day', 'day-fraction',
        'day-of-month', 'day-of-week', 'day-of-year', 'daycount',
        'days-in-month', 'days-in-year', 'dd-mm-yyyy', 'declaration',
        'decode', 'decoder', 'default', 'DEFINITE', 'delayed',
        'DELETE-KEY', 'DELETE-POS', 'denominator', 'desc', 'DESTROY',
        'destroyers', 'dev', 'devnull', 'devtype', 'did-you-mean',
        'dir-sep', 'dir-with-entries', 'dirname', 'DISTROnames', 'do',
        'does', 'dynamic', 'e', 'eager', 'earlier', 'enclosing',
        'encode', 'encoder', 'encoding', 'ends-with',
        'enum_from_value', 'enum_value_list', 'enum_values', 'enums',
        'eof', 'err', 'exception', 'excludes-max', 'excludes-min',
        'EXISTS-KEY', 'EXISTS-POS', 'exitcode', 'expected',
        'extension', 'f', 'feature', 'file', 'filename', 'find_method',
        'find_method_qualified', 'finish', 'first-date-in-month',
        'flatmap', 'flush', 'fmt', 'format', 'formatter', 'freeze',
        'from', 'from-list', 'from-loop', 'from-posix', 'from-slurpy',
        'full', 'get_value', 'got', 'grab', 'grabpairs', 'handle',
        'handled', 'handles', 'hardware', 'has_accessor', 'headers',
        'hh-mm-ss', 'hidden', 'hides', 'hour', 'HOW', 'how', 'hyper',
        'id', 'illegal', 'im', 'in', 'in-range', 'in-timezone',
        'indent', 'indices', 'infinite', 'infix', 'inode',
        'install_method_cache', 'instead', 'int-bounds', 'interval',
        'invalid-str', 'invert', 'invocant', 'is-absolute',
        'is-hidden', 'is-implementation-detail', 'is-initial-thread',
        'is-int', 'is-lazy', 'is-leap-year', 'is-relative',
        'is-routine', 'is-setting', 'is-win', 'is_trusted', 'is_type',
        'isa', 'isNaN', 'iterator', 'julian-date', 'keep', 'kept',
        'KERNELnames', 'key', 'keyof', 'kill', 'kxxv', 'l', 'lang',
        'last', 'last-date-in-month', 'later', 'lazy', 'leading',
        'level', 'line', 'listen', 'live', 'local', 'lock', 'lookup',
        'made', 'make', 'match', 'maxpairs', 'merge', 'message',
        'method', 'method_table', 'methods', 'migrate', 'minpairs',
        'minus', 'minute', 'misplaced', 'mm-dd-yyyy', 'mode',
        'modified', 'modified-julian-date', 'modifier', 'month', 'mro',
        'multi', 'multi-invocant', 'multiness', 'my', 'name', 'named',
        'named_names', 'narrow', 'native-descriptor', 'new',
        'new-from-daycount', 'new-from-pairs', 'new_type', 'next',
        'next-handle', 'next-interesting-index', 'nice', 'nl-in',
        'nl-out', 'norm', 'nude', 'numerator', 'of', 'offset',
        'offset-in-hours', 'offset-in-minutes', 'old', 'on-close',
        'on-switch', 'opened', 'operation', 'optional', 'orig',
        'os-error', 'osname', 'out', 'out-buffer', 'outer',
        'outer-caller-idx', 'package', 'package-kind', 'package-name',
        'packages', 'pair', 'pairup', 'parameter', 'params', 'parent',
        'parent-name', 'parents', 'parse', 'parsefile', 'parts',
        'path', 'path-sep', 'payload', 'peer-host', 'peer-port',
        'perl', 'phaser', 'pickpairs', 'pid', 'placeholder', 'plus',
        'polar', 'poll', 'polymod', 'port', 'pos', 'positional',
        'posix', 'postfix', 'postmatch', 'precomp-ext',
        'precomp-target', 'pred', 'prefix', 'prematch', 'print-nl',
        'print-to', 'private', 'private_method_table', 'proc',
        'protect', 'pull-one', 'push-all', 'push-at-least',
        'push-exactly', 'push-until-lazy', 'qualifier-type', 'quit',
        'r', 'race', 'radix', 'raku', 'range', 'raw', 're', 'read',
        'readchars', 'readonly', 'ready', 'reallocate', 'reals',
        'reason', 'rebless', 'receive', 'recv', 'redispatcher', 'redo',
        'rel2abs', 'relative', 'release', 'remove', 'replace-with',
        'replacement', 'REPR', 'reserved', 'resolve', 'restore',
        'result', 'resume', 'rethrow', 'returns', 'right', 'role',
        'roles_to_compose', 'rolish', 'rootdir', 'rotor',
        'routine-type', 'rw', 'rwx', 's', 'schedule-on', 'scheduler',
        'scope', 'second', 'seek', 'send', 'serial', 'set-instruments',
        'set_hidden', 'set_name', 'set_package', 'set_rw', 'set_value',
        'setup_finalization', 'shape', 'share', 'sibling', 'sigil',
        'signals', 'signature', 'sink', 'sink-all', 'skip-at-least',
        'skip-at-least-pull-one', 'skip-one', 'slice', 'slurp-rest',
        'slurpy', 'socket-host', 'socket-port', 'source',
        'source-package', 'spawn', 'SPEC', 'splitdir', 'splitpath',
        'stable', 'start', 'started', 'starts-with', 'status',
        'stderr', 'stdout', 'sub_signature', 'subbuf', 'subname',
        'subparse', 'subst', 'subst-mutate', 'substr-eq', 'succ',
        'suffix', 'summary', 't', 'tap', 'target', 'target-name',
        'tell', 'then', 'throttle', 'throw', 'timezone', 'tmpdir',
        'to', 'to-posix', 'today', 'toggle', 'total', 'trailing',
        'trans', 'tree', 'truncated-to', 'trusts', 'try_acquire',
        'trying', 'twigil', 'type', 'type_captures', 'typename', 'udp',
        'uncaught_handler', 'univals', 'unlock', 'unset', 'unwrap',
        'updir', 'usage-name', 'utc', 'value', 'VAR', 'variable',
        'verbose-config', 'version', 'VMnames', 'volume', 'vow', 'w',
        'wait', 'watch', 'watch-path', 'week', 'week-number',
        'week-year', 'weekday-of-month', 'WHAT', 'when', 'WHERE',
        'WHEREFORE', 'WHICH', 'WHO', 'whole-second', 'WHY',
        'workaround', 'wrap', 'write', 'write-to', 'x', 'yada', 'year',
        'yield', 'yyyy-mm-dd', 'z', 'zip-latest',
    )

    RAKU_BUILTIN_CLASSES = (
        #Classes
        'Any','Array','Associative','AST','atomicint','Attribute','Backtrace',
        'Backtrace::Frame','Bag','Baggy','BagHash','Blob','Block','Bool','Buf',
        'Callable','CallFrame','Cancellation','Capture','CArray','Channel','Code',
        'Complex','ComplexStr','Cool','CurrentThreadScheduler',
        'Date','Dateish','DateTime','Distro','Duration','Encoding',
        'Exception','Failure','FatRat','Grammar','Hash','HyperWhatever','Instant',
        'Int','int16','int32','int64','int8','IntStr','IO','IO::ArgFiles',
        'IO::CatHandle','IO::Handle','IO::Notification','IO::Path',
        'IO::Path::Cygwin','IO::Path::QNX','IO::Path::Unix','IO::Path::Win32',
        'IO::Pipe','IO::Socket','IO::Socket::Async','IO::Socket::INET','IO::Spec',
        'IO::Spec::Cygwin','IO::Spec::QNX','IO::Spec::Unix','IO::Spec::Win32',
        'IO::Special','Iterable','Iterator','Junction','Kernel','Label','List',
        'Lock','Lock::Async','long','longlong','Macro','Map','Match',
        'Metamodel::AttributeContainer','Metamodel::C3MRO','Metamodel::ClassHOW',
        'Metamodel::EnumHOW','Metamodel::Finalization','Metamodel::MethodContainer',
        'Metamodel::MROBasedMethodDispatch','Metamodel::MultipleInheritance',
        'Metamodel::Naming','Metamodel::Primitives','Metamodel::PrivateMethodContainer',
        'Metamodel::RoleContainer','Metamodel::Trusting','Method','Mix','MixHash',
        'Mixy','Mu','NFC','NFD','NFKC','NFKD','Num','num32','num64',
        'Numeric','NumStr','ObjAt','Order','Pair','Parameter','Perl','Pod::Block',
        'Pod::Block::Code','Pod::Block::Comment','Pod::Block::Declarator',
        'Pod::Block::Named','Pod::Block::Para','Pod::Block::Table','Pod::Heading',
        'Pod::Item','Pointer','Positional','PositionalBindFailover','Proc',
        'Proc::Async','Promise','Proxy','PseudoStash','QuantHash','Range','Rat',
        'Rational','RatStr','Real','Regex','Routine','Scalar','Scheduler',
        'Semaphore','Seq','Set','SetHash','Setty','Signature','size_t','Slip',
        'Stash','Str','StrDistance','Stringy','Sub','Submethod','Supplier',
        'Supplier::Preserving','Supply','Systemic','Tap','Telemetry',
        'Telemetry::Instrument::Thread','Telemetry::Instrument::Usage',
        'Telemetry::Period','Telemetry::Sampler','Thread','ThreadPoolScheduler',
        'UInt','uint16','uint32','uint64','uint8','Uni','utf8','Variable',
        'Version','VM','Whatever','WhateverCode',
        # added from the Raku documentation
        'Allomorph', 'Collation', 'CompUnit',
        'CompUnit::PrecompilationRepository', 'CompUnit::Repository',
        'CompUnit::Repository::FileSystem',
        'CompUnit::Repository::Installation',
        'CompUnit::Repository::Unknown', 'Compiler', 'Distribution',
        'Distribution::Hash', 'Distribution::Locally', 'Distribution::Path',
        'Distribution::Resource', 'Encoding::Registry', 'Endian',
        'Enumeration', 'ForeignCode', 'Format', 'Formatter', 'HyperSeq',
        'IO::Notification::Change', 'IO::Path::Parts',
        'IO::Socket::Async::ListenSocket', 'IterationBuffer',
        'Lock::ConditionVariable', 'Metamodel::ConcreteRoleHOW',
        'Metamodel::CurriedRoleHOW', 'Metamodel::DefiniteHOW',
        'Metamodel::Documenting', 'Metamodel::MethodDelegation',
        'Metamodel::Mixins', 'Metamodel::PackageHOW',
        'Metamodel::ParametricRoleGroupHOW', 'Metamodel::ParametricRoleHOW',
        'Metamodel::RolePunning', 'Metamodel::Stashing',
        'Metamodel::TypePretense', 'Metamodel::Versioning', 'Pod::Defn',
        'Pod::FormattingCode', 'PredictiveIterator', 'PromiseStatus',
        'RaceSeq', 'Raku', 'RakuAST', 'Routine::WrapHandle', 'Sequence', 'Signal',
        'Telemetry::Instrument::ThreadPool', 'Unicode', 'ValueObjAt'
        # native types
        'array', 'blob8', 'blob16', 'blob32', 'blob64', 'buf8',
        'buf16', 'buf32', 'buf64', 'byte', 'int', 'num', 'str', 'uint',
    )

    RAKU_WORD_OPERATORS = (
        'after', 'and', 'andthen', 'before', 'but', 'cmp', 'coll',
        'div', 'eq', 'eqv', 'ff', 'fff', 'gcd', 'ge', 'gt', 'lcm',
        'le', 'leg', 'lt', 'max', 'min', 'minmax', 'mod', 'ne', 'not',
        'notandthen', 'or', 'orelse', 'R', 'so', 'unicmp', 'x', 'X',
        'xor', 'xx', 'Z',
    )

    RAKU_CONSTANTS = (
        '\u03c0', '\u03c4', '\u221e', '\U0001d452', 'pi', 'tau', 'Inf', 'NaN', 'e', 'i',
    )

    # Symbolic operators, including the Unicode synonyms. They are sorted
    # longest-first when the regex is built so that e.g. '==>' wins over '=='.
    RAKU_SYMBOL_OPERATORS = (
        '...', '\u2026', '<=>', '==>>', '<<==', '==>', '<==', '===', '=:=', '=~=',
        '!~~', '~~', '::=', ':=', '^..^', '..^', '^..', '..', '**', '++', '--',
        '&&', '||', '//', '^^', '??', '!!', '==', '!=', '<=', '>=', '=>', '%%',
        '+&', '+|', '+^', '+<', '+>', '~&', '~|', '~^', '~<', '~>', '?&', '?|',
        '?^', '-->', '<->', '->', '!', '?', '+', '-', '*', '/', '%', '~', '|',
        '&', '^', '<', '>', '=', '.',
        # Unicode operators
        '\u2208', '\u2209', '\u220b', '\u220c', '\u2282', '\u2284', '\u2283',
        '\u2285', '\u2286', '\u2288', '\u2287', '\u2289', '\u227c', '\u227d',
        '\u222a', '\u2229', '\u2216', '\u2296', '\u228d', '\u228e', '\u2264',
        '\u2265', '\u2260', '\u2245', '\u00d7', '\u00f7', '\u2212', '\u2218',
        '\u269b', '\u2261', '\u2262', '\u2a76', '\u2a75', '\u220a', '\u220d',
    )

    # After a dot every builtin routine is a builtin, and so is the name of a
    # type: "$x.Int" calls the coercion method, it does not name the type.
    _method_builtins = tuple(sorted(
        set(RAKU_BUILTINS) | set(RAKU_METHODS) | set(RAKU_BUILTIN_CLASSES)))
    _builtin_class_alternatives = '|'.join(re.escape(c) for c in sorted(
        RAKU_BUILTIN_CLASSES, key=len, reverse=True))

    # rules available in every grammar: <ws>, <alpha>, <before ...>, ...
    RAKU_REGEX_BUILTINS = (
        'alnum', 'alpha', 'after', 'at', 'before', 'blank', 'cntrl', 'digit',
        'graph', 'ident', 'lower', 'print', 'punct', 'same',
        'space', 'upper', 'wb', 'ws', 'ww', 'xdigit',
    )

    # Raku has a *lot* of possible bracketing characters
    # this list was lifted from STD.pm6 (https://github.com/perl6/std)
    RAKU_BRACKETS = {
        '\u0028': '\u0029', '\u003c': '\u003e', '\u005b': '\u005d',
        '\u007b': '\u007d', '\u00ab': '\u00bb', '\u0f3a': '\u0f3b',
        '\u0f3c': '\u0f3d', '\u169b': '\u169c', '\u2018': '\u2019',
        '\u201a': '\u2019', '\u201b': '\u2019', '\u201c': '\u201d',
        '\u201e': '\u201d', '\u201f': '\u201d', '\u2039': '\u203a',
        '\u2045': '\u2046', '\u207d': '\u207e', '\u208d': '\u208e',
        '\u2208': '\u220b', '\u2209': '\u220c', '\u220a': '\u220d',
        '\u2215': '\u29f5', '\u223c': '\u223d', '\u2243': '\u22cd',
        '\u2252': '\u2253', '\u2254': '\u2255', '\u2264': '\u2265',
        '\u2266': '\u2267', '\u2268': '\u2269', '\u226a': '\u226b',
        '\u226e': '\u226f', '\u2270': '\u2271', '\u2272': '\u2273',
        '\u2274': '\u2275', '\u2276': '\u2277', '\u2278': '\u2279',
        '\u227a': '\u227b', '\u227c': '\u227d', '\u227e': '\u227f',
        '\u2280': '\u2281', '\u2282': '\u2283', '\u2284': '\u2285',
        '\u2286': '\u2287', '\u2288': '\u2289', '\u228a': '\u228b',
        '\u228f': '\u2290', '\u2291': '\u2292', '\u2298': '\u29b8',
        '\u22a2': '\u22a3', '\u22a6': '\u2ade', '\u22a8': '\u2ae4',
        '\u22a9': '\u2ae3', '\u22ab': '\u2ae5', '\u22b0': '\u22b1',
        '\u22b2': '\u22b3', '\u22b4': '\u22b5', '\u22b6': '\u22b7',
        '\u22c9': '\u22ca', '\u22cb': '\u22cc', '\u22d0': '\u22d1',
        '\u22d6': '\u22d7', '\u22d8': '\u22d9', '\u22da': '\u22db',
        '\u22dc': '\u22dd', '\u22de': '\u22df', '\u22e0': '\u22e1',
        '\u22e2': '\u22e3', '\u22e4': '\u22e5', '\u22e6': '\u22e7',
        '\u22e8': '\u22e9', '\u22ea': '\u22eb', '\u22ec': '\u22ed',
        '\u22f0': '\u22f1', '\u22f2': '\u22fa', '\u22f3': '\u22fb',
        '\u22f4': '\u22fc', '\u22f6': '\u22fd', '\u22f7': '\u22fe',
        '\u2308': '\u2309', '\u230a': '\u230b', '\u2329': '\u232a',
        '\u23b4': '\u23b5', '\u2768': '\u2769', '\u276a': '\u276b',
        '\u276c': '\u276d', '\u276e': '\u276f', '\u2770': '\u2771',
        '\u2772': '\u2773', '\u2774': '\u2775', '\u27c3': '\u27c4',
        '\u27c5': '\u27c6', '\u27d5': '\u27d6', '\u27dd': '\u27de',
        '\u27e2': '\u27e3', '\u27e4': '\u27e5', '\u27e6': '\u27e7',
        '\u27e8': '\u27e9', '\u27ea': '\u27eb', '\u2983': '\u2984',
        '\u2985': '\u2986', '\u2987': '\u2988', '\u2989': '\u298a',
        '\u298b': '\u298c', '\u298d': '\u298e', '\u298f': '\u2990',
        '\u2991': '\u2992', '\u2993': '\u2994', '\u2995': '\u2996',
        '\u2997': '\u2998', '\u29c0': '\u29c1', '\u29c4': '\u29c5',
        '\u29cf': '\u29d0', '\u29d1': '\u29d2', '\u29d4': '\u29d5',
        '\u29d8': '\u29d9', '\u29da': '\u29db', '\u29f8': '\u29f9',
        '\u29fc': '\u29fd', '\u2a2b': '\u2a2c', '\u2a2d': '\u2a2e',
        '\u2a34': '\u2a35', '\u2a3c': '\u2a3d', '\u2a64': '\u2a65',
        '\u2a79': '\u2a7a', '\u2a7d': '\u2a7e', '\u2a7f': '\u2a80',
        '\u2a81': '\u2a82', '\u2a83': '\u2a84', '\u2a8b': '\u2a8c',
        '\u2a91': '\u2a92', '\u2a93': '\u2a94', '\u2a95': '\u2a96',
        '\u2a97': '\u2a98', '\u2a99': '\u2a9a', '\u2a9b': '\u2a9c',
        '\u2aa1': '\u2aa2', '\u2aa6': '\u2aa7', '\u2aa8': '\u2aa9',
        '\u2aaa': '\u2aab', '\u2aac': '\u2aad', '\u2aaf': '\u2ab0',
        '\u2ab3': '\u2ab4', '\u2abb': '\u2abc', '\u2abd': '\u2abe',
        '\u2abf': '\u2ac0', '\u2ac1': '\u2ac2', '\u2ac3': '\u2ac4',
        '\u2ac5': '\u2ac6', '\u2acd': '\u2ace', '\u2acf': '\u2ad0',
        '\u2ad1': '\u2ad2', '\u2ad3': '\u2ad4', '\u2ad5': '\u2ad6',
        '\u2aec': '\u2aed', '\u2af7': '\u2af8', '\u2af9': '\u2afa',
        '\u2e02': '\u2e03', '\u2e04': '\u2e05', '\u2e09': '\u2e0a',
        '\u2e0c': '\u2e0d', '\u2e1c': '\u2e1d', '\u2e20': '\u2e21',
        '\u3008': '\u3009', '\u300a': '\u300b', '\u300c': '\u300d',
        '\u300e': '\u300f', '\u3010': '\u3011', '\u3014': '\u3015',
        '\u3016': '\u3017', '\u3018': '\u3019', '\u301a': '\u301b',
        '\u301d': '\u301e', '\ufd3e': '\ufd3f', '\ufe17': '\ufe18',
        '\ufe35': '\ufe36', '\ufe37': '\ufe38', '\ufe39': '\ufe3a',
        '\ufe3b': '\ufe3c', '\ufe3d': '\ufe3e', '\ufe3f': '\ufe40',
        '\ufe41': '\ufe42', '\ufe43': '\ufe44', '\ufe47': '\ufe48',
        '\ufe59': '\ufe5a', '\ufe5b': '\ufe5c', '\ufe5d': '\ufe5e',
        '\uff08': '\uff09', '\uff1c': '\uff1e', '\uff3b': '\uff3d',
        '\uff5b': '\uff5d', '\uff5f': '\uff60', '\uff62': '\uff63',
    }

    def _build_word_match(words, boundary_regex_fragment=None, prefix='', suffix=''):
        if boundary_regex_fragment is None:
            return r'\b(' + prefix + r'|'.join(re.escape(x) for x in words) + \
                suffix + r')\b'
        else:
            return r'(?<!' + boundary_regex_fragment + r')' + prefix + r'(' + \
                r'|'.join(re.escape(x) for x in words) + r')' + suffix + r'(?!' + \
                _IDENT_END + r')'

    def brackets_callback(token_class):
        def callback(lexer, match, context):
            groups = match.groupdict()
            opening_chars = groups['delimiter']
            n_chars = len(opening_chars)
            adverbs = groups.get('adverbs')

            text = context.text
            start = match.start()
            if groups.get('ws'):
                yield start, Whitespace, groups['ws']
                start = match.end('ws')
            body_start = match.start('delimiter') + n_chars
            # comments and names have no escape sequences
            end_pos = _closing_position(
                text, opening_chars, match.start('delimiter'),
                escapes=token_class not in (Comment.Multiline, Name),
                regex=token_class is String.Regex)

            # `qq` strings (and the :qq / :c adverbs) interpolate.
            interpolate = groups.get('qtype') == 'qq' or \
                (adverbs is not None and re.search(r':(?:qq|c)\b', adverbs))

            if adverbs is not None and re.search(r':to\b', adverbs):
                # Heredoc: the body starts on the line *after* the one holding
                # the opening quote, and the rest of that line is ordinary code.
                delim_end = min(end_pos + n_chars, len(text))
                line_end = text.find('\n', delim_end)

                yield start, token_class, text[start:delim_end]
                if line_end == -1:
                    yield from _sublex(lexer, text[delim_end:], delim_end)
                    context.pos = len(text)
                    return

                rest_of_line = text[delim_end:line_end]
                yield from _sublex(lexer, rest_of_line, delim_end)
                yield line_end, Whitespace, '\n'

                # the bodies follow in the order their heredocs were opened
                heredocs = [(text[body_start:end_pos], interpolate)]
                heredocs += _extra_heredocs(rest_of_line)
                pos = line_end + 1
                for number, (terminator, interpolates) in enumerate(heredocs):
                    end_heredoc = re.compile(
                        r'^[ \t]*' + re.escape(terminator) + r'[ \t]*$',
                        re.MULTILINE).search(text, pos)
                    body_end = end_heredoc.start() if end_heredoc else len(text)
                    body = text[pos:body_end]
                    if interpolates:
                        yield from _sublex(lexer, body, pos, 'interpolated')
                    elif body:
                        yield pos, token_class, body
                    if not end_heredoc:
                        pos = len(text)
                        break
                    yield end_heredoc.start(), token_class, end_heredoc.group()
                    pos = end_heredoc.end()
                    if number + 1 < len(heredocs) and text[pos:pos + 1] == '\n':
                        yield pos, Whitespace, '\n'
                        pos += 1
                context.pos = pos
            elif token_class is String.Regex:
                yield start, token_class, text[start:body_start]
                yield from _sublex(lexer, text[body_start:end_pos], body_start,
                                   'regex-body')
                if text[end_pos:end_pos + n_chars]:
                    yield end_pos, token_class, text[end_pos:end_pos + n_chars]
                context.pos = end_pos + n_chars
            elif interpolate:
                yield start, token_class, text[start:body_start]
                yield from _sublex(lexer, text[body_start:end_pos], body_start,
                                   'interpolated')
                if text[end_pos:end_pos + n_chars]:
                    yield end_pos, token_class, text[end_pos:end_pos + n_chars]
                context.pos = end_pos + n_chars
            else:
                yield start, token_class, text[start:end_pos + n_chars]
                context.pos = end_pos + n_chars

        return callback

    def subst_callback(lexer, match, context):
        """s/pattern/replacement/, s{pattern}{replacement}, tr/from/to/, ..."""
        groups = match.groupdict()
        text = context.text
        is_tr = groups['kind'].lower() == 'tr'
        opening_chars = groups['delimiter']
        n_chars = len(opening_chars)
        closer = RakuLexer.RAKU_BRACKETS.get(opening_chars[0])
        closing_chars = closer * n_chars if closer else opening_chars

        # first part: the pattern (or the characters to translate)
        pattern_start = match.start('delimiter') + n_chars
        pattern_end = _scan_regex_end(text, pattern_start, opening_chars,
                                      closing_chars)
        yield match.start(), String.Regex, text[match.start():pattern_start]
        if is_tr:
            if pattern_end > pattern_start:
                yield pattern_start, String.Regex, text[pattern_start:pattern_end]
        else:
            yield from _sublex(lexer, text[pattern_start:pattern_end],
                               pattern_start, 'regex-body')
        pos = pattern_end
        if text[pos:pos + n_chars]:
            yield pos, String.Regex, text[pos:pos + n_chars]
        pos = min(pos + n_chars, len(text))

        # second part: the replacement
        if closer is None:
            # s/a/b/ : the replacement follows directly and uses the same delimiter
            repl_start = pos
            repl_end = _scan_regex_end(text, repl_start, opening_chars,
                                       closing_chars, skip_match_variable=True)
            has_repl = True
        else:
            # s{a}{b} : another bracketed group, possibly after whitespace;
            # s{a} = b : an ordinary assignment, not part of this token
            gap = re.compile(r'\s*').match(text, pos).end()
            has_repl = gap < len(text) and text[gap] in RakuLexer.RAKU_BRACKETS
            if has_repl:
                if gap > pos:
                    yield pos, Whitespace, text[pos:gap]
                open2 = re.compile(
                    '(?:' + re.escape(text[gap]) + ')+').match(text, gap).group()
                yield gap, String.Regex, open2
                repl_start = gap + len(open2)
                n_chars = len(open2)
                closing_chars = RakuLexer.RAKU_BRACKETS[open2[0]] * n_chars
                repl_end = _scan_regex_end(text, repl_start, open2, closing_chars)
        if has_repl:
            if is_tr:
                if repl_end > repl_start:
                    yield repl_start, String.Regex, text[repl_start:repl_end]
            else:
                yield from _sublex(lexer, text[repl_start:repl_end],
                                   repl_start, 'interpolated')
            if text[repl_end:repl_end + n_chars]:
                yield repl_end, String.Regex, text[repl_end:repl_end + n_chars]
            pos = min(repl_end + n_chars, len(text))
        context.pos = pos

    def pod_callback(lexer, match, context):
        yield from _sublex(lexer, match.group(), match.start(), 'pod-body')
        context.pos = match.end()

    # Code embedded in something else (a closure inside a string, or code
    # inside a regex) is lexed in the 'embedded' state, which keeps count of
    # its own braces so it knows when to hand control back to the caller.
    def embedded_open_callback(lexer, match, context):
        if not hasattr(context, 'raku_brace_levels'):
            context.raku_brace_levels = []
        context.raku_brace_levels.append(1)
        yield match.start(), Punctuation, match.group()
        context.pos = match.end()
        context.stack.append('embedded')

    def opening_brace_callback(lexer, match, context):
        context.raku_brace_levels[-1] += 1
        yield match.start(), Punctuation, match.group()
        context.pos = match.end()

    def closing_brace_callback(lexer, match, context):
        yield match.start(), Punctuation, match.group()
        context.pos = match.end()
        context.raku_brace_levels[-1] -= 1
        if context.raku_brace_levels[-1] == 0:
            context.raku_brace_levels.pop()
            context.stack.pop()

    # --- building blocks for the rules below

    _nondigit = r'(?![' + _SUPERSCRIPTS + r'])[^\W\d]'
    _word = r'(?:(?![' + _SUPERSCRIPTS + r'])\w)'
    # an identifier; a hyphen or apostrophe must be followed by a letter
    _ident = _nondigit + _word + r"*(?:['\-]" + _nondigit + _word + r'*)*'
    _qualified_ident = _ident + r'(?:::' + _ident + r')*'
    # the operator part of a routine name: infix:<+>, circumfix:«[ ]», foo:sym<bar>
    _op_name_suffix = r'(?::(?:sym)?(?:<[^>\n]+>|«[^»\n]+»|\[[^\]\n]+\]))'
    _routine_name = r'[!^]?' + _qualified_ident + _op_name_suffix + r'?'
    _op_categories = r'(?:infix|prefix|postfix|circumfix|postcircumfix|term|trait_mod)'
    _type_name = r'[A-Z]' + _word + r"*(?:['\-]" + _nondigit + _word + r'*)*(?:::' + \
        _ident + r')*'
    # type smileys: Int:D, Foo:U, Any:_
    _smiley = r"(?::[UD_](?![\w'\-]))"
    _not_a_builtin_type = r'(?!(?:' + _builtin_class_alternatives + \
        r'|True|False|Nil)' + _smiley + r'?(?!' + _IDENT_END + r'))'
    _after_dot = r'(?:(?<=\.)(?<!\.\.)|(?<=\.[\^?&+*]))'
    _subscript = r'(?:\[[^\]\n]*\]|\{[^}\n]*\}|<[^>\n]*>)'
    # %h<key>, %h<<$key>>, %h«key»; kept to one line so that a stray '<' in
    # code can't swallow what follows it
    _angle_subscripts = r'(?:<<[^>\n]*>>|<[^>\n]*>|«[^»\n]*»)*'
    # Where a "/" starts a regex rather than being a division: after an
    # operator or an opening bracket, or after a word that takes a term.
    _regex_position = r'(?:(?<=[=(,{\[;:!~|&?])|(?<==>)|' + '|'.join(
        r'(?<=\b' + word + r' )' for word in (
            'say', 'put', 'print', 'when', 'if', 'elsif', 'unless', 'while',
            'until', 'given', 'with', 'without', 'and', 'or', 'not', 'so',
            'return', 'grep', 'map', 'first', 'split', 'comb', 'match',
            'subst', 'contains', 'ff', 'fff', 'xor', 'andthen', 'orelse')) + r')'

    _symbol_operators = '|'.join(re.escape(op) for op in sorted(
        RAKU_SYMBOL_OPERATORS, key=len, reverse=True))

    # If you're modifying these rules, be careful if you need to process '{' or '}'
    # characters. We have special logic for processing these characters (due to the fact
    # that you can nest Raku code in strings and regex blocks), so if you need to
    # process one of them, make sure you also process the corresponding one!
    tokens = {
        'common': [
            # --- comments and Pod
            (r'#[`|=](?P<delimiter>(?P<first_char>[' + ''.join(RAKU_BRACKETS) + r'])(?P=first_char)*)',
             brackets_callback(Comment.Multiline)),
            (r'#[|=][^\n]*$', Comment.Special),
            (r'#[^\n]*$', Comment.Single),
            (r'^=finish\b.*', pod_callback),
            (r'^(\s*)=begin\s+(\w+)\b.*?^\1=end\s+\2', pod_callback),
            (r'^(\s*)=for.*?\n\s*?\n', pod_callback),
            (r'^=.*?\n\s*?\n', pod_callback),

            # --- regex declarations. "token", "rule" and "regex" are ordinary
            # words in "$x.rule", "rule => 1" or "/regex/", so only take them
            # as declarators when a name or a block follows.
            (r"(?<![\w'.:$@%&/<\-])(regex|token|rule)(\s+)(" + _ident + r':sym)',
             bygroups(Keyword.Declaration, Whitespace, Name.Function), 'token-sym-brackets'),
            (r"(?<![\w'.:$@%&/<\-])(regex|token|rule)(?=\s+[^\W\d]|\s*\{)(?!\s+\w+\s*=>)(\s*)(" +
             _qualified_ident + r')?',
             bygroups(Keyword.Declaration, Whitespace, Name.Function), 'pre-token'),
            # deal with a special case in the Raku grammar (role q { ... })
            (r'(role)(\s+)(q)(\s*)',
             bygroups(Keyword.Declaration, Whitespace, Name, Whitespace)),

            # --- quote-like constructs; before the keyword and builtin rules,
            # which would otherwise take s, m, q, ... for words
            (r"(?<![\w'-])(?P<qtype>qq|q|Q)[a-zA-Z]?\s*(?P<adverbs>:[\w\s:]+)?\s*(?P<delimiter>(?P<first_char>[^0-9a-zA-Z:\s=,;)])"
             r'(?P=first_char)*)', brackets_callback(String)),
            (r"(?<![\w'-])(?:m|ms|rx)\s*(?P<adverbs>:[\w\s:]+)?\s*(?P<delimiter>(?P<first_char>[^\w:\s=,;)])"
             r'(?P=first_char)*)', brackets_callback(String.Regex)),
            # substitution and transliteration: s/a/b/, S{a}{b}, s:2nd/a/b/, tr/a-z/A-Z/
            (r"(?<![\w'-])(?P<kind>ss|s|SS|S|tr|TR)(?=\s*:!?\w)\s*(?P<adverbs>(?::!?[\w-]+(?:\([^)\n]*\))?\s*)+)"
             r'(?P<delimiter>(?P<first_char>[^\w:\s$@%&=,;)])(?P=first_char)*)', subst_callback),
            (r"(?<![\w'-])(?P<kind>ss|s|SS|S|tr|TR)\s*"
             r'(?P<delimiter>(?P<first_char>[/{(\[|!^~@%])(?P=first_char)*)', subst_callback),
            # a regex without m or rx: $s ~~ /x/, .subst(/x/, ''), say /x/
            (_regex_position + r'(?P<ws>\s*)(?P<delimiter>/)(?!/)(?=(?:\\.|[^/\\\n])*/)',
             brackets_callback(String.Regex)),
            # curly and corner quotes: ‘raw’, “interpolating”, ｢no escapes｣
            (r'[\u2018\u201a][^\u2018\u2019]*[\u2019\u2018]', String.Single),
            (r'\uff62[^\uff63]*\uff63', String),
            (r'[\u201c\u201e]', String.Double, 'dq-curly'),

            # --- names whose meaning depends on where they are
            # method calls: .say, .Int, .^name, .?foo
            (_after_dot + _build_word_match(_method_builtins, _IDENT_CHAR), Name.Builtin),
            (_after_dot + _ident, Name.Function),
            # the key of a pair is a plain word, whatever it spells: name => 1
            (r'(?<!' + _IDENT_CHAR + r')' + _qualified_ident + r'(?=\s*=>)', Name),
            # operators used by name: infix:<+>(1, 2)
            (r'(?<!' + _IDENT_CHAR + r')' + _op_categories + _op_name_suffix, Name.Function),
            # traits: is rw, is copy, is export
            (r'(?<!' + _IDENT_CHAR + r')(is)(\s+)' +
             _build_word_match(RAKU_TRAITS, _IDENT_CHAR), bygroups(Keyword, Whitespace, Keyword)),
            # user-defined types: "is Foo", "of Foo", "--> Foo"
            (r'(?<!' + _IDENT_CHAR + r')(is|does|of|returns|handles|trusts|hides)(\s+)' +
             _not_a_builtin_type + r'(' + _type_name + _smiley + r'?)(?!' + _IDENT_END + r')',
             bygroups(Keyword, Whitespace, Name.Class)),
            (r'(-->)(\s*)' + _not_a_builtin_type + r'(' + _type_name + _smiley +
             r'?)(?!' + _IDENT_END + r')',
             bygroups(Operator, Whitespace, Name.Class)),
            # version literals: use v6.d; use v6.e.PREVIEW; use v6.d+;
            (r"(?<![\w'-])(use|need|require)(\s+)(v\d+(?:\.(?:\d+|\*|[A-Za-z]+))*\+?)(?![\w'\-])",
             bygroups(Keyword.Namespace, Whitespace, Number)),
            # module names: use Foo::Bar; need Baz; (pragmas such as 'use lib' too)
            (r"(?<![\w'-])(use|need|require|import|no)(\s+)(" + _qualified_ident +
             r')(?!' + _IDENT_END + r')',
             bygroups(Keyword.Namespace, Whitespace, Name.Namespace)),

            # --- declarations
            (_build_word_match(('class', 'role', 'grammar', 'module', 'package',
                                'knowhow', 'enum', 'subset'),
                               _IDENT_CHAR) + r'(\s+)(' + _qualified_ident + r')',
             bygroups(Keyword.Declaration, Whitespace, Name.Class)),
            (_build_word_match(('sub', 'method', 'submethod', 'macro'),
                               _IDENT_CHAR) + r'(\s+)(' + _routine_name + r')',
             bygroups(Keyword.Declaration, Whitespace, Name.Function)),
            # "sub" is optional after multi, proto and only: multi foo(Int $x) { }
            (_build_word_match(('multi', 'proto', 'only'), _IDENT_CHAR) +
             r'(\s+)(?!(?:sub|method|submethod|token|rule|regex|macro)(?!' + _IDENT_END +
             r'))(' + _routine_name + r')(?=\s*[({])',
             bygroups(Keyword.Declaration, Whitespace, Name.Function)),

            # --- keywords and other reserved words
            (_build_word_match(RAKU_DECLARATORS, _IDENT_CHAR), Keyword.Declaration),
            (_build_word_match(RAKU_NAMESPACE_KEYWORDS, _IDENT_CHAR), Keyword.Namespace),
            (_build_word_match(RAKU_KEYWORDS, _IDENT_CHAR), Keyword),
            (_build_word_match(('True', 'False', 'Nil'), _IDENT_CHAR), Keyword.Constant),
            (_build_word_match(('self',), _IDENT_CHAR), Name.Builtin.Pseudo),
            (_build_word_match(RAKU_CONSTANTS, _IDENT_CHAR), Name.Constant),
            (r'[\u221e\u2205]', Name.Constant),
            # version literals: v6.d, v1.2.3, v1.2+
            (r"(?<!" + _IDENT_CHAR + r")v\d+(?:\.(?:\d+|\*|[a-z]))*\+?(?!" + _IDENT_END + r")", Number),
            # meta operators: Z+, X~, R-, Z=>, Rcmp, and x=, xx=
            (r"(?<![\w'-])[RXZ](?:\*\*|//|&&|\|\||<=>|=>|==|!=|<=|>=|~~|[-+*/%~,&|^?<>]|"
             r"(?:cmp|eq|ne|lt|gt|le|ge|leg|eqv|min|max|div|mod|and|or|xor|x|xx)(?![\w'-]))",
             Operator),
            (r"(?<![\w'-])(?:xx?|min|max)=(?![=~>])", Operator),
            (_build_word_match(RAKU_WORD_OPERATORS, _IDENT_CHAR, suffix=r'(?!\()'),
             Operator.Word),

            # --- types and routines that come with the language
            # exception and AST class families: X::AdHoc, CX::Done, RakuAST::Name...
            (r"(?<![\w':-])(?:X|CX|RakuAST)(?:::[A-Za-z][\w'-]*)+" + _smiley + r"?(?![\w'-])",
             Keyword.Type),
            (_build_word_match(RAKU_BUILTIN_CLASSES, _IDENT_CHAR, suffix=_smiley + r'?'),
             Keyword.Type),
            (_build_word_match(RAKU_BUILTINS, _IDENT_CHAR), Name.Builtin),
            # a user-defined type: with a smiley, or constraining a variable
            (r'(?<!' + _IDENT_CHAR + r')' + _type_name + _smiley, Name.Class),
            (r'(?<!' + _IDENT_CHAR + r')' + _type_name + r'(?=\s+[$@%&\\])', Name.Class),
            (r'(?<=[\w>])' + _smiley, Keyword.Type),

            # --- variables
            # attributes ($!x, $.x) and compile-time / pod variables ($?FILE, $=pod)
            (r'[$@%&][.!]' + _qualified_ident + _angle_subscripts, Name.Variable.Instance),
            (r'[$@%&][?=]' + _ident + _angle_subscripts, Name.Variable.Magic),
            (r'::\?\w+', Name.Variable.Global),
            (r'[$@%&]\*' + _qualified_ident + _angle_subscripts, Name.Variable.Global),
            (r'\$[!/\u00a2]' + _angle_subscripts, Name.Variable.Global),
            (r'&' + _op_categories + _op_name_suffix, Name.Variable),
            (r'[$@%&][\^:~]?(?:::)?(?:' + _qualified_ident + r'|\d+)' + _angle_subscripts,
             Name.Variable),
            (r'\$(?:<[^>\n]*>)+', Name.Variable),
            # anonymous variables: "state $ = 0", "$++"
            (r'[$@](?=[\s=;,)\]]|\+\+|--)', Name.Variable.Anonymous),
            (r'%(?=[,)])', Name.Variable.Anonymous),
            # contextualizers: $(...), @(...), @$x, $@a
            (r'[$@](?=[(\[{])', Operator),
            (r'[$@%&](?=[$@%&][\w.!*?^])', Operator),
            # sigilless variables (\x), capture literals \(1, 2) and unspace
            (r'\\' + _ident, Name.Variable),
            (r'\\(?=[\s(])', Operator),
            # type captures: ::T
            (r'::' + _ident, Name.Class),

            # --- numbers
            (r'0x[0-9A-Fa-f]+(_[0-9A-Fa-f]+)*', Number.Hex),
            (r'0o[0-7]+(_[0-7]+)*', Number.Oct),
            (r'0b[01]+(_[01]+)*', Number.Bin),
            (r'0d\d+(_\d+)*', Number.Integer),
            # radix literals: :16<FF>
            (r'(?i):\d+<[0-9a-z_.]+>', Number),
            # imaginary numbers
            (r'(?i)(?:\d+(?:_\d+)*(?:\.\d+(?:_\d+)*)?|\.\d+(?:_\d+)*)(?:e[+-]?\d+)?i(?![\w\'-])',
             Number.Float),
            (r'(?i)(?:\d+(?:_\d+)*)?\.\d+(?:_\d+)*(?:e[+-]?\d+)?', Number.Float),
            (r'(?i)\d+(?:_\d+)*e[+-]?\d+', Number.Float),
            (r'\d+(_\d+)*', Number.Integer),
            # rational and complex literals: <1/3>, <1+2i>
            (r'<[-+]?\d+/\d+>', Number),
            (r'<[-+]?[\d.]+[-+][\d.]+i>', Number),

            # --- operators that could be mistaken for quotes
            # hyper operators: »+«, >>+<<, +« (prefix), »++ and ».say (postfix)
            (r'(?:«|»|<<|>>)[-+*/%~=!&|^?<>]+(?:«|»|<<|>>)', Operator),
            (r'[-+*/%~!?|^]+«', Operator),
            # ... and around a bracketed operator: «[op]«
            (r'[«»]\[[^\]\n]+\][«»]|>>\[[^\]\n]+\]<<', Operator),
            (r'(?:»|>>)(?=\.[\w^?!(]|\+\+|--|[\[{<(])', Operator),
            # reduction operators: [+], [\*], [<=], [max], [Z+]
            (r"(?<![\w\])}>'\-])\[\\?(?:[RXZ]?[-+*/%~=<>!&|^?,]+|"
             r'[RXZ]?(?:min|max|gcd|lcm|and|or|xor|x|xx|cmp|eq|ne|lt|gt|le|ge|leg|eqv))\]',
             Operator),
            # quote-like words with interpolation: «a $b» and <<a $b>>. After
            # an operator or a term these are hyper operators, not quotes.
            (r'(?<![\w\])}>+\-*/%~!?^|&.])«', String.Double, 'ww-guillemets'),
            (r'(?<![\w\])}>])<<(?!=)', String.Double, 'ww-angles'),
            (r'(?!<->)<[^\s=<>{};()](?:[^<>{};()]*[^\s<>{};()])?>', String),

            # --- labels, pairs and operators
            # loop labels: OUTER: for ...
            (r"(?<![\w'-])([A-Z][A-Z0-9_]*)(:)(?=\s*(?:for|while|until|loop|repeat|given|if|unless|do|\{|$))",
             bygroups(Name.Label, Punctuation)),
            # colon-pairs and adverbs: :name, :!flag, :name(...), :2nd
            (r'(:!?)(' + _ident + r')', bygroups(Punctuation, Name.Attribute)),
            (r'(:)(\d+)(' + _ident + r')',
             bygroups(Punctuation, Number.Integer, Name.Attribute)),
            # superscript exponents: $x², A⁻¹
            (r'[' + _SUPERSCRIPTS + r']+', Operator),
            # a prefix operator in front of a function reference: ~&foo, +&bar
            (r'[-~+?!|^](?=&[^\W\d])', Operator),
            # assignment meta operators: +=, //=, ||=, ...
            (r'(?:\*\*|//|\|\||&&|%%|[-+*/%~|&^?])=(?![=~>])', Operator),
            (_symbol_operators, Operator),
            (_qualified_ident, Name),
            (r"'(\\\\|\\[^\\]|[^'\\])*'", String.Single),
            (r'"', String.Double, 'dq-string'),
            (r'[;,:()\[\]]', Punctuation),
        ],
        'root': [
            include('common'),
            (r'[{}]', Punctuation),
            (r'\s+', Whitespace),
            # anything else that is not a letter: user-defined operators, ...
            (r'[^\w\s]', Operator),
            (r'.+?', Text),
        ],
        'embedded': [
            include('common'),
            (r'\{', opening_brace_callback),
            (r'\}', closing_brace_callback),
            (r'\s+', Whitespace),
            (r'[^\w\s]', Operator),
            (r'.+?', Text),
        ],
        'pre-token': [
            # a statement that ends before any block was not a declaration
            (r';', Punctuation, '#pop'),
            include('common'),
            (r'\{', Punctuation, ('#pop', 'token')),
            (r'\s+', Whitespace),
            (r'[^\w\s]', Operator),
            (r'.+?', Text),
        ],
        'token-sym-brackets': [
            (r'(?P<delimiter>(?P<first_char>[' + ''.join(RAKU_BRACKETS) + '])(?P=first_char)*)',
             brackets_callback(Name), ('#pop', 'pre-token')),
            default(('#pop', 'pre-token')),
        ],
        'token': [
            (r'\}', Punctuation, '#pop'),
            include('regex-body'),
        ],
        # The body of a token/rule/regex, or of a m//, rx// or s/// pattern
        'regex-body': [
            (r'\s+', Whitespace),
            (r'#[^\n]*$', Comment.Single),
            # :my $x = ...; declarations are ordinary code
            (r':(?=(?:my|our|state|constant|temp|let)\b)', Punctuation),
            (r'(?<=:)(?:my|our|state|constant|temp|let).*?;', using(this)),
            # adverbs: :i, :sigspace, :!ratchet, :Perl5(...)
            (r'(:!?)([A-Za-z][\w-]*)', bygroups(Punctuation, Name.Attribute)),
            # character classes: <[a..z]>, <-[\d] + [_]>
            (r'<(?:[-+!?.]\s*)?\[(?:\\.|[^\]\\])*\]'
             r'(?:\s*[-+]\s*(?:\[(?:\\.|[^\]\\])*\]|:?[\w-]+))*\s*>', String.Regex),
            # unicode properties: <:Lu>, <+:L>, <:L + :N>, <:L - [a]>
            (r'<[-+!?.]?\s*:[\w-]+(?:\([^)\n]*\))?'
             r'(?:\s*[-+]\s*(?::[\w-]+|\[(?:\\.|[^\]\\])*\]))*\s*>', Name.Builtin),
            # named assertions: <foo>, <.foo>, <?before ...>, <name=rule>
            (r'(<)([?!.+-]?)(\s*)(' + _ident + r')(=)(' + _ident + r')(>)',
             bygroups(Punctuation, Punctuation, Whitespace, Name.Variable, Operator,
                      Name.Function, Punctuation)),
            (r'(<)([?!.+-]?)(\s*)' + _build_word_match(RAKU_REGEX_BUILTINS, _IDENT_CHAR) + r'(>)?',
             bygroups(Punctuation, Punctuation, Whitespace, Name.Builtin, Punctuation)),
            (r'(<)([?!.+-]?)(\s*)(' + _ident + r')(>)?',
             bygroups(Punctuation, Punctuation, Whitespace, Name.Function, Punctuation)),
            # code blocks and variables
            (r'\{', embedded_open_callback),
            (r"\$<[\w'-]+>", Name.Variable),
            (r'\$\d+', Name.Variable),
            (r'[$@][.^:?=!~*]?' + _qualified_ident + _angle_subscripts, Name.Variable),
            # literals
            (r"'(?:\\.|[^'\\])*'", String.Single),
            (r'[\u2018\u201a][^\u2018\u2019\n]*[\u2019\u2018]', String.Single),
            (r'\uff62[^\uff63]*\uff63', String),
            (r'"', String.Double, 'dq-string'),
            (r'[\u201c\u201e]', String.Double, 'dq-curly'),
            (r'\\[xXcCoO]\[[^\]\n]*\]|\\x[0-9a-fA-F]+|\\.', String.Escape),
            # anchors, quantifiers, alternation, separators
            (r'\^\^|\$\$|<<|>>|\u00ab|\u00bb|\^|\$', Operator),
            (r'\*\*|\|\||&&|%%|\.\.\.?|::?:?|[|&*+?!%~=.]', Operator),
            (r'[()\[\]<>]', Punctuation),
            (r'\}', Punctuation),
            (r'\w+', String.Regex),
            (r'.', String.Regex),
        ],
        # Pod documentation blocks
        'pod-body': [
            (r'^(\s*)(=head\d*)([^\n]*)',
             bygroups(Whitespace, Comment.Preproc, Generic.Heading)),
            (r'^(\s*)(=(?:begin|end|for|finish))([ \t]*)(\w*)',
             bygroups(Whitespace, Comment.Preproc, Whitespace, Name.Namespace)),
            (r'^(\s*)(=[A-Za-z]\w*)', bygroups(Whitespace, Comment.Preproc)),
            # formatting codes: B<bold>, I<italic>, C<code>, L<link>, ...
            (r'(B)(<)([^<>\n]*)(>)',
             bygroups(Name.Decorator, Punctuation, Generic.Strong, Punctuation)),
            (r'(I)(<)([^<>\n]*)(>)',
             bygroups(Name.Decorator, Punctuation, Generic.Emph, Punctuation)),
            (r'([CKTV])(<)([^<>\n]*)(>)',
             bygroups(Name.Decorator, Punctuation, String.Backtick, Punctuation)),
            (r'([ALENPRSUXZ])(<)([^<>\n]*)(>)',
             bygroups(Name.Decorator, Punctuation, Comment.Multiline, Punctuation)),
            (r'[^\n]+?(?=[A-Z]<|$)', Comment.Multiline),
            (r'[A-Z]', Comment.Multiline),
            (r'\n', Whitespace),
        ],
        # What can appear inside an interpolating string
        'interpolation': [
            (r'\\(?:[abefnrt0"\'\\$@%&{}<>«»]|[xXoOcCdD]\[[^\]\n]*\]|x[0-9a-fA-F]+)',
             String.Escape),
            (r'\\.', String.Double),
            # contextualizers: $(...), @(...)
            (r'[$@%&]\((?:[^()\n]|\([^()\n]*\))*\)', String.Interpol),
            # scalars always interpolate, optionally followed by subscripts
            # and method calls with parentheses
            (r'\$(?:[*.!^?=~:]?' + _qualified_ident + r'|[!/&\u00a2]|\d+|<[^>\n]+>)'
             r'(?:' + _subscript + r'|\.' + _ident + r'\([^)\n]*\))*',
             String.Interpol),
            # arrays, hashes and functions only if they are subscripted/called
            (r'[@%][*.!^?=~:]?' + _qualified_ident + _subscript + r'(?:' + _subscript + r')*',
             String.Interpol),
            (r'&' + _ident + r'\([^)\n]*\)', String.Interpol),
            (r'\{', embedded_open_callback),
        ],
        'interpolated': [
            include('interpolation'),
            (r'[^\\$@%&{]+', String.Double),
            (r'[$@%&\\]', String.Double),
        ],
        'dq-string': [
            (r'"', String.Double, '#pop'),
            include('interpolation'),
            (r'[^"\\$@%&{]+', String.Double),
            (r'[$@%&\\]', String.Double),
        ],
        'dq-curly': [
            (r'[\u201d\u201c]', String.Double, '#pop'),
            include('interpolation'),
            (r'[^\u201d\u201c\\$@%&{]+', String.Double),
            (r'[$@%&\\]', String.Double),
        ],
        'ww-guillemets': [
            (r'»', String.Double, '#pop'),
            include('interpolation'),
            (r'[^»\\$@%&{]+', String.Double),
            (r'[$@%&\\]', String.Double),
        ],
        'ww-angles': [
            (r'>>', String.Double, '#pop'),
            include('interpolation'),
            (r'[^>\\$@%&{]+', String.Double),
            # (a lone '>' does not close the quote)
            (r'[$@%&\\>]', String.Double),
        ],
    }

    def analyse_text(text):
        def strip_pod(lines):
            in_pod = False
            stripped_lines = []

            for line in lines:
                if re.match(r'^=(?:end|cut)', line):
                    in_pod = False
                elif re.match(r'^=\w+', line):
                    in_pod = True
                elif not in_pod:
                    stripped_lines.append(line)

            return stripped_lines

        # XXX handle block comments
        lines = text.splitlines()
        lines = strip_pod(lines)
        text = '\n'.join(lines)

        if shebang_matches(text, r'perl6|raku|rakudo|niecza|pugs'):
            return True

        saw_perl_decl = False
        rating = False

        # check for my/our/has declarations
        if re.search(r"(?:my|our|has)\s+(?:" + RakuLexer.RAKU_IDENTIFIER_RANGE +
                     r"+\s+)?[$@%&(]", text):
            rating = 0.8
            saw_perl_decl = True

        for line in lines:
            line = re.sub('#.*', '', line)
            if re.match(r'^\s*$', line):
                continue

            # match v6; use v6; use v6.d; use v6.*; use v6.e.PREVIEW; use v6.d+;
            if re.match(r'^\s*(?:use\s+)?v6(?:\.(?:\d+|[A-Za-z]+|\*))*\+?\s*;', line):
                return True
            # match unit module Foo; unit class Foo; ...
            if re.match(r'^\s*unit\s+(?:module|class|grammar|role|package)\b', line):
                return True
            # match class, module, role, enum, grammar declarations
            class_decl = re.match(r'^\s*(?:(?P<scope>my|our)\s+)?(?:module|class|role|enum|grammar)', line)
            if class_decl:
                if saw_perl_decl or class_decl.group('scope') is not None:
                    return True
                rating = 0.05
                continue
            break

        if ':=' in text:
            # Same logic as above for PerlLexer
            rating /= 2

        return rating

    def __init__(self, **options):
        super().__init__(**options)
        self.encoding = options.get('encoding', 'utf-8')


# Backwards compatibility alias for the former name.
Perl6Lexer = RakuLexer
