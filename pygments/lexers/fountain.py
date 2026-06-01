"""
    pygments.lexers.fountain
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Fountain screenplay format.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer
from pygments.token import Comment, Generic, Keyword, Name, Operator, \
    Punctuation, String, Text, Whitespace

__all__ = ['FountainLexer']


class FountainLexer(Lexer):
    """
    Lexer for Fountain screenplay markup.
    """

    name = 'Fountain'
    aliases = ['fountain']
    filenames = ['*.fountain', '*.spmd']
    mimetypes = []
    url = 'https://fountain.io/syntax/'
    version_added = '2.21'
    _example = 'fountain/example.fountain'

    _title_key_re = re.compile(r'^([A-Za-z][A-Za-z ]*[A-Za-z]|[A-Za-z]):(?:[ \t].*)?$')
    _title_start_re = re.compile(
        r'^(title|credit|author|authors|source|draft date|contact|copyright|notes):(?:[ \t].*)?$',
        re.IGNORECASE,
    )
    _title_continuation_re = re.compile(r'^(?: {3,}|\t).*$')
    _character_name_re = re.compile(r"^[A-Z0-9 .\-()'/]+$")
    _scene_heading_re = re.compile(
        r'^(?:INT\./EXT|INT/EXT|INT|EXT|EST|I/E)(?:\.|\s)',
        re.IGNORECASE,
    )
    _scene_number_re = re.compile(r'^(.*?)(\s*)(#[-.A-Za-z0-9]+#)$')
    _inline_scene_number_re = re.compile(r'#[-.A-Za-z0-9]+#')
    _section_re = re.compile(r'^(#+)(\s*.*)$')
    _page_break_re = re.compile(r'^={3,}$')
    _transition_re = re.compile(r'^(?=[^a-z]*[A-Z])[^a-z]*TO:$')

    def __init__(self, **options):
        super().__init__(**options)
        self._in_boneyard = False
        self._in_note = False

    @staticmethod
    def analyse_text(text):  # type: ignore[override]
        if re.search(r'^(Title|Credit|Author|Authors|Source|Draft date|Contact):',
                     text, re.MULTILINE):
            return 0.3
        if re.search(r'^(INT\.|EXT\.|EST\.|INT\./EXT\.|INT/EXT\.|I/E\.)',
                     text, re.MULTILINE | re.IGNORECASE):
            return 0.2
        if re.search(r'^[A-Z0-9 .\-\'()]+\n(?:\([^\n]+\)\n)?.+',
                     text, re.MULTILINE):
            return 0.1
        return 0.0

    def get_tokens_unprocessed(self, text):
        self._in_boneyard = False
        self._in_note = False

        lines = text.splitlines(True)
        pos = 0
        i = 0
        in_title_page = self._starts_title_page(lines)
        in_dialogue = False

        while i < len(lines):
            raw_line = lines[i]
            line, newline = self._split_line(raw_line)
            stripped = line.strip()

            if self._in_boneyard or self._in_note:
                yield from self._emit_inline(line, pos, Text)
                if newline:
                    yield pos + len(line), Whitespace, newline
                pos += len(raw_line)
                i += 1
                continue

            if in_title_page:
                if stripped == '':
                    in_title_page = False
                    yield pos, Whitespace, raw_line
                    pos += len(raw_line)
                    i += 1
                    continue
                if self._is_title_entry(line, started=True) or self._is_title_continuation(line):
                    yield from self._emit_title_line(line, pos)
                    if newline:
                        yield pos + len(line), Whitespace, newline
                    pos += len(raw_line)
                    i += 1
                    continue
                in_title_page = False

            if in_dialogue:
                if stripped == '':
                    if line and line.strip() == '' and len(line) >= 2:
                        yield pos, String, line
                        if newline:
                            yield pos + len(line), Whitespace, newline
                    else:
                        yield pos, Whitespace, raw_line
                        in_dialogue = False
                    pos += len(raw_line)
                    i += 1
                    continue
                if self._is_parenthetical(stripped):
                    yield from self._emit_parenthetical(line, pos)
                else:
                    yield from self._emit_dialogue(line, pos)
                if newline:
                    yield pos + len(line), Whitespace, newline
                pos += len(raw_line)
                i += 1
                continue

            if stripped == '':
                yield pos, Whitespace, raw_line
            elif self._page_break_re.match(stripped):
                yield from self._emit_simple_line(line, pos, Operator)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif stripped.startswith('>') and stripped.endswith('<') and len(stripped) >= 2:
                yield from self._emit_centered(line, pos)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif stripped.startswith('#'):
                yield from self._emit_section(line, pos)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif stripped.startswith('='):
                yield from self._emit_simple_line(line, pos, Comment.Special)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif self._is_forced_transition(stripped):
                yield from self._emit_transition(line, pos, forced=True)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif self._is_forced_scene_heading(stripped):
                yield from self._emit_scene_heading(line, pos, forced=True)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif stripped.startswith('!'):
                yield from self._emit_forced_action(line, pos)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif stripped.startswith('~'):
                yield from self._emit_lyric(line, pos)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif self._is_transition(lines, i):
                yield from self._emit_transition(line, pos, forced=False)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif self._is_scene_heading(lines, i):
                yield from self._emit_scene_heading(line, pos, forced=False)
                if newline:
                    yield pos + len(line), Whitespace, newline
            elif self._is_character(lines, i, forced=stripped.startswith('@')):
                yield from self._emit_character(line, pos)
                if newline:
                    yield pos + len(line), Whitespace, newline
                in_dialogue = True
            else:
                yield from self._emit_action(line, pos)
                if newline:
                    yield pos + len(line), Whitespace, newline

            pos += len(raw_line)
            i += 1

    def _split_line(self, raw_line):
        if raw_line.endswith('\n'):
            return raw_line[:-1], '\n'
        return raw_line, ''

    def _starts_title_page(self, lines):
        for raw_line in lines:
            line, _ = self._split_line(raw_line)
            if line.strip() == '':
                continue
            return self._is_title_entry(line, started=False)
        return False

    def _is_title_entry(self, line, started):
        stripped = line.strip()
        if not self._title_key_re.match(stripped):
            return False
        if started:
            return True
        return bool(self._title_start_re.match(stripped))

    def _is_title_continuation(self, line):
        return bool(self._title_continuation_re.match(line))

    def _prev_blank(self, lines, index):
        if index == 0:
            return True
        prev, _ = self._split_line(lines[index - 1])
        return prev.strip() == ''

    def _next_blank(self, lines, index):
        if index + 1 >= len(lines):
            return True
        nxt, _ = self._split_line(lines[index + 1])
        return nxt.strip() == ''

    def _next_is_nonblank(self, lines, index):
        if index + 1 >= len(lines):
            return False
        nxt, _ = self._split_line(lines[index + 1])
        return nxt.strip() != ''

    def _has_alpha(self, text):
        return re.search(r'[^\W\d_]', text, re.UNICODE) is not None

    def _is_scene_heading(self, lines, index):
        line, _ = self._split_line(lines[index])
        stripped = line.strip()
        return self._prev_blank(lines, index) and self._next_blank(lines, index) and \
            bool(self._scene_heading_re.match(stripped))

    def _is_forced_scene_heading(self, stripped):
        return len(stripped) > 1 and stripped[0] == '.' and stripped[1].isalnum()

    def _is_transition(self, lines, index):
        line, _ = self._split_line(lines[index])
        stripped = line.strip()
        return self._prev_blank(lines, index) and self._next_blank(lines, index) and \
            bool(self._transition_re.match(stripped))

    def _is_forced_transition(self, stripped):
        return stripped.startswith('>') and not stripped.endswith('<')

    def _is_parenthetical(self, stripped):
        return stripped.startswith('(') and stripped.endswith(')')

    def _is_character(self, lines, index, forced=False):
        line, _ = self._split_line(lines[index])
        stripped = line.strip()
        if not forced and not self._prev_blank(lines, index):
            return False

        body = stripped[1:] if forced and stripped.startswith('@') else stripped
        body = body.rstrip()
        if body.endswith('^'):
            body = body[:-1].rstrip()
        if not body:
            return False
        if self._transition_re.match(body) or self._scene_heading_re.match(body):
            return False
        if not self._next_is_nonblank(lines, index):
            return False
        next_line, _ = self._split_line(lines[index + 1])
        if not next_line.strip():
            return False

        ext_index = body.find('(')
        if ext_index != -1:
            name = body[:ext_index].rstrip()
            ext = body[ext_index:]
            if not re.fullmatch(r'(?:\s*\([^\n)]*\)\s*)+', ext):
                return False
        else:
            name = body
        if not name or not self._has_alpha(name):
            return False
        return forced or bool(self._character_name_re.fullmatch(name))

    def _indent_len(self, line):
        return len(line) - len(line.lstrip(' \t'))

    def _emit_simple_line(self, line, pos, token):
        if line:
            yield pos, token, line

    def _emit_title_line(self, line, pos):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        if self._is_title_continuation(line):
            yield from self._emit_inline(body, pos + indent, String)
            return

        key, _, value = body.partition(':')
        yield pos + indent, Name.Attribute, key
        yield pos + indent + len(key), Punctuation, ':'
        if value:
            if value.startswith((' ', '\t')):
                ws_len = len(value) - len(value.lstrip(' \t'))
                if ws_len:
                    yield pos + indent + len(key) + 1, Whitespace, value[:ws_len]
                yield from self._emit_inline(
                    value[ws_len:],
                    pos + indent + len(key) + 1 + ws_len,
                    String,
                )

    def _emit_section(self, line, pos):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        match = self._section_re.match(body)
        if match is None:
            yield from self._emit_simple_line(line, pos, Generic.Subheading)
            return
        hashes, rest = match.groups()
        yield pos + indent, Punctuation, hashes
        if rest:
            yield from self._emit_inline(rest, pos + indent + len(hashes), Generic.Subheading)

    def _emit_transition(self, line, pos, forced):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        offset = pos + indent
        if forced:
            yield offset, Punctuation, '>'
            body = body[1:]
            offset += 1
        yield from self._emit_inline(body, offset, Keyword)

    def _emit_scene_heading(self, line, pos, forced):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        offset = pos + indent
        if forced:
            yield offset, Punctuation, '.'
            body = body[1:]
            offset += 1

        match = self._scene_number_re.match(body.rstrip())
        if match:
            heading, spaces, scene_no = match.groups()
            yield from self._emit_inline(heading, offset, Generic.Heading)
            if spaces:
                yield offset + len(heading), Generic.Heading, spaces
            yield offset + len(heading) + len(spaces), Name.Label, scene_no
            trailing = body[len(heading) + len(spaces) + len(scene_no):]
            if trailing:
                yield offset + len(heading) + len(spaces) + len(scene_no), Generic.Heading, trailing
            return

        yield from self._emit_inline(body, offset, Generic.Heading)

    def _emit_forced_action(self, line, pos):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        if not body:
            return
        yield pos + indent, Punctuation, '!'
        yield from self._emit_inline(body[1:], pos + indent + 1, Text)

    def _emit_lyric(self, line, pos):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        if not body:
            return
        yield pos + indent, Punctuation, '~'
        yield from self._emit_inline(body[1:], pos + indent + 1, String.Other)

    def _emit_character(self, line, pos):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        offset = pos + indent

        if body.startswith('@'):
            yield offset, Punctuation, '@'
            body = body[1:]
            offset += 1

        dual = ''
        if body.rstrip().endswith('^'):
            stripped_body = body.rstrip()
            dual_start = len(stripped_body) - 1
            dual = body[dual_start:]
            body = body[:dual_start]

        ext_index = body.find('(')
        if ext_index == -1:
            name = body.rstrip()
            rest = body[len(name):]
            yield offset, Name.Label, name
            if rest:
                yield offset + len(name), Whitespace, rest
        else:
            name = body[:ext_index].rstrip()
            rest = body[len(name):ext_index]
            yield offset, Name.Label, name
            cursor = offset + len(name)
            if rest:
                yield cursor, Whitespace, rest
                cursor += len(rest)
            ext = body[ext_index:]
            last_end = 0
            for match in re.finditer(r'\s*\([^\n)]*\)', ext):
                segment = match.group(0)
                gap = ext[last_end:match.start()]
                if gap:
                    yield cursor, Whitespace, gap
                    cursor += len(gap)
                ws_len = len(segment) - len(segment.lstrip(' '))
                if ws_len:
                    yield cursor, Whitespace, segment[:ws_len]
                    cursor += ws_len
                yield cursor, Name.Decorator, segment[ws_len:]
                cursor += len(segment[ws_len:])
                last_end = match.end()
            tail = ext[last_end:]
            if tail:
                yield cursor, Whitespace, tail
        if dual:
            # dual[0] is always '^'; dual[1:] is any trailing whitespace
            yield offset + len(body), Punctuation, '^'
            trailing = dual[1:]
            if trailing:
                yield offset + len(body) + 1, Whitespace, trailing

    def _emit_parenthetical(self, line, pos):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        yield pos + indent, Name.Decorator, body

    def _emit_dialogue(self, line, pos):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        yield from self._emit_inline(body, pos + indent, String)

    def _emit_action(self, line, pos):
        indent = self._indent_len(line)
        if indent:
            yield pos, Whitespace, line[:indent]
        yield from self._emit_inline(line[indent:], pos + indent, Text)

    def _emit_centered(self, line, pos):
        indent = self._indent_len(line)
        body = line[indent:]
        if indent:
            yield pos, Whitespace, line[:indent]
        offset = pos + indent
        if len(body) < 2:
            yield from self._emit_inline(body, offset, Text)
            return
        yield offset, Punctuation, '>'
        inner = body[1:-1]
        yield from self._emit_inline(inner, offset + 1, Generic.Strong)
        yield offset + 1 + len(inner), Punctuation, '<'

    def _emit_inline(self, text, pos, default_token):
        yield from self._emit_inline_segment(text, pos, default_token)

    def _emit_inline_segment(self, text, pos, default_token):
        i = 0
        while i < len(text):
            if self._in_boneyard:
                end = text.find('*/', i)
                if end == -1:
                    yield pos + i, Comment.Multiline, text[i:]
                    return
                yield pos + i, Comment.Multiline, text[i:end + 2]
                self._in_boneyard = False
                i = end + 2
                continue

            if self._in_note:
                end = text.find(']]', i)
                if end == -1:
                    yield pos + i, Comment.Special, text[i:]
                    return
                yield pos + i, Comment.Special, text[i:end + 2]
                self._in_note = False
                i = end + 2
                continue

            if text.startswith('/*', i):
                end = text.find('*/', i + 2)
                if end == -1:
                    self._in_boneyard = True
                    yield pos + i, Comment.Multiline, text[i:]
                    return
                yield pos + i, Comment.Multiline, text[i:end + 2]
                i = end + 2
                continue

            if text.startswith('[[', i):
                end = text.find(']]', i + 2)
                if end == -1:
                    self._in_note = True
                    yield pos + i, Comment.Special, text[i:]
                    return
                yield pos + i, Comment.Special, text[i:end + 2]
                i = end + 2
                continue

            if text[i] == '\\' and i + 1 < len(text):
                yield pos + i, String.Escape, text[i:i + 2]
                i += 2
                continue

            match = self._inline_scene_number_re.match(text, i)
            if match is not None:
                value = match.group(0)
                yield pos + i, Name.Label, value
                i += len(value)
                continue

            for marker, token in (
                ('***', Generic.EmphStrong),  # bold-italic
                ('**', Generic.Strong),        # bold
                ('*', Generic.Emph),           # italic
                ('_', Generic.EmphStrong),     # underline (no dedicated token; EmphStrong is closest)
            ):
                if text.startswith(marker, i):
                    end = self._find_emphasis_end(text, i, marker)
                    if end != -1:
                        yield pos + i, Punctuation, marker
                        inner_start = i + len(marker)
                        inner_end = end
                        yield from self._emit_inline_segment(
                            text[inner_start:inner_end],
                            pos + inner_start,
                            token,
                        )
                        yield pos + end, Punctuation, marker
                        i = end + len(marker)
                        break
            else:
                next_special = self._next_special_index(text, i)
                if next_special == i:
                    yield pos + i, default_token, text[i]
                    i += 1
                    continue
                yield pos + i, default_token, text[i:next_special]
                i = next_special
                continue
            continue

    def _find_emphasis_end(self, text, start, marker):
        inner_start = start + len(marker)
        end = text.find(marker, inner_start)
        while end != -1:
            inner = text[inner_start:end]
            if inner and not inner[0].isspace() and not inner[-1].isspace():
                return end
            end = text.find(marker, end + len(marker))
        return -1

    def _next_special_index(self, text, start):
        i = start
        while i < len(text):
            if text.startswith(('/*', '[[', '***', '**'), i):
                return i
            if text[i] in '\\*_#':
                return i
            i += 1
        return len(text)
