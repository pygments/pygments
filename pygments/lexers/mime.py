# -*- coding: utf-8 -*-
"""
    pygments.lexers.mime
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for Multipurpose Internet Mail Extensions (MIME) data.

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, do_insertions
from pygments.lexers import get_lexer_for_mimetype
from pygments.token import Text, Whitespace, Name, String, Comment, Other
from pygments.util import get_int_opt, ClassNotFound

__all__ = ["MIMELexer"]


class MIMELexer(RegexLexer):
    """
    Lexer for Multipurpose Internet Mail Extensions (MIME) data. This lexer is
    designed to process the nested mulitpart data.

    It assumes that the given data contains both header and body (and is
    splitted by empty line). If no valid header is found, then the entire data
    would be treated as body.

    Additional options accepted:

    `MIME-max-level`
        Max recurssion level for nested MIME structure. Any negative number
        would treated as unlimited. (default: -1)

    `Content-Type`
        Treat the data as specific content type. Useful when header is
        missing, or this lexer would try to parse from header. (default: None)

    `Content-Transfer-Encoding`
        Treat the data as specific encoding. Or this lexer would try to parse
        from header by default. (default: None)
    """

    name = "MIME"
    aliases = ["mime"]
    mimetypes = ["multipart/mixed",
                 "multipart/related",
                 "multipart/alternative"]

    def __init__(self, **options):
        RegexLexer.__init__(self, **options)
        self.boundary = options.get("boundary")
        self.content_transfer_encoding = options.get("Content_Transfer_Encoding")
        self.content_type = options.get("Content_Type", "text/plain")
        self.max_nested_level = get_int_opt(options, "MIME-max-level", -1)

    def analyse_text(text):
        try:
            header, body = text.strip().split("\n\n", 1)
            if not body.strip():
                return 0.1

            invalid_headers = MIMELexer.tokens["header"].sub("", header)
            if invalid_headers.strip():
                return 0.1
            else:
                return 1

        except ValueError:
            return 0.1

    def get_header_tokens(self, match):
        field = match.group(1)

        if field.lower() in self.attention_headers:
            yield match.start(1), Name.Tag, field + ":"
            yield match.start(2), Whitespace, match.group(2)

            pos = match.end(2)
            body = match.group(3)
            for i, t, v in self.get_tokens_unprocessed(body, ("root", field.lower())):
                yield pos + i, t, v

        else:
            yield match.start(1), Comment.Special, field + ":"
            yield match.start(2), Whitespace, match.group(2)
            yield match.start(3), Comment.Multiline, match.group(3)

    def get_body_tokens(self, match):
        pos_body_start = match.start()
        entire_body = match.group()

        # if it is not a mulitpart
        if not self.content_type.startswith("multipart") or not self.boundary:
            for i, t, v in self.get_bodypart_tokens(entire_body):
                yield pos_body_start + i, t, v
            return

        # find boundary
        bdry_pattern = r"^--%s(--)?\n" % re.escape(self.boundary)
        bdry_matcher = re.compile(bdry_pattern, re.MULTILINE)

        # process tokens of each body part
        pos_part_start = pos_body_start
        for m in bdry_matcher.finditer(entire_body):
            # bodypart
            lpos_start = pos_part_start - pos_body_start
            lpos_end = m.start()
            part = entire_body[lpos_start:lpos_end]
            for i, t, v in self.get_bodypart_tokens(part):
                yield pos_part_start + i, t, v

            # boundary
            yield pos_body_start + m.start(), String.Delimiter, m.group()
            pos_part_start = pos_body_start + m.end()

        # check for tailing context
        lpos_start = pos_part_start - pos_body_start
        if lpos_start != len(entire_body):
            yield pos_part_start, Other, entire_body[lpos_start:]

    def get_bodypart_tokens(self, text):
        # return if:
        #  * no content
        #  * no content type specific
        #  * content encoding is not readable
        #  * max recurrsion exceed
        if not text.strip() or not self.content_type:
            return [(0, Other, text)]

        cte = self.content_transfer_encoding
        if cte and cte not in {"8bit", "7bit", "quoted-printable"}:
            return [(0, Other, text)]

        if self.max_nested_level == 0:
            return [(0, Other, text)]

        # get lexer
        try:
            lexer = get_lexer_for_mimetype(self.content_type)
        except ClassNotFound:
            return [(0, Other, text)]

        if isinstance(lexer, type(self)):
            lexer.max_nested_level = self.max_nested_level - 1

        return lexer.get_tokens_unprocessed(text)

    def store_content_type(self, match):
        self.content_type = match.group(1)

        prefix_len = match.start(1) - match.start(0)
        yield match.start(0), Whitespace, match.group(0)[:prefix_len]
        yield match.start(1), Name.Label, match.group(2)
        yield match.end(2), String.Delimiter, "/"
        yield match.start(3), Name.Label, match.group(3)

    def store_boundary(self, match):
        if match.group(1):
            prefix_len = match.start(1) - match.start(0)
            yield match.start(0), Text, match.group(0)[:prefix_len]
            yield match.start(1), String.Delimiter, match.group(1)
            yield match.end(1), Text, '"'
            self.boundary = match.group(1)
        else:
            prefix_len = match.start(2) - match.start(0)
            yield match.start(0), Text, match.group(0)[:prefix_len]
            yield match.start(2), String.Delimiter, match.group(2)
            self.boundary = match.group(2)

    def store_content_transfer_encoding(self, match):
        self.content_transfer_encoding = match.group(0).lower()
        yield match.start(0), Text, match.group(0)

    attention_headers = {"content-type", "content-transfer-encoding"}

    tokens = {
        "root": [
            (r"^([\w-]+):( *)([\s\S]*?\n)(?![ \t])", get_header_tokens),
            (r"^$[\s\S]+", get_body_tokens),
        ],
        "header": [
            # folding
            (r"\n[ \t]", Whitespace),
            (r"\n(?![ \t])", Whitespace, "#pop"),
        ],
        "content-type": [
            include("header"),
            (
                r"^\s*((multipart|application|audio|font|image|model|text|video"
                r"|message)/([\w-]+))",
                store_content_type,
            ),
            (r"boundary=(?:\"([\w'()+,.\/:? =-]+)\"|([\w'()+,.\/:? =-]+)\b)",
             store_boundary),
            (r'\S+', Text),
            (r'[ \t]+', Whitespace),
        ],
        "content-transfer-encoding": [
            include("header"),
            (r"([\w-]+)", store_content_transfer_encoding),
        ],
    }
