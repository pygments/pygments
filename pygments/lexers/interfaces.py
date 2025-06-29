"""
    pygments.lexers.interfaces
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for /etc/network/interfaces.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer
from pygments.token import (Comment, Keyword, Name, Number, Text, Token,
                            Whitespace)

__all__ = ["InterfacesLexer"]


class InterfacesLexer(Lexer):
    """
    Lexer for Alpine Linux /etc/network/interfaces syntax.
    """

    name = "Interfaces"
    url = "https://wiki.alpinelinux.org/wiki/Configure_Networking"
    aliases = ["interfaces", "alpineinterfaces", "iface"]
    filenames = ["interfaces"]
    version_added = "2.20"

    keywords = {
        "auto",
        "iface",
        "address",
        "netmask",
        "gateway",
        "network",
        "broadcast",
        "dns-nameservers",
        "dns-search",
        "metric",
        "up",
        "down",
        "pre-up",
        "post-up",
        "pre-down",
        "post-down",
        "hwaddress",
        "mtu",
        "arp",
        "inet",
        "inet6",
        "loopback",
        "static",
        "dhcp",
        "manual",
        "allow-hotplug",
        "source",
        "source-directory",
        "autoconf",
    }

    ipv4_pattern = re.compile(
        r"\b(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)"
        r"(?:\.(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)){3}\b"
    )

    ipv6_pattern = re.compile(
        r"\b(([a-f0-9:]+:+)+[a-f0-9]+)\b",
        re.IGNORECASE,
    )

    def get_tokens_unprocessed(self, text):
        pos = 0
        lines = text.splitlines(keepends=True)

        for line in lines:
            stripped = line.lstrip()
            indent_len = len(line) - len(stripped)
            if indent_len > 0:
                yield pos, Whitespace, line[:indent_len]
                pos += indent_len

            if stripped.strip() == "":
                yield pos, Text, line[indent_len:]
                pos += len(line) - indent_len
                continue

            if stripped.startswith("#"):
                yield pos, Comment, line[indent_len:]
                pos += len(line) - indent_len
                continue

            rest = stripped
            while rest:
                # Try matching IPv4 at start
                m4 = self.ipv4_pattern.match(rest)
                if m4:
                    ip = m4.group(0)
                    yield pos, Token.Literal.IP, ip
                    pos += len(ip)
                    rest = rest[len(ip) :]
                    continue

                # Try matching IPv6 at start
                m6 = self.ipv6_pattern.match(rest)
                if m6:
                    ip = m6.group(0)
                    yield pos, Token.Literal.IP, ip
                    pos += len(ip)
                    rest = rest[len(ip) :]
                    continue

                # Match whitespace
                ws_len = len(rest) - len(rest.lstrip())
                if ws_len > 0:
                    ws = rest[:ws_len]
                    yield pos, Whitespace, ws
                    pos += ws_len
                    rest = rest[ws_len:]
                    continue

                # Match next token (non-whitespace sequence)
                token_match = re.match(r"\S+", rest)
                if token_match:
                    token = token_match.group(0)
                    # Highlight keywords
                    if token in self.keywords:
                        yield pos, Keyword, token
                    else:
                        yield pos, Number if token.isnumeric() else Name, token
                    pos += len(token)
                    rest = rest[len(token) :]
                else:
                    # If no match, yield the rest as Text and break
                    yield pos, Text, rest
                    pos += len(rest)
                    break
