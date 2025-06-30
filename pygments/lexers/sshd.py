"""
    pygments.lexers.sshd
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for /etc/ssh/sshd_config.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer
from pygments.token import (Comment, Keyword, Literal, Name, Number, String,
                            Text, Whitespace)

__all__ = ["SshdConfigLexer"]


class SshdConfigLexer(Lexer):
    """
    Lexer for OpenSSH sshd configuration files
    """

    name = "SSHD Config"
    url = "https://man.openbsd.org/sshd_config"
    aliases = ["sshd", "sshd_config", "sshdconfig"]
    filenames = ["sshd_config", "sshd_config.d/*"]
    version_added = "2.20"

    # Common sshd keywords (directive names)
    keywords = {
        "include",
        "port",
        "addressfamily",
        "syslogfacility",
        "allowtcpforwarding",
        "kexalgorithms",
        "macs",
        "acceptenv",
        "allowagentforwarding",
        "allowgroups",
        "allowusers",
        "authenticationmethods",
        "authorizedkeysfile",
        "authorizedkeyscommand",
        "authorizedkeyscommanduser",
        "banner",
        "challengeresponseauthentication",
        "chrootdirectory",
        "ciphers",
        "clientaliveinterval",
        "clientalivecountmax",
        "compression",
        "denygroups",
        "denyusers",
        "gatewayports",
        "gssapiauthentication",
        "hostkey",
        "hostbasedauthentication",
        "ignorerhosts",
        "kbdinteractiveauthentication",
        "listenaddress",
        "logingracetime",
        "loglevel",
        "maxauthtries",
        "maxsessions",
        "passwordauthentication",
        "permitemptypasswords",
        "permitrootlogin",
        "permittty",
        "permittunnel",
        "printmotd",
        "printlastlog",
        "protocol",
        "pubkeyauthentication",
        "rekeylimit",
        "revokedkeys",
        "subsystem",
        "tcpkeepalive",
        "usedns",
        "usepam",
        "x11forwarding",
        "x11displayoffset",
        "x11uselocalhost",
        "permituserenvironment",
        "pidfile",
        "maxstartups",
        "versionaddendum",
        "match",
        "user",
        "forcecommand",
        "rhostsrsaauthentication",
    }

    literals = {
        "yes",
        "no",
        "any",
        "none",
        "inet",
        "inet6",
        "User",
        "Group",
        "Host",
        "Address",
        "LocalAddress",
        "LocalPort",
        "RDomain",
        "All",
        "Invalid-User",
    }

    ipv4_pattern = re.compile(
        r"\b(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)"
        r"(?:\.(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)){3}\b"
    )

    ipv6_pattern = re.compile(r"\b([a-f0-9:]+:+)+[a-f0-9]+\b", re.IGNORECASE)

    def get_tokens_unprocessed(self, text):
        pos = 0
        lines = text.splitlines(keepends=True)

        for line in lines:
            stripped = line.lstrip()
            indent_len = len(line) - len(stripped)

            # Yield leading whitespace
            if indent_len > 0:
                yield pos, Whitespace, line[:indent_len]
                pos += indent_len

            # Blank line
            if stripped.strip() == "":
                yield pos, Text, line[indent_len:]
                pos += len(line) - indent_len
                continue

            # Comment line (starting with #)
            if stripped.startswith("#"):
                yield pos, Comment, line[indent_len:]
                pos += len(line) - indent_len
                continue

            rest = stripped
            while rest:
                # Match keyword directives at start of token
                m_keyword = re.match(r"(\S+)", rest)

                if m_keyword:
                    keyword = m_keyword.group(1)

                    if keyword.lower() in self.keywords:
                        yield pos, Keyword, keyword
                    else:
                        yield pos, Name, keyword

                    pos += len(keyword)
                    rest = rest[len(keyword) :]

                    # After keyword or token, match whitespace
                    ws_len = len(rest) - len(rest.lstrip())
                    if ws_len > 0:
                        ws = rest[:ws_len]
                        yield pos, Whitespace, ws
                        pos += ws_len
                        rest = rest[ws_len:]

                    # Now try to match IP addresses or strings in the rest of the line

                    while rest:
                        # Match IPv4
                        m4 = self.ipv4_pattern.match(rest)
                        if m4:
                            ip = m4.group(0)
                            yield pos, Name.Constant, ip
                            pos += len(ip)
                            rest = rest[len(ip) :]
                            continue

                        # Match IPv6
                        m6 = self.ipv6_pattern.match(rest)
                        if m6:
                            ip = m6.group(0)
                            yield pos, Name.Constant, ip
                            pos += len(ip)
                            rest = rest[len(ip) :]
                            continue

                        # Match quoted string
                        if rest.startswith('"'):
                            end_quote = rest.find('"', 1)
                            if end_quote == -1:
                                # No closing quote, highlight rest as string
                                yield pos, String.Double, rest
                                pos += len(rest)
                                rest = ""
                            else:
                                quoted = rest[: end_quote + 1]
                                yield pos, String.Double, quoted
                                pos += len(quoted)
                                rest = rest[len(quoted) :]
                            continue

                        # Match whitespace
                        ws_len = len(rest) - len(rest.lstrip())
                        if ws_len > 0:
                            ws = rest[:ws_len]
                            yield pos, Whitespace, ws
                            pos += ws_len
                            rest = rest[ws_len:]
                            continue

                        # Match non-whitespace token
                        m_token = re.match(r"\S+", rest)
                        if m_token:
                            val = m_token.group(0)

                            if val.isdigit():
                                yield pos, Number, val
                            elif val.isupper():
                                yield pos, Name.Constant, val
                            elif val in self.literals:
                                yield pos, Literal, val
                            else:
                                yield pos, Name, val

                            pos += len(val)
                            rest = rest[len(val) :]
                            continue

                        # If nothing matched, yield rest as Text
                        yield pos, Text, rest
                        pos += len(rest)
                        rest = ""
                else:
                    # No token matched, yield rest as Text and break
                    yield pos, Text, rest
                    pos += len(rest)
                    rest = ""
