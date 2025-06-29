"""
    pygments.lexers.ssh
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for OpenSSH client configuration files (~/.ssh/config).

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer
from pygments.token import (Comment, Keyword, Literal, Name, Number, String,
                            Text, Whitespace)

__all__ = ["SshConfigLexer"]


class SshConfigLexer(Lexer):
    """
    Lexer for OpenSSH client configuration files (~/.ssh/config).
    """

    name = "SSH Config"
    url = "https://man.openbsd.org/ssh_config"
    aliases = ["sshconfig", "ssh_config", "ssh"]
    filenames = ["config", ".ssh/config"]
    version_added = "2.20"

    keywords = {
        "Host",
        "Match",
        "AddressFamily",
        "BatchMode",
        "BindAddress",
        "CanonicalDomains",
        "CanonicalizeFallbackLocal",
        "CanonicalizeHostname",
        "CanonicalizeMaxDots",
        "CanonicalizePermittedCNAMEs",
        "CertificateFile",
        "ChallengeResponseAuthentication",
        "CheckHostIP",
        "Ciphers",
        "ClearAllForwardings",
        "Compression",
        "ConnectionAttempts",
        "ConnectTimeout",
        "ControlMaster",
        "ControlPath",
        "ControlPersist",
        "DynamicForward",
        "EscapeChar",
        "ExitOnForwardFailure",
        "FingerprintHash",
        "ForwardAgent",
        "ForwardX11",
        "ForwardX11Timeout",
        "ForwardX11Trusted",
        "GatewayPorts",
        "GlobalKnownHostsFile",
        "GSSAPIAuthentication",
        "GSSAPIDelegateCredentials",
        "HashKnownHosts",
        "HostbasedAuthentication",
        "HostKeyAlgorithms",
        "Hostname",
        "IdentitiesOnly",
        "IdentityAgent",
        "IdentityFile",
        "IPQoS",
        "KbdInteractiveAuthentication",
        "KexAlgorithms",
        "LocalCommand",
        "LocalForward",
        "LogLevel",
        "MACs",
        "NoHostAuthenticationForLocalhost",
        "NumberOfPasswordPrompts",
        "PasswordAuthentication",
        "PermitLocalCommand",
        "PKCS11Provider",
        "Port",
        "PreferredAuthentications",
        "ProxyCommand",
        "ProxyJump",
        "ProxyUseFdpass",
        "PubkeyAuthentication",
        "RekeyLimit",
        "RemoteForward",
        "RequestTTY",
        "SendEnv",
        "ServerAliveInterval",
        "ServerAliveCountMax",
        "StrictHostKeyChecking",
        "TCPKeepAlive",
        "Tunnel",
        "TunnelDevice",
        "UpdateHostKeys",
        "User",
        "UserKnownHostsFile",
        "VerifyHostKeyDNS",
        "VisualHostKey",
        "XAuthLocation",
    }

    literals = {
        "yes",
        "no",
        "ask",
        "confirm",
        "auto",
        "none",
        "any",
        "all",
        "clear",
        "exit-on-forward-failure",
        "no-pty",
        "yes-pty",
        "local",
        "remote",
        "both",
        "none",
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
                # Match keyword/directive at start of token
                m_keyword = re.match(r"(\S+)", rest)
                if not m_keyword:
                    # No token matched, yield rest as Text and break
                    yield pos, Text, rest
                    pos += len(rest)
                    break

                token = m_keyword.group(1)

                if token in self.keywords:
                    yield pos, Keyword, token
                else:
                    yield pos, Name, token

                pos += len(token)
                rest = rest[len(token) :]

                # Match whitespace after token
                ws_len = len(rest) - len(rest.lstrip())
                if ws_len > 0:
                    ws = rest[:ws_len]
                    yield pos, Whitespace, ws
                    pos += ws_len
                    rest = rest[ws_len:]

                # Now parse the rest of the line (arguments, IPs, strings, etc.)
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
                        elif val.lower() in self.literals:
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
