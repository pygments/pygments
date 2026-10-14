"""
    pygments.lexers.fstab
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for Fstab.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer
from pygments.token import (Comment, Keyword, Name, Number, Operator,
                            Punctuation, Text, Whitespace)

__all__ = ["FstabLexer"]


class FstabLexer(Lexer):
    """
    Lexer for /etc/fstab files
    """

    name = "Filesystem Table"
    url = "https://linux.die.net/man/5/fstab"
    aliases = ["fstab"]
    filenames = ["fstab"]
    version_added = "2.20"

    known_options = {
        # Common mount options
        "defaults",
        "ro",
        "rw",
        "suid",
        "nosuid",
        "dev",
        "nodev",
        "exec",
        "noexec",
        "auto",
        "noauto",
        "user",
        "nouser",
        "async",
        "sync",
        "dirsync",
        "remount",
        "mand",
        "nomand",
        "atime",
        "noatime",
        "diratime",
        "nodiratime",
        "relatime",
        "norelatime",
        "strictatime",
        "nostrictatime",
        "bind",
        "rbind",
        "unbindable",
        "runbindable",
        "private",
        "rprivate",
        "shared",
        "rshared",
        "slave",
        "rslave",
        "move",
        # Swap options
        "sw",
        "pri",
        "discard",
        "comment",
        # XFS options
        "usrquota",
        "grpquota",
        # NFS options
        "_netdev",
        "fsc",
        "hard",
        "soft",
        "intr",
        "nointr",
        "nolock",
        "lock",
        "tcp",
        "udp",
        "vers",
        "rsize",
        "wsize",
        # CIFS/SMB options
        "username",
        "password",
        "credentials",
        "domain",
        "file_mode",
        "dir_mode",
        "nounix",
        "iocharset",
        "sec",
        # tmpfs options
        "size",
        "mode",
        "nr_inodes",
        # ext4/ext3/ext2 options
        "journal",
        "nojournal",
        "data=ordered",
        "data=writeback",
        "data=journal",
        "barrier",
        "nobarrier",
        "errors=remount-ro",
        "errors=panic",
        "errors=continue",
        # Other options
        "nosymfollow",
        "size",
    }

    def get_tokens_unprocessed(self, text):
        """
        Parse fstab line
        """

        for index, line in enumerate(text.splitlines(True)):
            # Check for comments or blank lines first
            if re.match(r"^\s*$", line):
                yield index, Text, line
                continue
            if re.match(r"^\s*#", line):
                yield index, Comment, line
                continue

            # Try to match full fstab line with 6 fields
            m = re.match(
                r"^(\S+)(\s+)"
                r"(\S+)(\s+)"
                r"(\S+)(\s+)"
                r"(\S+)(\s+)"
                r"(\d+)(\s+)"
                r"(\d+)(\s*)$",
                line,
            )

            if m:
                # Extract groups
                (
                    device,
                    ws1,
                    mount,
                    ws2,
                    fstype,
                    ws3,
                    options,
                    ws4,
                    dump,
                    ws5,
                    passno,
                    ws6,
                ) = m.groups()

                pos = 0
                # device-spec
                yield pos, Name.Constant, device
                pos += len(device)
                yield pos, Whitespace, ws1
                pos += len(ws1)

                # mount point
                yield pos, Name.Namespace, mount
                pos += len(mount)
                yield pos, Whitespace, ws2
                pos += len(ws2)

                # fs-type
                yield pos, Keyword.Type, fstype
                pos += len(fstype)
                yield pos, Whitespace, ws3
                pos += len(ws3)

                # options
                opt_start = pos
                options_len = len(options)
                # Split options by comma
                opts = options.split(",")

                current_pos = opt_start
                for idx, opt in enumerate(opts):
                    opt_stripped = opt.strip()

                    # Highlight known options as Keyword
                    if opt_stripped in self.known_options:
                        yield current_pos, Keyword, opt_stripped
                    else:
                        # key=value pairs

                        if "=" in opt_stripped:
                            key, val = opt_stripped.split("=", 1)
                            yield current_pos, Name.Attribute, key
                            current_pos += len(key)
                            yield current_pos, Operator, "="
                            current_pos += 1
                            yield current_pos, Name.Constant, val
                        else:
                            yield current_pos, Name.Attribute, opt_stripped

                    current_pos += len(opt)

                    # Add comma punctuation except after last option
                    if idx != len(opts) - 1:
                        yield current_pos, Punctuation, ","
                        current_pos += 1
                pos += options_len
                yield pos, Whitespace, ws4
                pos += len(ws4)

                # dump
                yield pos, Number.Integer, dump
                pos += len(dump)
                yield pos, Whitespace, ws5
                pos += len(ws5)

                # pass
                yield pos, Number.Integer, passno
                pos += len(passno)
                yield pos, Whitespace, ws6
                pos += len(ws6)

                continue

            # Partial line fallback: device and mount point at least
            m2 = re.match(r"^(\S+)(\s+)(\S+)(.*)$", line)
            if m2:
                device, ws1, mount, rest = m2.groups()
                pos = 0
                yield pos, Name.Constant, device
                pos += len(device)
                yield pos, Whitespace, ws1
                pos += len(ws1)
                yield pos, Name.Namespace, mount
                pos += len(mount)
                yield pos, Text, rest
                continue

            # Fallback: just text
            yield 0, Text, line
