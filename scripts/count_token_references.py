#!/usr/bin/env python
"""
Count number of references to tokens in lexer source
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:program:`count_token_references` counts how many references to all existing
tokens it can find by "grepping" the the source code of the lexers. This can
be used to find typos in token names, as those tokens are only used by one lexer.

:program:`count_token_references` supports the following options:

.. program:: count_token_references

.. option:: -v, --verbose
    This gives output while the script is collecting information.

.. option:: --minfiles <COUNT>
    Only report about tokens that are referenced in at least this many lexer
    source files (default 1).

.. option:: --maxfiles <COUNT>
    Only report about tokens that are referenced in at most this many lexer
    source files (default 1).

.. option:: --minlines <COUNT>
    Only report about tokens that are referenced in at least this many lexer
    source lines (default 1).

.. option:: --maxlines <COUNT>
    Only report about tokens that are referenced in at most this many lexer
    source lines (default 10).

.. option:: -s, --subtokens
    When ``--subtoken`` is given each token is also counted for each of its
    parent tokens. I.e. if we have 10 occurrences of the token
    ``Token.Literal.Number.Integer`` and 10 occurrences of the token
    ``Token.Literal.Number.Hex`` but none for ``Token.Literal.Number``, with
    ``--subtoken`` ``Token.Literal.Number`` would be counted as having
    20 references.
"""

import sys, argparse, re, pathlib

from pygments import token, lexers


def lookup_all_lexers():
    """
    Iterate through all lexers and fetch them.
    This should create all tokens that any of the lexers produce.
    """
    count = 0
    for (_, aliases, patterns, mimetypes) in lexers.get_all_lexers():
        for alias in aliases:
            _ = lexers.get_lexer_by_name(alias)
            break
        else:
            for pattern in patterns:
                _ = lexers.get_lexer_for_filename(pattern)
                break
            else:
                for mimetype in mimetypes:
                    _ = lexers.get_lexer_for_mimetype(mimetype)
                    break
        count += 1
    return count


def fetch_lexer_sources():
    """
    Return the source code of all lexers as a dictionary, mapping filenames
    to a list of lines.
    """
    lexer_dir = pathlib.Path(__file__).parent / "../pygments/lexers"
    lexer_dir = lexer_dir.resolve()
    lexer_sources = {
        fn: fn.read_text(encoding='utf-8').splitlines(keepends=False)
        for fn in lexer_dir.glob("*.py")
    }
    return lexer_sources


def sub_tokens(token):
    """
    Generator that yields a token and all of its sub-tokens recursively.
    """
    yield token
    for subtoken in token.subtypes:
        yield from sub_tokens(subtoken)


class FileCount:
    """
    Stores information about line numbers in a file.

    This is used to store from which lines in a files a certain token is
    referenced.
    """
    def __init__(self, filename):
        self.filename = filename
        self.lines = []

    def __str__(self):
        if len(self.lines) > 3:
            lines = ", ".join(f"{line:,}" for line in self.lines[:5])
            lines = f"{lines}, ... ({len(lines):,} lines)"
        else:
            lines = ", ".join(f"{line:,}" for line in self.lines)
        return f"{self.filename.name}[{lines}]"

    def add(self, linenumber):
        self.lines.append(linenumber)

    def count_lines(self):
        return len(self.lines)


class TokenCount:
    """
    Stores information about a token and in which files it is referenced.
    """
    def __init__(self, token):
        self.token = token
        self.files = {}

    def add(self, filename, linenumber):
        if filename not in self.files:
            self.files[filename] = FileCount(filename)
        self.files[filename].add(linenumber)

    def __str__(self):
        if len(self.files) > 3:
            files = []
            for (i, filecount) in enumerate(self.files.values()):
                files.append(str(filecount))
                if i >= 5:
                    break
            files = ", ".join(files) + f", ... ({len(self.files):,} files)"
        else:
            files = ", ".join(str(filecount) for filecount in self.files.values())
        return f"{self.count_files():,} files, {self.count_lines():,} locations: {files}"

    def count_files(self):
        return len(self.files)

    def count_lines(self):
        return sum(fc.count_lines() for fc in self.files.values())


def find_token_references(lexer_sources, args):
    """
    Find all references to all tokens in the source code of all lexers.

    Note that this can't be 100% reliable, as it searches the source code for
    certain patterns: It searches for the last two components of a token name,
    i.e. to find references to the token ``Token.Literal.Number.Integer.Long``
    it searches for the regular expression ``\\bInteger.Long\\b``. This
    won't work reliably for top level token like ``Token.String`` since this
    is often referred to as ``String``, but searching for ``\\bString\\b``
    yields too many false positives.
    """

    # Maps token to :class:`TokenCount` objects.
    token_references = {}

    # Search for each token in each lexer source file and record in which file
    # and in which line they are referenced
    for t in sub_tokens(token.Token):
        parts = list(t)[-2:]
        if len(parts) == 0:
            name = "Token"
        elif len(parts) == 1:
            name = f"Token.{parts[0]}"
        else:
            name = ".".join(parts)

        token_references[t] = tokencount = TokenCount(t)

        if name != "Token":
            pattern = re.compile(f"\\b{name}\\b")

            for (filename, sourcelines) in lexer_sources.items():
                for (i, line) in enumerate(sourcelines, 1):
                    if pattern.search(line) is not None:
                        tokencount.add(filename, i)
                        if args.subtoken:
                            t2 = t
                            while t2 is not token.Token:
                                t2 = t2.parent
                                tokencount2 = token_references[t2]
                                tokencount2.add(filename, i)

    return token_references


def print_result(token_references, args):
    def key(item):
        return (item[1].count_files(), item[1].count_lines())

    for (tok, locations) in sorted(token_references.items(), key=key):
        if args.minfiles <= locations.count_files() <= args.maxfiles and \
           args.minlines <= locations.count_lines() <= args.maxlines:
            print(f"{tok}: {locations}")


def main(args=None):
    p = argparse.ArgumentParser(description="Count how often each token is used by the lexers")
    p.add_argument(
        "-v", "--verbose",
        dest="verbose", help="Give more output.",
        default=False, action="store_true"
    )
    p.add_argument(
        "--minfiles",
        dest="minfiles", metavar="COUNT", type=int,
        help="Report all tokens referenced by at least COUNT lexer source files (default %(default)s)",
        default=1
    )
    p.add_argument(
        "--maxfiles",
        dest="maxfiles", metavar="COUNT", type=int,
        help="Report all tokens referenced by at most COUNT lexer source files (default %(default)s)",
        default=1
    )
    p.add_argument(
        "--minlines",
        dest="minlines", metavar="COUNT", type=int,
        help="Report all tokens referenced by at least COUNT lexer source lines (default %(default)s)",
        default=1
    )
    p.add_argument(
        "--maxlines",
        dest="maxlines", metavar="COUNT", type=int,
        help="Report all tokens referenced by at most COUNT lexer source lines (default %(default)s)",
        default=10
    )
    p.add_argument(
        "-s", "--subtoken",
        dest="subtoken",
        help="Include count of references to subtokens in the count for each token (default %(default)s)",
        default=False, action="store_true"
    )

    args = p.parse_args(args)

    if args.verbose:
        print("Looking up all lexers ... ", end="", flush=True)
    count = lookup_all_lexers()
    if args.verbose:
        print(f"found {count:,} lexers")

    if args.verbose:
        print("Fetching lexer source code ... ", end="", flush=True)
    lexer_sources = fetch_lexer_sources()
    if args.verbose:
        print(f"found {len(lexer_sources):,} lexer source files")

    if args.verbose:
        print("Finding token references ... ", end="", flush=True)
    token_references = find_token_references(lexer_sources, args)
    if args.verbose:
        print(f"found references to {len(token_references):,} tokens")

    if args.verbose:
        print()
        print("Result:")
    print_result(token_references, args)


if __name__ == "__main__":
    sys.exit(main())
