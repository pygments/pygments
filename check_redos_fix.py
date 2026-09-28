"""Standalone regression check for CVE-2021-27291-style ReDoS in ScalaLexer.

Verifies that lexing an unterminated Scala string with many trailing
backslashes completes quickly (instead of hanging via catastrophic
backtracking), and that normal escaped strings still lex correctly.
"""
import sys
import time

from pygments.lexers.jvm import ScalaLexer

TIMEOUT = 2.0


def check_redos():
    src = '"' + '\\' * 48
    lexer = ScalaLexer()
    start = time.time()
    list(lexer.get_tokens(src))
    elapsed = time.time() - start
    print(f"ReDoS check: lexed {len(src)}-char adversarial input in {elapsed:.4f}s")
    if elapsed >= TIMEOUT:
        print(f"FAIL: took {elapsed:.4f}s, expected < {TIMEOUT}s")
        return False
    return True


def check_normal_string():
    src = '"hello \\"world\\""'
    lexer = ScalaLexer()
    tokens = list(lexer.get_tokens(src))
    text = ''.join(t[1] for t in tokens)
    print(f"Normal string check: {tokens}")
    if text != src + '\n' and text != src:
        # get_tokens appends a trailing newline normalization
        if text.rstrip('\n') != src:
            print("FAIL: token text does not round-trip to source")
            return False
    if len(tokens) < 1:
        print("FAIL: no tokens produced")
        return False
    return True


if __name__ == '__main__':
    ok_redos = check_redos()
    ok_normal = check_normal_string()
    if ok_redos and ok_normal:
        print("PASS")
        sys.exit(0)
    else:
        print("FAIL")
        sys.exit(1)
