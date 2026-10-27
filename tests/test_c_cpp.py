"""
    Pygments C/C++ lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Tests for MSVC braced inline-assembly (``__asm { ... }``) highlighting in
    the C and C++ lexers, plus guards that ordinary C/C++ behaviour is
    unaffected. See https://github.com/pygments/pygments/issues/2942.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import CLexer, CppLexer
from pygments.token import Comment, Error, Keyword, Name, Number, \
    Punctuation, String


@pytest.fixture(scope='module', params=['c', 'cpp'])
def lexer(request):
    """Run every test through both the C and the C++ lexer (AC2)."""
    yield CLexer() if request.param == 'c' else CppLexer()


def tokens(lexer, text):
    return list(lexer.get_tokens(text))



# --- AC1 / AC2: instructions and registers get assembly token types --------

def test_msvc_asm_instructions_and_registers(lexer):
    src = (
        "void f(void) {\n"
        "    __asm {\n"
        "        mov eax, 1\n"
        "        add eax, 2\n"
        "    }\n"
        "    return;\n"
        "}\n"
    )
    toks = tokens(lexer, src)
    # Instruction mnemonics must not remain plain Name tokens.
    assert (Name.Function, 'mov') in toks
    assert (Name.Function, 'add') in toks
    # Registers must be classified as builtins, not plain Name tokens.
    assert (Name.Builtin, 'eax') in toks
    # Guard against the pre-fix behaviour where these were plain Name.
    assert (Name, 'mov') not in toks
    assert (Name, 'eax') not in toks


# --- AC3: whitespace, newlines and empty blocks ----------------------------

def test_msvc_asm_whitespace_and_empty_block(lexer):
    # Newline and spaces between __asm and the opening brace are allowed,
    # and an empty block must not emit Error tokens.
    src = "void f(void) {\n    __asm\n    {\n    }\n}\n"
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    # The surrounding function braces are still punctuation.
    assert (Punctuation, '{') in toks
    assert (Punctuation, '}') in toks


# --- AC4: returns to C/C++ after the block ---------------------------------

def test_msvc_asm_returns_to_c_tokens(lexer):
    src = (
        "void f(void) {\n"
        "    __asm { mov eax, 1 }\n"
        "    int x = 5;\n"
        "    __asm { nop }\n"
        "    return;\n"
        "}\n"
        "int g(void) { return 0; }\n"
    )
    toks = tokens(lexer, src)
    # Code after the first block is normal C/C++ again.
    assert (Keyword.Type, 'int') in toks
    assert (Keyword, 'return') in toks
    # A function declared after the asm-containing function keeps Name.Function.
    assert (Name.Function, 'g') in toks
    # Both asm blocks were recognised.
    assert (Name.Function, 'mov') in toks
    assert (Name.Function, 'nop') in toks


# --- AC4: multiple independent blocks do not leak lexer state --------------

def test_msvc_asm_multiple_blocks(lexer):
    src = (
        "void f(void) {\n"
        "    __asm { mov eax, 1 }\n"
        "    __asm { mov ebx, 2 }\n"
        "}\n"
    )
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    assert (Name.Builtin, 'eax') in toks
    assert (Name.Builtin, 'ebx') in toks


# --- AC5: comments and strings containing braces do not end the block ------

def test_msvc_asm_comments_and_strings(lexer):
    src = (
        "void f(void) {\n"
        "    __asm {\n"
        "        mov eax, 1 ; closing brace } in comment\n"
        '        mov ebx, offset "a } b"\n'
        "        nop\n"
        "    }\n"
        "    return;\n"
        "}\n"
    )
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    # The comment and string braces did not terminate the block early:
    # 'nop' is still inside the asm block and classified as an instruction.
    assert (Name.Function, 'nop') in toks
    # The MASM comment is a comment token.
    assert any(tok in Comment and '}' in val for tok, val in toks)
    # The quoted text is a string token.
    assert any(tok in String and 'a } b' in val for tok, val in toks)
    # Code after the block resumes normally.
    assert (Keyword, 'return') in toks


def test_msvc_asm_c_style_comments_with_braces(lexer):
    # MSVC permits C/C++ comments inside __asm blocks.  A brace inside a
    # // or /* */ comment must not close the block (issue #2942).
    src = (
        "void f(void) {\n"
        "    __asm {\n"
        "        mov eax, 1 // trailing brace } in line comment\n"
        "        mov ebx, 2 /* brace } in block comment */\n"
        "        nop\n"
        "    }\n"
        "    return;\n"
        "}\n"
    )
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    # The braces lived inside comments, so 'nop' is still an instruction.
    assert (Name.Function, 'nop') in toks
    # Both comment forms are classified as comments and keep their brace.
    assert any(tok is Comment.Single and '}' in val for tok, val in toks)
    assert any(tok is Comment.Multiline and '}' in val for tok, val in toks)
    # Highlighting resumes as C/C++ after the block.
    assert (Keyword, 'return') in toks


# --- AC6: nested braces ----------------------------------------------------

def test_msvc_asm_nested_blocks(lexer):
    src = (
        "void f(void) {\n"
        "    __asm {\n"
        "        mov eax, 1\n"
        "        { nop }\n"
        "        add eax, 2\n"
        "    }\n"
        "    return;\n"
        "}\n"
    )
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    # 'add' after the nested brace pair is still an instruction (we did not
    # exit the outer block on the inner closing brace).
    assert (Name.Function, 'add') in toks
    # We returned to C after the real end of the block.
    assert (Keyword, 'return') in toks


# --- AC5: source preservation ----------------------------------------------

@pytest.mark.parametrize('src', [
    "void f(void) {\n    __asm { mov eax, 1 }\n    return;\n}\n",
    "void f(void) {\n    __asm {\n        mov eax, 1 ; } note\n        nop\n    }\n}\n",
    "void f(void) {\n    __asm {\n    }\n}\n",
])
def test_msvc_asm_preserves_source(lexer, src):
    # Concatenating every token value must reproduce the input exactly.
    reconstructed = ''.join(val for _, val in tokens(lexer, src))
    assert reconstructed == src


# --- AC7: ordinary C/C++ is unchanged --------------------------------------

def test_msvc_asm_does_not_change_ordinary_c(lexer):
    src = (
        "int __asmfoo = 1;\n"
        'const char *s = "__asm { not asm }";\n'
        "// __asm { also not asm }\n"
        "int y = 2;\n"
    )
    toks = tokens(lexer, src)
    # Identifier that merely starts with __asm is not treated as a block.
    assert (Name, '__asmfoo') in toks
    # The string literal content stays a string.
    assert any(tok in String and '__asm { not asm }' in val for tok, val in toks)
    # The line comment stays a comment.
    assert any(tok in Comment and '__asm' in val for tok, val in toks)
    # Code is normal.
    assert (Number.Integer, '2') in toks


def test_gnu_style_asm_unchanged(lexer):
    # GNU/paren style asm(...) must not trigger the braced-block handling.
    src = 'void f(void) {\n    asm("nop");\n    __asm__("nop");\n}\n'
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    # The paren-form mnemonic stays inside a C string, not an asm instruction.
    assert any(tok in String and 'nop' in val for tok, val in toks)
    assert (Name.Function, 'nop') not in toks


# --- AC3: unterminated block terminates safely -----------------------------

def test_msvc_asm_unterminated_block(lexer):
    # Incomplete editor input: no closing brace. Lexing must finish and
    # preserve all text rather than hang or drop characters.
    src = "void f(void) {\n    __asm {\n        mov eax, 1\n"
    toks = tokens(lexer, src)
    reconstructed = ''.join(val for _, val in toks)
    assert reconstructed == src
    # The instruction inside the block is still recognised.
    assert (Name.Function, 'mov') in toks


# --- Review regressions: multiline comments, root fragments, labels --------

def test_msvc_asm_multiline_c_comment_with_brace(lexer):
    # A multiline C comment whose body contains a brace must be a single
    # Comment.Multiline token and must not close the asm block (#2942).
    src = (
        "void f(void) {\n"
        "    __asm {\n"
        "        /* comment begins\n"
        "           } still inside comment\n"
        "        */\n"
        "        mov eax, 1\n"
        "    }\n"
        "    return;\n"
        "}\n"
    )
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    # The whole comment, including its brace and newlines, is one token.
    assert any(tok is Comment.Multiline and "} still inside comment" in val
               for tok, val in toks)
    assert (Name.Function, "mov") in toks
    assert (Name.Builtin, "eax") in toks
    assert (Keyword, "return") in toks


def test_msvc_asm_multiline_comment_after_operands(lexer):
    # Multiline comment starting after an instruction's operands, followed by
    # another instruction.
    src = (
        "void f(void) {\n"
        "    __asm {\n"
        "        mov eax, 1 /* note }\n"
        "           still comment */ add eax, 2\n"
        "        nop\n"
        "    }\n"
        "    return;\n"
        "}\n"
    )
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    assert any(tok is Comment.Multiline and "still comment" in val
               for tok, val in toks)
    assert (Name.Function, "add") in toks
    assert (Name.Function, "nop") in toks
    assert (Keyword, "return") in toks


def test_msvc_asm_unterminated_multiline_comment(lexer):
    # An unterminated multiline comment must not drop text or hang, and must
    # not leak into broken C highlighting.
    src = (
        "void f(void) {\n"
        "    __asm {\n"
        "        mov eax, 1 /* unterminated } comment\n"
        "        more text\n"
    )
    toks = tokens(lexer, src)
    assert "".join(val for _, val in toks) == src


def test_msvc_asm_standalone_fragment(lexer):
    # A bare __asm block at the root (no enclosing function) must still be
    # highlighted as assembly (#2942).
    for keyword in ("__asm", "_asm"):
        src = keyword + " {\n    mov eax, 1\n}\n"
        toks = tokens(lexer, src)
        assert not any(tok is Error for tok, _ in toks), (keyword, toks)
        assert (Name.Function, "mov") in toks, (keyword, toks)
        assert (Name.Builtin, "eax") in toks, (keyword, toks)
        assert "".join(val for _, val in toks) == src


def test_msvc_asm_label_then_instruction(lexer):
    # A label followed by an instruction on the same line: the label is
    # Name.Label, the mnemonic is Name.Function.
    src = (
        "void f(void) {\n"
        "    __asm {\n"
        "        again: mov eax, 1\n"
        "        add eax, 2\n"
        "    }\n"
        "    return;\n"
        "}\n"
    )
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    assert (Name.Label, "again") in toks
    assert (Name.Function, "mov") in toks
    assert (Name.Function, "add") in toks
    assert (Name.Builtin, "eax") in toks
    assert (Keyword, "return") in toks

def test_msvc_asm_inline_block_comment_keeps_operand_context(lexer):
    # A /* */ comment with no newline between operands must NOT end the
    # instruction: the operand after it is still an operand (Name), not a
    # new mnemonic (Name.Function).  Standalone root block (#2942).
    src = "__asm { mov /* explanation */ result, eax\n}\n"
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    assert (Name, "result") in toks
    assert (Name.Function, "result") not in toks
    assert (Name.Function, "mov") in toks
    assert any(tok is Comment.Multiline and "explanation" in val
               for tok, val in toks)
    assert "".join(val for _, val in toks) == src


def test_msvc_asm_inline_block_comment_before_size_keyword(lexer):
    # A newline-free /* */ comment before a size keyword must keep operand
    # context so ``dword`` is Keyword.Type, not a mnemonic (Name.Function).
    src = "__asm { mov eax, /* explanation */ dword ptr [ebx]\n}\n"
    toks = tokens(lexer, src)
    assert not any(tok is Error for tok, _ in toks)
    assert (Keyword.Type, "dword") in toks
    assert (Name.Function, "dword") not in toks
    assert (Name.Function, "mov") in toks
    assert any(tok is Comment.Multiline and "explanation" in val
               for tok, val in toks)
    assert "".join(val for _, val in toks) == src
