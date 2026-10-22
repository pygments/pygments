"""Regression tests for commands following a Bash heredoc delimiter."""

import pytest

from pygments.lexers.shell import BashLexer
from pygments.token import Punctuation, String, Text, Whitespace


@pytest.mark.parametrize('delimiter', ['EOF', "'EOF'", r'\EOF'])
def test_redirect_after_heredoc(delimiter):
    source = f'cat <<{delimiter} >out.txt\nbody\nEOF\n'
    assert list(BashLexer().get_tokens_unprocessed(source)) == [
        (0, Text, 'cat'),
        (3, Whitespace, ' '),
        (4, String, f'<<{delimiter}'),
        (6 + len(delimiter), Whitespace, ' '),
        (7 + len(delimiter), Text, '>out.txt'),
        (15 + len(delimiter), String, '\nbody\nEOF'),
        (24 + len(delimiter), Whitespace, '\n'),
    ]


def test_continued_pipeline_after_heredoc():
    source = "cat <<'EOF' | tr a-z \\\nA-Z\nHello World\nEOF\n"
    tokens = list(BashLexer().get_tokens_unprocessed(source))
    assert ''.join(value for _, _, value in tokens) == source
    assert (12, Punctuation, '|') in tokens
    assert (23, Text, 'A-Z') in tokens
    assert (26, String, '\nHello World\nEOF') in tokens


def test_heredoc_body_is_not_tokenized_as_a_command():
    source = 'cat <<EOF\n>out.txt | $variable\nEOF\n'
    tokens = list(BashLexer().get_tokens_unprocessed(source))
    assert (9, String, '\n>out.txt | $variable\nEOF') in tokens
    assert ''.join(value for _, _, value in tokens) == source
