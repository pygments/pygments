# -*- coding: utf-8 -*-
"""
    Basic PromQLLexer Tests
    ~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import PromQLLexer
from pygments.token import Token


@pytest.fixture(scope="module")
def lexer():
    yield PromQLLexer()


def test_metric(lexer):
    fragment = "go_gc_duration_seconds"
    tokens = [
        (Token.Name.Variable, "go_gc_duration_seconds"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_metric_one_label(lexer):
    fragment = 'go_gc_duration_seconds{instance="localhost:9090"}'
    tokens = [
        (Token.Name.Variable, "go_gc_duration_seconds"),
        (Token.Punctuation, "{"),
        (Token.Name.Label, "instance"),
        (Token.Operator, "="),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "localhost:9090"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, "}"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_metric_multiple_labels(lexer):
    fragment = 'go_gc_duration_seconds{instance="localhost:9090",job="alertmanager"}'
    tokens = [
        (Token.Name.Variable, "go_gc_duration_seconds"),
        (Token.Punctuation, "{"),
        (Token.Name.Label, "instance"),
        (Token.Operator, "="),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "localhost:9090"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, ","),
        (Token.Name.Label, "job"),
        (Token.Operator, "="),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "alertmanager"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, "}"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_metric_multiple_labels_with_spaces(lexer):
    fragment = 'go_gc_duration_seconds{ instance="localhost:9090",  job="alertmanager" }'
    tokens = [
        (Token.Name.Variable, "go_gc_duration_seconds"),
        (Token.Punctuation, "{"),
        (Token.Text.Whitespace, " "),
        (Token.Name.Label, "instance"),
        (Token.Operator, "="),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "localhost:9090"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "  "),
        (Token.Name.Label, "job"),
        (Token.Operator, "="),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "alertmanager"),
        (Token.Punctuation, '"'),
        (Token.Text.Whitespace, " "),
        (Token.Punctuation, "}"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_expression_and_comment(lexer):
    fragment = 'go_gc_duration_seconds{instance="localhost:9090"} # single comment\n'
    tokens = [
        (Token.Name.Variable, "go_gc_duration_seconds"),
        (Token.Punctuation, "{"),
        (Token.Name.Label, "instance"),
        (Token.Operator, "="),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "localhost:9090"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, "}"),
        (Token.Text.Whitespace, " "),
        (Token.Comment.Single, "# single comment"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_delta(lexer):
    fragment = 'delta(cpu_temp_celsius{host="zeus"}[2h])'
    tokens = [
        (Token.Keyword.Reserved, "delta"),
        (Token.Operator, "("),
        (Token.Name.Variable, "cpu_temp_celsius"),
        (Token.Punctuation, "{"),
        (Token.Name.Label, "host"),
        (Token.Operator, "="),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "zeus"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, "}"),
        (Token.Punctuation, "["),
        (Token.Literal.String, "2h"),
        (Token.Punctuation, "]"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_sum_with_args(lexer):
    fragment = 'sum by (app, proc) (instance_memory_usage_bytes)\n'
    tokens = [
        (Token.Keyword, "sum"),
        (Token.Text.Whitespace, " "),
        (Token.Keyword, "by"),
        (Token.Text.Whitespace, " "),
        (Token.Operator, "("),
        (Token.Name.Variable, "app"),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, " "),
        (Token.Name.Variable, "proc"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, " "),
        (Token.Operator, "("),
        (Token.Name.Variable, "instance_memory_usage_bytes"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_multi_line(lexer):
    fragment = """label_replace(
    sum by (instance) (
        irate(node_disk_read_bytes_total[2m])
    ) / 1024 / 1024,
    "device",
    'disk',
    "instance",
    ".*"
)
"""
    tokens = [
        (Token.Keyword.Reserved, "label_replace"),
        (Token.Operator, "("),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Keyword, "sum"),
        (Token.Text.Whitespace, " "),
        (Token.Keyword, "by"),
        (Token.Text.Whitespace, " "),
        (Token.Operator, "("),
        (Token.Name.Variable, "instance"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, " "),
        (Token.Operator, "("),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "        "),
        (Token.Keyword.Reserved, "irate"),
        (Token.Operator, "("),
        (Token.Name.Variable, "node_disk_read_bytes_total"),
        (Token.Punctuation, "["),
        (Token.Literal.String, "2m"),
        (Token.Punctuation, "]"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, " "),
        (Token.Operator, "/"),
        (Token.Text.Whitespace, " "),
        (Token.Literal.Number.Integer, "1024"),
        (Token.Text.Whitespace, " "),
        (Token.Operator, "/"),
        (Token.Text.Whitespace, " "),
        (Token.Literal.Number.Integer, "1024"),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "device"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Punctuation, "'"),
        (Token.Literal.String, "disk"),
        (Token.Punctuation, "'"),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "instance"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Punctuation, '"'),
        (Token.Literal.String, ".*"),
        (Token.Punctuation, '"'),
        (Token.Text.Whitespace, "\n"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_multi_line_with_offset(lexer):
    fragment = """label_replace(
    avg by(instance)
        (irate(node_cpu_seconds_total{mode = "idle"}[5m] offset 3s)
    ) * 100,
    "device",
    "cpu",
    "instance",
    ".*"
)"""
    tokens = [
        (Token.Keyword.Reserved, "label_replace"),
        (Token.Operator, "("),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Keyword, "avg"),
        (Token.Text.Whitespace, " "),
        (Token.Keyword, "by"),
        (Token.Operator, "("),
        (Token.Name.Variable, "instance"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "        "),
        (Token.Operator, "("),
        (Token.Keyword.Reserved, "irate"),
        (Token.Operator, "("),
        (Token.Name.Variable, "node_cpu_seconds_total"),
        (Token.Punctuation, "{"),
        (Token.Name.Label, "mode"),
        (Token.Text.Whitespace, " "),
        (Token.Operator, "="),
        (Token.Text.Whitespace, " "),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "idle"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, "}"),
        (Token.Punctuation, "["),
        (Token.Literal.String, "5m"),
        (Token.Punctuation, "]"),
        (Token.Text.Whitespace, " "),
        (Token.Keyword, "offset"),
        (Token.Text.Whitespace, " "),
        (Token.Literal.String, "3s"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, " "),
        (Token.Operator, "*"),
        (Token.Text.Whitespace, " "),
        (Token.Literal.Number.Integer, "100"),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "device"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "cpu"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Punctuation, '"'),
        (Token.Literal.String, "instance"),
        (Token.Punctuation, '"'),
        (Token.Punctuation, ","),
        (Token.Text.Whitespace, "\n"),
        (Token.Text.Whitespace, "    "),
        (Token.Punctuation, '"'),
        (Token.Literal.String, ".*"),
        (Token.Punctuation, '"'),
        (Token.Text.Whitespace, "\n"),
        (Token.Operator, ")"),
        (Token.Text.Whitespace, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
