import types

import pytest

from pygments import formatters, lexers


def test_lazy_packages_remain_standard_modules():
    assert type(lexers) is types.ModuleType
    assert type(formatters) is types.ModuleType


def test_lazy_lexer_attributes_are_loaded_and_cached():
    lexers.__dict__.pop("PythonLexer", None)
    cls = lexers.PythonLexer
    assert lexers.__dict__["PythonLexer"] is cls
    assert lexers.Python3Lexer is cls


def test_lazy_formatter_attributes_are_loaded_and_cached():
    formatters.__dict__.pop("HtmlFormatter", None)
    cls = formatters.HtmlFormatter
    assert formatters.__dict__["HtmlFormatter"] is cls


def test_unknown_lazy_attribute_raises_attribute_error():
    with pytest.raises(AttributeError):
        getattr(lexers, "DefinitelyNotALexer")
    with pytest.raises(AttributeError):
        getattr(formatters, "DefinitelyNotAFormatter")
