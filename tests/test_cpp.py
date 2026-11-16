import pytest

from pygments.lexers import CppLexer
from pygments.token import Comment, Keyword, Name, Operator, Whitespace


@pytest.mark.parametrize('specifier', ['private', 'protected', 'public'])
@pytest.mark.parametrize('separator', ['', ' ', ' /* access */ '])
def test_first_method_after_access_specifier_is_function(specifier, separator):
    code = (
        "struct Type {\n"
        f"{specifier}{separator}:\n"
        "    void A(int i, double j);\n"
        "    void B(int i, double j);\n"
        "};\n"
    )
    tokens = list(CppLexer().get_tokens(code))
    functions = [value for token, value in tokens if token is Name.Function]
    assert functions == ["A", "B"]
    assert (Keyword, specifier) in tokens
    assert (Operator, ':') in tokens
    if separator:
        assert (Whitespace, ' ') in tokens
    if '/*' in separator:
        assert (Comment.Multiline, '/* access */') in tokens
