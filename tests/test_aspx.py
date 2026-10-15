"""Client JavaScript and server code routing in ASP.NET pages."""

import pytest

from pygments.lexers.dotnet import CSharpAspxLexer, VbNetAspxLexer
from pygments.token import Error, Keyword, Name, String


@pytest.mark.parametrize('lexer', [CSharpAspxLexer, VbNetAspxLexer])
@pytest.mark.parametrize('attributes', [
    'language="javascript"',
    "language='JavaScript'",
    'type="text/javascript"',
    'type="module"',
    '',
    'data-example="runat=server"',
    'data-example="x > y" language="javascript"',
])
def test_client_javascript(lexer, attributes):
    source = '<script ' + attributes + '>if (a != null && a.b) c = 5.0;</script>'
    tokens = list(lexer().get_tokens(source))
    assert (Keyword.Constant, 'null') in tokens
    assert (Keyword, 'null') not in tokens
    assert ''.join(value for _, value in tokens) == source + '\n'
    assert not any(kind in Error for kind, _ in tokens)


@pytest.mark.parametrize('attributes', [
    'runat="server"',
    "language='C#' RUNAT = 'SERVER'",
    'runat=server',
    'type="text/javascript" runat="server"',
])
def test_server_script_stays_csharp(attributes):
    source = '<script ' + attributes + '>if (a != null) c = 5;</script>'
    tokens = list(CSharpAspxLexer().get_tokens(source))
    assert (Keyword, 'null') in tokens
    assert (Keyword.Constant, 'null') not in tokens
    assert ''.join(value for _, value in tokens) == source + '\n'


def test_server_script_stays_vb():
    tokens = list(VbNetAspxLexer().get_tokens('<script runat="server">Dim x = Nothing</script>'))
    assert (Keyword, 'Nothing') in tokens


def test_script_tag_case():
    source = '<SCRIPT language="javascript">const x = null;</SCRIPT>'
    tokens = list(CSharpAspxLexer().get_tokens(source))
    assert (Keyword.Constant, 'null') in tokens
    assert ''.join(value for _, value in tokens) == source + '\n'


@pytest.mark.parametrize('lexer,server_value', [
    (CSharpAspxLexer, 'null'),
    (VbNetAspxLexer, 'Nothing'),
])
def test_embedded_server_expression_in_client_string(lexer, server_value):
    source = ('<script>const x = "before<%= ' + server_value +
              ' %>after"; const y = null;</script>')
    tokens = list(lexer().get_tokens(source))
    assert (Keyword, server_value) in tokens
    assert (Keyword.Constant, 'null') in tokens
    assert (Name.Tag, '<%=') in tokens
    assert any(kind in String and 'after' in value for kind, value in tokens)
    assert ''.join(value for _, value in tokens) == source + '\n'
    assert not any(kind in Error for kind, _ in tokens)


def test_server_block_outside_script():
    source = '<p><%= null %></p><script>const x = null;</script>'
    tokens = list(CSharpAspxLexer().get_tokens(source))
    assert (Keyword, 'null') in tokens
    assert (Keyword.Constant, 'null') in tokens
    assert ''.join(value for _, value in tokens) == source + '\n'
