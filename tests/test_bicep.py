"""
    Tests for the Bicep lexer
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers import BicepLexer
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
    Whitespace
)


class BicepLexerTest(unittest.TestCase):
    """Test cases for the BicepLexer."""

    def setUp(self):
        self.lexer = BicepLexer()

    def get_tokens(self, text):
        """Helper to get list of tokens from text."""
        return list(self.lexer.get_tokens(text))

    def test_keywords(self):
        """Test that Bicep keywords are recognized."""
        text = 'targetScope resource module param var output'
        tokens = self.get_tokens(text)

        keywords = [tok for tok, val in tokens if tok == Keyword]
        self.assertEqual(len(keywords), 6)

    def test_decorators(self):
        """Test that decorators are recognized."""
        text = '@description("test")\n@secure()'
        tokens = self.get_tokens(text)

        decorators = [val for tok, val in tokens if tok == Name.Decorator]
        self.assertEqual(decorators, ['@description', '@secure'])

    def test_string_single_quoted(self):
        """Test single-quoted strings."""
        text = "'hello world'"
        tokens = self.get_tokens(text)

        string_tokens = [(tok, val) for tok, val in tokens if tok in (String.Single,)]
        self.assertEqual(len(string_tokens), 3)  # Opening quote, content, closing quote

    def test_string_interpolation(self):
        """Test string interpolation with ${}."""
        text = "'Hello ${name}!'"
        tokens = self.get_tokens(text)

        # Check that String.Interpol is present
        interpol_tokens = [tok for tok, val in tokens if tok == String.Interpol]
        self.assertEqual(len(interpol_tokens), 2)  # Opening ${ and closing }

    def test_multiline_string(self):
        """Test multi-line strings with '''."""
        text = "'''line1\nline2\nline3'''"
        tokens = self.get_tokens(text)

        string_tokens = [tok for tok, val in tokens if tok == String.Single]
        self.assertGreater(len(string_tokens), 0)

    def test_comments_single_line(self):
        """Test single-line comments."""
        text = '// This is a comment\nvar x = 1'
        tokens = self.get_tokens(text)

        comment_tokens = [val for tok, val in tokens if tok == Comment.Single]
        self.assertEqual(len(comment_tokens), 1)
        self.assertIn('comment', comment_tokens[0])

    def test_comments_multiline(self):
        """Test multi-line comments."""
        text = '/* This is\na multi-line\ncomment */'
        tokens = self.get_tokens(text)

        comment_tokens = [tok for tok, val in tokens if tok == Comment.Multiline]
        self.assertGreater(len(comment_tokens), 0)

    def test_numbers(self):
        """Test number recognition."""
        text = '42 123 5'
        tokens = self.get_tokens(text)

        number_tokens = [val for tok, val in tokens if tok == Number.Integer]
        self.assertEqual(number_tokens, ['42', '123', '5'])

    def test_operators(self):
        """Test operator recognition."""
        text = '= == != < > <= >= + - * / % && || ! ?? .? =>'
        tokens = self.get_tokens(text)

        operator_tokens = [tok for tok, val in tokens if tok == Operator]
        self.assertGreater(len(operator_tokens), 10)

    def test_ternary_operator(self):
        """Test ternary operator ? :."""
        text = 'condition ? true : false'
        tokens = self.get_tokens(text)

        # Check that ? and : are recognized as operators
        operators = [val for tok, val in tokens if tok == Operator and val in ('?', ':')]
        self.assertEqual(operators, ['?', ':'])

    def test_union_type_pipe(self):
        """Test pipe operator for union types."""
        text = "type SkuType = 'Basic' | 'Standard' | 'Premium'"
        tokens = self.get_tokens(text)

        # Check that | is recognized as an operator
        pipe_tokens = [val for tok, val in tokens if tok == Operator and val == '|']
        self.assertEqual(len(pipe_tokens), 2)

    def test_resource_type_string(self):
        """Test resource type strings with version."""
        text = "'Microsoft.Storage/storageAccounts@2023-01-01'"
        tokens = self.get_tokens(text)

        # Should be recognized as String.Symbol
        symbol_tokens = [tok for tok, val in tokens if tok == String.Symbol]
        self.assertEqual(len(symbol_tokens), 1)

    def test_property_name_attribute(self):
        """Test that property names are recognized as Name.Attribute."""
        text = 'properties: {\n  name: value\n}'
        tokens = self.get_tokens(text)

        # Property names before colons should be Name.Attribute
        attr_tokens = [val for tok, val in tokens if tok == Name.Attribute]
        self.assertGreater(len(attr_tokens), 0)

    def test_boolean_constants(self):
        """Test boolean literals."""
        text = 'true false'
        tokens = self.get_tokens(text)

        bool_tokens = [val for tok, val in tokens if tok == Keyword.Constant]
        self.assertEqual(bool_tokens, ['true', 'false'])

    def test_null_constant(self):
        """Test null literal."""
        text = 'null'
        tokens = self.get_tokens(text)

        # null is treated as a type keyword in Bicep
        null_tokens = [val for tok, val in tokens if tok == Keyword.Type]
        self.assertEqual(null_tokens, ['null'])

    def test_builtin_types(self):
        """Test built-in type keywords."""
        text = 'string int bool object array'
        tokens = self.get_tokens(text)

        type_tokens = [val for tok, val in tokens if tok == Keyword.Type]
        self.assertEqual(len(type_tokens), 5)

    def test_scope_functions(self):
        """Test scope function names."""
        text = 'resourceGroup() subscription() managementGroup() tenant()'
        tokens = self.get_tokens(text)

        # These should be recognized as Keyword.Constant
        scope_tokens = [val for tok, val in tokens if tok == Keyword.Constant]
        self.assertEqual(len(scope_tokens), 4)

    def test_nested_comments(self):
        """Test nested multi-line comments."""
        text = '/* outer /* inner */ still in comment */'
        tokens = self.get_tokens(text)

        # All should be Comment.Multiline
        comment_tokens = [tok for tok, val in tokens if tok == Comment.Multiline]
        self.assertGreater(len(comment_tokens), 0)

    def test_string_escapes(self):
        """Test string escape sequences."""
        text = r"'Line 1\nLine 2\tTabbed'"
        tokens = self.get_tokens(text)

        # Check for String.Escape tokens
        escape_tokens = [val for tok, val in tokens if tok == String.Escape]
        self.assertEqual(len(escape_tokens), 2)  # \n and \t

    def test_analyse_text_resource(self):
        """Test analyse_text with resource declaration."""
        text = "resource storage 'Microsoft.Storage/storageAccounts@2023-01-01' = {}"
        score = self.lexer.analyse_text(text)
        self.assertGreater(score, 0.3)

    def test_analyse_text_targetscope(self):
        """Test analyse_text with targetScope."""
        text = "targetScope = 'subscription'"
        score = self.lexer.analyse_text(text)
        self.assertGreater(score, 0.3)

    def test_analyse_text_decorator_param(self):
        """Test analyse_text with decorator and param."""
        text = "@description('test')\nparam location string"
        score = self.lexer.analyse_text(text)
        self.assertGreater(score, 0.3)


if __name__ == '__main__':
    unittest.main()
