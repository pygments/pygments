import unittest
import textwrap

from pygments.lexers import GraphQLLexer
from pygments.token import (
    Comment,
    Error,
    Keyword,
    Name,
    Number,
    Punctuation,
    String,
    Text,
)


def assert_tokens_equal(self, text, expected, stack=("root",)):
    text = textwrap.dedent(text)
    lexer = GraphQLLexer()
    tokens_itr = lexer.get_tokens_unprocessed(text, stack=stack)
    self.assertEqual(list(tokens_itr), expected)


def assert_value_equal(self, text, expected):
    if not isinstance(expected, list):
        expected = [expected]
    assert_tokens_equal(self, text, expected, stack=("value",))


def assert_string_equal(self, text, tokens):
    start = (0, String, '"')
    end = (len(text) - 1, String, '"')
    expected = [start, *tokens, end]
    assert_tokens_equal(self, text, expected, stack=("value",))


class TestValue(unittest.TestCase):

    maxDiff = None

    def test_float(self):
        assert_value_equal(self, "5.5", (0, Number.Float, "5.5"))
        assert_value_equal(self, "5e3", (0, Number.Float, "5e3"))
        assert_value_equal(self, "5E3", (0, Number.Float, "5E3"))
        assert_value_equal(self, "5e-3", (0, Number.Float, "5e-3"))
        assert_value_equal(self, "5E-3", (0, Number.Float, "5E-3"))
        assert_value_equal(self, "5e+3", (0, Number.Float, "5e+3"))
        assert_value_equal(self, "5E+3", (0, Number.Float, "5E+3"))
        assert_value_equal(self, "5.1e3", (0, Number.Float, "5.1e3"))
        assert_value_equal(self, "5.1E3", (0, Number.Float, "5.1E3"))
        assert_value_equal(self, "5.1e-3", (0, Number.Float, "5.1e-3"))
        assert_value_equal(self, "5.1E-3", (0, Number.Float, "5.1E-3"))
        assert_value_equal(self, "5.1e+3", (0, Number.Float, "5.1e+3"))
        assert_value_equal(self, "5.1E+3", (0, Number.Float, "5.1E+3"))

    def test_integer(self):
        assert_value_equal(self, "5", (0, Number.Integer, "5"))
        assert_value_equal(self, "-0", (0, Number.Integer, "-0"))
        # Not really following the specifications
        assert_value_equal(self, "-02", (0, Number.Integer, "-02"))

    def test_string(self):
        assert_string_equal(self, '"asdf"', [(1, String, "asdf")])
        assert_string_equal(
            self,
            r'"asdf\""',
            [
                (1, String, "asdf"),
                (5, String.Escape, r"\""),
            ],
        )
        assert_string_equal(
            self, r'"asdf\m"', [(1, String, "asdf"), (5, Error, "\\"), (6, String, "m")]
        )
        assert_string_equal(
            self,
            r'"asdf\u00E8"',
            [
                (1, String, "asdf"),
                (5, String.Escape, r"\u00E8"),
            ],
        )

    def test_boolean(self):
        assert_value_equal(self, "true", (0, Name.Builtin, "true"))
        assert_value_equal(self, "false", (0, Name.Builtin, "false"))

    def test_variable(self):
        assert_value_equal(self, "$hello", (0, Name.Variable, "$hello"))
        assert_value_equal(
            self,
            "$0hello",
            [
                (0, Error, "$"),
                (1, Number.Integer, "0"),
                (2, Name.Constant, "hello"),
            ],
        )

    def test_list_value(self):
        assert_value_equal(
            self,
            '[1, 1.1, "abc"]',
            [
                (0, Punctuation, "["),
                (1, Number.Integer, "1"),
                (2, Punctuation, ","),
                (3, Text, " "),
                (4, Number.Float, "1.1"),
                (7, Punctuation, ","),
                (8, Text, " "),
                (9, String, '"'),
                (10, String, "abc"),
                (13, String, '"'),
                (14, Punctuation, "]"),
            ],
        )
        assert_value_equal(
            self,
            '[1, 1.1, ["abc"]]',
            [
                (0, Punctuation, "["),
                (1, Number.Integer, "1"),
                (2, Punctuation, ","),
                (3, Text, " "),
                (4, Number.Float, "1.1"),
                (7, Punctuation, ","),
                (8, Text, " "),
                (9, Punctuation, "["),
                (10, String, '"'),
                (11, String, "abc"),
                (14, String, '"'),
                (15, Punctuation, "]"),
                (16, Punctuation, "]"),
            ],
        )


class TestGraphqlLexer(unittest.TestCase):

    maxDiff = None

    def assert_builtin(self, text):
        assert_tokens_equal(self, text, [(0, Name.Builtin, text)], stack=("builtins",))

    def test_keyworkd(self):
        assert_tokens_equal(self, "type", [(0, Keyword, "type")])
        assert_tokens_equal(self, "schema", [(0, Keyword, "schema")])
        assert_tokens_equal(self, "extend", [(0, Keyword, "extend")])
        assert_tokens_equal(self, "enum", [(0, Keyword, "enum")])
        assert_tokens_equal(self, "scalar", [(0, Keyword, "scalar")])
        assert_tokens_equal(self, "implements", [(0, Keyword, "implements")])
        assert_tokens_equal(self, "interface", [(0, Keyword, "interface")])
        assert_tokens_equal(self, "union", [(0, Keyword, "union")])
        assert_tokens_equal(self, "input", [(0, Keyword, "input")])
        assert_tokens_equal(self, "directive", [(0, Keyword, "directive")])

    def test_arguments_integer(self):
        text = """\
        {
          field(arg: 24)
          field
        }
        """

        expected = [
            (0, Punctuation, "{"),
            (1, Text, "\n  "),
            (4, Name, "field"),
            (9, Punctuation, "("),
            (10, Name, "arg"),
            # (10, Name.Attribute, 'arg'),
            (13, Punctuation, ":"),
            (14, Text, " "),
            (15, Number.Integer, "24"),
            (17, Punctuation, ")"),
            (18, Text, "\n  "),
            (21, Name, "field"),
            (26, Text, "\n"),
            (27, Punctuation, "}"),
            (28, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_arguments(self):
        text = """\
        {
          human(id: "1000") {
            name
            height
          }
        }
        """
        expected = [
            (0, Punctuation, "{"),
            (1, Text, "\n  "),
            (4, Name, "human"),
            (9, Punctuation, "("),
            # (10, Name.Attribute, 'id'),
            (10, Name, "id"),
            (12, Punctuation, ":"),
            (13, Text, " "),
            (14, String, '"'),
            (15, String, "1000"),
            (19, String, '"'),
            (20, Punctuation, ")"),
            (21, Text, " "),
            (22, Punctuation, "{"),
            (23, Text, "\n    "),
            (28, Name, "name"),
            (32, Text, "\n    "),
            (37, Name, "height"),
            (43, Text, "\n  "),
            (46, Punctuation, "}"),
            (47, Text, "\n"),
            (48, Punctuation, "}"),
            (49, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_argument_scalar(self):
        text = """\
        {
          human(id: "1000") {
            name
            height(unit: FOOT)
          }
        }
        """
        expected = [
            (0, Punctuation, "{"),
            (1, Text, "\n  "),
            (4, Name, "human"),
            (9, Punctuation, "("),
            # (10, Name.Attribute, 'id'),
            (10, Name, "id"),
            (12, Punctuation, ":"),
            (13, Text, " "),
            (14, String, '"'),
            (15, String, "1000"),
            (19, String, '"'),
            (20, Punctuation, ")"),
            (21, Text, " "),
            (22, Punctuation, "{"),
            (23, Text, "\n    "),
            (28, Name, "name"),
            (32, Text, "\n    "),
            (37, Name, "height"),
            (43, Punctuation, "("),
            # (44, Name.Attribute, 'unit'),
            (44, Name, "unit"),
            (48, Punctuation, ":"),
            (49, Text, " "),
            (50, Name.Constant, "FOOT"),
            (54, Punctuation, ")"),
            (55, Text, "\n  "),
            (58, Punctuation, "}"),
            (59, Text, "\n"),
            (60, Punctuation, "}"),
            (61, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_alias(self):
        text = """\
        {
          empireHero: hero(episode: EMPIRE) {
            name
          }
          jediHero: hero(episode: JEDI) {
            name
          }
        }
        """
        expected = [
            (0, Punctuation, "{"),
            (1, Text, "\n  "),
            (4, Name.Label, "empireHero"),
            (14, Punctuation, ":"),
            (15, Text, " "),
            (16, Name, "hero"),
            (20, Punctuation, "("),
            # (21, Name.Attribute, 'episode'),
            (21, Name, "episode"),
            (28, Punctuation, ":"),
            (29, Text, " "),
            (30, Name.Constant, "EMPIRE"),
            (36, Punctuation, ")"),
            (37, Text, " "),
            (38, Punctuation, "{"),
            (39, Text, "\n    "),
            (44, Name, "name"),
            (48, Text, "\n  "),
            (51, Punctuation, "}"),
            (52, Text, "\n  "),
            (55, Name.Label, "jediHero"),
            (63, Punctuation, ":"),
            (64, Text, " "),
            (65, Name, "hero"),
            (69, Punctuation, "("),
            # (70, Name.Attribute, 'episode'),
            (70, Name, "episode"),
            (77, Punctuation, ":"),
            (78, Text, " "),
            (79, Name.Constant, "JEDI"),
            (83, Punctuation, ")"),
            (84, Text, " "),
            (85, Punctuation, "{"),
            (86, Text, "\n    "),
            (91, Name, "name"),
            (95, Text, "\n  "),
            (98, Punctuation, "}"),
            (99, Text, "\n"),
            (100, Punctuation, "}"),
            (101, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_variables(self):
        text = """\
        mutation testVar($int: Int, $myType: MyType,
            $required: String!, $list: [Int!]!)
        {
          doSomenthing(int: $int, myType: $myType,
              required: $required, list: $list)
          {
            response
          }
        }
        """
        expected = [
            (0, Keyword, "mutation"),
            (8, Text, " "),
            (9, Name.Function, "testVar"),
            (16, Punctuation, "("),
            (17, Name.Variable, "$int"),
            (21, Punctuation, ":"),
            (22, Text, " "),
            (23, Name.Builtin, "Int"),
            (26, Punctuation, ","),
            (27, Text, " "),
            (28, Name.Variable, "$myType"),
            (35, Punctuation, ":"),
            (36, Text, " "),
            (37, Name.Class, "MyType"),
            (43, Punctuation, ","),
            (44, Text, "\n    "),
            (49, Name.Variable, "$required"),
            (58, Punctuation, ":"),
            (59, Text, " "),
            (60, Name.Builtin, "String"),
            (66, Punctuation, "!"),
            (67, Punctuation, ","),
            (68, Text, " "),
            (69, Name.Variable, "$list"),
            (74, Punctuation, ":"),
            (75, Text, " "),
            (76, Punctuation, "["),
            (77, Name.Builtin, "Int"),
            (80, Punctuation, "!"),
            (81, Punctuation, "]"),
            (82, Punctuation, "!"),
            (83, Punctuation, ")"),
            (84, Text, "\n"),
            (85, Punctuation, "{"),
            (86, Text, "\n  "),
            (89, Name, "doSomenthing"),
            (101, Punctuation, "("),
            (102, Name, "int"),
            (105, Punctuation, ":"),
            (106, Text, " "),
            (107, Name.Variable, "$int"),
            (111, Punctuation, ","),
            (112, Text, " "),
            (113, Name, "myType"),
            (119, Punctuation, ":"),
            (120, Text, " "),
            (121, Name.Variable, "$myType"),
            (128, Punctuation, ","),
            (129, Text, "\n      "),
            (136, Name, "required"),
            (144, Punctuation, ":"),
            (145, Text, " "),
            (146, Name.Variable, "$required"),
            (155, Punctuation, ","),
            (156, Text, " "),
            (157, Name, "list"),
            (161, Punctuation, ":"),
            (162, Text, " "),
            (163, Name.Variable, "$list"),
            (168, Punctuation, ")"),
            (169, Text, "\n  "),
            (172, Punctuation, "{"),
            (173, Text, "\n    "),
            (178, Name, "response"),
            (186, Text, "\n  "),
            (189, Punctuation, "}"),
            (190, Text, "\n"),
            (191, Punctuation, "}"),
            (192, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_operation_definition_directives(self):
        text = """\
        query ($foo: Boolean = true, $bar: Boolean = false) {
          field @skip(if: $foo) {
            subfieldA
          }
          field @skip(if: $bar) {
            subfieldB
          }
        }
        """
        expected = [
            (0, Keyword, "query"),
            (5, Text, " "),
            (6, Punctuation, "("),
            (7, Name.Variable, "$foo"),
            (11, Punctuation, ":"),
            (12, Text, " "),
            (13, Name.Builtin, "Boolean"),
            (20, Text, " "),
            (21, Punctuation, "="),
            (22, Text, " "),
            (23, Name.Builtin, "true"),
            (27, Punctuation, ","),
            (28, Text, " "),
            (29, Name.Variable, "$bar"),
            (33, Punctuation, ":"),
            (34, Text, " "),
            (35, Name.Builtin, "Boolean"),
            (42, Text, " "),
            (43, Punctuation, "="),
            (44, Text, " "),
            (45, Name.Builtin, "false"),
            (50, Punctuation, ")"),
            (51, Text, " "),
            (52, Punctuation, "{"),
            (53, Text, "\n  "),
            (56, Name, "field"),
            (61, Text, " "),
            (62, Name.Decorator, "@skip"),
            (67, Punctuation, "("),
            (68, Name, "if"),
            (70, Punctuation, ":"),
            (71, Text, " "),
            (72, Name.Variable, "$foo"),
            (76, Punctuation, ")"),
            (77, Text, " "),
            (78, Punctuation, "{"),
            (79, Text, "\n    "),
            (84, Name, "subfieldA"),
            (93, Text, "\n  "),
            (96, Punctuation, "}"),
            (97, Text, "\n  "),
            (100, Name, "field"),
            (105, Text, " "),
            (106, Name.Decorator, "@skip"),
            (111, Punctuation, "("),
            (112, Name, "if"),
            (114, Punctuation, ":"),
            (115, Text, " "),
            (116, Name.Variable, "$bar"),
            (120, Punctuation, ")"),
            (121, Text, " "),
            (122, Punctuation, "{"),
            (123, Text, "\n    "),
            (128, Name, "subfieldB"),
            (137, Text, "\n  "),
            (140, Punctuation, "}"),
            (141, Text, "\n"),
            (142, Punctuation, "}"),
            (143, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_operation_definition_multi_directives(self):
        text = """\
        query ($foo: Boolean = true, $bar: Boolean = false) {
          field @skip(if: $foo) @skip(if: $bar)
        }
        """
        expected = [
            (0, Keyword, "query"),
            (5, Text, " "),
            (6, Punctuation, "("),
            (7, Name.Variable, "$foo"),
            (11, Punctuation, ":"),
            (12, Text, " "),
            (13, Name.Builtin, "Boolean"),
            (20, Text, " "),
            (21, Punctuation, "="),
            (22, Text, " "),
            (23, Name.Builtin, "true"),
            (27, Punctuation, ","),
            (28, Text, " "),
            (29, Name.Variable, "$bar"),
            (33, Punctuation, ":"),
            (34, Text, " "),
            (35, Name.Builtin, "Boolean"),
            (42, Text, " "),
            (43, Punctuation, "="),
            (44, Text, " "),
            (45, Name.Builtin, "false"),
            (50, Punctuation, ")"),
            (51, Text, " "),
            (52, Punctuation, "{"),
            (53, Text, "\n  "),
            (56, Name, "field"),
            (61, Text, " "),
            (62, Name.Decorator, "@skip"),
            (67, Punctuation, "("),
            (68, Name, "if"),
            (70, Punctuation, ":"),
            (71, Text, " "),
            (72, Name.Variable, "$foo"),
            (76, Punctuation, ")"),
            (77, Text, " "),
            (78, Name.Decorator, "@skip"),
            (83, Punctuation, "("),
            (84, Name, "if"),
            (86, Punctuation, ":"),
            (87, Text, " "),
            (88, Name.Variable, "$bar"),
            (92, Punctuation, ")"),
            (93, Text, "\n"),
            (94, Punctuation, "}"),
            (95, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_fragment_spread(self):
        text = """\
        query withNestedFragments {
          user(id: 4) {
            friends(first: 10) {
              ...friendFields
            }
            mutualFriends(first: 10) {
              ...friendFields @skip(if: true)
            }
          }
        }
        """
        expected = [
            (0, Keyword, "query"),
            (5, Text, " "),
            (6, Name.Function, "withNestedFragments"),
            (25, Text, " "),
            (26, Punctuation, "{"),
            (27, Text, "\n  "),
            (30, Name, "user"),
            (34, Punctuation, "("),
            (35, Name, "id"),
            (37, Punctuation, ":"),
            (38, Text, " "),
            (39, Number.Integer, "4"),
            (40, Punctuation, ")"),
            (41, Text, " "),
            (42, Punctuation, "{"),
            (43, Text, "\n    "),
            (48, Name, "friends"),
            (55, Punctuation, "("),
            (56, Name, "first"),
            (61, Punctuation, ":"),
            (62, Text, " "),
            (63, Number.Integer, "10"),
            (65, Punctuation, ")"),
            (66, Text, " "),
            (67, Punctuation, "{"),
            (68, Text, "\n      "),
            (75, Punctuation, "..."),
            (78, Name, "friendFields"),
            (90, Text, "\n    "),
            (95, Punctuation, "}"),
            (96, Text, "\n    "),
            (101, Name, "mutualFriends"),
            (114, Punctuation, "("),
            (115, Name, "first"),
            (120, Punctuation, ":"),
            (121, Text, " "),
            (122, Number.Integer, "10"),
            (124, Punctuation, ")"),
            (125, Text, " "),
            (126, Punctuation, "{"),
            (127, Text, "\n      "),
            (134, Punctuation, "..."),
            (137, Name, "friendFields"),
            (149, Text, " "),
            (150, Name.Decorator, "@skip"),
            (155, Punctuation, "("),
            (156, Name, "if"),
            (158, Punctuation, ":"),
            (159, Text, " "),
            (160, Name.Builtin, "true"),
            (164, Punctuation, ")"),
            (165, Text, "\n    "),
            (170, Punctuation, "}"),
            (171, Text, "\n  "),
            (174, Punctuation, "}"),
            (175, Text, "\n"),
            (176, Punctuation, "}"),
            (177, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_inline_fragment(self):
        text = """\
        query inlineFragmentTyping {
          profiles(handles: ["zuck", "cocacola"]) {
            handle
            ... on User {
              friends {
                count
              }
            }
            ... on Page {
              likers {
                count
              }
            }
          }
        }
        """
        expected = [
            (0, Keyword, "query"),
            (5, Text, " "),
            (6, Name.Function, "inlineFragmentTyping"),
            (26, Text, " "),
            (27, Punctuation, "{"),
            (28, Text, "\n  "),
            (31, Name, "profiles"),
            (39, Punctuation, "("),
            (40, Name, "handles"),
            (47, Punctuation, ":"),
            (48, Text, " "),
            (49, Punctuation, "["),
            (50, String, '"'),
            (51, String, "zuck"),
            (55, String, '"'),
            (56, Punctuation, ","),
            (57, Text, " "),
            (58, String, '"'),
            (59, String, "cocacola"),
            (67, String, '"'),
            (68, Punctuation, "]"),
            (69, Punctuation, ")"),
            (70, Text, " "),
            (71, Punctuation, "{"),
            (72, Text, "\n    "),
            (77, Name, "handle"),
            (83, Text, "\n    "),
            (88, Punctuation, "..."),
            (91, Text, " "),
            (92, Keyword, "on"),
            (94, Text, " "),
            (95, Name.Class, "User"),
            (99, Text, " "),
            (100, Punctuation, "{"),
            (101, Text, "\n      "),
            (108, Name, "friends"),
            (115, Text, " "),
            (116, Punctuation, "{"),
            (117, Text, "\n        "),
            (126, Name, "count"),
            (131, Text, "\n      "),
            (138, Punctuation, "}"),
            (139, Text, "\n    "),
            (144, Punctuation, "}"),
            (145, Text, "\n    "),
            (150, Punctuation, "..."),
            (153, Text, " "),
            (154, Keyword, "on"),
            (156, Text, " "),
            (157, Name.Class, "Page"),
            (161, Text, " "),
            (162, Punctuation, "{"),
            (163, Text, "\n      "),
            (170, Name, "likers"),
            (176, Text, " "),
            (177, Punctuation, "{"),
            (178, Text, "\n        "),
            (187, Name, "count"),
            (192, Text, "\n      "),
            (199, Punctuation, "}"),
            (200, Text, "\n    "),
            (205, Punctuation, "}"),
            (206, Text, "\n  "),
            (209, Punctuation, "}"),
            (210, Text, "\n"),
            (211, Punctuation, "}"),
            (212, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_input_value_nested_array(self):
        text = """\
        {
          node(arg: [1, [2, 3]])
        }
        """
        expected = [
            (0, Punctuation, "{"),
            (1, Text, "\n  "),
            (4, Name, "node"),
            (8, Punctuation, "("),
            (9, Name, "arg"),
            (12, Punctuation, ":"),
            (13, Text, " "),
            (14, Punctuation, "["),
            (15, Number.Integer, "1"),
            (16, Punctuation, ","),
            (17, Text, " "),
            (18, Punctuation, "["),
            (19, Number.Integer, "2"),
            (20, Punctuation, ","),
            (21, Text, " "),
            (22, Number.Integer, "3"),
            (23, Punctuation, "]"),
            (24, Punctuation, "]"),
            (25, Punctuation, ")"),
            (26, Text, "\n"),
            (27, Punctuation, "}"),
            (28, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_input_value_object(self):
        text = """\
        {
          node(arg: {a: "a", b: "b"})
        }
        """
        expected = [
            (0, Punctuation, "{"),
            (1, Text, "\n  "),
            (4, Name, "node"),
            (8, Punctuation, "("),
            (9, Name, "arg"),
            (12, Punctuation, ":"),
            (13, Text, " "),
            (14, Punctuation, "{"),
            (15, Name, "a"),
            (16, Punctuation, ":"),
            (17, Text, " "),
            (18, String, '"'),
            (19, String, "a"),
            (20, String, '"'),
            (21, Punctuation, ","),
            (22, Text, " "),
            (23, Name, "b"),
            (24, Punctuation, ":"),
            (25, Text, " "),
            (26, String, '"'),
            (27, String, "b"),
            (28, String, '"'),
            (29, Punctuation, "}"),
            (30, Punctuation, ")"),
            (31, Text, "\n"),
            (32, Punctuation, "}"),
            (33, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_input_nested_value_object(self):
        text = """\
        {
          node(arg: {a: "a", b: {c: "c", d: {e: "e"}}})
        }
        """
        expected = [
            (0, Punctuation, "{"),
            (1, Text, "\n  "),
            (4, Name, "node"),
            (8, Punctuation, "("),
            (9, Name, "arg"),
            (12, Punctuation, ":"),
            (13, Text, " "),
            (14, Punctuation, "{"),
            (15, Name, "a"),
            (16, Punctuation, ":"),
            (17, Text, " "),
            (18, String, '"'),
            (19, String, "a"),
            (20, String, '"'),
            (21, Punctuation, ","),
            (22, Text, " "),
            (23, Name, "b"),
            (24, Punctuation, ":"),
            (25, Text, " "),
            (26, Punctuation, "{"),
            (27, Name, "c"),
            (28, Punctuation, ":"),
            (29, Text, " "),
            (30, String, '"'),
            (31, String, "c"),
            (32, String, '"'),
            (33, Punctuation, ","),
            (34, Text, " "),
            (35, Name, "d"),
            (36, Punctuation, ":"),
            (37, Text, " "),
            (38, Punctuation, "{"),
            (39, Name, "e"),
            (40, Punctuation, ":"),
            (41, Text, " "),
            (42, String, '"'),
            (43, String, "e"),
            (44, String, '"'),
            (45, Punctuation, "}"),
            (46, Punctuation, "}"),
            (47, Punctuation, "}"),
            (48, Punctuation, ")"),
            (49, Text, "\n"),
            (50, Punctuation, "}"),
            (51, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)

    def test_input_value_mixed(self):
        text = """\
        {
          node1(arg: {a: [1, 2]})
          node2(arg: [1, {a: "a"}])
        }
        """
        expected = [
            (0, Punctuation, "{"),
            (1, Text, "\n  "),
            (4, Name, "node1"),
            (9, Punctuation, "("),
            (10, Name, "arg"),
            (13, Punctuation, ":"),
            (14, Text, " "),
            (15, Punctuation, "{"),
            (16, Name, "a"),
            (17, Punctuation, ":"),
            (18, Text, " "),
            (19, Punctuation, "["),
            (20, Number.Integer, "1"),
            (21, Punctuation, ","),
            (22, Text, " "),
            (23, Number.Integer, "2"),
            (24, Punctuation, "]"),
            (25, Punctuation, "}"),
            (26, Punctuation, ")"),
            (27, Text, "\n  "),
            (30, Name, "node2"),
            (35, Punctuation, "("),
            (36, Name, "arg"),
            (39, Punctuation, ":"),
            (40, Text, " "),
            (41, Punctuation, "["),
            (42, Number.Integer, "1"),
            (43, Punctuation, ","),
            (44, Text, " "),
            (45, Punctuation, "{"),
            (46, Name, "a"),
            (47, Punctuation, ":"),
            (48, Text, " "),
            (49, String, '"'),
            (50, String, "a"),
            (51, String, '"'),
            (52, Punctuation, "}"),
            (53, Punctuation, "]"),
            (54, Punctuation, ")"),
            (55, Text, "\n"),
            (56, Punctuation, "}"),
            (57, Text, "\n"),
        ]
        assert_tokens_equal(self, text, expected)
