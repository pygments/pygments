/* Multiline /
comment */
local x = 100;
/* Multiline
comment */

/**
 * Docs
 **/


local i = import 'foo';
local foo = 'bar';
local lambda = function(foo, bar) ['baz', 'qux'];
local named(foo, bar=10, baz=20) = ['baz', 'qux'];
local in1troduction = 'introduction';
assert 5 > 3: "interesting";
//comment
{
  local foo = "bar",
  spam: 'eggs',
}

{local foo = "bar"}


{
  // this is a comment
  # python-style comment
  assert 1 == 1: 'huh?',
  spam: 'eggs // /* #',
  spam2(foo, bar):: 2,
  spa: 'eggs',
  foo: 'bar',
  block: |||
    Hello
    Block
  |||,
  baz: 'qux' + 'fuzz',
  'm:oo': 'cow',
  "b:oo": ('cow'),
  goo: ['bar', 'ba\nz'],
  spam3: 2.25,
  spam4::: -2.25,
  spam5::: +2.25,
  spam6//funky
  : 'hello' //moo
    , //moo
  ['spa\'m']: 'eggs',
  ["spa\"m2"]: "eggs",
  raw1: @'hello\',
  raw2: @"hello\",
  spam7: foo,
  spam8: lambda(1, 2),
  spam9: self.spam2(3, 4),
  intro: in1troduction,
  spam_10: 10,
  spam_11(y=10):: 11,
  spam12: std.type('null'),
  spam13+: 27,
  spam14: $.spam13,
  spam15: ~5,
  spam16: !false,
  spam17: 0 - 5,
  spam18: 5 & 3,
  spam19: 5 | 3,
  spam20: 5 ^ 3,
  spam21: 5 == 3,
  spam22: 5 < 3,
  spam23: 5 > 3,
  spam24: 5 * 3,
  spam25: 5 / 3,
  spam26: 5 % 3,
}
