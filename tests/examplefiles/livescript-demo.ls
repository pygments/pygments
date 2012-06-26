a = -> [1 to 50]
const b = --> [2 til 5]
var c = ~~> 10_000_000km * 500ms
e = (a) -> (b) ~> (c) --> (d, e) ~~> <[list of words]>
dashes-identifiers = ->
  a - a b -- c 1-1 1- -1 a- a a -a
underscores_i$d = ->
  /regexp1/
  //regexp2//g
  'strings' and "strings" and \strings

[2 til 10]
  |> map (* 2)
  |> filter (> 5)
  |> fold (+)

class Class extends Anc-est-or
  (args) ->
    -> ~> !-> !~> !--> !~~>
    <-! # Comment
    <~! /* Comment */

copy = (from, to, callback) -->
  error, data <- read file
  return callback error if error?
  error <~ write file, data
  return callback error if error?
  callback()
