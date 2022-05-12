import re

for x in lines("myfile.txt"):
  if x =~ re"(\w+)=(.*)":
    echo "Key: ", matches[0],
         " Value: ", matches[1]

echo("What's your name? ")
var name: string = readLine(stdin)
if name == "":
  echo("Poor soul, you lost your name?")
elif name == "name":
  echo("Very funny, your name is name.")
else:
  echo("Hi, ", name, "!")

var name = readLine(stdin)
case name
of "":
  echo("Poor soul, you lost your name?")
of "name":
  echo("Very funny, your name is name.")
else:
  echo("Hi, ", name, "!")

from strutils import parseInt

echo("A number please: ")
var n = parseInt(readLine(stdin))
case n
of 0..2, 4..7: echo("The number is in the set: {0, 1, 2, 4, 5, 6, 7}")
of 3, 8: echo("The number is 3 or 8")

echo("Counting to 10: ")
var i = 1
while i <= 10:
  echo($i)
  inc(i)

proc yes(question: string): bool =
  echo(question, " (y/n)")
  while true:
    case readLine(stdin)
    of "y", "Y", "yes", "Yes": return true
    of "n", "N", "no", "No": return false
    else: echo("Please be clear: yes or no")

proc even(n: int): bool

proc odd(n: int): bool =
  if n == 1: return true
  else: return even(n-1)

iterator countup(a, b: int): int =
  var res = a
  while res <= b:
    yield res
    inc(res)

type
  TPerson = object of TObject
    name*: string  # the * means that `name` is accessible from other modules
    age: int       # no * means that the field is hidden from other modules

  TStudent = object of TPerson # TStudent inherits from TPerson
    id: int                    # with an id field

var
  student: TStudent
  person: TPerson
assert(student is TStudent)

echo({'a', 'b', 'c'}.card)
stdout.writeln("Hallo")
var
  f: TFile
if open(f, "numbers.txt"):
  try:
    var a = readLine(f)
    var b = readLine(f)
    echo("sum: " & $(parseInt(a) + parseInt(b)))
  except EOverflow:
    echo("overflow!")
  except EInvalidValue:
    echo("could not convert string to integer")
  except EIO:
    echo("IO error!")
  except:
    echo("Unknown exception!")
    # reraise the unknown exception:
    raise
  finally:
    close(f)


let
  aaa: string = "aaa"
  bbb: int = 3
  ccc: seq[char] = @['a', 'b']
  ddd: float32 = 43.21
  eee: bool = true

