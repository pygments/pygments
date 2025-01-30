//// An example file that contains all the basic Gleam syntax

import gleam/io
import gleam/io.{println}
import gleam/int
import gleam/float
import gleam/string
import gleam/bool
import gleam/option.{type Option, None, Some}

const ints: List(Int) = [1, 2, 3]

const floats = [1.0, 2.0, 3.0]

pub type UserId =
  Int

pub type Season {
  Spring
  Summer
  Autumn
  Winter
}

pub type SchoolPerson {
  Teacher(name: String, subject: String)
  Student(String)
}

pub type Option(inner) {
  Some(inner)
  None
}

// An option of string
pub const name: Option(String) = Some("Annah")

// An option of int
pub const level: Option(Int) = Some(10)

pub fn main() {
  // Print to the console
  io.println("Hello, Joe!")

  // Use the function in a qualified fashion
  io.println("This is qualified")

  // Or an unqualified fashion
  println("This is unqualified")

  io.debug(1 + 1)
  io.debug(5 - 1)
  io.debug(5 / 2)
  io.debug(3 * 3)
  io.debug(5 % 2)

  // Int comparisons
  io.debug(2 > 1)
  io.debug(2 < 1)
  io.debug(2 >= 1)
  io.debug(2 <= 1)

  // Equality works for any type
  io.debug(1 == 1)
  io.debug(2 == 1)

  // Standard library int functions
  io.debug(int.max(42, 77))
  io.debug(int.clamp(5, 10, 20))

  // Float arithmetic
  io.debug(1.0 +. 1.5)
  io.debug(5.0 -. 1.5)
  io.debug(5.0 /. 2.5)
  io.debug(3.0 *. 3.5)

  // Float comparisons
  io.debug(2.2 >. 1.3)
  io.debug(2.2 <. 1.3)
  io.debug(2.2 >=. 1.3)
  io.debug(2.2 <=. 1.3)

  // Equality works for any type
  io.debug(1.1 == 1.1)
  io.debug(2.1 == 1.2)

  // Division by zero is not an error
  io.debug(3.14 /. 0.0)

  // Standard library float functions
  io.debug(float.max(2.0, 9.5))
  io.debug(float.ceiling(5.4))

  // Underscores
  io.debug(1_000_000)
  io.debug(10_000.01)

  // Binary, octal, and hex Int literals
  io.debug(0b00001111)
  io.debug(0o17)
  io.debug(0xF)

  // Scientific notation Float literals
  io.debug(7.0e7)
  io.debug(3.0e-4)

  io.debug(100 == 100)
  io.debug(1.5 != 0.1)

  // String literals
  io.debug("👩‍💻 こんにちは Gleam 🏳️‍🌈")
  io.debug(
    "multi
    line
    string",
  )
  io.debug("\u{1F600}")

  // Double quote can be escaped
  io.println("\"X\" marks the spot")

  // String concatenation
  io.debug("One " <> "Two")

  // String functions
  io.debug(string.reverse("1 2 3 4 5"))
  io.debug(string.append("abc", "def"))

  // Bool operators
  io.debug(True && False)
  io.debug(True && True)
  io.debug(False || False)
  io.debug(False || True)

  // Bool functions
  io.debug(bool.to_string(True))
  io.debug(bool.to_int(False))

  let x = "Original"
  io.debug(x)

  // Assign `y` to the value of `x`
  let y = x
  io.debug(y)

  // Assign `x` to a new value
  let x = "New"
  io.debug(x)

  // The `y` still refers to the original value
  io.debug(y)

  // This variable is never used
  let _score = 1000

  let _name: String = "Gleam"

  let _is_cool: Bool = True

  let _version: Int = 1

  let one: UserId = 1
  let two: Int = 2

  // UserId and Int are the same type
  io.debug(one == two)

  let fahrenheit = {
    let degrees = 64
    degrees
  }
  // io.debug(degrees) // <- This will not compile

  // Changing order of evaluation
  let celsius = { fahrenheit - 32 } * 5 / 9
  io.debug(celsius)

  let ints = [1, 2, 3]

  io.debug(ints)

  // Immutably prepend
  io.debug([-1, 0, ..ints])

  // Uncomment this to see the error
  // io.debug(["zero", ..ints])

  // The original lists are unchanged
  io.debug(ints)

  io.debug(ints)
  io.debug(ints == [1, 2, 3])

  io.debug(floats)
  io.debug(floats == [1.0, 2.0, 3.0])

  io.debug(double(10))

  io.debug(<<6147:size(16)>>)

  // A bit array of UTF8 data
  io.debug(<<"Hello, Joe!":utf8>>)

  // Assign an anonymous function to a variable
  let add_one = fn(a) { a + 1 }
  io.debug(twice(1, add_one))

  // Pass an anonymous function as an argument
  io.debug(twice(1, fn(a) { a * 2 }))

  // These two statements are equivalent
  let add_one_v1 = fn(x) { add(1, x) }
  let add_one_v2 = add(1, _)
  let add_one = fn(x) { x + 1 }
  let exclaim = fn(x) { x <> "!" }

  // Without the pipe operator
  io.debug(string.drop_left(string.drop_right("Hello, Joe!", 1), 7))

  // With the pipe operator
  "Hello, Mike!"
  |> string.drop_right(1)
  |> string.drop_left(7)
  |> io.debug

  // Changing order with function capturing
  "1"
  |> string.append("2")
  |> string.append("3", _)
  |> io.debug

  // Using labelled arguments
  io.debug(calculate(1, add: 2, multiply: 3))

  // case expressions
  let result = case x {
    // Match specific values
    0 -> "Zero"
    1 -> "One"

    // Match any other value
    _ -> "Other"
  }

  let result = case int.random(5) {
    // Match specific values
    0 -> "Zero"
    1 -> "One"

    // Match any other value and assign it to a variable
    other -> "It is " <> int.to_string(other)
  }

  case x {
    "Hello, " <> name -> name
    _ -> "Unknown"
  }

  // list patterns
  let result = case x {
    [] -> "Empty list"
    [1] -> "List of just 1"
    [4, ..] -> "List starting with 4"
    [_, _] -> "List of 2 elements"
    _ -> "Some other list"
  }

  // multiple subjects
  let result = case x, y {
    0, 0 -> "Both are zero"
    0, _ -> "First is zero"
    _, 0 -> "Second is zero"
    _, _ -> "Neither are zero"
  }

  // alternative patterns
  let result = case number {
    2 | 4 | 6 | 8 -> "This is an even number"
    1 | 3 | 5 | 7 -> "This is an odd number"
    _ -> "I'm not sure"
  }

  // pattern aliases
  case lists {
    [[_, ..] as first, ..] -> first
    [_, ..rest] -> get_first_non_empty(rest)
    [] -> []
  }

  // guards
  case lists {
    [first, ..] if first > limit -> first
    [_, ..rest] -> get_first_larger(rest, limit)
    [] -> 0
  }

  // tuples
  let triple = #(1, 2.2, "three")
  let #(a, _, _) = triple

  case season {
    Spring -> "Mild"
    Summer -> "Hot"
    Autumn -> "Windy"
    Winter -> "Cold"
  }

  let teacher = Teacher(name: "Miss Percy", subject: "Physics")

  let scores = dict.from_list([#("Lucy", 13), #("Drew", 15)])
}

fn double(a: Int) -> Int {
  multiply(a, 2)
}

fn multiply(a: Int, b: Int) -> Int {
  a * b
}

fn twice(argument: Int, passed_function: fn(Int) -> Int) -> Int {
  passed_function(passed_function(argument))
}

fn add_one(argument: Int) -> Int {
  argument + 1
}

fn twice(argument: Int, function: fn(Int) -> Int) -> Int {
  function(function(argument))
}

// Labelled arguments
fn calculate(value: Int, add addend: Int, multiply multiplier: Int) {
  value * multiplier + addend
}

@deprecated("Use new_function instead")
fn old_function() {
  Nil
}

fn sum_list(list: List(Int), total: Int) -> Int {
  case list {
    [first, ..rest] -> sum_list(rest, total + first)
    [] -> total
  }
}

pub fn without_use() {
  result.try(get_username(), fn(username) {
    result.try(get_password(), fn(password) {
      result.map(log_in(username, password), fn(greeting) {
        greeting <> ", " <> username
      })
    })
  })
}

pub fn with_use() {
  use username <- result.try(get_username())
  use password <- result.try(get_password())
  use greeting <- result.map(log_in(username, password))
  greeting <> ", " <> username
}

pub fn main() {
  let x = {
    use username <- result.try(get_username())
    use password <- result.try(get_password())
    use greeting <- result.map(log_in(username, password))
    greeting <> ", " <> username
  }

  case x {
    Ok(greeting) -> io.println(greeting)
    Error(error) -> io.println("ERROR:" <> error)
  }
}

pub fn main() {
  todo as "I haven't written this code yet!"
}

pub fn todo_without_reason() {
  todo
}

// panic
pub fn print_score(score: Int) {
  case score {
    score if score > 1000 -> io.println("High score!")
    score if score > 0 -> io.println("Still working on it")
    _ -> panic as "Scores should never be negative!"
  }
}

// let assert
pub fn unsafely_get_first_element(items: List(a)) -> a {
  // This will panic if the list is empty.
  // A regular `let` would not permit this partial pattern
  let assert [first, ..] = items
  first
}

// externals
// An external function that creates an instance of the type
@external(erlang, "calendar", "local_time")
@external(javascript, "./my_package_ffi.mjs", "now")
pub fn now() -> DateTime
