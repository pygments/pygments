/*
 * A multi-line comment.
 */

// A single-line comment.

component Counter {
  property name : String = "Joe"
  property step : Number = 1

  state counter : Number = 0
  state label : String = "count"

  const LIMIT : Number = 10

  fun increment {
    next { counter: counter + step }
  }

  fun format (value : Number) : String {
    "#{label}: #{value}"
  }

  get doubled : Number {
    counter * 2
  }

  fun render : Html {
    <div class="counter" data-value={counter} onClick={increment}>
      <span::test>"Hello #{name}!"</span>
      if (counter > LIMIT) {
        <b>{counter}</b>
      } else {
        "below the limit"
      }
      for (item of [1, 2, 3]) {
        <li>{item}</li>
      }
      <input type="text" value={label}/>
    </div>
  }

  style test {
    color: red;
    background: #{counter};
    font-size: 12px;
  }
}

store Session {
  state user : String = "guest"

  fun login (name : String) {
    next { user: name }
  }
}

module Main {
  fun render : Html {
    <>
      <div as ref/>
      <br/>
      <Counter name="world"/>
    </>
  }
}

type Maybe(value) {
  Just(value)
  Nothing
}

type Person {
  name : String,
  age : Number
}

fun compute (a : Number, b : Number) : String {
  let sum = a + b
  let ratio = a / b
  let text = "first" \
             "second"
  let pattern = /ab+c/gi
  let code = `console.log(#{sum})`
  let neg = -5
  let float = 3.14

  case (sum) {
    => "zero"
    1 => "one"
    _ => sum * 2
  }
}
