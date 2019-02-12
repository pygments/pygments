# Examples taken from http://crystal-lang.org/docs/
# Copyright 2012-2016 Manas Technology Solutions.


require "http/server"

server = HTTP::Server.new(8080) do |context|
  context.response.content_type = "text/plain"
  context.response.print "Hello world! The time is #{Time.now}"
end

puts "Listening on http://0.0.0.0:8080"
server.listen


module HTTP
  class RequestHandler
  end
end

alias NumericValue = Float32 | Float64 | Int32 | Int64

enum Time::DayOfWeek
end


$global_greeting = "Hello world"

class Greeting
  @@default_greeting = "Hello world"

  def initialize(@custom_greeting = nil)
  end

  def print_greeting
    greeting = @custom_greeting || @@default_greeting
    puts greeting
  end
end


LUCKY_NUMBERS     = [3, 7, 11]
DOCUMENTATION_URL = "http://crystal-lang.org/docs"


module Scorecard
  class Parser
    def parse(score_text)
      begin
        score_text.scan(SCORE_PATTERN) do |match|
          handle_match(match)
        end
      rescue err : ParseError
        # handle error ...
      end
    end
  end
end


module Money
  CURRENCIES = {
    "EUR" => 1.0,
    "ARS" => 10.55,
    "USD" => 1.12,
    "JPY" => 134.15,
  }

  class Amount
    getter :currency, :value

    def initialize(@currency, @value)
    end
  end

  class CurrencyConversion
    def initialize(@amount, @target_currency)
    end

    def amount
      # implement conversion ...
    end
  end
end


i = 0
while i < 10
  proc = ->(x : Int32) do
    spawn do
      puts(x)
    end
  end
  proc.call(i)
  i += 1
end

Fiber.yield


# A buffered channel of capacity 2
channel = Channel(Int32).new(2)

spawn do
  channel.send(1)
  channel.send(2)
  channel.send(3)
end

3.times do |i|
  puts channel.receive
end


class MyDictionary(K, V)
end


MyBox.new(1)       #:: MyBox(Int32)
MyBox.new("hello") #:: MyBox(String)


module Moo(T)
  def t
    T
  end
end

class Foo(U)
  include Moo(U)

  def initialize(@value : U)
  end
end

foo = Foo.new(1)
foo.t # Int32


class Parent(T)
end

class Int32Child < Parent(Int32)
end

class GenericChild(T) < Parent(T)
end


class Person
end


a = 1
ptr = pointerof(a)
ptr[100_000] = 2   # undefined behaviour, probably a segmentation fault


alias Int32OrString = Int32 | String


alias Int32OrNil = Int32?


alias Int32OrNil_ = Int32 | ::Nil


alias Int32Ptr = Int32*


alias Int32Ptr_ = Pointer(Int32)


alias Int32_8 = Int32[8]


alias Int32_8_ = StaticArray(Int32, 8)


alias Int32StringTuple = {Int32, String}


alias Int32StringTuple_ = Tuple(Int32, String)


alias Int32ToString = Int32 -> String


alias Int32ToString_ = Proc(Int32, String)


alias ProcThatReturnsInt32 = -> Int32


alias Int32AndCharToString = Int32, Char -> String


alias ComplexProc = (Int32 -> Int32) -> String


def foo(x : Int32)
  "instance"
end

def foo(x : Int32.class)
  "class"
end

foo 1     # "instance"
foo Int32 # "class"


class Parent
end

class Child1 < Parent
end

class Child2 < Parent
end

ary = [] of Parent.class
ary << Child1
ary << Child2


# Same as not specifying a restriction, not very useful
def foo(x : _)
end

# A bit more useful: any two arguments Proc that returns an Int32:
def foo(x : _, _ -> Int32)
end


#alias SameAsInt32 = typeof(2)
#alias Int32OrString_ = typeof(1, "a")


class Person
  def initialize(name)
    @name = name
    @age = 0
  end

  def name
    @name
  end

  def age
    @age
  end
end


john = Person.new "John"
peter = Person.new "Peter"

john.name #=> "John"
john.age #=> 0

peter.name #=> "Peter"


class Person
  def self.new(name)
    instance = Person.allocate
    instance.initialize(name)
    instance
  end
 end


if a.is_a?(String)
  # here a is a String
end

if b.is_a?(Number)
  # here b is a Number
end


a = some_condition ? 1 : "hello"
# a : Int32 | String

if a.is_a?(Number)
  # a : Int32
else
  # a : String
end


if a.is_a?(String) && b.is_a?(Number)
  # here a is a String and b is a Number
end


a.+(b)


struct Vector2
  getter x, y

  def initialize(@x, @y)
  end

  def +(other)
    Vector2.new(x + other.x, y + other.y)
  end
end

v1 = Vector2.new(1, 2)
v2 = Vector2.new(3, 4)
v1 + v2               #=> Vector2(@x=4, @y=6)




struct Vector2
  def -
    Vector2.new(-x, -y)
  end
end

v1 = Vector2.new(1, 2)
-v1                    #=> Vector2(@x=-1, @y=-2)





class MyArray
  def [](index)
    # ...
  end

  def [](index1, index2, index3)
    # ...
  end

  def []=(index, value)
    # ...
  end
end

array = MyArray.new

array[1]       # invokes the first method
array[1, 2, 3] # invokes the second method
array[1] = 2   # invokes the third method

array.[](1)       # invokes the first method
array.[](1, 2, 3) # invokes the second method
array.[]=(1, 2)   # invokes the third method


raise "OH NO!"
raise Exception.new("Some error")


class MyException < Exception
end


begin
  raise MyException.new("OH NO!")
rescue ex : MyException
  puts "Rescued MyException: #{ex.message}"
end


begin
  # ...
rescue ex : MyException | MyOtherException
  # only MyException or MyOtherException
rescue
  # any other kind of exception
ensure
  puts "Cleanup..."
end


def some_method
  something_dangerous
rescue
  # execute if an exception is raised
end


array = [1, 2, 3]
array[4]  # raises because of IndexError
array[4]? # returns nil because of index out of bounds


def some_proc(&block : Int32 -> Int32)
  block
end

x = 0
proc = ->(i : Int32) { x += i }
proc = some_proc(&proc)
proc.call(1)  #=> 1
proc.call(10) #=> 11
x #=> 11


def add(x, y)
  x + y
end

adder = ->add(Int32, Int32)
adder.call(1, 2) #=> 3


module Curses
  class Window
  end
end

Curses::Window.new


module ItemsSize
  def size
    items.size
  end
end

class Items
  include ItemsSize

  def items
    [1, 2, 3]
  end
end

items = Items.new
items.size #=> 3


module Base64
  extend self

  def encode64(string)
    # ...
  end

  def decode64(string)
    # ...
  end
end

Base64.encode64 "hello" #=> "aGVsbG8="


if some_condition
  a = 1
else
  a = "hello"
end

a_as_int = a as Int32
a_as_int.abs          # works, compiler knows that a_as_int is Int32


ptr = Pointer(Int32).malloc(1)
ptr as Int8*                    #:: Pointer(Int8)


array = [1, 2, 3]

# object_id returns the address of an object in memory,
# so we create a pointer with that address
ptr = Pointer(Void).new(array.object_id)

# Now we cast that pointer to the same type, and
# we should get the same value
array2 = ptr as Array(Int32)
array2.same?(array) #=> true


a = 1
b = a as Int32 | Float64
b #:: Int32 | Float64


ary = [1, 2, 3]

# We want to create an array 1, 2, 3 of Int32 | Float64
ary2 = ary.map { |x| x as Int32 | Float64 }

ary2 #:: Array(Int32 | Float64)
ary2 << 1.5 # OK


class Person
  def initialize(@name)
  end

  def name
    @name
  end
end

a = [] of Person
x = a.map { |f| f.name } # Error: can't infer block return type


a = [] of Person
x = a.map { |f| f.name as String } # OK


Person.new "John"

a = [] of Person
x = a.map { |f| f.name } # OK


loop do
  do_something
  break if some_condition
end


class Point
  def initialize(@x, @y)
  end
end

Point.new 1, 2

# 2 x Int32 = 2 x 4 = 8
instance_sizeof(Point) #=> 12


a = 1
while a < 5
  a += 1
  if a == 3
    next
  end
  puts a
end
# The above prints the numbers 2, 4 and 5


lib C
  # In C: double cos(double x)
  fun cos(value : Float64) : Float64

  fun getch : Int32

  fun srand(seed : UInt32)

  fun exit(status : Int32) : NoReturn

  fun printf(format : UInt8*, ...) : Int32
end

C.cos(1.5) #=> 0.0707372
C.srand(1_u32)

a = 1
b = 2
C.printf "%d + %d = %d\n", a, b, a + b


lib LibSDL
  fun init = SDL_Init(flags : UInt32) : Int32
end

lib LLVMIntrinsics
  fun ceil_f32 = "llvm.ceil.f32"(value : Float32) : Float32
end

lib MyLib
  fun my_fun(some_size : LibC::SizeT)
end

@[Link("pcre")]
lib LibPCRE
end


lib C
  ifdef x86_64
    alias SizeT = UInt64
  else
    alias SizeT = UInt32
  end

  fun memcmp(p1 : Void*, p2 : Void*, size : C::SizeT) : Int32
end


lib X
  enum SomeEnum
    Ten = 10
    Twenty = 10 * 2
    ThirtyTwo = 1 << 5
  end
end


lib X
  enum SomeEnum
    A = 1_u32
  end
end


X::SomeEnum::Zero #=> 0_i8
X::SomeEnum::Two  #=> 2_i8


lib X
  fun callback(f : Int32 -> Int32)
end


f = ->(x : Int32) { x + 1 }
X.callback(f)


X.callback ->(x) { x + 1 }


X.callback nil


lib LibFoo
  fun store_callback(callback : ->)
  fun execute_callback
end

LibFoo.store_callback ->{ raise "OH NO!" }
LibFoo.execute_callback


lib LibFoo
  fun store_callback(callback : ->)

  @[Raises]
  fun execute_callback
end


@[Link("pcre")]
lib PCRE
  INFO_CAPTURECOUNT = 2
end

PCRE::INFO_CAPTURECOUNT #=> 2


lib U
  # In C:
  #
  #  union IntOrFloat {
  #    int some_int;
  #    double some_float;
  #  };
  union IntOrFloat
    some_int : Int32
    some_float : Float64
  end
end


value = U::IntOrFloat.new


value = uninitialized U::IntOrFlaot
value.some_int #=> some garbage value


value = U::IntOrFloat.new
value.some_int = 1
value.some_int #=> 1
value.some_float #=> 4.94066e-324


def change_it(value)
  value.some_int = 1
end

value = U::IntOrFloat.new
change_it value
value.some_int #=> 0


lib C
  # In C:
  #
  #  struct TimeZone {
  #    int minutes_west;
  #    int dst_time;
  #  };
  struct TimeZone
    minutes_west : Int32
    dst_time     : Int32
  end
end


lib C
  # This is a forward declaration
  struct Node
  end

  struct Node
    node : Node*
  end
end


tz = C::TimeZone.new


tz = uninitialized C::TimeZone
tz.minutes_west #=> some garbage value


tz = C::TimeZone.new
tz.minutes_west = 1
tz.minutes_west #=> 1


tz = C::TimeZone.new minutes_west: 1, dst_time: 2
tz.minutes_west #=> 1
tz.dst_time     #=> 2


def change_it(tz)
  tz.minutes_west = 1
end

tz = C::TimeZone.new
change_it tz
tz.minutes_west #=> 0


lib C
  $errno : Int32
end


C.errno #=> some value
C.errno = 0
C.errno #=> 0


lib C
  @[ThreadLocal]
  $errno : Int32
end


lib C
  fun waitpid(pid : Int32, status_ptr : Int32*, options : Int32) : Int32
end


status_ptr = uninitialized Int32

C.waitpid(pid, pointerof(status_ptr), options)


C.waitpid(pid, out status_ptr, options)


lib X
  type CInt = Int32
end


ifdef x86_64
  # some specific code for 64 bits platforms
else
  # some specific code for non-64 bits platforms
end


ifdef linux && x86_64
  # some specific code for linux 64 bits
end


lib C
  ifdef linux
    struct SomeStruct
      some_field : Int32
    end
  else
    struct SomeStruct
      some_field : Int64
    end
  end
end


# Assigns to a local variable
local = 1

# Assigns to a global variable
$global = 4

class Testing
  # Assigns to an instance variable
  @instance = 2

  # Assigns to a class variable
  @@class = 3
end


local += 1  # same as: local = local + 1

# The above is valid with these operators:
# +, -, *, /, %, |, &, ^, **, <<, >>

local ||= 1 # same as: local || (local = 1)
local &&= 1 # same as: local && (local = 1)


# A setter
person.name=("John")

# The above can be written as:
person.name = "John"

# An indexed assignment
objects.[]=(2, 3)

# The above can be written as:
objects[2] = 3

# Not assignment-related, but also syntax sugar:
objects.[](2, 3)

# The above can be written as:
objects[2, 3]


person.age += 1        # same as: person.age = person.age + 1

person.name ||= "John" # same as: person.name || (person.name = "John")
person.name &&= "John" # same as: person.name && (person.name = "John")

objects[1] += 2        # same as: objects[1] = objects[1] + 2

objects[1] ||= 2       # same as: objects[1]? || (objects[1] = 2)
objects[1] &&= 2       # same as: objects[1]? && (objects[1] = 2)


alias PInt32 = Pointer(Int32)

ptr = PInt32.malloc(1) # : Pointer(Int32)


alias RecArray = Array(Int32) | Array(RecArray)

ary = [] of RecArray
ary.push [1, 2, 3]
ary.push ary
ary #=> [[1, 2, 3], [...]]


module Json
  alias Type = Nil |
               Bool |
               Int64 |
               Float64 |
               String |
               Array(Type) |
               Hash(String, Type)
end


a = 1
if a > 0
  a = 10
end
a #=> 10

b = 1
if b > 2
  b = 10
else
  b = 20
end
b #=> 20


if some_condition
  do_something
elsif some_other_condition
  do_something_else
else
  do_that
end


a = 1
if some_condition
  a = "hello"
else
  a = true
end
# a : String | Bool

b = 1
if some_condition
  b = "hello"
end
# b : Int32 | String

if some_condition
  c = 1
else
  c = "hello"
end
# c : Int32 | String

if some_condition
  d = 1
end
# d : Int32 | Nil


a = 1
if some_condition
  a = "hello"
  # a : String
  a.size
end
# a : String | Int32


if some_condition
  e = 1
else
  e = "hello"
  # e : String
  return
end
# e : Int32


enum Color : UInt8
  Red         # 0
  Green       # 1
  Blue   = 5  # overwritten to 5
  Yellow      # 6 (5 + 1)

  def red?
    self == Color::Red
  end
end

Color::Red.value #:: UInt8


@[Flags]
enum IOMode
  Read # 1
  Write  # 2
  Async # 4
end


IOMode::None.value #=> 0
IOMode::All.value  #=> 7


puts(Color::Red)                    # prints "Red"
puts(IOMode::Write | IOMode::Async) # prints "Write, Async"


puts Color.new(1) #=> prints "Green"


puts Color.new(10) #=> prints "10"


Color::Red.red?  #=> true
Color::Blue.red? #=> false


def paint(color : Color)
  case color
  when Color::Red
    # ...
  else
    # Unusual, but still can happen
    raise "unknown color: #{color}"
  end
end

paint Color::Red


def paint(color : Symbol)
  case color
  when :red
    # ...
  else
    raise "unknown color: #{color}"
  end
end

paint :red


name = "Crystal"
age = 1


flower = "Tulip"
# At this point 'flower' is a String

flower = 1
# At this point 'flower' is an Int32


class Foo
  def finalize
    # Invoked when Foo is garbage-collected
    puts "Bye bye from #{self}!"
  end
end

# Prints "Bye bye ...!" for ever
loop do
  Foo.new
end


# Defines a method in the program
def add(x, y)
  x + y
end

# Invokes the add method in the program
add(1, 2) #=> 3


def even?(num)
  if num % 2 == 0
    return true
  end

  return false
end


def add(x, y)
  x + y
end

class Foo
  def bar
    # invokes the program's add method
    add(1, 2)

    # invokes Foo's baz method
    baz(1, 2)
  end

  def baz(x, y)
    x * y
  end
end


def baz(x, y)
  x + y
end

class Foo
  def bar
    baz(4, 2) #=> 2
    ::baz(4, 2) #=> 6
  end

  def baz(x, y)
    x - y
  end
end


x = 1

def add(y)
  x + y # error: undefined local variable or method 'x'
end

add(2)


add 1, 2 # same as add(1, 2)


class Counter
  @@instances = 0

  def initialize
    @@instances += 1
  end

  def self.instances
    @@instances
  end
end

Counter.instances #=> 0
Counter.new
Counter.new
Counter.new
Counter.instances #=> 3


class Counter
  def self.increment
    @@instances += 1
  end
end

Counter.increment # Error: undefined method '+' for Nil


class Parent
  @@counter = 0
end

class Child < Parent
  def self.counter
    @@counter
  end
end

Child.counter #=> nil


unless some_condition
  then_expression
else
  else_expression
end

# Can also be written as a suffix
close_door unless door_closed?


a = 1
b = typeof(a) #=> Int32


typeof(1, "a", 'a') #=> (Int32 | String | Char)


hash = {} of Int32 => String
another_hash = typeof(hash).new #:: Hash(Int32, String)


class Array
  def self.elem_type(typ)
    if typ.is_a?(Array)
      elem_type(typ.first)
    else
      typ
    end
  end
end

nest = [1, ["b", [:c, ['d']]]]
flat = Array(typeof(Array.elem_type(nest))).new
typeof(nest) #=> Array(Int32 | Array(String | Array(Symbol | Array(Char))))
typeof(flat) #=> Array(String | Int32 | Symbol | Char)


a = 2 if some_condition


x = 0
proc = ->{ x += 1; x }
proc.call #=> 1
proc.call #=> 2
x         #=> 2


def counter
  x = 0
  ->{ x += 1; x }
end

proc = counter
proc.call #=> 1
proc.call #=> 2


def foo
  yield
end

x = 1
foo do
  x = "hello"
end
x # : Int32 | String


x = 1
foo do
  x = "hello"
end
x # : Int32 | String

x = 'a'
x # : Char


def capture(&block)
  block
end

x = 1
capture { x = "hello" }

x = 'a'
x # : Int32 | String | Char


def capture(&block)
  block
end

x = 1
->{ x = "hello" }

x = 'a'
x # : Int32 | String | Char


abstract class Animal
  # Makes this animal talk
  abstract def talk
end

class Dog < Animal
  def talk
    "Woof!"
  end
end

class Cat < Animal
  def talk
    "Miau"
  end
end

class Person
  getter pet

  def initialize(@name, @pet)
  end
end

john = Person.new "John", Dog.new
peter = Person.new "Peter", Cat.new


john.pet.talk #=> "Woof!"


a = 1 > 2 ? 3 : 4

# The above is the same as:
a = if 1 > 2
      3
    else
      4
    end


def some_method : String
  "hello"
end


PI = 3.14

module Earth
  RADIUS = 6_371_000
end

PI #=> 3.14
Earth::RADIUS #=> 6_371_000


TEN = begin
  a = 0
  while a < 10
    a += 1
  end
  a
end

TEN #=> 10


class Person
  getter name

  def initialize(@name)
    @age = 0
  end
end

john = Person.new "John"
john.name #=> "John"
john.name.size #=> 4


one = Person.new 1
one.name #=> 1
one.name + 2 #=> 3


john = Person.new "John"
one = Person.new 1


john = Person.new "John"
one = Person.new 1

# Error: undefined method 'size' for Int32
john.name.size

# Error: no overload matches 'String#+' with types Int32
john.name + 3


john = Person.new "John"
john.name.size
one = Person.new 1


class Person
  getter name

  def initialize(@name)
    @age = 0
  end

  def address
    @address
  end

  def address=(@address)
  end
end

john = Person.new "John"
john.address = "Argentina"


# Error: undefined method 'size' for Nil
john.address.size


class Person
  @age = 0

  def initialize(@name)
  end
end


class Person
  @age : Int32

  def initialize(@name)
    @age = 0
  end
end


a = if 2 > 1
      3
    else
      4
    end
a #=> 3


if 1 > 2
else
  3
end


def twice(&block)
  yield
  yield
end


twice() do
  puts "Hello!"
end

twice do
  puts "Hello!"
end

twice { puts "Hello!" }


def twice
  yield 1
  yield 2
end

twice do |i|
  puts "Got #{i}"
end


twice { |i| puts "Got #{i}" }


def many
  yield 1, 2, 3
end

many do |x, y, z|
  puts x + y + z
end

# Output: 6


def many
  yield 1, 2, 3
end

many do |x, y|
  puts x + y
end

# Output: 3


def twice
  yield
  yield
end

twice do |i|
  puts i.inspect
end


def some
  yield 1, 'a'
  yield true, "hello"
  yield 2
end

some do |first, second|
  # first is Int32 | Bool
  # second is Char | String | Nil
end


method do |argument|
  argument.some_method
end


method(&.some_method)


method &.some_method(arg1, arg2)


method &.+(2)
method &.[index]


def twice
  v1 = yield 1
  puts v1

  v2 = yield 2
  puts v2
end

twice do |i|
  i + 1
end


ary = [1, 2, 3]
ary.map { |x| x + 1 }         #=> [2, 3, 4]
ary.select { |x| x % 2 == 1 } #=> [1, 3]


def transform(value)
  yield value
end

transform(1) { |x| x + 1 } #=> 2


def thrice
  puts "Before 1"
  yield 1
  puts "Before 2"
  yield 2
  puts "Before 3"
  yield 3
  puts "After 3"
end

thrice do |i|
  if i == 2
    break
  end
end


def twice
  yield 1
  yield 2
end

twice { |i| i + 1 } #=> 3
twice { |i| break "hello" } #=> "hello"


value = twice do |i|
  if i == 1
    break "hello"
  end
  i + 1
end
value #:: Int32 | String


values = twice { break 1, 2 }
values #=> {1, 2}


value = twice { break }
value #=> nil


def twice
  yield 1
  yield 2
end

twice do |i|
  if i == 1
    puts "Skipping 1"
    next
  end

  puts "Got #{i}"
end



def twice
  v1 = yield 1
  puts v1

  v2 = yield 2
  puts v2
end

twice do |i|
  if i == 1
    next 10
  end

  i + 1
end

# Output
# 10
# 3


class Foo
  def one
    1
  end

  def yield_with_self
    with self yield
  end

  def yield_normally
    yield
  end
end

def one
  "one"
end

Foo.new.yield_with_self { one } # => 1
Foo.new.yield_normally { one }  # => "one"


def twice
  yield 1
  yield 2
end

twice do |i|
  puts "Got: #{i}"
end


i = 1
puts "Got: #{i}"
i = 2
puts "Got: #{i}"


3.times do |i|
  puts i
end


struct Int
  def times
    i = 0
    while i < self
      yield i
      i += 1
    end
  end
end


i = 0
while i < 3
  puts i
  i += 1
end


class Person
  def initialize(@name)
  end

  def greet
    puts "Hi, I'm #{@name}"
  end
end

class Employee < Person
end

employee = Employee.new "John"
employee.greet # "Hi, I'm John"


class Person
  def initialize(@name)
  end
end

class Employee < Person
  def initialize(@name, @company_name)
  end
end

Employee.new "John", "Acme" # OK
Employee.new "Peter" # Error: wrong number of arguments
                     # for 'Employee:Class#new' (1 for 2)


class Person
  def greet(msg)
    puts "Hi, #{msg}"
  end
end

class Employee < Person
  def greet(msg)
    puts "Hello, #{msg}"
  end
end

p = Person.new
p.greet "everyone" # "Hi, everyone"

e = Employee.new
e.greet "everyone" # "Hello, everyone"


class Person
  def greet(msg)
    puts "Hi, #{msg}"
  end
end

class Employee < Person
  def greet(msg : Int32)
    puts "Hi, this is a number: #{msg}"
  end
end

e = Employee.new
e.greet "everyone" # "Hi, everyone"

e.greet 1 # "Hi, this is a number: 1"


class Person
  def greet(msg)
    puts "Hello, "#{msg}"
  end
end

class Employee < Person
  def greet(msg)
    super # Same as: super(msg)
    super("another message")
  end
end


def int_to_int(&block : Int32 -> Int32)
  block
end

proc = int_to_int { |x| x + 1 }
proc.call(1) #=> 2


class Model
  def on_save(&block)
    @on_save_callback = block
  end

  def save
    if callback = @on_save_callback
      callback.call
    end
  end
end

model = Model.new
model.on_save { puts "Saved!" }
model.save # prints "Saved!"


def some_proc(&block : Int32 ->)
  block
end

proc = some_proc { |x| x + 1 }
proc.call(1) # void


def some_proc(&block : Int32 -> _)
  block
end

proc = some_proc { |x| x + 1 }
proc.call(1) # 2

proc = some_proc { |x| x.to_s }
proc.call(1) # "1"


macro update_x
  x = 1
end

x = 0
update_x
x #=> 1


macro dont_update_x
  %x = 1
  puts %x
end

x = 0
dont_update_x # outputs 1
x #=> 0


macro fresh_vars_sample(*names)
  # First declare vars
  {% for name, index in names %}
    print "Declaring: ", "%name{index}", '\n'
    %name{index} = {{index}}
  {% end %}

  # Then print them
  {% for name, index in names %}
    print "%name{index}: ", %name{index}, '\n'
  {% end %}
end

fresh_vars_sample a, b, c

# Sample output:
# Declaring: __temp_255
# Declaring: __temp_256
# Declaring: __temp_257
# __temp_255: 0
# __temp_256: 1
# __temp_257: 2


class Object
  macro def instance_vars_names : Array(String)
    {{ @type.instance_vars.map &.name.stringify }}
  end
end

class Person
  def initialize(@name, @age)
  end
end

person = Person.new "John", 30
person.instance_vars_names #=> ["name", "age"]


class Object
  macro def has_instance_var?(name) : Bool
    # We cannot access name inside the macro expansion here,
    # instead we need to use the macro language to construct an array
    # and do the inclusion check at runtime.
    {{ @type.instance_vars.map &.name.stringify }}.includes? name
  end
end

person = Person.new "John", 30
person.has_instance_var?("name") #=> true
person.has_instance_var?("birthday") #=> false


class Parent
  macro inherited
    def {{@type.name.downcase.id}}
      1
    end
  end
end

class Child < Parent
end

Child.new.child #=> 1


macro method_missing(name, args, block)
  print "Got ", {{name.id.stringify}}, " with ", {{args.size}}, " arguments", '\n'
end

foo          # Prints: Got foo with 0 arguments
bar 'a', 'b' # Prints: Got bar with 2 arguments


sizeof(Int32)  #=> 4
sizeof(Int64)  #=> 8


# On a 64 bits machine
sizeof(Pointer(Int32)) #=> 8
sizeof(String)         #=> 8


a = 1
sizeof(typeof(a)) #=> 4


class Foo
  macro emphasize(value)
    "***#{ {{value}} }***"
  end

  def yield_with_self
    with self yield
  end
end

Foo.new.yield_with_self { emphasize(10) } #=> "***10***"


# This generates:
#
#     def :foo
#       1
#     end
define_method :foo, 1


macro define_method(name, content)
  def {{name.id}}
    {{content}}
  end
end

# This correctly generates:
#
#     def foo
#       1
#     end
define_method :foo, 1


macro define_method(name, content)
  def {{name}}
    {% if content == 1 %}
      "one"
    {% else %}
      {{content}}
    {% end %}
  end
end

define_method foo, 1
define_method bar, 2

foo #=> one
bar #=> 2


{% if env("TEST") %}
  puts "We are in test mode"
{% end %}


macro define_dummy_methods(names)
  {% for name, index in names %}
    def {{name.id}}
      {{index}}
    end
  {% end %}
end

define_dummy_methods [foo, bar, baz]

foo #=> 0
bar #=> 1
baz #=> 2


macro define_dummy_methods(hash)
  {% for key, value in hash %}
    def {{key.id}}
      {{value}}
    end
  {% end %}
end
define_dummy_methods({foo: 10, bar: 20})
foo #=> 10
bar #=> 20


{% for name, index in ["foo", "bar", "baz"] %}
  def {{name.id}}
    {{index}}
  end
{% end %}

foo #=> 0
bar #=> 1
baz #=> 2


macro define_dummy_methods(*names)
  {% for name, index in names %}
    def {{name.id}}
      {{index}}
    end
  {% end %}
end

define_dummy_methods foo, bar, baz

foo #=> 0
bar #=> 1
baz #=> 2


macro println(*values)
   print {{*values}}, '\n'
end

println 1, 2, 3 # outputs 123\n


VALUES = [1, 2, 3]

{% for value in VALUES %}
  puts {{value}}
{% end %}


until some_condition
  do_this
end

# The above is the same as:
while !some_condition
  do_this
end


a = some_condition ? nil : 3
# a is Int32 or Nil

if a
  # Since the only way to get here is if a is truthy,
  # a can't be nil. So here a is Int32.
  a.abs
end


if a = some_expression
  # here a is not nil
end


if a && b
  # here both a and b are guaranteed not to be Nil
end


if @a
  # here @a can be nil
end


# First option: assign it to a variable
if a = @a
  # here a can't be nil
end

# Second option: use `Object#try` found in the standard library
@a.try do |a|
  # here a can't be nil
end


if method # first call to a method that can return Int32 or Nil
          # here we know that the first call did not return Nil
  method  # second call can still return Int32 or Nil
end


class Person
  def become_older(by = 1)
    @age += by
  end
end

john = Person.new "John"
john.age #=> 0

john.become_older
john.age #=> 1

john.become_older 2
john.age #=> 3


john.become_older by: 5


def some_method(x, y = 1, z = 2, w = 3)
  # do something...
end

some_method 10 # x = 10, y = 1, z = 2, w = 3
some_method 10, z: 10 # x = 10, y = 1, z = 10, w = 3
some_method 10, w: 1, y: 2, z: 3 # x = 10, y = 2, z = 3, w = 1


case exp
when value1, value2
  do_something
when value3
  do_something_else
else
  do_another_thing
end


case var
when String
  # var : String
  do_something
when Int32
  # var : Int32
  do_something_else
else
  # here var is neither a String nor an Int32
  do_another_thing
end


case num
when .even?
  do_something
when .odd?
  do_something_else
end


case
when cond1, cond2
  do_something
when cond3
  do_something_else
end


a = 1
a.responds_to?(:abs)    #=> true
a.responds_to?(:size) #=> false


foo_or_bar = /foo|bar/
heeello    = /h(e+)llo/
integer    = /\d+/


r = /foo/imx


slash = /\//


r = %r(regex with slash: /)


"hello world"


"\"" # double quote
"\\" # backslash
"\e" # escape
"\f" # form feed
"\n" # newline
"\r" # carriage return
"\t" # tab
"\v" # vertical tab


"\101" # == "A"
"\123" # == "S"
"\12"  # == "\n"
"\1"   # string with one character with code point 1


"\u0041" # == "A"


"\u{41}"    # == "A"
"\u{1F52E}" # == "🔮"


"hello
      world" # same as "hello\n      world"


"hello " \
"world, " \
"no newlines" # same as "hello world, no newlines"


"hello \
     world, \
     no newlines" # same as "hello world, no newlines"


# Supports double quotes and nested parenthesis
%(hello ("world")) # same as "hello (\"world\")"

# Supports double quotes and nested brackets
%[hello ["world"]] # same as "hello [\"world\"]"

# Supports double quotes and nested curlies
%{hello {"world"}} # same as "hello {\"world\"}"

# Supports double quotes and nested angles
%<hello <"world">> # same as "hello <\"world\">"


<<-XML
<parent>
  <child />
</parent>
XML


# Same as "Hello\n  world"
<<-STRING
  Hello
    world
  STRING

# Same as "  Hello\n    world"
<<-STRING
    Hello
      world
  STRING


a = 1
b = 2
"sum = #{a + b}"        # "sum = 3"


1.0      # Float64
1.0_f32  # Float32
1_f32    # Float32

1e10     # Float64
1.5e10   # Float64
1.5e-7   # Float64

+1.3     # Float64
-0.5     # Float64


1_000_000.111_111 # better than 1000000.111111


'a'
'z'
'0'
'_'
'あ'


'\'' # single quote
'\\' # backslash
'\e' # escape
'\f' # form feed
'\n' # newline
'\r' # carriage return
'\t' # tab
'\v' # vertical tab


'\101' # == 'A'
'\123' # == 'S'
'\12'  # == '\n'
'\1'   # code point 1


'\u0041' # == 'A'


'\u{41}'    # == 'A'
'\u{1F52E}' # == '🔮'


{1 => 2, 3 => 4}     # Hash(Int32, Int32)
{1 => 2, 'a' => 3}   # Hash(Int32 | Char, Int32)


{} of Int32 => Int32 # same as Hash(Int32, Int32).new


{key1: 'a', key2: 'b'} # Hash(Symbol, Char)


{"key1": 'a', "key2": 'b'} # Hash(String, Char)


MyType{"foo": "bar"}


tmp = MyType.new
tmp["foo"] = "bar"
tmp


tmp = MyType(typeof("foo"), typeof("bar")).new
tmp["foo"] = "bar"
tmp


MyType(String, String) {"foo": "bar"}


:hello
:good_bye

# With spaces and symbols
:"symbol with spaces"

# Ending with question and exclamation marks
:question?
:exclamation!

# For the operators
:+
:-
:*
:/
:==
:<
:<=
:>
:>=
:!
:!=
:=~
:!~
:&
:|
:^
:~
:**
:>>
:<<
:%
:[]
:[]?
:[]=
:<=>
:===


x..y  # an inclusive range, in mathematics: [x, y]
x...y # an exclusive range, in mathematics: [x, y)


# A proc without arguments
->{ 1 } # Proc(Int32)

# A proc with one argument
->(x : Int32) { x.to_s } # Proc(Int32, String)

# A proc with two arguments:
->(x : Int32, y : Int32) { x + y } # Proc(Int32, Int32, Int32)


Proc(Int32, String).new { |x| x.to_s } # Proc(Int32, String)


proc = ->(x : Int32, y : Int32) { x + y }
proc.call(1, 2) #=> 3


def one
  1
end

proc = ->one
proc.call #=> 1


def plus_one(x)
  x + 1
end

proc = ->plus_one(Int32)
proc.call(41) #=> 42


str = "hello"
proc = ->str.count(Char)
proc.call('e') #=> 1
proc.call('l') #=> 2


tuple = {1, "hello", 'x'} # Tuple(Int32, String, Char)
tuple[0]                  #=> 1       (Int32)
tuple[1]                  #=> "hello" (String)
tuple[2]                  #=> 'x'     (Char)


[1, 2, 3]         # Array(Int32)
[1, "hello", 'x'] # Array(Int32 | String | Char)


[] of Int32 # same as Array(Int32).new


%w(one two three) # ["one", "two", "three"]


%i(one two three) # [:one, :two, :three]


MyType{1, 2, 3}


tmp = MyType.new
tmp << 1
tmp << 2
tmp << 3
tmp


tmp = MyType(typeof(1, 2, 3)).new
tmp << 1
tmp << 2
tmp << 3
tmp


MyType(Int32 | String) {1, 2, "foo"}


nil


1      # Int32

1_i8   # Int8
1_i16  # Int16
1_i32  # Int32
1_i64  # Int64

1_u8   # UInt8
1_u16  # UInt16
1_u32  # UInt32
1_u64  # UInt64

+10    # Int32
-20    # Int32

2147483648          # Int64
9223372036854775808 # UInt64


1_000_000 # better than 1000000


0b1101 # == 13


0o123 # == 83


0xFE012D # == 16646445
0xfe012d # == 16646445


true  # A Bool that is true
false # A Bool that is false


a = 1

ptr = pointerof(a)
ptr.value = 2

a #=> 2


class Point
  def initialize(@x, @y)
  end

  def x
    @x
  end

  def x_ptr
    pointerof(@x)
  end
end

point = Point.new 1, 2

ptr = point.x_ptr
ptr.value = 10

point.x #=> 10


def add(x : Number, y : Number)
  x + y
end

# Ok
add 1, 2 # Ok

# Error: no overload matches 'add' with types Bool, Bool
add true, false


def add(x, y)
  x + y
end

add true, false


# A class that has a + method but isn't a Number
class Six
  def +(other)
    6 + other
  end
end

# add method without type restrictions
def add(x, y)
  x + y
end

# OK
add Six.new, 10

# add method with type restrictions
def restricted_add(x : Number, y : Number)
  x + y
end

# Error: no overload matches 'restricted_add' with types Six, Int32
restricted_add Six.new, 10


class Person
  def ==(other : self)
    other.name == name
  end

  def ==(other)
    false
  end
end

john = Person.new "John"
another_john = Person.new "John"
peter = Person.new "Peter"

john == another_john #=> true
john == peter #=> false (names differ)
john == 1 #=> false (because 1 is not a Person)


class Person
  def self.compare(p1 : self, p2 : self)
    p1.name == p2.name
  end
end

john = Person.new "John"
peter = Person.new "Peter"

Person.compare(john, peter) # OK


def foo(x : Int32)
end

foo 1       # OK
foo "hello" # Error


def foo(x : Int32.class)
end

foo Int32  # OK
foo String # Error


def foo(x : Int32.class)
  puts "Got Int32"
end

def foo(x : String.class)
  puts "Got String"
end

foo Int32  # prints "Got Int32"
foo String # prints "Got String"


def foo(*args : Int32)
end

def foo(*args : String)
end

foo 1, 2, 3       # OK, invokes first overload
foo "a", "b", "c" # OK, invokes second overload
foo 1, 2, "hello" # Error
foo()             # Error


def foo
  # This is the empty-tuple case
end


def foo(x : T)
  T
end

foo(1)       #=> Int32
foo("hello") #=> String


def foo(x : Array(T))
  T
end

foo([1, 2])   #=> Int32
foo([1, "a"]) #=> (Int32 | String)


def foo(x : T.class)
  Array(T)
end

foo(Int32)  #=> Array(Int32)
foo(String) #=> Array(String)


class Person
  # Increases age by one
  def become_older
    @age += 1
  end

  # Increases age by the given number of years
  def become_older(years : Int32)
    @age += years
  end

  # Increases age by the given number of years, as a String
  def become_older(years : String)
    @age += years.to_i
  end

  # Yields the current age of this person and increases
  # its age by the value returned by the block
  def become_older
    @age += yield @age
  end
end

person = Person.new "John"

person.become_older
person.age #=> 1

person.become_older 5
person.age #=> 6

person.become_older "12"
person.age #=> 18

person.become_older do |current_age|
  current_age < 20 ? 10 : 30
end
person.age #=> 28


a = 1
a.is_a?(Int32)          #=> true
a.is_a?(String)         #=> false
a.is_a?(Number)         #=> true
a.is_a?(Int32 | String) #=> true


# One for each thread
@[ThreadLocal]
$values = [] of Int32


@[AlwaysInline]
def foo
  1
end


@[NoInline]
def foo
  1
end


lib LibFoo
  @[CallConvention("X86_StdCall")]
  fun foo : Int32
end


def sum(*elements)
  total = 0
  elements.each do |value|
    total += value
  end
  total
end

# elements is Tuple(Int32, Int32, Int32, Float64)
sum 1, 2, 3, 4.5


if a.responds_to?(:abs)
  # here a's type will be reduced to those responding to the 'abs' method
end


a = some_condition ? 1 : "hello"
# a : Int32 | String

if a.responds_to?(:abs)
  # here a will be Int32, since Int32#abs exists but String#abs doesn't
else
  # here a will be String
end


if (a = @a).responds_to?(:abs)
  # here a is guaranteed to respond to `abs`
end


def capture(&block)
  block
end

def invoke(&block)
  block.call
end

proc = capture { puts "Hello" }
invoke(&proc) # prints "Hello"




def capture(&block)
  block
end

def twice
  yield
  yield
end

proc = capture { puts "Hello" }
twice &proc


twice &->{ puts "Hello" }


def say_hello
  puts "Hello"
end

twice &->say_hello


def foo
  yield 1
end

def wrap_foo
  puts "Before foo"
  foo do |x|
    yield x
  end
  puts "After foo"
end

wrap_foo do |i|
  puts i
end


def foo
  yield 1
end

def wrap_foo(&block : Int32 -> _)
  puts "Before foo"
  foo(&block)
  puts "After foo"
end

wrap_foo do |i|
  puts i
end


foo_forward do |i|
  break # error
end


a = 2
while (a += 1) < 20
  if a == 10
    # goes to 'puts a'
    break
  end
end
puts a #=> 10


class Person
  private def say(message)
    puts message
  end

  def say_hello
    say "hello" # OK, no receiver
    self.say "hello" # Error, self is a receiver

    other = Person.new "Other"
    other.say "hello" # Error, other is a receiver
  end
end


class Employee < Person
  def say_bye
    say "bye" # OK
  end
end


module Namespace
  class Foo
    protected def foo
      puts "Hello"
    end
  end

  class Bar
    def bar
      # Works, because Foo and Bar are under Namespace
      Foo.new.foo
    end
  end
end

Namespace::Bar.new.bar


class Person
  protected def self.say(message)
    puts message
  end

  def say_hello
    Person.say "hello"
  end
end


buffer = uninitialized UInt8[256]
