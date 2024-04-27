from sys.info import (
    os_is_linux,
    os_is_macos,
    has_sse4,
    has_avx,
    has_avx2,
    num_physical_cores,
    num_logical_cores,
)
from sys.info import _current_cpu, _current_target, _triple_attr


def func() -> None:
    variable = "implicit variable"
    print("def function", variable)


fn strict_func():
    var variable = "explicit variable"
    print("fn function", variable)


fn generic_func[a: Int = 3, msg: StringLiteral = "woof"]():
    """
    A generic function.
    """
    print(msg, a)


fn get_sys_info() -> String:
    """
    Retrieves system information (based upon: https://github.com/modularml/mojo/blob/main/examples/deviceinfo.mojo).
    """
    var os = ""
    if os_is_linux():
        os = "linux"
    elif os_is_macos():
        os = "macOS"
    else:
        os = "windows"
    var cpu = String(_current_cpu())
    var arch = String(_triple_attr())
    var cpu_features = String("")
    if has_sse4():
        cpu_features += "sse4"
    if has_avx():
        cpu_features += " avx"
    if has_avx2():
        cpu_features += " avx2"

    var info = (
        """System information:
    OS             : """
        + os
        + """
    CPU            : """
        + cpu
        + """
    Arch           : """
        + arch
        + """
    Physical Cores : """
        + num_physical_cores()
        + """
    Logical Cores  : """
        + num_logical_cores()
        + """
    CPU Features   : """
        + cpu_features
    )

    return info


trait Greet:
    """
    A trait example.
    """

    fn say_hello(self) -> None:
        """Says hello."""
        ...


@value
struct Person(Greet):
    """
    A person.
    """

    var name: String

    fn __init__(inout self, name: String):
        self.name = name

    fn __str__(self) -> String:
        """
        Overridden __str__ method
        """
        return "Person named " + self.name

    fn say_hello(self) -> None:
        print(self.name, "says hello!")


fn main():
    for i in range(3):
        var pers = Person("Per_" + String(i))
        print(pers)
        pers.say_hello()

    generic_func()  # prints 'woof 3'
    generic_func[5]()  # prints 'woof 5'
    generic_func[7, "meow"]()  # prints 'meow 7'

    try:
        func()
        strict_func()

        raise Error("fail")
    except:
        print("error was raised")

    print(get_sys_info())
