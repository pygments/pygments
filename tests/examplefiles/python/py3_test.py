class Käse:
    pass


# PEP 701: a `#` inside a replacement field starts a comment that runs to
# the end of the line, so the field may close on a later line
a = 2
value = 3
message = f"abc{a  # This comment
          }"
other = f"""{
    value  # comment
}"""
