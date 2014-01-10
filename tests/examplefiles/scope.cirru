
-- https://github.com/Cirru/cirru-gopher/blob/master/code/scope.cr

set a (int 2)

print (self)

set c (child)

under c
  under parent
    print a

print $ get c a

set c x (int 3)
print $ get c x

set just-print $ code
  print a

print just-print

eval (self) just-print
eval just-print

print (string "string content\nand")

demo ((((()))))

"eval" $ string "eval"