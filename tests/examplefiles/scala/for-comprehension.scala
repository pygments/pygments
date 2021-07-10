for ( i <- 1 to 10 )
for (user <- userBase if user.age >= 20 && user.age < 30) yield user.name