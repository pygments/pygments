# Just some shenanigans here to check syntax highlighting.
(print "Hello, Janet!")
(def x 3)
(def y 4.5)
(def s "hello")
(def s2 @"hi there")
(def s3 @`chit chat`)

(var a [:a :b :c])
(var a2 @[:hi-o :hey-o :bye-o])

(var t {:ayy45 "this" :bee23 "that" :sea67 "other"})
(var t2 @{:a 1 :b 2 :c 3})

(eachp [k v] t
  (print k " --> " v))

(print 't2)
(print (math/sin (/ math/pi 4)))

(var sum 0)
(loop [i :range [0 10]]
  (+= sum i))
(print sum) # prints 45

(defn mult-by-2
  ``Docstring.

  Quite a docstring.

  And here it ends.``
  [n]
  (* n 2))

(print (mult-by-2 21) " ***")

(defn fully-charged? [n] true)
(defn foo->bar [n] "1000")

(if (fully-charged? x)
  (do
    (print (foo->bar 12))
    (print "=====")))

(let [a (filter even?
                (map (fn [x]
                       (* x x))
                     (range 10)))]  # => @[0 4 16 36 64]
  (each item a
    (print item)))
