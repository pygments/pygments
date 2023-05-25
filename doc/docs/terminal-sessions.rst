=================
Terminal sessions
=================

Pygments support the parsing and highlighting of terminal sessions, like
command-line shells, interactive consoles and language `REPL
<https://en.wikipedia.org/wiki/Read–eval–print_loop>`_.

They are typiccaly command lines or code, mixed with generic output.


Operating system shells
-----------------------

These lexers are expecting a prompt to identify user input. So to highlight a
shell session, prefix your code snippet with a specially formatted prompt.

Supported shells with examples are shown below. In each example, prompt parts
in brackets ``[any]`` represent optional parts of the prompt, and prompt parts
without brackets or in parenthesis ``(any)`` represent required parts of the
prompt.

* **Bash Session** (``console``, ``shell-session``):

  .. code-block:: console

     [any@any]$ ls -lh
     [any@any]# ls -lh
     [any@any]% ls -lh
     $ ls -lh
     # ls -lh
     % ls -lh
     > ls -lh

* **MSDOS Session** (``doscon``):

  .. code-block:: doscon

     [any]> dir
     > dir
     More? dir

* **PowerShell Session** (``ps1con``, ``pwsh-session``):

  .. code-block:: ps1con

     PS[any]> Get-ChildItem
     PS> Get-ChildItem
     >> Get-ChildItem

* **Tcsh Session** (``tcshcon``):

  .. code-block:: tcshcon

     (any)> ls -lh
     ? ls -lh


Interactive consoles
--------------------

Similarly to systems shells, Pygments recognize a variety of interactive
language sessions:

* **Dylan Console** (``dylan-console``, ``dylan-repl``):

  .. code-block:: dylan-console

     ? let a = 1;
     => 1
     ? a
     => 1

* **Elixir Console** (``iex``):

  .. code-block:: iex

      Erlang/OTP 23.0 [64-bit] [smp:2:2] [...]

      Interactive Elixir (1.14.4) - press Ctrl+C to exit
      iex> 40 + 2
      42
      iex> "hello" <> " world"
      "hello world"

* **GAP Console** (``gap-console``, ``gap-repl``):

  .. code-block:: gap-console

      gap> for i in [1..5] do
      >      Print( i, " ", i^2, " ", i^3, "\n" );
      >    od;
      1 1 1
      2 4 8
      3 9 27
      4 16 64
      5 25 125
      gap> g:= SmallGroup(12,5);
      <pc group of size 12 with 3 generators>
      gap> Print( g, "\n" );
      Group( [ f1, f2, f3 ] )
      gap> View( g );  Print( "\n" );
      <pc group of size 12 with 3 generators>

* **Julia Console** (``jlcon``, ``julia-repl``):

  .. code-block:: jlcon

     julia> x = 2
     2

     julia> """
            The factorial function.

            ```julia
            @assert fac(3) == 1 * 2 * 3
            ```
            """
            function fac(n)
                if n < 2
                    return 1
                else
                    return n * fac(n - 1) # <-- recursive call
                end
            end


            # Lets try the function out...
            f(x + 1)
     6

* **Matlab Console** (``matlabsession``):

  .. code-block:: matlabsession

     >> x = rand(3)    % a matrix

     x =

         0.8147    0.9134    0.2785
         0.9058    0.6324    0.5469
         0.1270    0.0975    0.9575

     >> 1/0

     ans =

        Inf

     >> foo
     ??? Undefined function or variable 'foo'.

* **Postgres Console** (``postgres-console``, ``postgresql-console``,
  ``psql``):

  .. code-block:: postgres-console

      psql (15.3)
      Type "help" for help.

      testdb=> \echo `date`
      Tue Oct 26 21:40:57 CEST 1999
      testdb=> CREATE TABLE my_table (
      testdb(>  first integer not null default 0,
      testdb(>  second text)
      testdb-> ;
      CREATE TABLE
      testdb=> SELECT * FROM my_table;
       first | second
      -------+--------
           1 | one
           2 | two
           3 | three
           4 | four
      (4 rows)
      testdb=>

* **Psysh Console** (``psysh``):

  .. code-block:: psysh

     >>> $object = (object) 10.88
     => {#2373
         +"scalar": 10.88,
       }
     >>> $object->scalar
     => 10.88
     >>> $fileHandle = fopen('hello.txt', 'w');
     => stream resource #400

* **Python Console** (``pycon``):

  .. code-block:: pycon

     >>> from multiprocessing import Pool
     >>> p = Pool(5)
     >>> def f(x):
     ...     return x*x
     ...

* **R Console** (``rconsole``, ``rout``):

  .. code-block:: rconsole

     R version 2.9.2 (2009-08-24)
     Copyright (C) 2009 The R Foundation for Statistical Computing
     ISBN 3-900051-07-0

     [R.app GUI 1.29 (5464) i386-apple-darwin8.11.1]

     > x <- function() {
     + cat("hello")
     + cat("world")
     + }
     > x
     function() {
     cat("hello")
     cat("world")
     }
     > x()
     helloworld
     > 2 + 2
     [1] 4
     >

* **Ruby Console** (``irb``, ``rbcon``):

  .. code-block:: irb

     irb(main):001:0> a = 1
     => 1
     irb(main):002:0> puts a
     1
     => nil

* **Sqlite Console** (``sqlite3``):

  .. code-block:: sqlite3

     SQLite version 3.4.2
     Enter ".help" for instructions
     sqlite> .schema
     CREATE TABLE paste (paste_id integer, code text, parsed_code text,
     pub_date varchar(24), language varchar(64), parent_id integer, url
     varchar(128));
     CREATE TABLE vars (key varchar(24), value varchar(128));
     sqlite> select count(language), language from paste group by language
        ...> order by count(language) desc;
     144|python
     76|text
     22|pycon
     9|ruby
     7|c
     7|js
     6|html+django
     4|html
     4|tex
     2|html+php
     1|cpp
     1|scheme
     sqlite>


Generic output
--------------

To display standalone terminal output and keep styling consistent, you can use
the generic ``output`` lexer:

* **Output** (``output``):

  .. code-block:: output

            ,
            \`-._           __
             \\  `-..____,.'  `.
              :`.         /    \`.
              :  )       :      : \
               ;'        '   ;  |  :
               )..      .. .:.`.;  :
              /::...  .:::...   ` ;
              ; _ '    __        /:\
              `:o>   /\o_>      ;:. `.
             `-`.__ ;   __..--- /:.   \
             === \_/   ;=====_.':.     ;
              ,/'`--'...`--....        ;
                   ;                    ;
                 .'                      ;
               .'                        ;
             .'     ..     ,      .       ;
            :       ::..  /      ;::.     |
           /      `.;::.  |       ;:..    ;
          :         |:.   :       ;:.    ;
          :         ::     ;:..   |.    ;
           :       :;      :::....|     |
           /\     ,/ \      ;:::::;     ;
         .:. \:..|    :     ; '.--|     ;
        ::.  :''  `-.,,;     ;'   ;     ;
     .-'. _.'\      / `;      \,__:      \
     `---'    `----'   ;      /    \,.,,,/
                        `----`              fsc


ANSI rendering
--------------

In all the examples above, all the command results are parsed as generic
output. Which means they are rendered as-is, without any tentative of rendering
the styling carried on by ANSI codes.

Here is a couple of third-party projects covering this use-case:

- `pygments-ansi-color
  <https://github.com/chriskuehl/pygments-ansi-color>`_: implements
  a new lexer and formatter to parse and render pure ANSI content.
- `Click Extra <https://github.com/kdeldycke/click-extra>`_: adds
  `ANSI-capable lexers
  <https://kdeldycke.github.io/click-extra/pygments.html#ansi-language-lexers>`_
  for each language listed above.
