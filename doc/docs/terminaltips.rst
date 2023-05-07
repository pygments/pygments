.. -*- mode: rst -*-

.. highlight:: shell

=============
Terminal Tips
=============

These are some tips how to get more out of Pygments on the terminal.


How do I determine which shell I am using?
------------------------------------------
Run this command::

    $ echo $SHELL

Mine outputs  ``/usr/bin/fish``, which is the location of the executable.
This means my computer is using the fish shell. ``neofetch`` (or a similar
utility) can  also provide that information.

Most shells are POSIX compliant.


Aliases (POSIX)
---------------

POSIX compliant shells includes ``bash`` and ``zsh``, and most shells.

Aliases allow you to call a function using a different name.  In this case it
calls :program:`pygmentize` by using a different name.  The simplest is to make
an alias called ``cath``::

    $ alias cath="pygmentize"

The only thing is that :program:`pygmentize` does not really work like ``cat``.
It does allow you to use a command that is similar to one use for cat,
displaying a file.

Add additional flags can be added as desired.  The monokai style works well for
a dark background, so this is how you would add that option::

    $ alias cath="pygmentize -O style=monokai"


Here is a version of ``less`` with syntax highlighting.  It is slightly more
complicated, because it uses the environment variable ``LESSOPEN`` to preprocess
the input file(s)::

    $ alias lessh='LESSOPEN="| pygmentize -O style=monokai %s" less -M -R '

These alias commands need to be added to your configuration in order to work
when you open a new terminal (or restart the computer).

Add above commands to your configuration file:

* ``bash`` - edit either ``~/.bash_aliases`` (if it exists) or ``~/.bashrc`` file.
* ``csh`` (C shell), edit  the ``~/.cshrc`` file
* ``ksh`` (Korn shell), edit the ``~/.kshrc`` file.
* ``tcsh``, edit the ``~/.tcshrc`` file.
* ``zsh``, edit the ``~/.zshrc`` file.

In order to apply the changes to the current shell environment systems: run ``source`` on the filename just edited::

    $ source [~/.filenamerc]


Aliases (fish shell)
--------------------
The above aliases for POSIX work just fine, but permanently saving an alias in
fish shell is a little different.  Functions are used to make permanent aliases.
``funced [command]`` is used to create the function, followed by
``funcsave [command]`` saves the function to the environment.


Running the ``funced cath``, brings up the default editor, and add the
following code in the function::

    function cath
            # 'cath' alias will highlight source code as cat does.
            alias cath="pygmentize"
    end

Here's what the ``cath`` function looks like with an additional argument::

    function cath
            alias cath="pygmentize -O style=monokai"
    end

Note: that just like the POSIX shell you may run ``alias cath="pygmentize -O style=monokai"``
on the command line to test out the alias beforehand.

Test the ``cath`` function. To save the function run ``funcsave cath``.
This saves the function for future sessions.


``funced lessh``, which is slightly more
complicated, because it uses the environment variable ``LESSOPEN`` to preprocess
the input file(s)::

    function lessh
            LESSOPEN="| pygmentize -O style=native %s" less -M -R $argv
    end

Test the ``lessh`` function. To save the function run ``funcsave lessh``.
