Interactive terminal/shell sessions
-----------------------------------

To highlight an interactive terminal or shell session, prefix your code snippet
with a specially formatted prompt.

Supported shells with examples are shown below. In each example, prompt parts in
brackets ``[any]`` represent optional parts of the prompt, and prompt parts
without brackets or in parenthesis ``(any)`` represent required parts of the
prompt.

* **Bash Session** (console, shell-session):

  .. code-block:: console

     [any@any]$ ls -lh
     [any@any]# ls -lh
     [any@any]% ls -lh
     $ ls -lh
     # ls -lh
     % ls -lh
     > ls -lh

* **MSDOS Session** (doscon):

  .. code-block:: doscon

     [any]> dir
     > dir
     More? dir

* **Tcsh Session** (tcshcon):

  .. code-block:: tcshcon

     (any)> ls -lh
     ? ls -lh

* **PowerShell Session** (ps1con):

  .. code-block:: ps1con

     PS[any]> Get-ChildItem
     PS> Get-ChildItem
     >> Get-ChildItem

