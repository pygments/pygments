scdoc(5)

# NAME

scdoc - document format for writing manual pages

# SYNTAX

Input files must use the UTF-8 encoding.

## PREAMBLE

Each scdoc file must begin with the following preamble:

	*name*(_section_) ["left\_footer" ["center\_header"]]

*name* is the name of the man page you are writing, and _section_ is the section
you're writing for (see *man*(1) for information on manual sections).

_left\_footer_ and _center\_header_ are optional arguments which set the text
positioned at those locations in the generated man page, and *must* be
surrounded with double quotes.

## SECTION HEADERS

Each section of your man page should begin with something similar to the
following:

	# HEADER NAME

Subsection headers are also understood - use two hashes. Each header must have
an empty line on either side.

## PARAGRAPHS

Begin a new paragraph with an empty line.

## LINE BREAKS

Insert a line break by ending a line with \+\+.

The result looks++
like this.

## FORMATTING

Text can be made *bold* or _underlined_ with asterisks and underscores: \*bold\*
or \_underlined\_. Underscores in the_middle_of_words will be disregarded.

## INDENTATION

You may indent lines with tab characters (*\\t*) to indent them by 4 spaces in
the output. Indented lines may not contain headers.

	The result looks something like this.

	You may use multiple lines and most _formatting_.

Deindent to return to normal, or indent again to increase your indentation
depth.

## LISTS

You may start bulleted lists with dashes (-), like so:

```
- Item 1
- Item 2
	- Subitem 1
	- Subitem 2
- Item 3
```

The result looks like this:

- Item 1
- Item 2
	- Subitem 1
	- Subitem 2
- Item 3

You may also extend long entries onto another line by giving it the same indent
level, plus two spaces. They will be rendered as a single list entry.

```
- Item 1 is pretty long so let's
  break it up onto two lines
- Item 2 is shorter
	- But its children can go on
	  for a while
```

- Item 1 is pretty long so let's
  break it up onto two lines
- Item 2 is shorter
	- But its children can go on
	  for a while

## NUMBERED LISTS

Numbered lists are similar to normal lists, but begin with periods (.) instead
of dashes (-), like so:

```
. Item 1
. Item 2
. Item 3,
  with multiple lines
```

. Item 1
. Item 2
. Item 3,
  with multiple lines

## TABLES

To begin a table, add an empty line followed by any number of rows.

Each line of a table should start with | or : to start a new row or column
respectively (or space to continue the previous cell on multiple lines),
followed by [ or - or ] to align the contents to the left, center, or right,
followed by a space and the contents of that cell.  You may use a space instead
of an alignment specifier to inherit the alignment of the same column in the
previous row.

The first character of the first row is not limited to | and has special
meaning. [ will produce a table with borders around each cell. | will produce a
table with no borders. ] will produce a table with one border around the whole
table.

To conclude your table, add an empty line after the last row.

```
[[ *Foo*
:- _Bar_
:-
|  *Row 1*
:  Hello
:] world!
|  *Row 2*
:  こんにちは
:  世界
   !
```

[[ *Foo*
:- _Bar_
:-
|  *Row 1*
:  Hello
:] world!
|  *Row 2*
:  こんにちは
:  世界
   !

## LITERAL TEXT

You may turn off scdoc formatting and output literal text with escape codes and
literal blocks. Inserting a \\ into your source will cause the subsequent symbol
to be treated as a literal and copied directly to the output. You may also make
blocks of literal syntax like so:

```
\```
_This formatting_ will *not* be interpreted by scdoc.
\```
```

These blocks will be indented one level. Note that literal text is shown
literally in the man viewer - that is, it's not a means for inserting your own
roff macros into the output. Note that \\ is still interpreted within literal
blocks, which for example can be useful to output \``` inside of a literal
block.

## COMMENTS

Lines beginning with ; and a space are ignored.

```
; This is a comment
```

# CONVENTIONS

By convention, all scdoc documents should be hard wrapped at 80 columns.

# SEE ALSO

*scdoc*(1)

# AUTHORS

Maintained by Drew DeVault <sir@cmpwn.com>. Up-to-date sources can be found at
https://git.sr.ht/~sircmpwn/scdoc and bugs/patches can be submitted by email to
~sircmpwn/public-inbox@lists.sr.ht.
