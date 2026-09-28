xquery version "4.0" encoding "utf-8";

(: A report over a library catalogue, written in XQuery 4.0.
   It is meant to exercise the 4.0 syntax in combination, not to be efficient. :)

declare namespace html = "http://www.w3.org/1999/xhtml";
declare default element namespace "http://example.com/catalogue";
declare boundary-space strip;
declare construction strip;
declare decimal-format local:money decimal-separator="," grouping-separator=".";

declare type local:isbn as xs:string;

declare record local:book(
  title    as xs:string,
  authors  as xs:string+,
  tags     as enum("fiction", "science", "history")*,
  price    as xs:decimal? := 0.0,
  year     as xs:integer := 0
);

declare context value as document-node(element(catalogue)) external;

declare variable $local:limit as xs:integer external := 0b1010;
declare variable $local:epsilon := 1_000;
declare variable $local:mask := 0xff_ff;

declare %private function local:format($amount as xs:decimal) as xs:string {
  format-number($amount, "#.##0,00", "local:money")
};

declare function local:describe(
  $book as local:book,
  $separator as xs:string := " · "
) as xs:string {
  string-join(($book?title, $book?authors => string-join(", ")), $separator)
};

declare function local:apply($items as item()*, $action as %updating fn(*)) {
  $items ! $action(.)
};

declare function local:classify($value as (xs:date | xs:integer)) as xs:string {
  typeswitch ($value) {
    case xs:date return "date"
    case xs:integer | xs:decimal return "number"
    default return "other"
  }
};

declare function local:in-order($a as gnode(), $b as gnode()) as xs:boolean {
  $a precedes-or-is $b and not($a is-not $b)
};

declare function local:label($id as attribute(id)?) as xs:string {
  switch (string($id)) {
    case "" return "untitled"
    case "?" return "unknown"
    default return string($id)
  }
};

declare function local:safe($input as xs:string?) as xs:string {
  try {
    ($input =!> normalize-space() => upper-case()) otherwise "n/a"
  } catch err:FORG0001 | err:XPTY0004 {
    "?"
  } finally {
    trace("classified " || local:classify(current-date()))
  }
};

let $books := for member $book in array { Q{http://example.com/catalogue}book }
              let $( $title, $year ) := ($book?title, $book?year)
              where $year gt 1950 and $year lt 2000
              order by $year descending, $title
                collation "http://www.w3.org/2013/collation/UCA"
              trace $title
              return $book

let $by-decade := map:build($books, fn($book) { $book?year idiv 10 * 10 })

for key $decade value $entries in $by-decade
count $position
while $position le $local:limit
return
  <html:section id="{ $decade }" data-größe="{ count($entries) }">
    <html:h2>Bücher aus den { $decade }ern — { count($entries) } Titel</html:h2>
    {
      for $entry at $index in $entries
      return <html:p class="entry">{
        `{ $index }. { local:describe($entry, separator := " – ") }`
      }</html:p>
    }
    { element #html:footer { ``[Preise in €: `{ $entries ! local:format(?price) }`]`` } }
    <html:aside>{{ literal braces }} &amp; entities &#x2026;</html:aside>
  </html:section>
