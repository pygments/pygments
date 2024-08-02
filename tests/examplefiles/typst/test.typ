#show link: set text(navy)
#show par: set block(below: 1.75em)
#set text(font: "Palatino", hyphenate: false, lang: "gb")
#set page(margin: (x: 1.5cm, y: 2.5cm), numbering: "1 / 1")
#let section(title, content) = grid(
      columns: (55pt, 1fr),
      text(darkgrey)[#date],
      h(1fr),
      content
)

== Title
#link("mailto:spam@example.com")
#text(darkgrey)[ | ] #link("https://example.com")[super example]
#line(length: 100%, stroke: 0.2mm)
#section([another \ test], [
  *#link("https://github.com/typst/typst")[typst]* --- bob #section_date([2022 -- present]) \
    some text
  / Itemone: -- first item
  ]
)

Another link, with http instead of https: http://example.com
