# Fountain Lexer Plan

## Goal

Add a new `FountainLexer` to Pygments for Fountain, the plain-text screenplay format described at <https://fountain.io/syntax/>.

## Recommended Approach

Implement Fountain as a small custom lexer with line-by-line block analysis rather than as a pure `RegexLexer`.

Fountain is context-sensitive:

- block types depend on surrounding blank lines
- uppercase lines can mean scene headings, transitions, character cues, or plain action
- dialogue depends on the preceding character or parenthetical line
- title page parsing only applies at the top of the file

Because of that, the cleanest first version is either:

- a `Lexer` subclass with `get_tokens_unprocessed()`, or
- a `RegexLexer` that still overrides `get_tokens_unprocessed()` for the main block parser

## File Placement

Add the lexer in a text/markup-oriented module, most likely:

- `pygments/lexers/markup.py`

Then update:

- module `__all__`
- generated lexer mapping via `tox -e mapfiles`

## Lexer Metadata

Use metadata along these lines:

```python
name = 'Fountain'
aliases = ['fountain']
filenames = ['*.fountain', '*.spmd']
mimetypes = []
```

Notes:

- `.fountain` is the main extension
- `.spmd` is also used by existing editors and older tooling
- `.txt` should not be claimed because it is too broad
- no official MIME type appears stable enough to rely on

## Parsing Strategy

Implement parsing in two passes.

### 1. Block Classification

Walk through the document line by line and classify blocks using context.

Support these block types first:

1. title page
2. boneyard comments
3. page breaks
4. sections
5. synopses
6. scene headings
7. transitions
8. centered text
9. character cues
10. parentheticals
11. dialogue
12. lyrics
13. action

### 2. Inline Markup

Once a block type is known, run a lighter inline tokenizer over relevant text for:

- `*italic*`
- `**bold**`
- `***bold italic***`
- `_underline_`
- backslash escapes
- inline `[[notes]]`
- scene numbers like `#12A#`

This should apply to scene headings, action, dialogue, centered text, title-page values, and similar text-bearing blocks.

## Syntax Features to Support First

### Title Page

Support title-page parsing only at the start of the file.

Rules:

- lines of the form `Key: value`
- keys may contain spaces
- indented continuation lines belong to the previous key
- stop title-page parsing once normal screenplay content begins

Examples from the local sample:

- `Title:`
- `Credit:`
- `Author:`
- `Source:`
- `Copyright:`

### Scene Headings

Support both automatic and forced scene headings.

Automatic scene-heading prefixes:

- `INT`
- `EXT`
- `EST`
- `INT./EXT`
- `INT/EXT`
- `I/E`

Rules:

- case-insensitive
- prefix must be followed by `.` or space
- normally requires a blank line before and after
- forced scene heading uses leading `.` followed by an alphanumeric character
- optional trailing scene numbers `#...#`

### Action

Treat action as the default fallback block type.

Also support forced action with leading `!`.

Behavior:

- preserve plain text lines that do not fit other elements
- treat ambiguous cases as action when possible
- preserve indentation as text

### Character and Dialogue

Support:

- automatic character cues for uppercase lines with valid context
- forced character cues with leading `@`
- character extensions on the same line, such as `(V.O.)`, `(O.S.)`, `(CONT'D)`
- parenthetical lines inside dialogue blocks
- dialogue lines following a character cue or parenthetical
- dual dialogue marker `^` at end of character line

Important heuristic:

- uppercase alone is not enough; the following nonblank lines must look like dialogue content

### Transitions and Centered Text

Support:

- automatic transitions for uppercase lines ending in `TO:` with blank lines around them
- forced transitions with leading `>`
- centered text with `>...<`

Disambiguation rule:

- if a line starts with `>` and also ends with `<`, treat it as centered text
- otherwise treat leading `>` as forced transition

### Lyrics

Support lyric lines forced with leading `~`.

### Notes and Boneyard

Support:

- inline and multiline notes with `[[...]]`
- multiline boneyard comments with `/* ... */`

Boneyard should be able to span many lines. It is the one syntax element that should clearly cross line boundaries without the usual paragraph limits.

### Sections and Synopses

Support:

- sections with leading `#`, `##`, `###`, etc.
- synopses with leading `=`

### Page Breaks

Support lines made of three or more `=` characters and nothing else.

## Recommended Token Mapping

Use a practical token mapping rather than trying to encode layout semantics.

Suggested mapping:

- scene headings -> `Generic.Heading`
- section lines -> `Generic.Subheading`
- synopses -> `Comment.Special`
- character cues -> `Name.Label`
- character extensions -> `Name.Decorator`
- parentheticals -> `Name.Decorator`
- dialogue -> `String`
- lyrics -> `String.Other`
- transitions -> `Keyword`
- centered text -> `Generic.Strong`
- title-page keys -> `Name.Attribute`
- title-page values -> `String`
- notes -> `Comment.Special`
- boneyard -> `Comment.Multiline`
- page breaks -> `Operator`
- scene numbers -> `Name.Label`
- inline bold -> `Generic.Strong`
- inline italics -> `Generic.Emph`
- inline underline -> `Generic.Emph` for now
- escapes -> `String.Escape`
- dual-dialogue caret -> `Punctuation`

## Heuristics and Edge Cases

These are the main ambiguity points the implementation should handle deliberately.

### Uppercase Disambiguation

An uppercase line may be:

- character cue
- transition
- scene heading
- action

Recommended rule:

- prefer explicit markers first (`.`, `!`, `@`, `>`, `~`)
- then apply block-context heuristics
- if still unclear, fall back to action

### Dialogue Continuation

Dialogue blocks need state.

Once a character cue is recognized:

- parenthetical lines should stay in dialogue context
- following text lines should be dialogue until a terminating blank line
- intentional blank lines inside dialogue may need to be preserved carefully

### Title Page Detection

Title page should only be recognized at the beginning of the document. Once the lexer enters screenplay body mode, later `Key: value` lines should not be treated as title-page metadata.

### Inline Emphasis

Keep the first implementation conservative:

- do not let emphasis span line breaks
- honor backslash escapes
- avoid overmatching when delimiters are surrounded by spaces

### Notes

Treat standalone and inline notes consistently. If exact paragraph-collapsing behavior is difficult in the first pass, prioritize correct highlighting over perfect structural removal.

## Suggested Implementation Order

1. Create the lexer skeleton and metadata.
2. Register it in `__all__` and regenerate mappings.
3. Implement title page, boneyard, page breaks, sections, and synopses.
4. Implement scene headings, transitions, centered text, and forced action.
5. Implement character, parenthetical, dialogue, and dual-dialogue state handling.
6. Add inline emphasis, escapes, notes, and scene-number highlighting.
7. Add tests and generate expected outputs.

## Test Plan

Add fixtures in the standard Pygments locations.

### Example Files

Create:

- `tests/examplefiles/fountain/`

Recommended contents:

- a short curated `example.fountain`
- optionally `Big-Fish.fountain` as a large real-world sample

Then generate goldens with:

```bash
pytest --update-goldens tests/examplefiles/fountain
```

### Snippets

Create:

- `tests/snippets/fountain/`

Include focused cases for:

- title page with indented continuation lines
- automatic scene heading
- forced scene heading with `.`
- forced action with `!`
- automatic character + dialogue
- forced character with `@`
- parenthetical inside dialogue
- dual dialogue `^`
- automatic transition
- forced transition `>`
- centered text `>...<`
- page break `===`
- sections and synopses
- inline note `[[...]]`
- multiline boneyard `/* ... */`
- emphasis and escape edge cases
- scene numbers like `#12A#`

### Guessing and Filename Matching

Rely primarily on filename matching for the first version.

At minimum, ensure:

- `.fountain` matches the lexer
- `.spmd` matches the lexer
- fixture directory name `fountain` matches the lexer alias

An `analyse_text()` implementation is optional for the first pass and can be added later if there is a reliable heuristic.

## Recommended Scope for the First Version

Aim for:

- strong support for official Fountain 1.1 syntax
- robust highlighting for real-world screenplay files
- predictable fallback to action/text when heuristics are uncertain

Do not over-optimize the first version for every parser-specific extension from editor plugins. Compatibility with the core syntax and the provided `Big-Fish.fountain` sample is the right initial target.

## Key Design Decision

Recommended default:

- when ambiguity remains after checking explicit markers and nearby blank lines, classify the block as action

That matches the Fountain spec's error-handling guidance and should make the lexer feel safer and less surprising.
