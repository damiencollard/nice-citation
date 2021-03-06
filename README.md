# Nice citations for Gnus

In Gnus' article mode and in message mode, this replaces the
traditional chevron citation marks `>` with a nicer Unicode vertical bar
with the same color as the text it is quoting.

The color of a mark at a given citation depth is taken from
`gnus-cite-face-list`.

The text of the buffer is unchanged, as the colored citation marks
make use of text property `'display`.

It is purely **presentation**: The underlying text is **unchanged**. In
particular, if you send an e-mail containing a citation, it will
contain the traditional chevron marks.

Standard | Nice citations
:-------:|:--------------:
![Without](images/without.png?raw=true "Without")  | ![With](images/with.png?raw=true "With")

## Installation

Copy `nice-citation.el` into a directory that appears in your Emacs' `load-path`.

## Usage

Add the following line to your emacs configuration:
```lisp
(require 'nice-citation)
```

## Customization

The symbol used as mark (a Unicode light vertical bar by default) can
be changed by customizing variable `nice-citation-mark`. It works well
with Fira Code, for instance.

The colors/faces used are taken directly from Gnus, so to change them,
you have to customize variable `gnus-cite-face-list`.

Variable `nice-citatation-improve-marks-alignment` makes the vertical
marks align with the surrounding text. Its effect is noticeable in
case of nested citations. It is disabled by default.

The nice citations can be disabled in article mode by setting
`nice-citation-treat-citations` in group `gnus-article-treat` to nil.
