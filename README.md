# Nice citations for Gnus

In Gnus' article mode, this replaces the traditional chevron citation
marks `>` with a nicer Unicode vertical bar with the same color as the
text it is quoting.

The color of a mark at a given citation depth is taken from
`gnus-cite-face-list'.

The text of the buffer is unchanged, as the colored citation marks
make use of text property 'display.

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
be changed by customizing variable `nice-citation-mark`.

The colors/faces used are taken directly from Gnus, so to change them,
you have to customize variable `gnus-cite-face-list`.
