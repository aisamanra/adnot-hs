# Adnot

The *Adnot* format is a simple data and configuration format intended
to have a slightly enriched data model when compared to JSON or
s-expressions but still retain the comparative simplicity of those
formats. Unlike JSON, Adnot chooses to avoid redundant structural
information like punctuation; unlike s-expressions, Adnot values
natively express a wider range of basic data types.

*Adnot* is not intended to be a data interchange format, but rather to
be a richer and more convenient syntax for certain kinds of data
description that might otherwise be done in more unwieldy formats like
YAML. As a first approximation, Adnot may be treated as a more human-
and version-control-friendly version of JSON whose data model is
intended to resemble the data model of statically typed functional
programming languages.

A given Adnot value is either one of four basic types—an integer, a
double, a string, or an identifier—or one of three composite types: a
sequence of values, a mapping of symbols to values, or a tagged
sequence of values which begins with a symbol:

```
expr ::= "{" (symbol expr) * "}"
       | "(" symbol expr* ")"
       | "[" expr* "]"
       | string
       | symbol
       | integer
       | double
```

Strings are understood in the same way as JSON strings, with the same
encoding and the same set of escapes. Symbols are unquoted strings
that start with a Unicode character with the `XID_Start` and continue
with the `XID_Continue` characters, and thus should resemble the
identifier syntax for a large number of C-like languages.

The three kinds of composite types are meant to resemble records, sum
or variant types, and lists, respectively. Zero or more
symbol-expression pairs inside curly brackets form a _map_:

```
# a basic map
{
  x 2
  y 3
  z 4
}
```

Pairs do not include colons and are not separated by commas. A map
_must_ contain an even number of sub-expressions, and every odd
subexpression _must_ be a symbol. (This restriction might be lifted in
the future?) Whitespace is ignored except as a separator between
tokens, so the above map is identical to

```
{x 2 y 3 z 4}
```

A _list_ is represented by square brackets with zero or more
possibly-heterogeneous expressions:

```
# a basic list
[ 2 "foo" bar ]
```

A _tagged expression_ is represented by parentheses with a single
symbol followed by zero or more possibly-heterogeneous expressions:

```
# a basic tagged expression
(some_tag blah 7.8 "??")
```

These are how tagged data-types are traditionally represented: because
the thing inside the parens _must_ be a symbol, it can correspond to a
data type in an ML-like language.
