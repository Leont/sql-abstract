[![Actions Status](https://github.com/Leont/sql-abstract/workflows/test/badge.svg)](https://github.com/Leont/sql-abstract/actions)

Name
====

SQL::Abstract - Generate SQL from Raku data structures

Synopsis
========

```raku
use SQL::Abstract;

my $abstract = SQL::Abstract.new(:placeholders<dbi>);
my $query = $abstract.select('table', <foo bar>, :id(3));
my $result = $dbh.query($result.sql, $result.arguments);

my $join = { :left<books>, :right<authors>, :using<author_id> };
my $result = $abstract.select($join, ['books.name', 'authors.name'], { :cost('<' => 10) });
```

Description
===========

SQL::Abstract abstracts the generation of SQL queries. Fundamentally its functionality has three components

  * An AST to represent SQL queries

  * A set of helpers to convert standard raku datatypes into such an AST

  * A renderer to turn that AST into an SQL query

It should be able to represent any `SELECT`, `UPDATE`, `INSERT`, or `DELETE` query that is valid in both Postgresql and SQLite. This subset should be generally portable to other databases as well.

Class SQL::Abstract
===================

This is the main class of the module.

### new(:$placeholders!, Bool :$quoting)

This creates a new `SQL::Abstract` object. It has one mandatory name argument, `$placeholders`, which takes one of the following values:

  * `'dbi'`/`SQL::Abstract::Placeholders::DBI`

    This will use DBI style `(?, ?)` placeholders

  * `'postgres'`/`SQL::Abstract::Placeholders::Postgres`

    This will use Postgres style `($1, $2)` placeholders.

It also takes an optional argument `:$quoting`, if enabled table and column names will always be quoted.

select
------

```raku
method select(Source(Any) $source, Column::List(Any) $columns = *, Conditions(Any) $where?, Common(Any) :$common,
Distinction(Any) :$distinct, GroupBy(Any) :$group-by, Conditions(Any) :$having, Window::Clauses(Any) :$windows,
Compound(Pair) :$compound, OrderBy(Any) :$order-by, Int :$limit, Int :$offset, Locking(Any) :$locking)
```

This will generate a `SELECT` query. It will select `$columns` from `$source`, filtering by $conditions.

```raku
my $join = { :left<books>, :right<authors>, :using<author_id> };
my $result = $abstract.select($join, ['books.name', 'authors.name'], { :cost{ '<' => 10 } });
# SELECT books.name, authors.name FROM books INNER JOIN authors USING (author_id) WHERE cost < 10

my @columns = [ 'name', :sum{ :function<count>, :arguments(*) } ];
my $counts = $$abstract.select('artists', @columns, { :name(like => 'A%') }, :group-by<name>, :order-by(:sum<desc>));
# SELECT name, COUNT(*) as sum FROM artists WHERE name LIKE 'A%' GROUP BY name ORDER BY sum DESC
```

update
------

```raku
method update(Table(Any) $target, Assigns(Any) $assigns, Conditions(Any) $where?,
Common(Any) :$common, Source(Any) :$from, Column::List(Any) :$returning)
```

This will update `$target` by assigning the columns and values from `$set` if they match `$where`, returning `$returning`.

insert
------

### Map insertion

```raku
method insert(Table(Any) $target, Assigns(Any) $values, Common(Any) :$common,
Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning)
```

Inserts the values in `$values` into the table `$target`, returning the columns in `$returning`

```raku
$abstract.insert('artists', { :name<Metallica> }, :returning(*));
# INSERT INTO artists (name) VALUES ('Metallica') RETURNING *
```

### List insertions

```raku
method insert(Table(Any) $target, Identifiers(Any) $columns, Rows(List) $rows, Common(Any) :$common,
Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning)
```

Insert into `$target`, assigning each of the values in Rows to a new row in the table. This way one can insert a multitude of rows into a table.

```raku
$abstract.insert('artists', ['name'], [ ['Metallica'], ['Motörhead'] ], :returning(*));
# INSERT INTO artists (name) VALUES ('Metallica'), ('Motörhead') RETURNING *

$abstract.insert('artists', List, [ [ 'Metallica'], ], :returning<id>);
# INSERT INTO artists VALUES ('Metallica') RETURNING id
```

### Select insertion

```raku
method insert(Table(Any) $target, Identifiers(Any) $columns, Select(Map) $select, Common(Any) :$common,
Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning)
```

This selects from a (usually different) table, and inserts the values into the table.

```raku
$abstract.insert('artists', 'name', { :source<new_artists>, :columns<name> }, :returning(*));
# INSERT INTO artists (name) SELECT name FROM new_artists RETURNING *
```

delete
------

```raku
method delete(Table(Any) $target, Conditions(Any) $where, Common(Any) :$common,
Source(Any) :$using, Column::List(Any) :$returning)
```

This deletes rows from the database, optionally returning their values.

```raku
$abstract.delete('artists', { :name<Madonna> });
# DELETE FROM artists WHERE name = 'Madonna'
```

Helper types
============

SQL::Abstract uses various helper types that will generally coerce from basic datastructures:

SQL::Abstract::Identifier
-------------------------

This represents an identifier (typically a table name or column name, or an alias for such). It can be coerced from a string (e.g. `"foo"` or `"foo.bar"`).

SQL::Abstract::Identifiers
--------------------------

This takes either a list of `Identifier()`, or a single `Identifier()`.

```raku
my SQL::Abstract::Identifiers() $identifiers = <name email website>;
```

SQL::Abstract::Source
---------------------

A source is source of data, usually a table or a join. If not passed as a `Source` object it will upconvert from the following types:

  * Str

    This represents the named table, e.g. `"my_table"`.

  * List

    This represents the named table, with the elements of the list representing the components of the table name. E.g. `<bar baz> ` is equivalent to `"bar.baz" `.

  * Pair (Str => Identifier(Cool))

    This will rename the table in the value to the name in the key.

  * Pair (Str => Select(Map))

    This will use the result of a subquery as if it's a table.

  * Map

    This will join two `Source`s, named `left` and `right`, it requires one of the following entries to join them on:

          * Join::Conditions() :$on

          * Identifiers() :$conditions

          * Bool :$natural

          * Bool :$cross

    e.g. `{ :left<artist>, :right<album>, :using<album_id> } ` or `{ :left<artist>, :right<album>, :on{'artist.id' => 'album.artist_id'} } `

    The first three joiners take an optional `:$type` argument that can be any of `"inner"`/`Join::Type::Inner`, `"left"`/`Join::Type::Left`, `"right"`/`Join::Type::Right` or `"full"`/`Join::Type::Full`.

SQL::Abstract::Table does SQL::Abstract::Source
-----------------------------------------------

This role takes the same conversions as `Source`, but only the ones that represent a table. Unlike other sources, this can be used for mutating operations (update/insert/delete).

SQL::Abstract::Column::List
---------------------------

```raku
my Column::List() $columns = ('name', :number(:count(*)));
# name, COUNT(*) AS number;
```

This is a list of items representing a column. Each item can either be a: much like `Identifiers`, however it will accept not just identifiers but any expression (e.g. comparisons, function calls, etc…). If given a pair it will rename the value to the key (`value AS key`). A whatever-star will represent all columns.

SQL::Abstract::Conditions
-------------------------

```raku
my Conditions() $conditions = { :name(:like<%leon%>), :age(25..45), :country('nl'|'be'|lu') };
# name LIKE '%leon%' AND AGE BETWEEN 25 AND 45 AND country IN('nl', 'be', 'lu')
```

This is a pair, a list of pairs, a hash or an `Expression`. In the former three cases, the key (called left in the rest of this section) shall be an `Identifier()` designating a column name, or an `Expression`. The right hand side can be one of several types:

### Expression

This will be used as-is

### Any:U

This will check if the left expression is `NULL`; `:left(Any)` equals `left IS NULL`.

### Pair

This will use the key as operator to compare left against another value or expression. E.g. `:left('<' => 42) ` renders like `left < 42 `. The following keys are known:

  * `=`

  * `!=`

  * `<> `

  * `< `

  * `<= `

  * `> `

  * `>= `

  * `like`

  * `not-like`

  * `distinct`

  * `not-distinct`

  * `||`

  * `-> `

  * `->> `

  * `#> `

  * `#>> `

  * `*`

  * `/`

  * `%`

  * `+`

  * `-`

  * `&`

  * `|`

  * `<< `

  * `>> `

A few operators are not binary operators as such.

  * `null`

    will do `IS NULL`, or `IS NOT NULL` if its argument is false(`:!null`).

  * `in`/`not-in`

    This takes a list of values that the column will be compared to. E.g. `:in(1, 2, 3)`.

  * `and`/`or`

    The logical operators take a list or arguments, that are all expanded like pair values. So `:foo(:and('>' => 3, '<' => 42)) ` renders as `foo > 3 AND foo < 42 `

### Range

This will check if a value is in a certain range. E.g. `:left(1..42)` will render like `left BETWEEN 1 AND 42`.

### Map

This will be interpreted as a conjunction of the hash pairs. E.g. `:left{ '>' => 3, '<' => 42 } ` will render like `left > 3 AND left < 42 `.

### Junction

This will check against the values in the function. E.g. `:left(1|2|4)` will render like `left IN (1, 2, 4)`.

### Capture

This will be expanded as a capture expression (. E.g. `:left(\'NOW()')` will render like `left = NOW()`. If it's an `:op` expression, the column will be inserted as first/left operand: `:left(\(:op('<' => 42))) ` renders like `left < 42 `.

### Any

If none of the above options match, the value will be compared to as is (as a placeholder). `:left(2)` will render equivalent to `left = 2`.

SQL::Abstract::Assigns
----------------------

This takes a list of pairs, or a hash. The keys shall be a value or an expression. E.g. `:name<author>, :id(SQL::Abstract::Values::Default), :timestamp(\'NOW()') `

SQL::Abstract::OrderBy
----------------------

This takes a list of things to sort by. Much like `Column::List` this accepts identifiers and expressions, but `*` isn't allowed and pair values are interpreted as order modifier (e.g. `:column<desc>`). A hash element will be expanded as well (e.g. `{ :column<column_name>, :order<desc>, :nulls<last> } `)

SQL::Abstract::Common
---------------------

This represents a common table expression. It converts from a pair or a list of pairs, with the keys being the name and the values being either a table name, a select hash or an `SQL::Abstract::Query` object.

```raku
my Common() $cte = recent => { :source<users>, :columns('name', :count(:count(*)), :group-by(name) };
# WITH recent AS (SELECT name, COUNT(*) AS count FROM users GROUP BY name);
```

SQL::Abstract::Locking
----------------------

This takes one or more locking clauses. A locking clause is usually taken ... strings: `'update'`, C'<no key update'>, `'share'`, `'key share'`, but it can also take a pair of stregth

SQL::Abstract::GroupBy
----------------------

This takes a list of grouping elements. Usually these are just columns, but they may also be arbitrary expressions (inclusing lists of columns). A pair is taken as a function call with the key as function name and the value as arguments.

SQL::Abstract::Conflicts
------------------------

This represents one or more upsert clause. It can either be the string `'nothing'`, or a pair with the columns as keys and an `Assigns(Map)`.

```raku
my SQL::Abstract::Conflicts = <name organization> => { :$email };
# ON CONFLICT (name, organization) DO UPDATE email = ?
```

SQL::Abstract::Distinction
--------------------------

This takes `True`for a distinct row, or a `Column::List` for specific rows that have to be distinct.

Window::Definition
------------------

Window definiton converts from a map taking the following keys, all optional:

  * Identifier(Cool) :$existing

    This takes the name of an existing window

  * Column::List(Any) :$partition-by

    This lists expressions to partition the rows by, somewhat similar to a `GROUP BY`.

  * OrderBy(Any) :$order-by

    The order within a frame.

  * Boundary(Any) :$from

    This argument defines the starting boundary of the frame. This can be any of:

        * 'preceding'

        * :preceding($amount)

        * 'current'

        * :following($amount)

It defaults to 'preceding'.

  * Boundary(Any) :$to

    This optional argument defines the ending boundary of the frame, This can be any of:

        * :preceding($amount)

        * 'current'

        * :following($amount)

        * 'following'

  * Mode:D(Str) :$mode = Mode::Range

    The mode of the frame takes one of `'rows'`, `'range'` or `'groups'`, defaulting to `'range'`.

  * Exclusion(Str) :$exclude

    The exclusion of the frame, it takes one of `'current row'`, `'group'`, `'ties'` or `'no others'` (the default).

```raku
my Window::Definition $d = { :partition-by<foo bar>, :from<current> :to(:following(5)), :exclude<ties> }
# PARTITION BY foo, bar RANGE BETWEEN CURRENT ROW AND 5 FOLLOWING EXCLUDE TIES
```

Window::Clauses
---------------

This takes one or more pairs, with the names being windows names and the values taking window definition maps.

```raku
my Windows::Clauses $clauses =
    over5 => { :frame{ :preceding(5) } },
    foo => { :partition-by<foo bar>, :mode<range>, :from<current> :to(:following(5)), :exclude<ties> };
# WINDOW
#   over5 AS (ROWS 5 PRECEDING),
#   foo as (PARTITION BY foo, bar RANGE BETWEEN CURRENT ROW AND 5 FOLLOWING EXCLUDE TIES)
```

Capture expressions
===================

Captures can be used in most places where Expressions can be used. They allow for SQL expressions that can't be encoded using simpler values.

There are two kinds of capture expressions. The first kind has one or more named arguments; the first will be used as literal SQL, the rest will be arguments for the literal. E.g. `\'NOW()'`.

The second kind takes a single named argument that may or may not contain a value. Currently supported are:

  * Any :$bind

    This represents a placeholder variable with the given value

  * Bool :$default

    This represents the `DEFAULT` keyword

  * Bool :$true

    This represents the `TRUE` keyword

  * Bool :$false

    This represents the `FALSE` keyword

  * Bool :$null

    This represents the `NULL` keyword

  * Str :$ident

    This represents an identifier (typically a column or table name)

  * Bool :$star

    This represents a `*`.

  * Any :$idents

    This represents a list of identifiers

  * Any :$columns

    This represents a list of column expressions

  * :op(@) ('not'|'+'|'-'|'~', Any $expr)

    This represents a unary operator (`NOT`, `+`, `-`, `~`).

  * :op(@) ('like'|'not-like', Any $left-expr, Any $right-expr, Any $escape-expr)

    This represents a `LIKE` operator, e.h. `column LIKE '%foo?' ESCAPE '\\'`

  * :op(@) ('between'|'not-between', Any $column-expr, Any $left-expr, Any $right-expr)

    This represents a `BETWEEN` expression.

  * :@and

    This represents an AND expresssion. Typically the contents of this will be further capture expressions.

  * :@or

    This represents an OR expresssion. Typically the contents of this will be further capture expressions.

  * :@op ('in'|'not-in', Any $left-expr, Capture $ (SQL::Abstract::Select(Map) :$select!))

    This represents an <IN> expression with subquery. E.g. `foo in (SELECT bar from baz where baz.id = table.id)`.

  * :@op ('in'|'not-in', Any $left-expr, *@args)

    This represents an <IN> expression with list. E.g. `foo in (1, 2, 3)`.

  * Select(Map) :$exists

    This represents an `EXISTS` expression. E.g. `\(:exists{ :source<table>, columns<1>, :where{ :id(\(:ident<outer.id>)) }` for `EXISTS(SELECT 1 FROM table WHERE id = outer.id`.

  * Select(Map) :$not-exists

    This is like :exists, but negated.

  * :@op ("cast", Any $expression, Str $typename)

    This is a <CAST> expression. E.g. `CAST(columns AS INTEGER)`.

  * :op(@) (Str $key, Any $left-expr, Any $right-expr)

    Any binary operator applied to two expressions.

  * Identifier(Any) :$current

    This represents a `CURRENT FOR cursor_name` clause, typically used in an `UPDATE` or `DELETE` statement.

  * List :$row

    This represents a row expression, e.g. `(a, b, c)`.

  * Function(Map) :$function

    This represents a function call.

  * Select(Map) :$select

    This represents a subquery expression.

Author
======

Leon Timmermans <fawaka@gmail.com>

Copyright and License
=====================

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

