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
my $result = $abstract.select($join, ['books.name', 'authors.name'], { 'cost' => { '<' => 10 }});
```

Description
===========

SQL::Abstract abstracts the generation of SQL queries. Fundamentally its functionality has three components

  * An AST to represent SQL queries

  * A set of helpers to convert standard raku datatypes into such an AST

  * A renderer to turn that AST into an SQL query

It should be able to represent any `SELECT`, `UPDATE`, `INSERT`, or `DELETE` query that is valid in both Postgresql and SQLite. This subset should be generally portable to other databases as well.

Helper types
============

SQL::Abstract uses various helper types:

SQL::Abstract::Identifier
-------------------------

This represents an identifier (typically a table name or column name, or an alias for such). It can be created from either:

  * Str

    This will interpret a string (e.g. `"foo"` or `"foo.bar"`) as an identifier.

  * List

    This will interpret the elements of the list representing the components of the name. E.g. `<bar baz> ` is equivalent to `"bar.baz" `.

SQL::Abstract::Identifiers
--------------------------

This takes either a list of `Identifier()`, or a single `Identifier()`. Note that a single list will be interpreted will be interpreted as a list of string identifiers, if one wants to pass a single list-from identifier the list must be nested (e.g. `[ <table column>,]`).

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

  * Hash

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

This is a list much like `Identifiers`, however it will accept not just identifiers but any expression (e.g. comparisons, function calls, etc???). If given a pair it will rename the value to the key (`value AS key`). A whatever-star will represent all columns.

SQL::Abstract::Conditions
-------------------------

This is a pair, a list of pairs, a hash or an `Expression`. In the former three cases, the key (called left in the rest of this section) shall be an `Identifier()` designating a column name, or an `Expression`. The right hand side can be one of several types:

### Expression

This will be used as-is

### Any:U

This will check if the left expression is `NULL`; `:left(Any)` equals `left IS NULL`.

### Pair

This will use the key as operator to compare left against another value or expression. E.g. `:left('<' => 42)` renders like `left < 42 `. The following keys are known:

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

### Range

This will check if a value is in a certain range. E.g. `:left(1..42)` will render like `left BETWEEN 1 AND 42`.

### Map

This will be interpreted as a conjunction of the hash pairs. E.g. `:left{ '>' => 3, '<' => 42 } ` will render like `left > 3 AND left < 42 `.

### Junction

This will check against the values in the function. E.g. `:left(1|2|4)` will render like `left IN (1, 2, 4)`.

### Capture

This will be used as a literal value. E.g. `:left(\'NOW()')` will render like `left = NOW()`.

### Any

If none of the above options match, the value will be compared to as is (as a placeholder). `:left(2)` will render equivalent to `left = 2`.

SQL::Abstract::Assigns
----------------------

This takes a list of pairs, or a hash. The keys shall be a value or an expression. E.g. `:name<author>, :id(SQL::Abstract::Values::Default), :timestamp(\'NOW()') `

SQL::Abstract::OrderBy
----------------------

This takes a list of things to sort by. Much like `Column::List` this accepts identifiers and expressions, but `*` isn't allowed and pair values are interpreted as order modifier (e.g. `:column<desc>`). A hash element will be expanded as well (e.g. `{ :column<column_name>, :order<desc>, :nulls<last> } `)

Class SQL::Abstract
===================

This is the main class of the 

### new(:$placeholders!)

This creates a new `SQL::Abstract` object. It has one mandatory name argument, `$placeholders`, which takes one of the following values:

  * `dbi`/`SQL::Abstract::Placeholders::DBI`

    This will use DBI style `(?, ?)` placeholders

  * `postgres`/`SQL::Abstract::Placeholders::Postgres`

    This will use Postgres style `($1, $2)` placeholders.

### select(Source() $source, Column::List() $columns = *, Conditions() $where?, Column::List() :$group-by, Conditions() :$having, OrderBy() :$order-by, Int :$limit, Int :$offset)

This will generate a `SELECT` query. It will select `$columns` from `$source`, filtering by $conditions. 

```raku
my $join = { :left<books>, :right<authors>, :using<author_id> };
my $result = $abstract.select($join, ['books.name', 'authors.name'], { :cost{ '<' => 10 } });
# SELECT books.name, authors.name FROM books INNER JOIN authors USING (author_id) WHERE cost < 10

my @columns = [ 'name', :sum{ :function<count>, :arguments(*) } ];
$abstract.select('artists', @columns, { :name(:like('A%')) }, :group-by<name>, :order-by(:sum<desc>));
# SELECT name, COUNT(*) as sum FROM artists WHERE name LIKE 'A%' GROUP BY name ORDER BY sum DESC
```

### update(Table(Cool) $target, Assigns(Hash) $set, Conditions() $where?, Source() :$from, Column::List() :$returning)

This will update `$target` by assigning the columns and values from `$set` if they match `$where`, returning `$returning`.

### insert(Table(Cool) $target, Column::List() $columns, Rows(List) $rows, Column::List() :$returning)

Insert into `$target`, assigning each of the values in Rows to a new row in the table.

```raku
$abstract.insert('artists', ['name'], [ [ 'Metallica'], ], :returning(*));
# INSERT INTO artists (name) VALUES ('Metallica') RETURNING *

$abstract.insert('artists', List, [ [ 'Metallica'], ], :returning(*));
# INSERT INTO artists VALUES ('Metallica') RETURNING *
```

### insert(Table(Cool) $target, Assigns() $values, Column::List() :$returning)

Inserts the values in `$values` into the table `$target`, returning the columns in `$returning`

```raku
$abstract.insert('artists', { :name<Metallica> }, :returning(*));
# INSERT INTO artists (name) VALUES ('Metallica') RETURNING *
```

### insert(Table(Cool) $target, Identifiers() $columns, Select(Map) $values, Column::List() :$returning)

```raku
$abstract.insert('artists', 'name', { :source<new_artists>, :columns<name> }, :returning(*));
# INSERT INTO artists (name) SELECT name FROM new_artists RETURNING *
```

### delete(Table(Cool) $target, Conditions() $where?, Column::List() :$returning)

```raku
$abstract.delete('artists', { :name<Madonna> });
# DELETE FROM artists WHERE name = 'Madonna'
```

Author
======

Leon Timmermans <fawaka@gmail.com>

Copyright and License
=====================

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

