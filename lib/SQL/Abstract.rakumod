unit class SQL::Abstract;

enum Precedence <Rowlike Comma And Or Assignment Between In Prefix Comparative Additive Multiplicative Postfix Term>;

multi prefix:<//>(Any $input) {
	return $input.defined;
}

role Expression {
	method precedence(--> Precedence) { ... }
}

class Literal does Expression {
	method precedence(--> Precedence::Or) {}
	has Str:D $.payload is required;
	has Any:D @.arguments;
	method new(Str:D $payload, @arguments) {
		self.bless(:$payload, :@arguments);
	}
}

class Value does Expression {
	method precedence(--> Precedence::Term) {}
	has Any $.value;
	method new(Any $value) {
		self.bless(:$value);
	}
}

my multi expand-value(Expression $expression) {
	$expression;
}
my multi expand-value(Any $value) {
	Value.new($value);
}

class Ident {
	has Str @.parts;
	multi method new(@parts) {
		self.bless(:@parts);
	}
	multi method new(Cool $name) {
		my @parts = $name.split('.');
		self.bless(:@parts);
	}
}

class Bind does Expression {
	method precedence(--> Precedence::Term) {}
	has Str $.column;
	has Any $.value;
	method new(Str $column, Any $value) {
		self.bless(:$column, :value(expand-value($value)));
	}
}

class Row does Expression {
	method precedence(--> Precedence::Rowlike) {}
	has Expression @.elements;
	method new(Expression @elements) {
		self.bless(:@elements);
	}
}

class Function does Expression {
	method precedence(--> Precedence::Term) {}
	has Str:D $.name is required;
	has Expression @.arguments;
	method new(Str $name, Expression @arguments) {
		self.bless(:$name, :@arguments);
	}
}

role Op does Expression {
}

role Op::Unary does Op {
	method precedence(--> Precedence::Term) {}
	has Expression:D $.input is required;
	method new(Expression $input) {
		self.bless(:$input);
	}
}

role Op::Unary::Postfix[Str $operator] does Op {
	has Expression:D $.value is required;
	method precedence(--> Precedence::Postfix) {}
	method postfix(--> Str) { $operator }
	method new(Expression $value) {
		self.bless(:$value);
	}
}

class Op::IsNull does Op::Unary::Postfix['IS NULL'] {
}
class Op::IsNotNull does Op::Unary::Postfix['IS NOT NULL'] {
}

class Op::Unary::Prefix does Op::Unary {
	method precedence(--> Precedence::Prefix) {}
	has Str:D $.operator is required;
	method new(Str $operator, Expression $input) {
		self.bless(:$operator, :$input);
	}
}

role Op::Binary does Op {
	method operator() { ... }
	has Expression:D $.left is required;
	has Expression:D $.right is required;
	method new(Expression $left, Expression $right) {
		self.bless(:$left, :$right);
	}
}

role Op::Comperative does Op::Binary {
	method precedence(--> Precedence::Comparative) {}
}

class Op::Equals does Op::Comperative {
	method operator(--> '=') {}
}
class Op::Unequals does Op::Comperative {
	method operator(--> '<>') {}
}
class Op::Like does Op::Comperative {
	method operator(--> 'LIKE') {}
}
class Op::Unlike does Op::Comperative {
	method operator(--> 'NOT LIKE') {}
}

class Op::Comperative::Custom does Op::Comperative {
	has Str:D $.operator is required;
	method new(Str:D $operator, Expression $left, Expression $right) {
		self.bless(:$operator, :$left, :$right);
	}
}

class Op::Assign does Op::Binary {
	method precedence(--> Precedence::Assignment) {}
	method operator(--> '=') {}
}

role Op::Binary::Other[Precedence $precedence] does Op::Binary {
	method precedence() { $precedence }
	has Str:D $.operator is required;
	method new(Str $operator, Expression $left, Expression $right) {
		self.bless(:$operator, :$left, :$right);
	}
}

class Op::Binary::Additive does Op::Binary::Other[Precedence::Additive] {
}

class Op::Between does Op {
	method precedence(--> Precedence::Between) {}
	has Expression:D $.left is required;
	has Expression:D $.min is required;
	has Expression:D $.max is required;
	method new(Expression $left, Expression $min, Expression $max) {
		self.bless(:$left, :$min, :$max);
	}
}

role Op::List does Op {
	has Expression @.elements is required;
}

role Op::Logical[Str $operator, Precedence $precedence] does Op::List {
	method precedence(--> Precedence) { $precedence }
	has Expression:D $.left is required;
	method operator(--> Str) { $operator }
	method new(@elements) {
		self.bless(:@elements);
	}
}
class Op::And does Op::Logical['AND', Precedence::And] {}
class Op::Or does Op::Logical['OR', Precedence::Or] {}

role Op::ListCompare[Str $operator] does Op::List {
	method precedence(--> Precedence::In) {}
	has Expression:D $.left is required;
	method operator(--> Str) { $operator }
	method new(Expression $left, Expression @elements) {
		self.bless(:$left, :@elements);
	}
}
class Op::In does Op::ListCompare['IN'] {}
class Op::NotIn does Op::ListCompare['NOT IN'] {}

class Op::Cast does Op {
	method precedence(--> Precedence::Term) {}
	has Expression:D $.primary is required;
	has Str:D $.typename is required;
	method new() {}
}

class Values {
	has Row @.rows;
	method new(@rows) {
		self.bless(:@rows);
	}
}

role Source does Expression {
	method precedence(--> Precedence::Rowlike) {}
}

role Source::Singular does Source { }

class Table does Source::Singular {
	has Ident:D $.name is required;
	method new(Ident:D(Cool:D) $name) {
		self.bless(:$name);
	}
}

role Renamed {
	has Ident $.alias;
}

class Source::Renamed does Source does Renamed {
	has Source::Singular:D $.source is required;
	method new(Source::Singular $source, Ident:D(Cool:D) $alias) {
		self.bless(:$source, :$alias);
	}
}

role Column does Expression {
}

role Command is Source::Singular { }

role Sorter {}

class Column::Named does Column does Sorter {
	method precedence(--> Precedence::Term) {}
	has Ident:D $.ident is required;
	method new(Ident(Cool) $ident) {
		self.bless(:$ident);
	}
}

class Column::Renamed does Column does Sorter does Renamed {
	method precedence(--> Precedence::Comma) {}
	has Column:D $.source is required;
	multi method new(Column::Named(Cool) $column, Ident(Cool) $alias) {
		self.bless(:$column, :$alias);
	}
	multi method new(Column $column, Ident(Cool) $alias) {
		self.bless(:$column, :$alias);
	}
}

class Column::All does Column {
	method precedence(--> Precedence::Term) {}
}

class Column::Expression does Column does Sorter {
	has Expression:D $.expression handles<precedence> is required;
	method new(Expression $expression) {
		self.bless(:$expression);
	}
}

class Join does Source {
	enum Type <Inner Left Right Outer>;
	has Source:D $.left is required;
	has Source:D $.right is required;
	has Expression $.on;
	has Column @.using;
	has Type:D $.type is required;
	has Bool $.lateral;
	multi method new(Table(Cool) $left, Table(Cool) $right, Column::Named(Cool:D) :$using!, Type :$type = Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :using[$using], :$type, :$lateral);
	}
	multi method new(Table(Cool) $left, Table(Cool) $right, Column :@using!, Type :$type = Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :@using, :$type, :$lateral);
	}
	multi method new(Table(Cool) $left, Table(Cool) $right, Expression:D :$on!, Type :$type = Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :$on, :$type, :$lateral);
	}
}

role Sorter::Modifier does Sorter {
	has Column:D $.column is required;
	method modifier() { ... }
	method new(Column(Cool) $column) {
		self.bless(:$column);
	}
}

class Sorter::Asc does Sorter::Modifier {
	method modifier(--> 'ASC') {}
}
class Sorter::Desc does Sorter::Modifier {
	method modifier(--> 'DESC') {}
}

class Select does Command does Expression {
	has Column @.columns is required;
	has Source $.source is required;
	has Expression @.conditions;
	has Column @.group-by;
	has Expression @.having;
	has Sorter @.order-by;
	has Int $.limit;
	has Int $.offset;
}

class Insert does Command {
	has Table:D $.target is required;
	has Column @.fields;
	has Row @.rows;
	has Column @.returning;
}

class Update does Command {
	has Table:D $.target is required;
	has Pair @.set;
	has Expression @.conditions;
	has Column @.returning;
}

class Delete does Command {
	has Table:D $.target  is required;
	has Expression @.conditions;
	has Column @.returning;
}

my multi expand-expression(Expression $expression) {
	$expression;
}
my multi expand-expression(Any $value) {
	Value.new($value);
}
my multi expand-expression(Capture $literal) {
	my ($sql, @arguments) = |$literal;
	Literal.new($sql, @arguments);
}

my sub expand-row(@values) {
	my Expression @expanded = @values.map: { expand-expression($^value) };
	Row.new(@expanded);
}

my multi expand-condition(Column::Named $name, Expression:D $expression) {
	Op::Equals.new($name, $expression);
}
my multi expand-condition(Column::Named $name, Any:D $value where Str|Numeric|Date|DateTime) {
	Op::Equals.new($name, Value.new($value));
}
my multi expand-condition(Column::Named $name, Any:U) {
	Op::IsNull.new($name);
}
my multi expand-condition(Column::Named $name, Pair $ (:$key where $key eq 'isnull', :$value)) {
	Op::IsNull.new($name);
}
my multi expand-condition(Column::Named $name, Pair $ (:$key where $key eq 'isnotnull', :$value)) {
	Op::IsNotNull.new($name);
}
my multi expand-condition(Column::Named $name, Pair $ (:$key where $key eq '=', :$value)) {
	Op::Equals.new($name, expand-expression($value));
}
my multi expand-condition(Column::Named $name, Pair $ (:$key where $key eq '!=', :$value)) {
	Op::Unequals.new($name, expand-expression($value));
}
my multi expand-condition(Column::Named $name, Pair $ (:$key where $key eq 'like', :$value)) {
	Op::Like.new($name, expand-expression($value));
}
my multi expand-condition(Column::Named $name, Pair $ (:$key where $key eq 'not-like', :$value)) {
	Op::Unlike.new($name, expand-expression($value));
}
my multi expand-condition(Column::Named $name, Pair $ (:$key, :$value)) {
	Op::Comperative::Custom.new($key, $name, expand-expression($value));
}
my multi expand-condition(Column::Named $name, %hash) {
	%hash.sort(*.key).map: { expand-condition($name, $^pair) };
}
my multi expand-condition(Column::Named $name, @list) {
	@list.map: { expand-condition($name, $^elem) };
}
my multi expand-condition(Column::Named $name, Range $range) {
	Op::Between.new($name, Value.new($range.min), Value.new($range.max));
}

my multi expand-condition(Column::Named $name, Junction $junction) {
	use nqp;
	my $type = nqp::box_s(nqp::getattr($junction, Junction, '$!type'), Str);
	my @eigenstates = nqp::getattr($junction, Junction, '$!eigenstates').List;
	my Expression @values = @eigenstates.map(&expand-expression);
	given $type {
		when 'any' {
			all(@values) ~~ Value ?? Op::In.new($name, @values) !! Op::Or.new(@values.map: { Op::Equals.new($name, $^expression) });
		}
		when 'all' {
			Op::And.new(@values.map: { Op::Equals.new($name, $^expression) });
		}
		when 'none' {
			all(@values) ~~ Value ?? Op::NotIn.new($name, @values) !! Op::And.new(@values.map: { Op::Unequals.new($name, $^expression) });
		}
		when 'one' {
			my @comparisons = @values.map: { Op::Equals.new($name, $^value) };
			my $addition = @comparisons.reduce: { Op::Binary::Additive.new('+', $^left, $^right) };
			Op::Equals.new($addition, Value.new(1));
		}
	}
}

my multi expand-where(Expression $expression) {
	$expression;
}
my multi expand-where(@input) {
	@input.map(&expand-expression);
}
my multi expand-where(%hash) {
	my @expanded = flat %hash.kv.map: { $^key X=> expand-condition(Column::Named.new($^key), $^value) };
	@expanded.sort(*.key).map(*.value);
}
my multi expand-where(Any:U) {
	();
}

my multi expand-column(Column $column) {
	$column;
}
my multi expand-column(Str $ident) {
	Column::Named.new($ident);
}
my multi expand-column(Whatever) {
	Column::All.new;
}
my multi expand-column(Expression $expression) {
	Column::Expression.new($expression);
}
my multi expand-column(Any:U) {
	();
}

my multi expand-columns($column) {
	expand-column($column);
}
my multi expand-columns(@list) {
	@list.map(&expand-columns);
}

my multi expand-sorter(Sorter $sorter) {
	$sorter;
}
my multi expand-sorter(Str $ident) {
	Column::Named.new($ident);
}
my multi expand-sorter(Any:U) {
	();
}

my multi expand-sorters($sorter) {
	expand-sorter($sorter);
}
my multi expand-sorters(@list) {
	@list.map(&expand-sorter);
}

role Generator {
	method select() { ... }
	method insert() { ... }
	method update() { ... }
	method delete() { ... }
}

class Generator::Default does Generator {
	method select($pre-source, $columns = *, $conditions?, :$group-by, :$having, :$order-by, Int :$limit, Int :$offset) {
		my Source $source = $pre-source ~~ Source ?? $pre-source !! Table.new($pre-source);
		my @columns = expand-columns($columns);
		my @conditions = expand-where($conditions);
		my @group-by = $group-by ?? expand-columns($group-by) !! ();
		my @having = $having ?? expand-where($having) !! ();
		my @order-by = $order-by ?? expand-sorters($order-by) !! ();
		Select.new(:@columns, :$source, :@conditions, :@group-by, :@having, :@order-by, :$limit, :$offset);
	}

	multi method insert(Table(Cool) $target, %values, :$returning) {
		my @pairs = %values.pairs.sort: *.key;
		my @fields = @pairs».key.map: { Column::Named.new($^key) };
		my Expression @values = @pairs».value.map: { expand-expression($^value) };
		my @rows = Row.new(@values);
		my @returning = expand-columns($returning);
		Insert.new(:$target, :@fields, :@rows, :@returning);
	}
	multi method insert(Table(Cool) $target, @pre-fields, @pre-rows, :$returning) {
		my @fields = @pre-fields.map: { $^field ~~ Column ?? $^field !! Column::Named.new($^field) };
		my Row @rows = @pre-rows.map({ expand-row($^row) });
		my @returning = expand-columns($returning);
		Insert.new(:$target, :@fields, :@rows, :@returning);
	}

	method update(Table(Cool) $target, %set, $conditions?, :$returning) {
		my @set = %set.kv.map: { $^key => Op::Assign.new(Column::Named.new($key), expand-expression($^value)) };
		my @conditions = expand-where($conditions);
		my @returning = expand-columns($returning);
		Update.new(:$target, :@set, :@conditions, :@returning);
	}

	method delete(Table(Cool) $target, $conditions?, :$returning) {
		my @conditions = expand-where($conditions);
		my @returning = expand-columns($returning);
		Delete.new(:$target, :@conditions, :@returning);
	}
}

role Arguments {
	has @.values;
	method bind(Str $name, Any $value) { ... }
}

role Renderer {
	method render-command() { ... }
}

role Renderer::SQL { ... }

role Expression::Custom does Expression { # XXX
	method render-sql(Renderer::SQL $renderer) { ... }
}

role Renderer::SQL does Renderer {
	method arguments(--> Arguments) { ... }

	has Bool:D $.quoting = False;

	my sub join-elems(*@args --> Str) {
		@args.grep(*.chars).join(' ');
	}

	my sub parenthesize-maybe(Str $input, Bool $parenthese --> Str) {
		$parenthese ?? "($input)" !! $input;
	}

	method render-list(Arguments $arguments, @elements, Bool $parenthesized --> Str) {
		my @elems = @elements.map: { self.render-expression($arguments, $^element, Precedence::Comma) };
		parenthesize-maybe(@elems.join(', '), $parenthesized);
	}

	method quote-identifier(Str $identifier --> Str) {
		$!quoting ?? '"' ~ $identifier.subst('"', '""', :g) ~ '"' !! $identifier;
	}

	method render-identifier(Ident $ident --> Str) {
		$ident.parts.map({ self.quote-identifier($^id) }).join('.');
	}
	multi method render-expression(Arguments $arguments, Literal $literal, Precedence $ --> Str) {
		my @arguments = $literal.arguments;
		$literal.payload.subst(/'$' (<digit>+)/, { $arguments.bind(Str, @arguments[~$0 - 1]) }, :g);
	}
	multi method render-expression(Arguments $arguments, Value $value, Precedence $ --> Str) {
		$arguments.bind(Str, $value.value);
	}
	multi method render-expression(Arguments $arguments, Bind $bind, Precedence $ --> Str) {
		$arguments.bind($bind.column, $bind.value);
	}
	multi method render-expression(Arguments $arguments, Row $row, Precedence $ --> Str) {
		self.render-list($arguments, $row.elements, True);
	}
	multi method render-expression(Arguments $arguments, Function $func, Precedence $ --> Str) {
		$func.name ~ self.render-list($arguments, $func.arguments, True);
	}
	multi method render-expression(Arguments $arguments, Op::Unary::Postfix $op, Precedence $precedence --> Str) {
		my $value = self.render-expression($arguments, $op.value, Precedence::Postfix);
		parenthesize-maybe($value ~ ' ' ~ $op.postfix, $precedence > Precedence::Postfix);
	}
	multi method render-expression(Arguments $arguments, Op::Unary::Prefix $op, Precedence $precedence --> Str) {
		my $primary = self.render-expression($arguments, $op.primary, Precedence::Prefix);
		parenthesize-maybe($op.operator ~ ' ' ~ $primary, $precedence > $Precedence::Prefix);
	}
	multi method render-expression(Arguments $arguments, Op::Binary $op, Precedence $precedence --> Str) {
		my $left = self.render-expression($arguments, $op.left, $op.precedence);
		my $right = self.render-expression($arguments, $op.right, $op.precedence);
		parenthesize-maybe("$left $op.operator() $right", $precedence > $op.precedence);
	}
	multi method render-expression(Arguments $arguments, Op::ListCompare $in, Precedence $precedence --> Str) {
		my $left = self.render-expression($arguments, $in.left, $in.precedence);
		my $candidates = self.render-list($arguments, $in.elements, True);
		parenthesize-maybe("$left $in.operator() $candidates", $precedence > $in.precedence);
	}
	multi method render-expression(Arguments $arguments, Op::Between $between, Precedence $precedence --> Str) {
		my $left = self.render-expression($arguments, $between.left, Precedence::Between);
		my $min = self.render-expression($arguments, $between.min, Precedence::Between);
		my $max = self.render-expression($arguments, $between.max, Precedence::Between);
		parenthesize-maybe("$left BETWEEN $min AND $max", $precedence > Precedence::Between);
	}
	method render-sub-expression(Arguments $arguments, Expression $element, Precedence $precedence --> Str) {
		self.render-expression($arguments, $element, $precedence);
	}
	multi method render-expression(Arguments $arguments, Op::Logical $logical, Precedence $precedence --> Str) {
		my @elems = $logical.elements.map: { self.render-sub-expression($arguments, $^element, $logical.precedence) };
		@elems.join(" $logical.operator() ");
	}
	multi method render-expression(Arguments $arguments, Op::Cast $cast, Precedence $ --> Str) {
		'CAST(' ~ self.render-expression($arguments, $cast.primary, Precedence::Comma) ~ " AS $cast.type())";
	}
	multi method render-expression(Arguments $arguments, Command $select, Precedence $precedence --> Str) {
		'(' ~ self.render-command-expression($arguments, $select) ~ ')';
	}
	multi method render-expression(Arguments $arguments, Column::Named $column, Precedence $precedence --> Str) {
		self.render-identifier($column.ident);
	}

	multi method render-column(Arguments $arguments, Column::Named $column --> Str) {
		self.render-identifier($column.ident);
	}
	multi method render-column(Arguments $arguments, Column::Renamed $column --> Str) {
		my $source = self.render-column($column.source);
		my $alias = self.render-identifier($column.alias);
		"$source AS $alias";
	}
	multi method render-column(Arguments $arguments, Column::All --> '*') {}
	multi method render-column(Arguments $arguments, Column::Expression $column --> Str) {
		self.render-expression($arguments, $column.expression, Precedence::Comma);
	}

	method render-columns(Arguments $arguments, Column @columns, Bool $parenthesized = False --> Str) {
		my @elems = @columns.map: { self.render-column($arguments, $^column) };
		parenthesize-maybe(@elems.join(', '), $parenthesized);
	}

	multi method render-sorter(Arguments $arguments, Column $column --> Str) {
		self.render-column($arguments, $column);
	}
	multi method render-sorter(Arguments $arguments, Sorter::Modifier $column --> Str) {
		self.render-column($arguments, $column.column) ~ ' ' ~ $column.modifier;
	}

	method render-table(Table $source --> Str) {
		self.render-identifier($source.name);
	}

	multi method render-source(Arguments $, Table $table --> Str) {
		self.render-table($table);
	}
	multi method render-source(Arguments $arguments, Command $command --> Str) {
		'(' ~ self.render-command-expression($arguments, $command) ~ ')';
	}
	multi method render-source(Arguments $arguments, Source::Renamed $renamed--> Str) {
		my $source = self.render-source($arguments, $renamed.source);
		my $alias = self.render-identifier($renamed.alias);
		"$source $alias";
	}
	multi method render-source(Arguments $arguments, Join $join --> Str) {
		my $left = self.render-source($arguments, $join.left);
		my $right = self.render-source($arguments, $join.right);
		my $type = $join.type.uc;
		my $lateral = $join.lateral ?? 'LATERAL' !! '';
		my $on = $join.on    ?? 'ON ' ~ self.render-expression($arguments, $join.on, Precedence::Comma)
		    !!   $join.using ?? 'USING ' ~ self.render-columns($arguments, $join.using, True) 
		    !!                  '';
		join-elems($left, $type, 'JOIN', $lateral, $right, $on);
	}

	method render-values(Arguments $arguments, Row @rows --> Str) {
		my @sets = @rows.map: { self.render-expression($arguments, $^row, Precedence::Comma) };
		'VALUES ' ~ @sets.join(', ');
	}

	multi method render-where(Arguments $arguments, Expression @conditions --> Str) {
		my $expression = @conditions > 1 ?? Op::And.new(@conditions) !! @conditions[0];
		'WHERE ' ~ self.render-expression($arguments, $expression, Precedence::And);
	}
	multi method render-where(Arguments $arguments, Expression @conditions where @conditions == 0 --> Str) {
		'';
	}

	multi method render-order-by(Arguments $arguments, Sorter:D @sorters --> Str) {
		my @elems = @sorters.map: { self.render-sorter($arguments, $^column) };
		'ORDER BY ' ~ @elems.join(', ');
	}
	multi method render-order-by(Arguments $arguments, Sorter:D @sorters where @sorters == 0 --> Str) {
		''
	}

	multi method render-group-by(Arguments $arguments, Column:D @columns, Expression @having --> Str) {
		my $group-by = self.render-columns($arguments, @columns);
		my $having = self.render-where($arguments, @having).subst(/^WHERE/, 'HAVING');
		join-elems('GROUP BY', $group-by, $having);
	}
	multi method render-group-by(Arguments $arguments, Column:D @columns where @columns == 0, Expression @having --> Str) {
		''
	}

	multi method render-returning(Arguments $arguments, Column:D @returning --> Str) {
		'RETURNING ' ~ self.render-columns($arguments, @returning);
	}
	multi method render-returning(Arguments $arguments, Column:D @returning where @returning == 0 --> Str) {
		'';
	}

	multi method render-limit(Int:D $limit, Int $offset) {
		$offset.defined ?? "LIMIT $limit OFFSET $offset" !! "LIMIT $limit";
	}
	multi method render-limit(Int:U $limit, Int $offset) {
		''
	}

	multi method render-command-expression(Arguments $arguments, Select $select --> Str) {
		my $columns    = self.render-columns($arguments, $select.columns);
		my $source     = self.render-source($arguments, $select.source);
		my $conditions = self.render-where($arguments, $select.conditions);
		my $group-by   = self.render-group-by($arguments, $select.group-by, $select.having);
		my $order-by   = self.render-order-by($arguments, $select.order-by);
		my $limit      = self.render-limit($select.limit, $select.offset);
		join-elems('SELECT', $columns, 'FROM', $source, $conditions, $group-by, $order-by, $limit);
	}

	multi method render-command-expression(Arguments $arguments, Update $update --> Str) {
		my $target      = self.render-table($update.target);
		my @expressions = $update.set.sort(*.key).map(*.value);
		my $set         = self.render-list($arguments, @expressions, False);
		my $conditions  = self.render-where($arguments, $update.conditions);
		my $returning   = self.render-returning($arguments, $update.returning);
		join-elems('UPDATE', $target, 'SET', $set, $conditions, $returning);
	}

	multi method render-command-expression(Arguments $arguments, Insert $insert --> Str) {
		my $target = self.render-table($insert.target);
		my $fields = self.render-columns($arguments, $insert.fields, True);
		my $values = self.render-values($arguments, $insert.rows);
		my $returning = self.render-returning($arguments, $insert.returning);
		join-elems('INSERT INTO', $target, $fields, $values, $returning);
	}

	multi method render-command-expression(Arguments $arguments, Delete $delete --> Str) {
		my $target = self.render-table($delete.target);
		my $conditions = self.render-where($arguments, $delete.conditions);
		my $returning = self.render-returning($arguments, $delete.returning);
		join-elems('DELETE FROM', $target, $conditions, $returning);
	}

	method render-command(Command $command) {
		my $arguments = self.arguments.new;
		my $sql = self.render-command-expression($arguments, $command);
		$sql, $arguments.values;
	}
}

class Renderer::DBIish does Renderer::SQL {
	class Arguments does Arguments {
		method bind(Str $name, Any $value) {
			@!values.push($value);
			'?';
		}
	}
	method arguments { Arguments }
}

class Renderer::Postgres does Renderer::SQL {
	class Arguments does Arguments {
		has Int $.counter = 0;
		method bind(Str $name, Any $value) {
			@!values.push($value);
			'$' ~ ++$!counter;
		}
	}
	method arguments { Arguments }

	multi method render-expression(Arguments $arguments, Op::Cast $cast --> Str) {
		self.render-expression($arguments, $cast.primary, Precedence::Term) ~ '::' ~ $cast.type;
	}
}

class Renderer::Noop does Renderer {
	method render-expression(Arguments $arguments, Expression $expression) { $expression }
	method render-command(Command $command) { $command }
}

has Generator:D $.generator = Generator::Default.new;
has Renderer:D $.renderer = Renderer::Postgres.new;

method select(|arguments) {
	my $select = $!generator.select(|arguments);
	$!renderer.render-command($select);
}

method insert(|arguments) {
	my $insert = $!generator.insert(|arguments);
	$!renderer.render-command($insert);
}

method update(|arguments) {
	my $update = $!generator.update(|arguments);
	$!renderer.render-command($update);
}

method delete(|arguments) {
	my $delete = $!generator.delete(|arguments);
	$!renderer.render-command($delete);
}

=begin pod

=head1 NAME

SQL::Abstract - blah blah blah

=head1 SYNOPSIS

=begin code :lang<raku>

use SQL::Abstract;

=end code

=head1 DESCRIPTION

SQL::Abstract is ...

=head1 AUTHOR

Leon Timmermans <fawaka@gmail.com>

=head1 COPYRIGHT AND LICENSE

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
