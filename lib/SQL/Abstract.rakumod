unit class SQL::Abstract;

use fatal;

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
	multi method new(Str $name, Expression $arguments) {
		self.bless(:$name, :arguments[$arguments]);
	}
	multi method new(Str $name, Expression @arguments) {
		self.bless(:$name, :@arguments);
	}
}

role Op does Expression {
}

role Op::Negatable does Op {
	method negation(--> Op) { ... }
}
role Op::Unary does Op {
	has Expression:D $.value is required;
	method new(Expression $value) {
		self.bless(:$value);
	}
}

role Op::Unary::Postfix[Str $operator] does Op::Unary {
	method precedence(--> Precedence::Postfix) {}
	method postfix(--> Str) { $operator }
	method new(Expression $value) {
		self.bless(:$value);
	}
}

class Op::IsNotNull { ... }
class Op::IsNull does Op::Unary::Postfix['IS NULL'] does Op::Negatable {
	method negation() { Op::IsNotNull }
}
class Op::IsNotNull does Op::Unary::Postfix['IS NOT NULL'] does Op::Negatable {
	method negation() { Op::IsNull }
}

class Op::Unary::Prefix does Op::Unary {
	method precedence(--> Precedence::Prefix) {}
	has Str:D $.operator is required;
	method new(Str $operator, Expression $value) {
		self.bless(:$operator, :$value);
	}
}

role Op::Binary does Op {
	method operator() { ... }
	has Expression:D $.left is required;
	has Expression:D $.right is required;
	method new(Expression:D $left, Expression:D $right) {
		self.bless(:$left, :$right);
	}
}

role Op::Comperative does Op::Binary {
	method precedence(--> Precedence::Comparative) {}
}

class Op::Unequals { ... }
class Op::Equals does Op::Comperative does Op::Negatable {
	method operator(--> '=') {}
	method negation(--> Op::Comperative) { Op::Unequals }
}
class Op::Unequals does Op::Comperative does Op::Negatable {
	method operator(--> '<>') {}
	method negation(--> Op::Comperative) { Op::Equals }
}

class Op::GreaterOrEqual { ... }
class Op::LessThan does Op::Comperative does Op::Negatable {
	method operator(--> '<') {}
	method negation(--> Op::Comperative) { Op::GreaterOrEqual }
}
class Op::GreaterThan { ... }
class Op::LessOrEqual does Op::Comperative does Op::Negatable {
	method operator(--> '<=') {}
	method negation(--> Op::Comperative) { Op::GreaterThan }
}
class Op::GreaterThan does Op::Comperative does Op::Negatable {
	method operator(--> '>') {}
	method negation(--> Op::Comperative) { Op::LessOrEqual }
}
class Op::GreaterOrEqual does Op::Comperative does Op::Negatable {
	method operator(--> '>=') {}
	method negation(--> Op::Comperative) { Op::LessThan }
}

class Op::Unlike { ... }
class Op::Like does Op::Comperative does Op::Negatable {
	method operator(--> 'LIKE') {}
	method negation(--> Op::Unlike) { }
}
class Op::Unlike does Op::Comperative does Op::Negatable {
	method operator(--> 'NOT LIKE') {}
	method negation(--> Op::Like) { }
}

my %binary-map =
	'='        => Op::Equals,
	'!='       => Op::Unequals,
	'<>'       => Op::Unequals,
	'<'        => Op::LessThan,
	'<='       => Op::LessOrEqual,
	'>'        => Op::GreaterThan,
	'>='       => Op::GreaterOrEqual,
	'like'     => Op::Like,
	'not-like' => Op::Unlike;
my $binary-like = any(%binary-map.keys);

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

role Op::BetweenLike does Op does Op::Negatable {
	method precedence(--> Precedence::Between) {}
	method negated(--> Bool) { ... }
	has Expression:D $.left is required;
	has Expression:D $.min is required;
	has Expression:D $.max is required;
	method new(Expression $left, Expression $min, Expression $max) {
		self.bless(:$left, :$min, :$max);
	}
}

class Op::NotBetween { ... }
class Op::Between does Op::BetweenLike {
	method negated(--> False) { }
	method negation() { Op::NotBetween }
}
class Op::NotBetween does Op::BetweenLike {
	method negated(--> True) { }
	method negation() { Op::Between }
}

role Op::List does Op {
	has Expression @.elements is required;
}

role Op::Logical[Str $operator, Precedence $precedence] does Op::List {
	method precedence(--> Precedence) { $precedence }
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

my proto expand-source(Any $input) { * }
role Source does Expression {
	method precedence(--> Precedence::Rowlike) {}
	method COERCE(Any $input) {
		expand-source($input);
	}
}

role Source::Singular does Source { }

class Table does Source::Singular {
	has Ident:D $.name is required;
	method new(Ident(Cool:D) $name) {
		self.bless(:$name);
	}
}

role Renamed {
	has Ident:D $.alias is required;
}

class Source::Renamed does Source does Renamed {
	has Source::Singular:D $.source is required;
	method new(Source::Singular $source, Ident(Cool:D) $alias) {
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
	multi method new(Column::Named(Cool) $source, Ident(Cool) $alias) {
		self.bless(:$source, :$alias);
	}
	multi method new(Column $source, Ident(Cool) $alias) {
		self.bless(:$source, :$alias);
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

my proto expand-columns(Any $columns) { * }
class Columns {
	has Column:D @.columns is required;
	method new(@columns) {
		self.bless(:@columns);
	}
	method COERCE(Any $columns) {
		expand-columns($columns);
	}
}

class Join does Source {
	enum Type <Inner Left Right Outer>;
	has Source:D $.left is required;
	has Source:D $.right is required;
	has Expression $.on;
	has Columns $.using;
	has Type:D $.type is required;
	has Bool $.lateral;
	multi method new(Table(Cool) $left, Table(Cool) $right, Columns(Any) :$using!, Type :$type = Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :$using, :$type, :$lateral);
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

my proto expand-conditions(Any $condition) { * }
class Conditions {
	has Expression:D @.expressions is required;
	method new(Expression @expressions) {
		self.bless(:@expressions);
	}
	method COERCE(Any $condition) {
		expand-conditions($condition);
	}
}

my proto expand-sorters(Any $condition) { * }
class Sorters {
	has Sorter:D @.elems is required;
	method new(@elems) {
		self.bless(:@elems);
	}
	method COERCE(Any $elems) {
		expand-sorters($elems);
	}
}

class Rows {
}

class Set {
	has Pair @.pairs;
}

class Select does Command does Expression {
	has Columns:D $.columns is required;
	has Source $.source is required;
	has Conditions $.conditions is required;
	has Columns $.group-by;
	has Conditions $.having is required;
	has Sorters $.order-by;
	has Int $.limit;
	has Int $.offset;
}

class Insert does Command {
	has Table:D $.target is required;
	has Columns $.fields;
	has Row @.rows;
	has Columns $.returning;
}

class Update does Command {
	has Table:D $.target is required;
	has Expression @.set;
	has Conditions $.conditions;
	has Columns $.returning;
}

class Delete does Command {
	has Table:D $.target  is required;
	has Conditions $.conditions;
	has Columns $.returning;
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

subset Simple of Any where Str|Numeric|Bool|Date|DateTime;

role Partial {
	method resolve(Column::Named:D $name) { ... }
	method resolve-negated(Column::Named:D $name) {
		my $positive = self.resolve($name);
		Op::Unary::Prefix.new('NOT', $positive);
	}
}

class Partial::Unary does Partial {
	has Op::Unary:U $.class is required;
	method new(Op::Unary:U $class) {
		self.bless(:$class);
	}
	method resolve(Column::Named:D $name) {
		$!class.new($name);
	}
	method resolve-negated(Column::Named:D $name) {
		$!class ~~ Op::Negatable ?? $!class.negation.new($name) !! nextsame;
	}
}

class Partial::Binary does Partial {
	has Op::Binary:U $.class is required;
	has Expression:D $.expression is required;
	method new(Op::Binary:U $class, Expression:D $expression) {
		self.bless(:$class, :$expression);
	}
	method resolve(Column::Named:D $name) {
		$!class.new($name, $!expression);
	}
	method resolve-negated(Column::Named:D $name) {
		$!class ~~ Op::Negatable ?? $!class.negation.new($name, $!expression) !! nextsame;
	}
}

class Partial::ListCompare does Partial {
	has Op::ListCompare:U $.class is required;
	has Value @.values is required;
	method new(Op::ListCompare:U $class, @values) {
		self.bless(:$class, :@values);
	}
	method resolve(Column::Named:D $name) {
		$!class.new($name, @!values);
	}
	method resolve-negated(Column::Named:D $name) {
		$!class ~~ Op::Negatable ?? $!class.negation.new($name, @!values) !! nextsame;
	}
}

class Partial::Between does Partial {
	has Op::BetweenLike:U $.class is required;
	has Expression:D $.min is required;
	has Expression:D $.max is required;
	method new(Op::BetweenLike:U $class, Expression:D $min, Expression:D $max) {
		self.bless(:$class, :$min, :$max);
	}
	method resolve(Column::Named:D $name) {
		$!class.new($name, $!min, $!max);
	}
	method resolve-negated(Column::Named:D $name) {
		$!class.negation.new($name, $!min, $!max);
	}
}

class Partial::Custom does Partial {
	has Code:D $.callback is required;
	method new(Code:D $callback) {
		self.bless(:$callback);
	}
	method resolve(Column::Named:D $name) {
		$!callback($name);
	}
}

my multi expand-partial(Expression:D $expression) {
	Partial::Binary.new(Op::Equals, $expression);
}
my multi expand-partial(Simple:D $value) {
	Partial::Binary.new(Op::Equals, Value.new($value));
}
my multi expand-partial(Any:U) {
	Partial::Unary.new(Op::IsNull);
}
my multi expand-partial(Pair $ (:$key where $key eq 'isnull', :$value)) {
	Partial::Unary.new(Op::IsNull);
}
my multi expand-partial(Pair $ (:$key where $key eq 'isnotnull', :$value)) {
	Partial::Unary.new(Op::IsNotNull);
}
my multi expand-partial(Pair $ (:$key where $key eq $binary-like, :$value)) {
	Partial::Binary.new(%binary-map{$key}, expand-expression($value));
}
my multi expand-partial(Pair $ (:$key, :$value)) {
	my $expanded = expand-expression($value);
	Partial::Custom.new: -> $name {
		Op::Comperative::Custom.new($key, $name, $expanded);
	}
}
my multi expand-partial(%hash) {
	%hash.sort(*.key).map: { expand-partial($^pair) };
}
my multi expand-partial(Range $range) {
	my $min = Value.new($range.min);
	my $max = Value.new($range.max);
	Partial::Between.new(Op::Between, $min, $max);
}

subset PartialEquals of Partial::Binary where .class ~~ Op::Equals;

my multi expand-junction('any', @partials) {
	Partial::Custom.new: -> $name { Op::Or.new(@partials.map(*.resolve($name))) };
}
my multi expand-junction('any', @partials where all(@partials) ~~ PartialEquals) {
	Partial::ListCompare.new(Op::In, @partials».expression)
}
my multi expand-junction('all', @partials) {
	Partial::Custom.new: -> $name { Op::And.new(@partials.map(*.resolve($name))) }
}
my multi expand-junction('none', @partials) {
	Partial::Custom.new: -> $name { Op::And.new(@partials.map(*.resolve-negated($name))) };
}
my multi expand-junction('none', @partials where all(@partials) ~~ PartialEquals) {
	Partial::ListCompare.new(Op::NotIn, @partials».expression);
}
my multi expand-junction('one', @partials) {
	Partial::Custom.new: -> $name {
		my @comparisons = @partials.map(*.resolve);
		my $addition = @comparisons.reduce: { Op::Binary::Additive.new('+', $^left, $^right) };
		Op::Equals.new($addition, Value.new(1));
	}
}

my multi expand-partial(Junction $junction) {
	use nqp;
	my $type = nqp::box_s(nqp::getattr($junction, Junction, '$!type'), Str);
	my @eigenstates = nqp::getattr($junction, Junction, '$!eigenstates').List;
	expand-junction($type, @eigenstates.map(&expand-partial));
}

my sub expand-condition(Column::Named:D $name, Mu $value) {
	my @partial = flat expand-partial($value);
	@partial.map: { $^elem.resolve($name) };
}

my multi expand-conditions(Expression $expression) {
	Conditions.new($expression);
}
my multi expand-conditions(@input) {
	my Expression @expressions = @input.map(&expand-condition);
	Conditions.new(@expressions);
}
my multi expand-conditions(%hash) {
	my @expanded = flat %hash.kv.map: { $^key X=> expand-condition(Column::Named.new($^key), $^value) };
	my Expression @expressions = @expanded.sort(*.key).map(*.value);
	Conditions.new(@expressions);
}
my multi expand-conditions(Any:U) {
	Conditions.new; # XXX
}

my multi expand-column(Column $column) {
	$column;
}
my multi expand-column(Str $ident) {
	Column::Named.new($ident);
}
my multi expand-column(Pair $ (:$key, :$value)) {
	Column::Renamed.new(expand-column($key), $value);
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
	Columns.new(Array[Column].new(expand-column($column)));
}
my multi expand-columns(@list) {
	Columns.new(@list.map(&expand-column).list);
}
my multi expand-columns(%columns) {
	Columns.new(%columns.sort(*.key).map(&expand-column));
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

my multi expand-sorters(Any:D $sorter) {
	Sorters.new(expand-sorter($sorter).list);
}
my multi expand-sorters(Any:U $sorter) {
	Sorters;
}
my multi expand-sorters(@list) {
	Sorter.new(@list.map(&expand-sorter));
}

my multi expand-source(Source $source) {
	$source;
}
my multi expand-source(Pair $ (:$key, :$value)) {
	my $source = expand-source($key);
	Source::Renamed.new($source, $value);
}
my multi expand-source(Cool $pre-source) {
	Table.new($pre-source);
}

role Generator {
	method select() { ... }
	method insert() { ... }
	method update() { ... }
	method delete() { ... }
}

class Generator::Default does Generator {
	method select(Source(Any:D) $source, Columns(Any:D) $columns = *, Conditions(Any) $conditions?, Columns(Any) :$group-by, Conditions(Any) :$having?, Sorters(Any) :$order-by, Int :$limit, Int :$offset) {
		Select.new(:$columns, :$source, :$conditions, :$group-by, :$having, :$order-by, :$limit, :$offset);
	}

	multi method insert(Table(Cool) $target, %values, Columns(Any) :$returning) {
		my @pairs = %values.pairs.sort: *.key;
		my $fields = Columns.COERCE(@pairs».key);
		my @rows = expand-row(@pairs».value);
		Insert.new(:$target, :$fields, :@rows, :$returning);
	}
	multi method insert(Table(Cool) $target, Columns(Any) $fields, @pre-rows, Columns(Any) :$returning) {
		my Row @rows = @pre-rows.map({ expand-row($^row) });
		Insert.new(:$target, :$fields, :@rows, :$returning);
	}

	method update(Table(Cool) $target, %set, Conditions(Any) $conditions?, Columns(Any) :$returning) {
		my @pairs = %set.pairs.sort(*.key);
		my @set = @pairs.map: -> (:$key, :$value) { Op::Assign.new(Column::Named.new($key), expand-expression($value)) };
		Update.new(:$target, :@set, :$conditions, :$returning);
	}

	method delete(Table(Cool) $target, Conditions(Any) $conditions?, Columns(Any) :$returning) {
		Delete.new(:$target, :$conditions, :$returning);
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
		my $primary = self.render-expression($arguments, $op.value, Precedence::Prefix);
		parenthesize-maybe($op.operator ~ ' ' ~ $primary, $precedence > Precedence::Prefix);
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
	multi method render-expression(Arguments $arguments, Op::BetweenLike $between, Precedence $precedence --> Str) {
		my $left = self.render-expression($arguments, $between.left, Precedence::Between);
		my $min = self.render-expression($arguments, $between.min, Precedence::Between);
		my $max = self.render-expression($arguments, $between.max, Precedence::Between);
		my $not = $between.negated ?? 'NOT' !! '';
		my $base = join-elems($left, $not, 'BETWEEN', $min, 'AND', $max);
		parenthesize-maybe($base, $precedence > Precedence::Between);
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
		my $source = self.render-column($arguments, $column.source);
		my $alias = self.render-identifier($column.alias);
		"$source AS $alias";
	}
	multi method render-column(Arguments $arguments, Column::All --> '*') {}
	multi method render-column(Arguments $arguments, Column::Expression $column --> Str) {
		self.render-expression($arguments, $column.expression, Precedence::Comma);
	}

	method render-columns(Arguments $arguments, Columns $columns, Bool $parenthesized = False --> Str) {
		my @elems = $columns.columns.map: { self.render-column($arguments, $^column) };
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

	multi method render-conditions(Arguments $arguments, Conditions:D $conditions, Str $type = 'WHERE' --> Str) {
		my @conditions = $conditions.expressions;
		my $expression = @conditions > 1 ?? Op::And.new(@conditions) !! @conditions[0];
		"$type " ~ self.render-expression($arguments, $expression, Precedence::And);
	}
	multi method render-conditions(Arguments $arguments, Conditions:D $conditions where $conditions.expressions == 0, Str $type = Str --> Str) {
		'';
	}
	multi method render-conditions(Arguments $arguments, Conditions:U $conditions, Str $type = Str --> Str) {
		'';
	}

	multi method render-order-by(Arguments $arguments, Sorters:D $sorters --> Str) {
		my @elems = $sorters.elems.map: { self.render-sorter($arguments, $^column) };
		'ORDER BY ' ~ @elems.join(', ');
	}
	multi method render-order-by(Arguments $arguments, Sorters:U $sorters --> Str) {
		''
	}

	multi method render-group-by(Arguments $arguments, Columns:D $columns, Conditions $conditions --> Str) {
		my $group-by = self.render-columns($arguments, $columns);
		my $having = self.render-conditions($arguments, $conditions, 'HAVING');
		join-elems('GROUP BY', $group-by, $having);
	}
	multi method render-group-by(Arguments $arguments, Columns:U $columns, Conditions $having --> Str) {
		''
	}

	multi method render-returning(Arguments $arguments, Columns:D $returning --> Str) {
		'RETURNING ' ~ self.render-columns($arguments, $returning);
	}
	multi method render-returning(Arguments $arguments, Columns:U $returning --> Str) {
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
		my $conditions = self.render-conditions($arguments, $select.conditions);
		my $group-by   = self.render-group-by($arguments, $select.group-by, $select.having);
		my $order-by   = self.render-order-by($arguments, $select.order-by);
		my $limit      = self.render-limit($select.limit, $select.offset);
		join-elems('SELECT', $columns, 'FROM', $source, $conditions, $group-by, $order-by, $limit);
	}

	multi method render-command-expression(Arguments $arguments, Update $update --> Str) {
		my $target      = self.render-table($update.target);
		my @expressions = $update.set;
		my $set         = self.render-list($arguments, @expressions, False);
		my $conditions  = self.render-conditions($arguments, $update.conditions);
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
		my $conditions = self.render-conditions($arguments, $delete.conditions);
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
