unit class SQL::Abstract;

use fatal;

enum Precedence <Rowlike Comma And Or Assignment Between In Prefix Comparative Additive Multiplicative Postfix Term>;

multi prefix:<//>(\input) {
	return input.defined;
}

class Op::Unary::Prefix { ... }
role Expression {
	method precedence(--> Precedence) { ... }
	method negate(--> Op) {
		Op::Unary::Prefix.new('NOT', self);
	}
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
		self.bless(:$column, :value(expand-expression($value)));
	}
}

class Row does Expression {
	method precedence(--> Precedence::Rowlike) {}
	has Expression @.elements;
	method new(@elements) {
		self.bless(:@elements);
	}
	method COERCE(@values) {
		Row.new(@values.map: { expand-expression($^value) });
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
class Op::IsNull does Op::Unary::Postfix['IS NULL'] {
	method negate() { Op::IsNotNull.new($!value) }
}
class Op::IsNotNull does Op::Unary::Postfix['IS NOT NULL'] {
	method negate() { Op::IsNull.value($!value) }
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
class Op::Equals does Op::Comperative {
	method operator(--> '=') {}
	method negate(--> Op::Comperative) {
		Op::Unequals.new($!left, $!right);
	}
}
class Op::Unequals does Op::Comperative {
	method operator(--> '<>') {}
	method negate(--> Op::Comperative) {
		Op::Equals.new($!left, $!right);
	}
}

class Op::GreaterOrEqual { ... }
class Op::LessThan does Op::Comperative {
	method operator(--> '<') {}
	method negate(--> Op::Comperative) {
		Op::GreaterOrEqual.new($!left, $!right);
	}
}
class Op::GreaterThan { ... }
class Op::LessOrEqual does Op::Comperative {
	method operator(--> '<=') {}
	method negate(--> Op::Comperative) {
		Op::GreaterThan.new($!left, $!right);
	}
}
class Op::GreaterThan does Op::Comperative {
	method operator(--> '>') {}
	method negate(--> Op::Comperative) {
		Op::LessOrEqual.new($!left, $!right);
	}
}
class Op::GreaterOrEqual does Op::Comperative {
	method operator(--> '>=') {}
	method negate(--> Op::Comperative) {
		Op::LessThan.new($!left, $!right);
	}
}

class Op::Unlike { ... }
class Op::Like does Op::Comperative {
	method operator(--> 'LIKE') {}
	method negate(--> Op::Comperative) {
		Op::Unlike.new($!left, $!right);
	}
}
class Op::Unlike does Op::Comperative {
	method operator(--> 'NOT LIKE') {}
	method negate(--> Op::Comperative) {
		Op::Like.new($!left, $!right);
	}
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

role Op::BetweenLike does Op {
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
	method negate() {
		Op::NotBetween.new($!left, $!min, $!max);
	}
}
class Op::NotBetween does Op::BetweenLike {
	method negated(--> True) { }
	method negate() {
		Op::Between.new($!left, $!min, $!max);
	}
}

role Op::List does Op {
	has Expression @.elements is required;
}

role Op::Logical[Str $operator, Precedence $precedence] does Op::List {
	method precedence(--> Precedence) { $precedence }
	method operator(--> Str) { $operator }
	method empty(--> Expression) { ... }
	method new(@elements) {
		self.bless(:@elements);
	}
}
class Op::And does Op::Logical['AND', Precedence::And] {
	method empty() {
		Op::Equals.new(Value.new(1), Value.new(1));
	}
}
class Op::Or does Op::Logical['OR', Precedence::Or] {
	method empty() {
		Op::Equals.new(Value.new(1), Value.new(0));
	}
}

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

class Rows {
	has Row @.elems;
	method COERCE(@input) {
		my @elems = @input.map: { Row.COERCE($^row) };
		self.new(:@elems);
	}
}

my proto expand-source(Any) { * }
role Source does Expression {
	method precedence(--> Precedence::Rowlike) {}
	method COERCE(Any $input) {
		expand-source($input);
	}
}
my multi expand-source(Source $source) {
	$source;
}

role Source::Singular does Source {}

class Table does Source::Singular {
	has Ident:D $.name is required;
	method new(Ident(Cool:D) $name) {
		self.bless(:$name);
	}
}
my multi expand-source(Cool $pre-source) {
	Table.new($pre-source);
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
my multi expand-source(Pair $ (:$key, :$value)) {
	my $source = expand-source($key);
	Source::Renamed.new($source, $value);
}

role Column does Expression {
}

role Command is Source::Singular {}

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
multi expand-expression(Whatever) {
	Column::All.new;
}

class Column::Expression does Column does Sorter {
	has Expression:D $.expression handles<precedence> is required;
	method new(Expression $expression) {
		self.bless(:$expression);
	}
}

my multi expand-column(Column $column) {
	$column;
}
my multi expand-column(Str $ident) {
	Column::Named.new($ident);
}
my multi expand-column(List $ident) {
	Column::Named.new($ident);
}
my multi expand-column(Pair $ (:$key, :$value)) {
	Column::Renamed.new(expand-column($key), $value);
}
subset HashPair of Hash where .elems == 1;
my multi expand-column(HashPair $pair) {
	my ($function, $argument) = $pair.kv;
	my $call = Function.new($function, expand-column($argument));
	Column::Expression.new($call);
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


class Columns {
	has Column:D @.elems is required;
	method new(@elems) {
		self.bless(:@elems);
	}
	multi method COERCE($column) {
		Columns.new(Array[Column].new(expand-column($column)));
	}
	multi method COERCE(@list) {
		Columns.new(@list.map(&expand-column).list);
	}
	multi method COERCE(%columns) {
		Columns.new(%columns.sort(*.key).map(&expand-column));
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

my sub join-conditions(Op::Logical:U $class, @expressions) {
	given @expressions.elems {
		when 1  { @expressions[0] }
		when 0  { $class.empty; }
		default { $class.new(@expressions) }
	}
}

role Conditional {
	has Expression:D $.expression is required;
	multi method new(Expression @expressions) {
		my $expression = join-conditions(Op::And, @expressions);
		self.bless(:$expression);
	}
	multi method new(Expression $expression) {
		self.bless(:$expression);
	}
}

class Conditions does Conditional {
	role Partial {
		method resolve(Column::Named:D $name) { ... }
	}
	class Partial::Simple does Partial {
		has Op:U $.class is required;
		has Capture:D $.arguments is required;

		method new(Op:U $class, |capture) {
			self.bless(:$class, :arguments(capture));
		}
		method resolve(Column::Named:D $name) {
			$!class.new($name, |$!arguments);
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

	subset Simple of Any where Str|Numeric|Bool|Date|DateTime;

	my multi expand-partial(Expression:D $expression) {
		Partial::Simple.new(Op::Equals, $expression);
	}
	my multi expand-partial(Simple:D $value) {
		Partial::Simple.new(Op::Equals, Value.new($value));
	}
	my multi expand-partial(Any:U) {
		Partial::Simple.new(Op::IsNull);
	}
	my multi expand-partial(Pair $ (:$key where $key eq 'isnull', :$value)) {
		Partial::Simple.new(Op::IsNull);
	}
	my multi expand-partial(Pair $ (:$key where $key eq 'isnotnull', :$value)) {
		Partial::Simple.new(Op::IsNotNull);
	}
	my $binary-like = any(%binary-map.keys);
	my multi expand-partial(Pair $ (:$key where $key eq $binary-like, :$value)) {
		Partial::Simple.new(%binary-map{$key}, expand-expression($value));
	}
	my multi expand-partial(%hash) {
		%hash.sort(*.key).map: { expand-partial($^pair) };
	}
	my multi expand-partial(Range $range) {
		my $min = Value.new($range.min);
		my $max = Value.new($range.max);
		Partial::Simple.new(Op::Between, $min, $max);
	}
	my multi expand-partial(Capture $capture) {
		Partial::Simple.new(Op::Equals, expand-expression($capture));
	}

	subset PartialEquals of Partial::Simple where .class ~~ Op::Equals && .arguments.elems == 1;

	my multi expand-junction('any', @partials) {
		Partial::Custom.new: -> $name { join-conditions(Op::Or, @partials.map(*.resolve($name))) };
	}
	my multi expand-junction('any', @partials where all(@partials) ~~ PartialEquals) {
		my Expression @values = @partials».arguments»[0];
		Partial::Simple.new(Op::In, @values);
	}
	my multi expand-junction('all', @partials) {
		Partial::Custom.new: -> $name { join-conditions(Op::And, @partials.map(*.resolve($name))) }
	}
	my multi expand-junction('none', @partials) {
		Partial::Custom.new: -> $name { join-conditions(Op::And, @partials.map(*.resolve($name).negate)) }
	}
	my multi expand-junction('none', @partials where all(@partials) ~~ PartialEquals) {
		my Expression @values = @partials».arguments»[0];
		Partial::Simple.new(Op::NotIn, @values);
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

	my sub expand-condition(Pair $ (Column::Named(Str) :$key, Mu :$value)) {
		expand-partial($value).map(*.resolve($key));
	}

	multi method COERCE(Expression $expression) {
		Conditions.bless(:$expression);
	}
	multi method COERCE(%hash) {
		my Expression @expressions = %hash.sort.map(&expand-condition).flat;
		Conditions.new(@expressions);
	}
	multi method COERCE(Any:U) {
		Conditions;
	}
}

class Join does Source {
	enum Type <Inner Left Right Outer>;
	has Source:D $.left is required;
	has Source:D $.right is required;

	class On does Conditional {
		sub expand(Pair $pair (Column::Named(Cool) :$key, Column::Named(Cool) :$value)) {
			Op::Equals.new($key, $value);
		}
		multi method COERCE(%hash) {
			my Expression @expressions = %hash.sort(*.key).map(&expand);
			On.new(@expressions);
		}
	}
	has On $.on;
	has Columns $.using;
	has Type:D $.type is required;
	has Bool $.lateral;
	proto method new(|args) { * }
	multi method new(Table(Cool) $left, Table(Cool) $right, Columns(Any) :$using!, Type :$type = Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :$using, :$type, :$lateral);
	}
	multi method new(Table(Cool) $left, Table(Cool) $right, On(Any:D) :$on!, Type :$type = Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :$on, :$type, :$lateral);
	}
}

class Sorters {
	has Sorter:D @.elems is required;
	method new(@elems) {
		self.bless(:@elems);
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

	multi method COERCE(Any:D $sorter) {
		Sorters.new(expand-sorter($sorter).list);
	}
	multi method COERCE(Any:U $sorter) {
		Sorters;
	}
	multi method COERCE(@list) {
		Sorter.new(@list.map(&expand-sorter));
	}
}

class Assigns {
	has Pair @.pairs;
	sub to-pair(Pair $ (:$key, :$value)) {
		my $column = Column::Named.new($key);
		my $expression = expand-expression($value);
		Pair.new($column, $expression);
	}
	method new(Pair @pairs) {
		self.bless(:@pairs);
	}
	method COERCE(%set) {
		my Pair @pairs = %set.sort(*.key).map: { to-pair($^pair) }
		self.new(@pairs);
	}
	method keys() {
		@!pairs».key;
	}
	method values() {
		@!pairs».value;
	}
}

class Locking {
	enum Strength <Update NoKeyUpdate Share KeyShare>;
	has Strength:D $.strength is required;
	has Table $.table;
	has Bool $.no-wait;
	has Bool $.skip-locked;
	method new(Strength:D $strength, Table(Cool) $table?, Bool :$no-wait, Bool :$skip-locked) {
		self.bless(:$strength, :$table, :$no-wait, :$skip-locked);
	}
	multi method COERCE(Pair $ (:$key, :$value)) {
		my $strength = Strength.WHO{$key.lc.tc};
		self.new($strength, $value);
	}
	multi method COERCE(Str $name) {
		my $strength = Strength.WHO{$name.lc.tc};
		self.new($strength);
	}
}

class Select does Command does Expression {
	has Columns:D $.columns is required;
	has Bool $.distinct;
	has Source $.source is required;
	has Conditions $.conditions is required;
	has Columns $.group-by;
	has Conditions $.having is required;
	has Sorters $.order-by;
	has Int $.limit;
	has Int $.offset;
	has Locking $.locking;
}

class Insert does Command {
	has Table:D $.target is required;
	has Columns $.fields;
	has Rows $.rows;
	has Columns $.returning;
}

class Update does Command {
	has Table:D $.target is required;
	has Assigns:D $.set is required;
	has Conditions $.conditions;
	has Columns $.returning;
}

class Delete does Command {
	has Table:D $.target  is required;
	has Conditions $.conditions;
	has Columns $.returning;
}

role Placeholders {
	has @.values;
	method bind(Str $name, Any $value) { ... }
}

class Placeholders::DBIish does Placeholders {
	method bind(Str $name, Any $value) {
		@!values.push($value);
		'?';
	}
}

class Placeholders::Postgres does Placeholders {
	has Int $.counter = 0;
	method bind(Str $name, Any $value) {
		@!values.push($value);
		'$' ~ ++$!counter;
	}
}

role Renderer {
	method render-command() { ... }
}

class Renderer::SQL { ... }

role Expression::Custom does Expression { # XXX
	method render-sql(Renderer::SQL $renderer) { ... }
}

class Renderer::SQL does Renderer {
	has Placeholders:U $.placeholders is required;
	has Bool:D $.quoting = False;

	my sub parenthesize-maybe(Str $input, Bool $parenthese --> Str) {
		$parenthese ?? "($input)" !! $input;
	}

	method render-list(Placeholders $arguments, @elements, Bool $parenthesized --> Str) {
		my @elems = @elements.map: { self.render-expression($arguments, $^element, Precedence::Comma) };
		parenthesize-maybe(@elems.join(', '), $parenthesized);
	}

	method quote-identifier(Str $identifier --> Str) {
		$!quoting ?? '"' ~ $identifier.subst('"', '""', :g) ~ '"' !! $identifier;
	}

	method render-identifier(Ident $ident --> Str) {
		$ident.parts.map({ self.quote-identifier($^id) }).join('.');
	}
	multi method render-expression(Placeholders $placeholders, Literal $literal, Precedence $ --> Str) {
		my @arguments = $literal.arguments;
		$literal.payload.subst(/'$' (<digit>+)/, { $placeholders.bind(Str, @arguments[~$0 - 1]) }, :g);
	}
	multi method render-expression(Placeholders $placeholders, Value $value, Precedence $ --> Str) {
		$placeholders.bind(Str, $value.value);
	}
	multi method render-expression(Placeholders $placeholders, Bind $bind, Precedence $ --> Str) {
		$placeholders.bind($bind.column, $bind.value);
	}
	multi method render-expression(Placeholders $placeholders, Row $row, Precedence $ --> Str) {
		self.render-list($placeholders, $row.elements, True);
	}
	multi method render-expression(Placeholders $placeholders, Function $func, Precedence $ --> Str) {
		$func.name.uc ~ self.render-list($placeholders, $func.arguments, True);
	}
	multi method render-expression(Placeholders $placeholders, Op::Unary::Postfix $op, Precedence $precedence --> Str) {
		my $value = self.render-expression($placeholders, $op.value, Precedence::Postfix);
		parenthesize-maybe($value ~ ' ' ~ $op.postfix, $precedence > Precedence::Postfix);
	}
	multi method render-expression(Placeholders $placeholders, Op::Unary::Prefix $op, Precedence $precedence --> Str) {
		my $primary = self.render-expression($placeholders, $op.value, Precedence::Prefix);
		parenthesize-maybe($op.operator ~ ' ' ~ $primary, $precedence > Precedence::Prefix);
	}
	multi method render-expression(Placeholders $placeholders, Op::Binary $op, Precedence $precedence --> Str) {
		my $left = self.render-expression($placeholders, $op.left, $op.precedence);
		my $right = self.render-expression($placeholders, $op.right, $op.precedence);
		parenthesize-maybe("$left $op.operator() $right", $precedence > $op.precedence);
	}
	multi method render-expression(Placeholders $placeholders, Op::ListCompare $in, Precedence $precedence --> Str) {
		my $left = self.render-expression($placeholders, $in.left, $in.precedence);
		my $candidates = self.render-list($placeholders, $in.elements, True);
		parenthesize-maybe("$left $in.operator() $candidates", $precedence > $in.precedence);
	}
	multi method render-expression(Placeholders $placeholders, Op::BetweenLike $between, Precedence $precedence --> Str) {
		my $left = self.render-expression($placeholders, $between.left, Precedence::Between);
		my $min = self.render-expression($placeholders, $between.min, Precedence::Between);
		my $max = self.render-expression($placeholders, $between.max, Precedence::Between);
		my @not = $between.negated ?? 'NOT' !! Empty;
		my $base = ($left, |@not, 'BETWEEN', $min, 'AND', $max).join(' ');
		parenthesize-maybe($base, $precedence > Precedence::Between);
	}
	method render-sub-expression(Placeholders $placeholders, Expression $element, Precedence $precedence --> Str) {
		self.render-expression($placeholders, $element, $precedence);
	}
	multi method render-expression(Placeholders $placeholders, Op::Logical $logical, Precedence $precedence --> Str) {
		my @elems = $logical.elements.map: { self.render-sub-expression($placeholders, $^element, $logical.precedence) };
		@elems.join(" $logical.operator() ");
	}
	multi method render-expression(Placeholders $placeholders, Op::Cast $cast, Precedence $ --> Str) {
		'CAST(' ~ self.render-expression($placeholders, $cast.primary, Precedence::Comma) ~ " AS $cast.type())";
	}
	multi method render-expression(Placeholders $placeholders, Command $select, Precedence $precedence --> Str) {
		'(' ~ self.render-command-expression($placeholders, $select) ~ ')';
	}
	multi method render-expression(Placeholders $placeholders, Column $column, Precedence $precedence --> Str) {
		self.render-column($placeholders, $column);
	}

	multi method render-column(Placeholders $placeholders, Column::Named $column --> Str) {
		self.render-identifier($column.ident);
	}
	multi method render-column(Placeholders $placeholders, Column::Renamed $column --> Str) {
		my $source = self.render-column($placeholders, $column.source);
		my $alias = self.render-identifier($column.alias);
		"$source AS $alias";
	}
	multi method render-column(Placeholders $placeholders, Column::All --> '*') {}
	multi method render-column(Placeholders $placeholders, Column::Expression $column --> Str) {
		self.render-expression($placeholders, $column.expression, Precedence::Comma);
	}

	method render-columns(Placeholders $placeholders, Columns $columns, Bool $parenthesized = False --> Str) {
		my @elems = $columns.elems.map: { self.render-column($placeholders, $^column) };
		parenthesize-maybe(@elems.join(', '), $parenthesized);
	}

	multi method render-sorter(Placeholders $placeholders, Column $column --> Str) {
		self.render-column($placeholders, $column);
	}
	multi method render-sorter(Placeholders $placeholders, Sorter::Modifier $column --> Str) {
		self.render-column($placeholders, $column.column) ~ ' ' ~ $column.modifier;
	}

	method render-table(Table $source --> Str) {
		self.render-identifier($source.name);
	}

	multi method render-source(Placeholders $, Table $table --> Str) {
		self.render-table($table);
	}
	multi method render-source(Placeholders $placeholders, Command $command --> Str) {
		'(' ~ self.render-command-expression($placeholders, $command) ~ ')';
	}
	multi method render-source(Placeholders $placeholders, Source::Renamed $renamed--> Str) {
		my $source = self.render-source($placeholders, $renamed.source);
		my $alias = self.render-identifier($renamed.alias);
		"$source AS $alias";
	}
	multi method render-source(Placeholders $placeholders, Join $join --> List) {
		my $left = self.render-source($placeholders, $join.left);
		my $right = self.render-source($placeholders, $join.right);
		my $type = $join.type.uc;
		my @lateral = $join.lateral ?? 'LATERAL' !! Empty;
		my @on = $join.on    ?? self.render-conditions($placeholders, $join.on, 'ON')
		    !!   $join.using ?? 'USING ' ~ self.render-columns($placeholders, $join.using, True)
		    !!                  Empty;
		$left, $type, 'JOIN', |@lateral, $right, |@on;
	}

	method render-values(Placeholders $placeholders, Rows $rows --> Str) {
		my @sets = $rows.elems.map: { self.render-list($placeholders, $^row.elements, True) };
		@sets.join(', ');
	}

	multi method render-conditions(Placeholders $placeholders, Conditional:D $conditions, Str $type = 'WHERE' --> Str) {
		"$type " ~ self.render-expression($placeholders, $conditions.expression, Precedence::And);
	}
	multi method render-conditions(Placeholders $placeholders, Conditional:U $conditions, Str $type = Str --> List) {
		Empty;
	}

	multi method render-order-by(Placeholders $placeholders, Sorters:D $sorters --> List) {
		my @elems = $sorters.elems.map: { self.render-sorter($placeholders, $^column) };
		'ORDER BY',  @elems.join(', ');
	}
	multi method render-order-by(Placeholders $placeholders, Sorters:U $sorters --> List) {
		Empty
	}

	multi method render-group-by(Placeholders $placeholders, Columns:D $columns, Conditions $conditions --> List) {
		my $group-by = self.render-columns($placeholders, $columns);
		my @having   = self.render-conditions($placeholders, $conditions, 'HAVING');
		'GROUP BY', $group-by, |@having;
	}
	multi method render-group-by(Placeholders $placeholders, Columns:U $columns, Conditions $having --> List) {
		Empty
	}

	multi method render-returning(Placeholders $placeholders, Columns:D $returning --> List) {
		'RETURNING', self.render-columns($placeholders, $returning);
	}
	multi method render-returning(Placeholders $placeholders, Columns:U $returning --> List) {
		Empty
	}

	multi method render-limit(Int:D $limit, Int $offset) {
		$offset.defined ?? "LIMIT $limit OFFSET $offset" !! "LIMIT $limit";
	}
	multi method render-limit(Int:U $limit, Int $offset) {
		Empty
	}

	multi method render-locking(Locking:D $locking --> List) {
		state @locking-key = ('UPDATE', 'NO KEY UPDATE', 'SHARE', 'KEY SHARE');
		my @result = 'FOR', @locking-key[+$locking.strength];
		@result.push('OF', self.render-table($locking.table)) with $locking.table;
		@result.push('NOWAIT') if $locking.no-wait;
		@result.push('SKIP LOCKED') if $locking.skip-locked;
		@result;
	}
	multi method render-locking(Locking:U --> List) {
		Empty;
	}

	multi method render-command-expression(Placeholders $placeholders, Select $select --> Str) {
		my $columns    = self.render-columns($placeholders, $select.columns);
		my @distinct   = $select.distinct ?? 'DISTINCT' !! Empty;
		my @source     = self.render-source($placeholders, $select.source);
		my @conditions = self.render-conditions($placeholders, $select.conditions);
		my @group-by   = self.render-group-by($placeholders, $select.group-by, $select.having);
		my @order-by   = self.render-order-by($placeholders, $select.order-by);
		my @limit      = self.render-limit($select.limit, $select.offset);
		my @locking    = self.render-locking($select.locking);
		('SELECT', @distinct, $columns, 'FROM', @source, @conditions, @group-by, @order-by, @limit, @locking).flat.join(' ');
	}

	multi method render-command-expression(Placeholders $placeholders, Update $update --> Str) {
		my $target      = self.render-table($update.target);
		my @expressions = $update.set.pairs.map: -> (:$key, :$value) { Op::Assign.new($key, $value) }
		my $set         = self.render-list($placeholders, @expressions, False);
		my @conditions  = self.render-conditions($placeholders, $update.conditions);
		my @returning   = self.render-returning($placeholders, $update.returning);
		('UPDATE', $target, 'SET', $set, @conditions, @returning).flat.join(' ');
	}

	multi method render-command-expression(Placeholders $placeholders, Insert $insert --> Str) {
		my $target    = self.render-table($insert.target);
		my $fields    = self.render-columns($placeholders, $insert.fields, True);
		my $values    = self.render-values($placeholders, $insert.rows);
		my @returning = self.render-returning($placeholders, $insert.returning);
		('INSERT INTO', $target, $fields, 'VALUES', $values, @returning).flat.join(' ');
	}

	multi method render-command-expression(Placeholders $placeholders, Delete $delete --> Str) {
		my $target     = self.render-table($delete.target);
		my @conditions = self.render-conditions($placeholders, $delete.conditions);
		my @returning  = self.render-returning($placeholders, $delete.returning);
		('DELETE FROM', $target, @conditions, @returning).flat.join(' ');
	}

	method render-command(Command $command) {
		my $placeholders = self.placeholders.new;
		my $sql          = self.render-command-expression($placeholders, $command);
		$sql, $placeholders.values;
	}
}

class Renderer::SQL::Postgres is Renderer::SQL {
	multi method render-expression(Placeholders $placeholders, Op::Cast $cast --> Str) {
		self.render-expression($placeholders, $cast.primary, Precedence::Term) ~ '::' ~ $cast.type;
	}
}

class Renderer::Noop does Renderer {
	method render-command(Command $command) { $command }
}

has Renderer:D $.renderer is required;

multi submethod BUILD(Renderer:D :$!renderer!) {}
multi submethod BUILD(Placeholders:U :$placeholders!, Renderer:U :renderer($renderer-class) = Renderer::SQL::Postgres) {
	$!renderer = $renderer-class.new(:$placeholders);
}

method select(Source(Any:D) $source, Columns(Any:D) $columns = *, Conditions(Any) $conditions?, Bool :$distinct, Columns(Any) :$group-by, Conditions(Any) :$having?, Sorters(Any) :$order-by, Int :$limit, Int :$offset, Locking(Any) :$locking) {
	my $select = Select.new(:$columns, :$distinct :$source, :$conditions, :$group-by, :$having, :$order-by, :$limit, :$offset, :$locking);
	$!renderer.render-command($select);
}

multi method insert(Table(Cool) $target, Assigns(Hash) $values, Columns(Any) :$returning) {
	my $fields = Columns.new($values.keys);
	my $rows = Rows.COERCE([$values.values,]);
	my $insert = Insert.new(:$target, :$fields, :$rows, :$returning);
	$!renderer.render-command($insert);
}
multi method insert(Table(Cool) $target, Columns(Any) $fields, Rows(Any) $rows, Columns(Any) :$returning) {
	my $insert = Insert.new(:$target, :$fields, :$rows, :$returning);
	$!renderer.render-command($insert);
}

method update(Table(Cool) $target, Assigns(Hash) $set, Conditions(Any) $conditions?, Columns(Any) :$returning) {
	my $update = Update.new(:$target, :$set, :$conditions, :$returning);
	$!renderer.render-command($update);
}

method delete(Table(Cool) $target, Conditions(Any) $conditions?, Columns(Any) :$returning) {
	my $delete = Delete.new(:$target, :$conditions, :$returning);
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
