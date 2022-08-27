unit class SQL::Abstract;

use fatal;

enum Precedence <Rowlike Comma Assignment And Or Not Between In LoosePrefix Postfix Comparative Additive Multiplicative Concatlike TightPrefix Termlike>;

class Op::Unary::Not { ... }

role Expression {
	method precedence(--> Precedence) { ... }

	method negate(--> Expression) {
		Op::Unary::Not.new(self);
	}
}

role Term does Expression {
	method precedence(--> Precedence::Termlike) {}
}

class Literal does Expression {
	has Precedence $.precedence = Precedence::Termlike;
	has Str:D $.payload is required;
	has Any:D @.arguments;

	method new(Str:D $payload, @arguments) {
		self.bless(:$payload, :@arguments);
	}
}

class Integer does Term {
	has Int $.value;

	method new(Int $value) {
		self.bless(:$value);
	}
}

class Placeholder does Term {
	has Any $.value;

	method new(Any $value) {
		self.bless(:$value);
	}
}

role Constant[Str $keyword] does Term {
	method keyword(--> Str) { $keyword }

	method Str() { $keyword }

	method WHICH() {
		ValueObjAt.new("SQL::Abstract::Constant|$keyword");
	}
}

class Parentheses does Term {
	has Expression $.inner;
	method new(Expression $inner) {
		self.bless(:$inner);
	}
}

class Value::Default does Constant['DEFAULT'] {}
class Value::True    does Constant['TRUE']    {}
class Value::False   does Constant['FALSE']   {}
class Value::Null    does Constant['NULL']    {}

my multi expand-expression(Expression $expression) {
	$expression;
}
my multi expand-expression(Any $value) {
	Placeholder.new($value);
}
my multi expand-expression(Capture $literal) {
	my ($sql, @arguments) = |$literal;
	Literal.new($sql, @arguments);
}

class Identifier does Term {
	has Str @.parts;

	proto method new(|) { * }
	multi method new(@parts) {
		self.bless(:@parts);
	}
	multi method new(Cool $name) {
		my @parts = $name.split('.');
		self.bless(:@parts);
	}

	method concat(::?CLASS $other) {
		self.new([|@!parts, |$other.parts]);
	}

	my sub quote(Str $part) {
		'"' ~ $part.subst('"', '""', :g) ~ '"';
	}
	method Str() {
		@!parts.map(&quote).join('.');
	}
	method WHICH() {
		ValueObjAt.new("{ self.WHAT.^name }|{ self }");
	}
}

class Expression::Renamed does Expression {
	method precedence(--> Precedence::Comma) {}

	has Identifier:D $.alias is required;
	has Expression:D $.source is required;

	proto method new(|) { * }
	multi method new(Expression $source, Identifier(Cool) $alias) {
		self.bless(:$source, :$alias);
	}
	multi method new(Identifier(Cool) $source, Identifier(Cool) $alias) {
		self.bless(:$source, :$alias);
	}

	multi method COERCE(Pair $ (Identifier(Cool) :$key, Expression :$value)) {
		self.new($value, $key);
	}
	multi method COERCE(Pair $ (Identifier(Cool) :$key, Identifier(Cool) :$value)) {
		self.new($value, $key);
	}
}

class Star does Term {
	method WHICH() {
		ValueObjAt.new("Star|*");
	}
}
multi expand-expression(Whatever) {
	Star.new;
}

class Identifiers {
	has Identifier:D @.elems is required;

	method new(@elems) {
		self.bless(:@elems);
	}

	multi method COERCE(Identifier(Any) $column) {
		self.new([$column]);
	}
	multi method COERCE(@list) {
		my Identifier(Cool) @columns = @list;
		self.new(@columns);
	}

	multi method merge(Identifiers $other) {
		self.new(|@!elems, |$other.elems);
	}
}

class Expression::List {
	has Expression:D @.elems is required;

	method new(@elems) {
		self.bless(:@elems);
	}

	my multi to-column(Expression $column) {
		$column;
	}
	my multi to-column(Identifier(Cool) $column) {
		$column;
	}
	my multi to-column(Expression::Renamed(Pair) $column) {
		$column;
	}
	my multi to-column(Pair $ (:$key, Bool :$value)) {
		Identifier.new($key);
	}
	my multi to-column(Whatever) {
		Star.new;
	}

	multi method COERCE(Any $column) {
		self.new([to-column($column)]);
	}
	multi method COERCE(@list) {
		my @columns = @list.map(&to-column);
		self.new(@columns);
	}
	multi method COERCE(Identifiers $columns) {
		self.new($columns.elems);
	}

	multi method merge(Expression::List $other) {
		self.new(|@!elems, |$other.elems);
	}
}

class Source::Function { ... }

class Function does Term {
	has Str:D $.name is required;
	has Expression @.arguments;

	proto method new(|) { * }
	multi method new(Str $name) {
		self.bless(:$name);
	}
	multi method new(Str $name, Expression $arguments) {
		self.bless(:$name, :arguments[$arguments]);
	}
	multi method new(Str $name, Expression @arguments) {
		self.bless(:$name, :@arguments);
	}

	method as(Identifier:D(Cool:D) $alias, Identifiers(Any) $columns?, Bool :$lateral, Bool :$ordinal) {
		Source::Function.new(self, $alias, $columns, :$lateral, :$ordinal);
	}
}

role Op::Unary does Expression {
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

class Op::IsNotNull does Op::Unary::Postfix['IS NOT NULL'] {
}
class Op::IsNull does Op::Unary::Postfix['IS NULL'] {
	method negate {
		Op::IsNotNull.new($!value);
	}
}

role Op::Unary::Prefix::Loose does Op::Unary {
	method precedence(--> Precedence::LoosePrefix) {}
	method operator(--> Str:D) { ... }
}

role Op::Unary::Prefix::Tight does Op::Unary {
	method precedence(--> Precedence::TightPrefix) {}
	method operator(--> Str:D) { ... }
}

class Op::Unary::Not does Op::Unary::Prefix::Loose {
	method precedence(--> Precedence::Not) {}
	method operator(--> 'NOT') {}

	method new(Expression $value) {
		self.bless(:$value);
	}

	method negated(--> Bool) {
		$!value;
	}
}

role Op::Binary does Expression {
	method operator() { ... }
	has Expression:D $.left is required;
	has Expression:D $.right is required;
	method new(Expression:D $left, Expression:D $right) {
		self.bless(:$left, :$right);
	}
}

role Op::Concatenative[Str $operator] does Op::Binary {
	method precedence(--> Precedence::Concatlike) {}
	method operator() { $operator }
}

class Op::Concat does Op::Concatenative['||'] {}

class Op::JSON::GetElem does Op::Concatenative['->'] {}
class Op::JSON::GetElemText does Op::Concatenative['->>'] {}
class Op::JSON::GetPath does Op::Concatenative['#>'] {}
class Op::JSON::GetPathText does Op::Concatenative['#>>'] {}

role Op::Comperative[Str $operator] does Op::Binary {
	method precedence(--> Precedence::Comparative) {}
	method operator(--> Str) { $operator }
}

class Op::Equals does Op::Comperative['='] {}
class Op::Unequals does Op::Comperative['<>'] {}

class Op::LessThan does Op::Comperative['<'] {}
class Op::GreaterOrEqual does Op::Comperative['>='] {}

class Op::LessOrEqual does Op::Comperative['<='] {}
class Op::GreaterThan does Op::Comperative['>'] {}

class Op::Like does Op::Comperative['LIKE'] {}
class Op::Unlike does Op::Comperative['NOT LIKE'] {}

my %binary-op-for =
	'='        => Op::Equals,
	'!='       => Op::Unequals,
	'<>'       => Op::Unequals,
	'<'        => Op::LessThan,
	'<='       => Op::LessOrEqual,
	'>'        => Op::GreaterThan,
	'>='       => Op::GreaterOrEqual,
	'like'     => Op::Like,
	'not-like' => Op::Unlike;

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

class Op::Binary::Additive does Op::Binary::Other[Precedence::Additive] { }
class Op::Binary::Multiplicative does Op::Binary::Other[Precedence::Multiplicative] { }

role Op::BetweenLike[Bool $positive] does Expression {
	method precedence(--> Precedence::Between) {}

	method negated(--> Bool) { !$positive }

	has Expression:D $.left is required;
	has Expression:D $.min is required;
	has Expression:D $.max is required;

	method new(Expression $left, Expression $min, Expression $max) {
		self.bless(:$left, :$min, :$max);
	}
}

class Op::NotBetween does Op::BetweenLike[False] {}
class Op::Between does Op::BetweenLike[True] {
	method negate() {
		Op::NotBetween.new($!left, $!min, $!max);
	}
}

role Op::List does Expression {
	has Expression @.elements is required;
}

role Op::Logical[Str $operator, Precedence $precedence, Constant $empty] does Op::List {
	method precedence(--> Precedence) { $precedence }
	method operator(--> Str) { $operator }

	method new(@elements) {
		self.bless(:@elements);
	}

	method pack(*@expressions) {
		given @expressions.elems {
			when 1  { @expressions[0] }
			when 0  { $empty.new }
			default { self.new(@expressions) }
		}
	}

	method unpack(Expression $expression) {
		given $expression {
			when self   { |$expression.elements }
			when $empty { Empty }
			default     { $expression }
		}
	}

	method merge(*@expressions) {
		my @unpacked = @expressions.map: { self.unpack($^expression) };
		self.pack(@unpacked);
	}
}
class Op::And does Op::Logical['AND', Precedence::And, Value::True] {}
class Op::Or  does Op::Logical['OR', Precedence::Or, Value::False] {}

role Op::ListCompare[Str $operator] does Op::List {
	method precedence(--> Precedence::In) {}
	method operator(--> Str) { $operator }

	has Expression:D $.left is required;

	method new(Expression $left, @elements) {
		self.bless(:$left, :@elements);
	}
}

class Op::NotIn does Op::ListCompare['NOT IN'] { }
class Op::In does Op::ListCompare['IN'] {
	method negate() {
		Op::NotIn.new($!left, @!elements);
	}
}

class Op::Cast does Term {
	has Expression:D $.primary is required;
	has Str:D $.typename is required;

	method new(Expression:D $primary, Str:D $typename) {
		self.bless(:$primary, :$typename);
	}
}

class Op::Case does Expression {
	method precedence(--> Precedence::Between) {}

	has Expression $.left;
	class When {
		has Expression:D $.condition is required;
		has Expression:D $.value is required;

		method new(Expression:D $condition, Expression:D $value) {
			self.bless(:$condition, :$value);
		}
	}
	has When:D @.whens;
	has Expression $.else;

	method new(Expression $left, @whens, Expression $else?) {
		self.bless(:$left, :@whens, :$else);
	}	
}

role Conditional {
	has Expression:D $.expression is required;

	proto method new(|) { * }
	multi method new(@expressions) {
		my $expression = Op::And.pack(@expressions);
		self.bless(:$expression);
	}
	multi method new(Expression $expression) {
		self.bless(:$expression);
	}

	multi method merge(Conditional:D: Conditional:D $other) {
		my $expression = Op::And.merge($!expression, $other.expression);
		self.new($expression);
	}
	multi method merge(Conditional:U: Conditional:D $other) {
		self.new($other.expression);
	}
	multi method merge(Conditional: Conditional:U $other) {
		self;
	}
}

class Conditions does Conditional {
	my multi expand-partial(Expression $left, Any:D $expression) {
		Op::Equals.new($left, expand-expression($expression));
	}
	my multi expand-partial(Expression $left, Any:U $expression) {
		Op::IsNull.new($left);
	}

	my multi expand-pair(Expression $left, 'isnull', $value) {
		Op::IsNull.new($left);
	}
	my multi expand-pair(Expression $left, 'isnotnull', $value) {
		Op::IsNotNull.new($left);
	}
	my multi expand-pair(Expression $left, Str $key, Any $value) {
		my $expression = expand-expression($value);
		if %binary-op-for{$key}:exists {
			%binary-op-for{$key}.new($left, $expression);
		} else {
			Op::Comperative[$key].new($left, $expression);
		}
	}
	my multi expand-partial(Expression $left, Pair $ (:$key, :$value)) {
		expand-pair($left, $key, $value);
	}

	my multi expand-partial(Expression $left, %hash) {
		%hash.sort(*.key).map: { expand-partial($left, $^pair) };
	}
	my multi expand-partial(Expression $left, Range $range) {
		my $min = Placeholder.new($range.min);
		my $max = Placeholder.new($range.max);
		Op::Between.new($left, $min, $max);
	}

	my multi expand-junction(Expression $left, 'any', @partials) {
		Op::Or.pack(@partials);
	}
	my multi expand-junction(Expression $left, 'any', @partials where @partials > 0 && all(@partials) ~~ Op::Equals && all(@partials).left === $left) {
		Op::In.new($left, @partials».right);
	}
	my multi expand-junction(Expression $left, 'all', @partials) {
		Op::And.pack(@partials);
	}
	my multi expand-junction(Expression $left, 'none', @partials) {
		Op::Or.pack(@partials).negate;
	}
	my multi expand-junction(Expression $left, 'none', @partials where @partials > 0 && all(@partials) ~~ Op::Equals && all(@partials).left === $left) {
		Op::NotIn.new($left, @partials».right);
	}
	my multi expand-junction(Expression $left, 'one', @partials) {
		my @comparisons = @partials».resolve;
		my $addition = @comparisons.reduce: { Op::Binary::Additive.new('+', $^left, $^right) };
		Op::Equals.new($addition, Integer.new(1));
	}
	my multi expand-partial(Expression $left, Junction $junction) {
		use nqp;
		my $type = nqp::box_s(nqp::getattr($junction, Junction, '$!type'), Str);
		my @eigenstates = nqp::getattr($junction, Junction, '$!eigenstates').List;
		expand-junction($left, $type, @eigenstates.map({ expand-partial($left, $^value) }));
	}

	my multi expand-condition(Pair $ (Identifier(Cool) :$key, Mu :$value)) {
		expand-partial($key, $value);
	}
	my multi expand-condition(Pair $ (Expression :$key, Mu :$value)) {
		expand-partial($key, $value);
	}

	multi method COERCE(Expression $expression) {
		self.bless(:$expression);
	}
	multi method COERCE(Pair $pair) {
		self.new(expand-condition($pair));
	}
	multi method COERCE(@list) {
		self.new(@list».&expand-condition);
	}
	multi method COERCE(%hash) {
		samewith(%hash.sort);
	}
}

class Order::Modifier does Expression {
	method precedence(--> Precedence::Comma) {}

	my enum Order (:Asc<ASC> :Desc<DESC>);
	has Expression:D $.column is required;
	has Order:D $.order is required;

	proto method new(|) { * }
	multi method new(Expression $column, Order $order) {
		self.bless(:$column, :$order);
	}
	multi method new(Expression $column, Str $order-name) {
		my $order = Order.WHO{$order-name.lc.tc};
		self.bless(:$column, :$order);
	}

	multi method COERCE(Pair $ (Expression:D :$key, Any :$value)) {
		self.new($key, $value);
	}
	multi method COERCE(Pair $ (Identifier:D(Cool:D) :$key, Any :$value)) {
		self.new($key, $value);
	}
}

class OrderBy {
	has Expression:D @.elems is required;

	method new(@elems) {
		self.bless(:@elems);
	}

	my multi sorter(Expression:D $expression) {
		$expression;
	}
	my multi to-sorter(Identifier:D(Cool:D) $ident) {
		$ident;
	}
	my multi to-sorter(Pair:D $pair) {
		Order::Modifier.COERCE($pair);
	}

	multi method COERCE(Any:D $sorter) {
		self.new([to-sorter($sorter)]);
	}
	multi method COERCE(@list) {
		my @sorters = @list.map(&to-sorter);
		self.new(@sorters);
	}
}

class Row does Expression {
	method precedence(--> Precedence::Comma) {}

	has Expression @.elements;

	method new(Expression @elements) {
		self.bless(:@elements);
	}

	method COERCE(@values) {
		my Expression @expressions = @values».&expand-expression;
		self.new(@expressions);
	}
}

class Rows {
	has Row @.elems;

	method COERCE(@input) {
		my Row(Any) @elems = @input;
		self.new(:@elems);
	}
}

class Assigns {
	has Pair @.pairs;

	sub transform-pair(Pair $ (:$key, :$value)) {
		my $column = Identifier.new($key);
		my $expression = expand-expression($value);
		$column => $expression;
	}
	multi method COERCE(@list) {
		my @pairs = @list.unique(:as(*.key))».&transform-pair;
		self.bless(:@pairs);
	}
	multi method COERCE(%set) {
		my @pairs = %set.sort(*.key)».&transform-pair;
		self.bless(:@pairs);
	}

	method keys() {
		@!pairs.map(*.key);
	}
	method values() {
		@!pairs.map(*.value);
	}
}

class Window::Definition {
	has Identifier $.existing;
	has Expression::List $.columns;
	has OrderBy $.order-by;

	method is-simple() {
		$!existing && !$!columns && !$!order-by;
	}
}

class Function::Window does Expression {
	method precedence(--> Precedence::Comma) {}

	has Function:D $.function is required;
	has Conditions $.filter;
	has Window::Definition:D $.window is required;

	method new(Function:D(Cool:D) $function, Conditions(Any) :$filter, Expression::List(Any) :$columns, OrderBy(Any) :$order-by, Identifier :$existing) {
		my $window = Window::Definition.new(:$existing, :$columns, :$order-by);
		self.bless(:$function, :$filter, :$window);
	}
}

class Window {
	has Identifier:D         $.name is required;
	has Window::Definition:D $.definition is required;
}

class Windows {
	has Window @.windows;

	multi method COERCE(@windows) {
		self.bless(:@windows);
	}
	multi method COERCE(Window $window) {
		self.bless(:windows[ $window ]);
	}
}

role Keyword does Expression {
	method precedence() { Precedence::Rowlike }
}

class Table { ... }
class Join::On { ... }
class Join::On::Conditions { ... }
class Join::Using { ... }
class Table::Renamed { ... }
class Select { ... }

class Source does Expression {
	method precedence(--> Precedence::Rowlike) {}

	multi submethod COERCE(Cool $pre-source) {
		Table.new($pre-source);
	}
	multi submethod COERCE(Pair $pair) {
		Table::Renamed.COERCE($pair);
	}

	multi method join(Source:D(Any:D) $right, Join::On::Conditions(Any) :$on!, *%arguments) {
		Join::On.new(self, $right, $on, |%arguments);
	}
	multi method join(Source:D(Any:D) $right, Identifiers(Any) :$using!, *%arguments) {
		Join::Using.new(self, $right, $using, |%arguments);
	}
	multi method join(Source:D(Any:D) $right, Bool :$natural!, *%arguments) {
		Join::Natural.new(self, $right, |%arguments);
	}
	multi method join(Source:D(Any:D) $right, Bool :$cross!, *%arguments) {
		Join::Cross.new(self, $right, |%arguments);
	}

	method select(Expression::List:D(Any:D) $columns = *, Conditions(Any) $where?, *%arguments) {
		Select.new(:$columns, :source(self), :$where, |%arguments);
	}
}

class Insert::Values { ... }
class Insert::Select { ... }
class Insert::Defaults { ... }
class Update::Pairwise { ... }
class Update::Row      { ... }
class Update::Select { ... }
class Delete { ... }

class Table is Source does Keyword {
	has Identifier:D $.name is required;
	has Bool          $.only;

	method new(Identifier(Cool:D) $name, Bool :$only) {
		self.bless(:$name, :$only);
	}

	method COERCE(Pair $ (:$key, Source(Any) :$value)) {
		Table::Renamed.new($value, $key);
	}

	multi method update(Assigns:D(Any:D) $assigns, Conditions(Any) $where?, *%arguments) {
		Update::Pairwise.new(:target(self), :$assigns, :$where, |%arguments);
	}
	multi method update(Identifiers:D(Any:D) $columns, Row $row, Conditions(Any) $where?, *%arguments) {
		Update::Row.new(:target(self), :$columns, :$row, :$where, |%arguments);
	}
	multi method update(Identifiers:D(Any:D) $columns, Select:D $select, Conditions(Any) $where?, *%arguments) {
		Update::Select.new(:target(self), :$columns, :$select, :$where, |%arguments);
	}

	multi method insert(Identifiers(Any) $fields, Rows:D(List:D) $rows, *%arguments) {
		Insert::Values.new(:target(self), :$fields, :$rows, |%arguments);
	}
	multi method insert(Assigns:D(Cool:D) $values, *%arguments) {
		samewith($values.keys, [$values.values,], |%arguments);
	}
	multi method insert(Identifiers(Any) $fields, Select $select, *%arguments) {
		Insert::Select.new(:target(self), :$fields, :$select, |%arguments);
	}
	multi method insert(Value::Default, *%arguments) {
		Insert::Defaults.new(:target(self), |%arguments);
	}

	method delete(Conditions(Any) $where?, *%arguments) {
		Delete.new(:target(self), :$where, |%arguments);
	}

	multi method as(Identifier:D(Cool:D) $alias) {
		Table::Renamed.new(self, $alias);
	}
	multi method as(Identifier:D(Cool:D) $alias, Identifiers(Any) $columns) {
		Table::Renamed.new(self, $alias, $columns);
	}
	multi method as(Pair $pair) {
		Table::Renamed.new(self, $pair.key, $pair.value);
	}

	multi method concat(Identifier $column) {
		Identifier.new($!name.concat($column.name));
	}
	multi method concat(Identifier @names) {
		@names.map: { self.concat($^name) };
	}
}

role Source::Aliased is Source {
	has Identifier:D $.alias is required;
	has Identifiers $.columns;
}

class Table::Renamed is Table does Source::Aliased {
	method new(Table:D $table, Identifier:D(Cool:D) $alias, Identifiers(Any) $columns?) {
		self.bless(:name($table.name), :only($table.only), :$alias, :$columns);
	}

	method COERCE(Pair $ (:$key, Table(Any) :$value)) {
		self.new($value, $key);
	}
}

class Join is Source {
	enum Type <Inner Left Right Outer>;

	has Source:D $.left is required;
	has Source:D $.right is required;
	has Type:D $.type = Type::Inner;
}

class Join::On::Conditions does Conditional {
	sub expand(Pair $pair (Identifier(Cool) :$key, Identifier(Cool) :$value)) {
		Op::Equals.new($key, $value);
	}

	multi method COERCE(Conditions $conditions) {
		self.new($conditions.expression);
	}
	multi method COERCE(%hash) {
		self.new(%hash.sort(*.key)».&expand);
	}
}
class Join::On is Join {
	has Join::On::Conditions $.on;

	method new(Source:D(Any:D) $left, Source:D(Any:D) $right, Join::On::Conditions:D(Any:D) $on, Join::Type :$type = Join::Type::Inner) {
		self.bless(:$left, :$right, :$on, :$type);
	}
}

class Join::Using is Join {
	has Identifiers $.using;
	method new(Source:D(Any:D) $left, Source:D(Any:D) $right, Identifiers(Any) $using, Join::Type :$type = Join::Type::Inner) {
		self.bless(:$left, :$right, :$using, :$type);
	}
}

class Join::Natural is Join {
	method new(Source:D(Any:D) $left, Source:D(Any:D) $right, Join::Type :$type = Join::Type::Inner) {
		self.bless(:$left, :$right, :$type);
	}
}

class Join::Cross is Join {
	method new(Source:D(Any:D) $left, Source:D(Any:D) $right) {
		self.bless(:$left, :$right);
	}
}

role Source::Nested does Source::Aliased {
	has Bool $.lateral;
}

class Source::Query does Source::Nested {
	has Keyword:D $.keyword is required;

	multi method new(Keyword:D $keyword, Identifier:D(Cool:D) $alias, Identifiers(Any) $columns?, Bool :$lateral) {
		self.bless(:$keyword, :$alias, :$columns, :$lateral);
	}
}

class Source::Function does Source::Nested {
	has Function:D $.function is required;
	has Bool       $.ordinal;

	multi method new(Function:D $function, Identifier:D(Cool:D) $alias, Identifiers $columns?, Bool :$lateral, Bool :$ordinal) {
		self.bless(:$function, :$alias, :$columns, :$lateral, :$ordinal);
	}
}

class Locking {
	enum Strength (:Update<UPDATE> :NoKeyUpdate('NO KEY UPDATE') :Share<SHARE> :KeyShare('KEY SHARE'));

	has Strength:D $.strength is required;
	has Identifier $.table;
	has Bool $.no-wait;
	has Bool $.skip-locked;

	method new(Strength:D $strength, Identifier(Cool) $table?, Bool :$no-wait, Bool :$skip-locked) {
		self.bless(:$strength, :$table, :$no-wait, :$skip-locked);
	}

	multi method COERCE(Str $name) {
		my $strength = Strength.WHO{$name.lc.tc};
		self.new($strength);
	}
	multi method COERCE(Pair $ (:$key, :$value)) {
		my $strength = Strength.WHO{$key.lc.tc};
		self.new($strength, $value);
	}
}

class Compound {
	enum Type <Union Intersect Except>;
	has Type:D $.type = Union;
	has Bool $.all;
	has Select $.next;

	method new(Type:D $type, Select $next, Bool $all = False) {
		self.bless(:$type, :$next, :$all);
	}

	method COERCE(Pair $pair) {
		my $key = $pair.key.lc;
		my $all = so $key ~~ s/ '-all' $ //;
		my $type = Type.WHO{$key.tc};
		self.new($type, $pair.value, $all);
	}
}

class Limit {
	class All does Constant['ALL'] {}

	has Expression $.value;

	method new(Expression $value) {
		self.bless(:$value);
	}

	multi method COERCE(Int $value) {
		self.new(Integer.new($value));
	}
	multi method COERCE(Inf) {
		self.new(All.new);
	}
}

class Offset {
	has Expression $.value;

	method new(Expression $value) {
		self.bless(:$value);
	}

	multi method COERCE(Int $value) {
		self.new(Integer.new($value));
	}
}

class Common {
	class Name {
		has Identifier:D $.name is required;
		has Identifiers $.columns;

		method new(Identifier:D $name, Identifiers $columns?) {
			self.bless(:$name, :$columns);
		}

		multi method COERCE(Identifier:D(Cool:D) $name) {
			self.new($name);
		}
		multi method COERCE($pair) {
			self.new($pair.key, Identifiers.COERCE($pair.value));
		}
	}
	class Rename {
		has Name:D $.alias is required;
		has Keyword:D $.keyword is required;
		method new(Name:D $alias, Keyword:D $keyword) {
			self.bless(:$alias, :$keyword);
		}
		method COERCE(Pair $pair) {
			my Name:D(Any:D) $alias = $pair.key;
			self.new($alias, $pair.value);
		}
	}
	has Rename @.tables;
	has Bool $.recursive;

	method new(Rename @tables, Bool :$recursive) {
		self.bless(:@tables, :$recursive);
	}

	method COERCE(@pairs) {
		my Rename(Pair) @tables = @pairs;
		self.bless(:@tables);
	}
}

role Keyword::Common does Keyword {
	has Common(Any)      $.common-tables;
}

class Values does Keyword {
	has Rows:D  $.rows is required;
	has OrderBy $.order-by;
	has Limit   $.limit;
	has Offset  $.offset;

	method values(Rows:D(List:D) $rows, OrderBy :$order-by, Limit :$limit, Offset :$offset) {
		self.bless(:$rows, :$order-by, :$limit, :$offset);
	}
}

class Distinct::Full { ... }
class Distinct::Columns { ... }

class Distinct {
	multi submethod COERCE(Bool $value) {
		$value ?? Distinct::Full.new !! Distinct;
	}
	multi submethod COERCE(Expression::List(Cool) $columns) {
		Distinct::Columns.new($columns);
	}
}

class Distinct::Full is Distinct {}

class Distinct::Columns is Distinct {
	has Expression::List $.columns;

	method new(Expression::List $columns) {
		self.bless(:$columns);
	}
	
}

class GroupBy {
	has Expression::List:D $.columns is required handles<elems>;
	has Bool                  $.all;

	method new(Expression::List:D(Any:D) $columns, Bool $all = False) {
		self.bless(:$columns, :$all);
	}
}

class Select does Keyword::Common does Expression {
	has Expression::List(Any) $.columns;
	has Distinct(Any)    $.distinct;
	has Source(Any)      $.source;
	has Conditions(Any)  $.where;
	has GroupBy(Any)     $.group-by;
	has Conditions(Any)  $.having;
	has Windows          $.windows;
	has Compound(Pair)   $.compound;

	has OrderBy(Any)     $.order-by;
	has Limit(Any)       $.limit;
	has Offset(Any)      $.offset;
	has Locking(Any)     $.locking;

	method as(Identifier:D(Cool:D) $alias, Bool :$lateral) {
		Source::Query.new(self, $alias, :$lateral);
	}
}

role Update does Keyword::Common {
	has Table:D(Any:D)           $.target is required;
	has Source(Any)              $.from;
	has Conditions(Any)          $.where;
	has Expression::List(Any) $.returning;
}

role Updater::Pairwise {
	has Assigns:D(Cool:D) $.assigns is required;
}
class Update::Pairwise does Update does Updater::Pairwise {}

role Updater::Row {
	has Identifiers:D(Any) $.columns;
	has Row(Cool)            $.row is required;
}
class Update::Row does Update does Updater::Row {}

role Updater::Select {
	has Identifiers:D(Any) $.columns;
	has Select:D             $.select is required;
}
class Update::Select does Update does Updater::Select {}

enum Overriding <System User>;

role Conflict::Target {}

class Conflict::Target::Columns does Conflict::Target {
	has Identifiers:D $.columns is required;
	has Conditions      $.where;

	method new(Identifiers:D(Any) $columns, Conditions(Any) $where?) {
		self.bless(:$columns, :$where);
	}
}

class Conflict::Target::Constraint does Conflict::Target {
	has Identifier:D $.name is required;
	method new(Identifier:D $name) {
		self.bless(:$name);
	}
}

class Conflict::Nothing { ... }

role Conflict {
	has Conflict::Target $.target;

	multi submethod COERCE(Str $string where $string.lc eq 'nothing') {
		Conflict::Nothing.new;
	}
}

class Conflict::Nothing does Conflict {
	method new(Conflict::Target $target?) {
		self.bless(:$target);
	}
}

role Conflict::Update does Conflict {
	has Conditions(Any)   $.where;
}

class Conflict::Update::Pairwise does Conflict::Update does Updater::Pairwise {
	multi method new(Conflict::Target:D $target, Assigns:D(Cool:D) $assigns, Conditions(Any) $where?) {
		self.bless(:$target, :$where, :$assigns);
	}
	multi method new(Conflict::Target::Columns:D(Any) $target, Assigns:D(Cool:D) $assigns, Conditions(Any) $where?) {
		self.bless(:$target, :$where, :$assigns);
	}
}

class Conflict::Update::Row does Conflict::Update does Updater::Row {
	multi method new(Conflict::Target:D $target, Row:D(Cool:D) $row, Conditions(Any) $where?) {
		self.bless(:$target, :$where, :$row);
	}
}

class Conflict::Update::Select does Conflict::Update does Updater::Select {
	multi method new(Conflict::Target:D $target, Select $select, Conditions(Any) $where?) {
		self.bless(:$target, :$where, :$select);
	}
}

role Insert does Keyword::Common {
	has Table:D(Any:D)     $.target is required;
	has Identifiers(Any) $.fields;
	has Overriding         $.overriding;
	has Conflict(Any)      $.conflict;
	has Expression::List(Any) $.returning;
}

class Insert::Values does Insert {
	has Rows:D(List:D)     $.rows is required;
}

class Insert::Select does Insert {
	has Select:D $.select is required;
}

class Insert::Defaults does Insert {
}

class Delete does Keyword::Common {
	has Table:D(Any:D)           $.target is required;
	has Source(Any)              $.using;
	has Conditions(Any)          $.where;
	has Expression::List(Any) $.returning;
}

role Placeholders {
	has @.values is built(False);
	method bind(Any $value) { ... }
}

class Placeholders::DBIish does Placeholders {
	method bind(Any $value --> Str) {
		@!values.push($value);
		'?';
	}
}

class Placeholders::Postgres does Placeholders {
	method bind(Any $value --> Str) {
		@!values.push($value);
		'$' ~ +@!values;
	}
}

class Delayed {
	has Str:D $.identifier is required;
	has Any:U $.type;
	has Bool:D $.has-default = False;
	has Any $.default;

	proto method new(|args) { * }
	multi method new(Str:D $identifier, Any :$default!, Any:U :$type = $default.WHAT) {
		self.bless(:$identifier, :$type, :$default, :has-default);
	}
	multi method new(Str:D $identifier, Any:U :$type!) {
		self.bless(:$identifier, :$type);
	}
	multi method new(Str:D $identifier) {
		self.bless(:$identifier);
	}
}

class Prepared {
	has @.arguments;

	method new(@arguments) {
		self.bless(:@arguments);
	}

	multi type-of(Any $value) {
		$value.WHAT;
	}
	multi type-of(Delayed $value) {
		$value.type;
	}
	method type-hints() {
		@!arguments.map(*.&type-of);
	}

	multi resolve-value(%replacements, Any $value) {
		$value;
	}
	multi resolve-value(%replacements, Delayed $ (:$identifier, :$type, :$has-default, :$default)) {
		if %replacements{$identifier}:exists {
			%replacements{$identifier};
		} elsif $has-default {
			$default;
		} else {
			die "No value given for delayed value '$identifier'"
		}
	}
	method resolve(%replacements = {}) {
		@!arguments.map: { resolve-value(%replacements, $^element) };
	}

	method identifiers() {
		@!arguments.grep(Delayed)».identifier;
	}
}

class Result {
	has Str:D $.sql is required;
	has Any @.arguments;
	method new(Str:D $sql, @arguments) {
		self.bless(:$sql, :@arguments);
	}
}

class Result::Prepared {
	has Str:D $.sql is required;
	has Prepared:D $.prepared is required handles<resolve type-hints identifiers>;
	method new(Str:D $sql, @arguments) {
		my $prepared = Prepared.new(@arguments);
		self.bless(:$sql, :$prepared);
	}
}

role Renderer {
	method render() { ... }
}

class Renderer::SQL { ... }

role Expression::Custom does Expression {
	method render-sql(Renderer::SQL $renderer) { ... }
}

class Renderer::SQL does Renderer {
	has Placeholders $.placeholders is required;
	has Any:U $.result-class = Result;
	has Bool:D $.quoting = False;

	my sub parenthesize-if(Str $input, Bool() $parenthese --> Str) {
		$parenthese ?? "($input)" !! $input;
	}

	method render-expression-list(Placeholders $arguments, @elements, Bool $parenthesize --> Str) {
		my $precedence = @elements > 1 ?? Precedence::Comma !! Precedence::Rowlike;
		my @elems = @elements.map: { self.render-expression($arguments, $^element, $precedence) };
		parenthesize-if(@elems.join(', '), $parenthesize);
	}

	method quote-identifier(Str $identifier --> Str) {
		$!quoting ?? '"' ~ $identifier.subst('"', '""', :g) ~ '"' !! $identifier;
	}

	method render-identifier(Identifier $ident --> Str) {
		$ident.parts.map({ self.quote-identifier($^id) }).join('.');
	}

	proto method render-expression(Placeholders $placeholders, Expression $expression, Precedence $outer --> Str) {
		my $result = {*};
		parenthesize-if($result, $outer > $expression.precedence);
	}
	multi method render-expression(Placeholders $placeholders, Literal $literal, Precedence --> Str) {
		$literal.payload.subst(/ <!after '\\'> '?'/, { $placeholders.bind($literal.arguments.shift) }, :g);
	}
	multi method render-expression(Placeholders $placeholders, Placeholder $placeholder, Precedence --> Str) {
		$placeholders.bind($placeholder.value);
	}
	multi method render-expression(Placeholders $placeholders, Integer $integer, Precedence --> Str) {
		~$integer.value;
	}
	multi method render-expression(Placeholders $placeholders, Constant $constant, Precedence --> Str) {
		$constant.keyword;
	}
	multi method render-expression(Placeholders $placeholders, Parentheses $parentheses, Precedence --> Str) {
		"({ self.render-expression($placeholders, $parentheses.inner, Precedence::Comma) })";
	}
	multi method render-expression(Placeholders $placeholders, Row $row, Precedence --> Str) {
		'ROW' ~ self.render-expression-list($placeholders, $row.elements, True);
	}
	multi method render-expression(Placeholders $placeholders, Op::Cast $cast, Precedence $ --> Str) {
		"CAST({ self.render-expression($placeholders, $cast.primary, Precedence::Comma) } AS $cast.type())";
	}
	method render-when(Placeholders $placeholders, Op::Case::When $when --> List) {
		my $condition = self.render-expression($placeholders, $when.condition, Precedence::Comma);
		my $value     = self.render-expression($placeholders, $when.value, Precedence::Comma);
		'WHEN', $condition, 'THEN', $value;
	}
	multi method render-expression(Placeholders $placeholders, Op::Case $case, Precedence $ --> Str) {
		my @result = 'CASE';
		@result.append: self.render-expression($placeholders, $case.left, Precedence::Comma);
		@result.append: $case.whens.map: { self.render-when($placeholders, $^when) };
		@result.append: 'ELSE', self.render-expression($placeholders, $case.else, Precedence::Comma) with $case.else;
		@result.append: 'END';
		@result.join(' ');
	}
	multi method render-expression(Placeholders $placeholders, Function $function, Precedence --> Str) {
		$function.name ~ self.render-expression-list($placeholders, $function.arguments, True);
	}
	multi method render-expression(Placeholders $placeholders, Op::Unary::Prefix::Tight $op, Precedence --> Str) {
		my $primary = self.render-expression($placeholders, $op.value, Precedence::TightPrefix);
		$op.operator ~ $primary;
	}
	multi method render-expression(Placeholders $placeholders, Op::Binary $op, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $op.left, $op.precedence);
		my $right = self.render-expression($placeholders, $op.right, $op.precedence);
		"$left $op.operator() $right";
	}
	multi method render-expression(Placeholders $placeholders, Op::Unary::Postfix $op, Precedence --> Str) {
		my $value = self.render-expression($placeholders, $op.value, Precedence::Postfix);
		"$value $op.postfix()";
	}
	multi method render-expression(Placeholders $placeholders, Op::Unary::Prefix::Loose $op, Precedence --> Str) {
		my $primary = self.render-expression($placeholders, $op.value, Precedence::LoosePrefix);
		"$op.operator() $primary";
	}
	multi method render-expression(Placeholders $placeholders, Op::ListCompare $in, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $in.left, $in.precedence);
		my $candidates = self.render-expression-list($placeholders, $in.elements, True);
		"$left $in.operator() $candidates";
	}
	multi method render-expression(Placeholders $placeholders, Op::BetweenLike $between, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $between.left, Precedence::Between);
		my $min = self.render-expression($placeholders, $between.min, Precedence::Between);
		my $max = self.render-expression($placeholders, $between.max, Precedence::Between);
		my @not = $between.negated ?? 'NOT' !! Empty;
		($left, |@not, 'BETWEEN', $min, 'AND', $max).join(' ');
	}
	multi method render-expression(Placeholders $placeholders, Op::Logical $logical, Precedence --> Str) {
		my @elems = $logical.elements.map: { self.render-expression($placeholders, $^element, $logical.precedence) };
		@elems.join(" $logical.operator() ");
	}
	multi method render-expression(Placeholders $placeholders, Identifier $column, Precedence --> Str) {
		self.render-identifier($column);
	}
	multi method render-expression(Placeholders $placeholders, Expression::Renamed $column, Precedence --> Str) {
		my $input = self.render-expression($placeholders, $column.source, Precedence::Comma);
		my $output = self.render-identifier($column.alias);
		"$input AS $output";
	}
	multi method render-expression(Placeholders $placeholders, Star, Precedence --> '*') {}

	multi method render-window-definition(Placeholders $placeholders, Window::Definition $definition --> Str) {
		my @inner;
		@inner.push: self.render-expression($placeholders, $definition.existing) with $definition.existing;
		@inner.push: self.render-column-expressions($placeholders, $definition.columns, 'PARTITION BY');
		@inner.push: self.render-order-by($placeholders, $definition.order-by);
		"({ @inner.join(' ') })";
	}
	multi method render-window-definition(Placeholders $placeholders, Window::Definition $definition where .is-simple --> Str) {
		self.render-expression($placeholders, $definition.existing, Precedence::Comma);
	}

	multi method render-expression(Placeholders $placeholders, Function::Window $column, Precedence --> Str) {
		my @result;
		@result.push: self.render-expression($placeholders, $column.function, Precedence::Comma);
		@result.push: self.render-conditions($placeholders, $column.filter, 'FILTER WHERE');
		@result.push: 'OVER';
		@result.push: self.render-window-definition($placeholders, $column.window);
		@result.join(' ');
	}

	multi method render-distinct(Placeholders $placeholders, Distinct::Columns:D $source --> List) {
		'DISTINCT ON', self.render-expression-list($placeholders, $source.columns.elems, True);
	}
	multi method render-distinct(Placeholders $placeholders, Distinct::Full:D $source, --> List) {
		'DISTINCT',;
	}
	multi method render-distinct(Placeholders $placeholders, Distinct:U $source --> List) {
		Empty;
	}

	multi method render-column-expressions(Placeholders $placeholders, Expression::List:D $columns, Str $prefix?, Bool :$parenthesize --> List) {
		my @elems = $columns.elems.map: { self.render-expression($placeholders, $^column, Precedence::Comma) };
		my $results = parenthesize-if(@elems.join(', '), $parenthesize);
		$prefix ?? ($prefix, $results) !! ($results,);
	}
	multi method render-column-expressions(Placeholders $placeholders, Expression::List:U $columns, Str $prefix?, Bool :$parenthesize --> List) {
		Empty;
	}

	multi method render-identifiers(Identifiers:D $columns, Bool :$parenthesize --> List) {
		my @elems = $columns.elems.map: { self.render-identifier($^column) };
		parenthesize-if(@elems.join(', '), $parenthesize),;
	}
	multi method render-identifiers(Identifiers:U $columns, Bool :$parenthesize --> List) {
		Empty;
	}

	multi method render-source-alias(Source::Aliased $source) {
		my $alias = self.render-identifier($source.alias);
		my @columns = self.render-identifiers($source.columns, :parenthesize);
		'AS', $alias, |@columns;
	}

	multi method render-table(Table $table --> List) {
		my $name = self.render-identifier($table.name);
		$table.only ?? ('ONLY', $name) !! $name,;
	}
	multi method render-table(Table::Renamed $renamed--> List) {
		my @result;
		@result.push: 'ONLY' if $renamed.only;
		@result.push: self.render-identifier($renamed.name);
		@result.push: self.render-source-alias($renamed);
		@result;
	}

	multi method render-source-nested(Placeholders $placeholders, Source::Query $query --> Str) {
		self.render-expression($placeholders, $query.keyword, Precedence::Rowlike);
	}
	multi method render-source-nested(Placeholders $placeholders, Source::Function $function --> Str) {
		my $result = self.render-expression($placeholders, $function.function, Precedence::Rowlike);
		my @ordinality = $function.ordinal ?? 'WITH ORDINALITY' !! Empty;
		$result, |@ordinality;
	}

	multi method render-source(Placeholders $, Table $table --> List) {
		self.render-table($table);
	}
	multi method render-source(Placeholders $placeholders, Source::Nested $nested --> List) {
		my @lateral = $nested.lateral ?? 'LATERAL' !! Empty;
		my $rendered = self.render-source-nested($placeholders, $nested);
		|@lateral, "($rendered)", self.render-source-alias($nested);
	}

	multi method render-join-type(Placeholders $placeholders, Join::Natural $join) {
		'NATURAL', $join.type.uc, 'JOIN';
	}
	multi method render-join-type(Placeholders $placeholders, Join::Cross $join) {
		'CROSS JOIN';
	}
	multi method render-join-type(Placeholders $placeholders, Join $join) {
		$join.type.uc, 'JOIN';
	}

	multi method render-join-condition(Placeholders $placeholders, Join::On $join) {
		self.render-conditions($placeholders, $join.on, 'ON');
	}
	multi method render-join-condition(Placeholders $placeholders, Join::Using $join) {
		'USING', self.render-identifiers($join.using, :parenthesize);
	}
	multi method render-join-condition(Placeholders $placeholders, Join $join) {
		Empty;
	}

	multi method render-source(Placeholders $placeholders, Join $join --> List) {
		my @result;
		@result.push: self.render-source($placeholders, $join.left);
		@result.push: self.render-join-type($placeholders, $join);
		@result.push: self.render-source($placeholders, $join.right);
		@result.push: self.render-join-condition($placeholders, $join);
		@result;
	}

	multi method render-from(Placeholders $placeholders, Source:D $source, Str $prefix = 'FROM') {
		$prefix, self.render-source($placeholders, $source);
	}
	multi method render-from(Placeholders $placeholders, Source:U $source, Str = Str) {
		Empty;
	}

	method render-common-table(Placeholders $placeholders, Common::Rename $rename --> List) {
		my $alias      = self.render-identifier($rename.alias.name);
		my @columns    = self.render-identifiers($rename.alias.columns, :parenthesize);
		my $expression = self.render-expression($placeholders, $rename.keyword, Precedence::Comma);
		$alias, 'AS', $expression;
	}

	multi method render-common-tables(Placeholders $placeholders, Common:D $common --> List) {
		my @recursive   = $common.recursive ?? 'RECURSIVE' !! Empty;
		my @expressions = $common.tables.map({ self.render-common-table($placeholders, $^table) });
		'WITH', |@recursive, @expressions.join(', ');
	}
	multi method render-common-tables(Placeholders $placeholders, Common:U --> List) {
		Empty;
	}

	multi method render-conditions(Placeholders $placeholders, Conditional:D $conditions, Str $type = 'WHERE' --> List) {
		$type, self.render-expression($placeholders, $conditions.expression, Precedence::And);
	}
	multi method render-conditions(Placeholders $placeholders, Conditional:U $conditions, Str $type = Str --> List) {
		Empty;
	}

	multi method render-expression(Placeholders $placeholders, Order::Modifier $column, Precedence --> Str) {
		"{ self.render-expression($placeholders, $column.column, Precedence::Comma) } $column.order()";
	}

	multi method render-group-by(Placeholders $placeholders, GroupBy:D $group-by, Conditions $conditions --> List) {
		my @result = 'GROUP BY';
		@result.push: 'ALL' if $group-by.all;
		@result.push: self.render-expression-list($placeholders, $group-by.elems, False);
		@result.push: self.render-conditions($placeholders, $conditions, 'HAVING');
		@result;
	}
	multi method render-group-by(Placeholders $placeholders, GroupBy:U $columns, Conditions $having --> List) {
		Empty;
	}

	method render-window(Placeholders $placeholders, Window $window --> Str) {
		"WINDOW $window.name() AS { self.render-window-definition($window.definition) }";
	}

	multi method render-windows(Placeholders $placeholders, Windows:D $windows --> List) {
		$windows.windows.map({ self.render-window($placeholders, $^window) }).join(', ');
	}
	multi method render-windows(Placeholders $placeholders, Windows:U $windows --> List) {
		Empty;
	}

	multi method render-compound(Placeholders $placeholders, Compound:D $compound --> List) {
		my @type = $compound.all ?? 'ALL' !! Empty;
		my $next = self.render-query($placeholders, $compound.next);
		$compound.type.uc, |@type, $next;
	}
	multi method render-compound(Placeholders $placeholders, Compound:U --> List) {
		Empty;
	}

	multi method render-order-by(Placeholders $placeholders, OrderBy:D $sorters --> List) {
		my @elems = $sorters.elems.map: { self.render-expression($placeholders, $^column, Precedence::Comma) };
		'ORDER BY', @elems.join(', ');
	}
	multi method render-order-by(Placeholders $placeholders, OrderBy:U $sorters --> List) {
		Empty;
	}

	multi method render-limit-offset(Placeholders $placeholders, Limit $limit, Offset $offset --> List) {
		my @result;
		@result.push: 'LIMIT',  self.render-expression($placeholders, $limit.value,  Precedence::LoosePrefix) with $limit;
		@result.push: 'OFFSET', self.render-expression($placeholders, $offset.value, Precedence::LoosePrefix) with $offset;
		@result;
	}

	multi method render-locking(Locking:D $locking --> List) {
		my @result = 'FOR', $locking.strength;
		@result.push('OF', self.render-identifier($locking.table)) with $locking.table;
		@result.push('NOWAIT') if $locking.no-wait;
		@result.push('SKIP LOCKED') if $locking.skip-locked;
		@result;
	}
	multi method render-locking(Locking:U --> List) {
		Empty;
	}

	method render-query(Placeholders $placeholders, Select $select --> Str) {
		my @parts = 'SELECT';
		@parts.append: self.render-distinct($placeholders, $select.distinct);
		@parts.append: self.render-column-expressions($placeholders, $select.columns);
		@parts.append: self.render-from($placeholders, $select.source);
		@parts.append: self.render-conditions($placeholders, $select.where);
		@parts.append: self.render-group-by($placeholders, $select.group-by, $select.having);
		@parts.append: self.render-compound($placeholders, $select.compound);
		@parts.append: self.render-windows($placeholders, $select.windows);
		@parts.join(' ');
	}

	multi method render-expression(Placeholders $placeholders, Select $select, Precedence --> Str) {
		my @parts;
		@parts.append: self.render-common-tables($placeholders, $select.common-tables);
		@parts.append: self.render-query($placeholders, $select);
		@parts.append: self.render-order-by($placeholders, $select.order-by);
		@parts.append: self.render-limit-offset($placeholders, $select.limit, $select.offset);
		@parts.append: self.render-locking($select.locking);
		@parts.join(' ');
	}

	multi method render-update-values(Placeholders $placeholders, Updater::Pairwise $update) {
		my @expressions = $update.assigns.pairs.map: -> (:$key, :$value) { Op::Assign.new($key, $value) }
		'SET', self.render-expression-list($placeholders, @expressions, False);
	}
	multi method render-update-values(Placeholders $placeholders, Updater::Row $update) {
		my $columns = self.render-identifiers($update.columns, :parenthesize);
		my $expression = self.render-expression($placeholders, $update.row, Precedence::Rowlike);
		"SET $columns = $expression";
	}
	multi method render-update-values(Placeholders $placeholders, Updater::Select $update) {
		my $columns = self.render-identifiers($update.columns, :parenthesize);
		my $select  = self.render-expression($placeholders, $update.select, Precedence::Assignment);
		"SET $columns = $select";
	}

	multi method render-expression(Placeholders $placeholders, Update $update, Precedence --> Str) {
		my @common       = self.render-common-tables($placeholders, $update.common-tables);
		my @target       = self.render-table($update.target);
		my @set          = self.render-update-values($placeholders, $update);
		my @from         = self.render-from($placeholders, $update.from);
		my @where        = self.render-conditions($placeholders, $update.where);
		my @returning    = self.render-column-expressions($placeholders, $update.returning, 'RETURNING');

		(@common, 'UPDATE', @target, @set, @from, @where, @returning).flat.join(' ');
	}

	multi method render-overriding(Overriding:D $override) {
		'OVERRIDING', $override.uc, 'VALUE';
	}
	multi method render-overriding(Overriding:U $override) {
		Empty;
	}

	method render-rows(Placeholders $placeholders, Rows $rows) {
		my @values = $rows.elems.map({ self.render-expression-list($placeholders, $^row.elements, True) });
		'VALUES', @values.join(', ');
	}

	multi method render-insert-content(Placeholders $placeholders, Insert::Values $insert) {
		self.render-rows($placeholders, $insert.rows);
	}
	multi method render-insert-content(Placeholders $placeholders, Insert::Select $insert) {
		self.render-expression($placeholders, $insert.select, Precedence::Rowlike);
	}
	multi method render-insert-content(Placeholders $placeholders, Insert::Defaults $insert) {
		'DEFAULT VALUES';
	}

	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target::Columns:D $target) {
		my $columns = self.render-identifiers($target.columns, :parenthesize);
		my $where = self.render-conditions($placeholders, $target.where);
		$columns, |@$where;
	}
	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target::Constraint:D $target) {
		'ON CONSTRAINT', self.render-identifier($target.name);
	}
	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target:U $conflict) {
		Empty;
	}

	multi method render-conflict(Placeholders $placeholders, Conflict::Update:D $conflict) {
		my @result = 'ON CONFLICT';
		@result.push: self.render-conflict-target($placeholders, $conflict.target);
		@result.push: 'DO UPDATE';
		@result.push: self.render-update-values($placeholders, $conflict);
		@result.push: self.render-conditions($placeholders, $conflict.where);
		@result;
	}
	multi method render-conflict(Placeholders $placeholders, Conflict::Nothing $conflict) {
		my @result = 'ON CONFLICT';
		@result.push: self.render-conflict-target($placeholders, $conflict.target);
		@result.push: 'DO NOTHING';
		@result;
	}
	multi method render-conflict(Placeholders $placeholders, Conflict:U $conflict) {
		Empty;
	}

	multi method render-expression(Placeholders $placeholders, Insert $insert, Precedence --> Str) {
		my @common     = self.render-common-tables($placeholders, $insert.common-tables);
		my @target     = self.render-table($insert.target);
		my @fields     = self.render-identifiers($insert.fields, :parenthesize);
		my @overriding = self.render-overriding($insert.overriding);
		my @content    = self.render-insert-content($placeholders, $insert);
		my @conflict   = self.render-conflict($placeholders, $insert.conflict);
		my @returning  = self.render-column-expressions($placeholders, $insert.returning, 'RETURNING');

		(@common, 'INSERT INTO', @target, @fields, @overriding, @content, @conflict, @returning).flat.join(' ');
	}

	multi method render-expression(Placeholders $placeholders, Delete $delete, Precedence) {
		my @common    = self.render-common-tables($placeholders, $delete.common-tables);
		my @target    = self.render-table($delete.target);
		my @using     = self.render-from($placeholders, $delete.using, 'USING');
		my @where     = self.render-conditions($placeholders, $delete.where);
		my @returning = self.render-column-expressions($placeholders, $delete.returning, 'RETURNING');

		(@common, 'DELETE FROM', @target, @using, @where, @returning).flat.join(' ');
	}

	multi method render-expression(Placeholders $placeholders, Values $values, Precedence --> Str) {
		my @rows     = $values.render-rows($placeholders, $values.rows);
		my @order-by = self.render-order-by($placeholders, $values.order-by);
		my @limits   = self.render-limit-offset($placeholders, $values.limit, $values.offset);

		(@rows, @order-by, @limits).flat.join(' ');
	}

	method render(Expression $expression, :$result-class = $!result-class) {
		my $placeholders = self.placeholders.new;
		my $sql          = self.render-expression($placeholders, $expression, Precedence::Rowlike);
		$result-class.new($sql, $placeholders.values);
	}
}

has Renderer:D $.renderer is required;

multi submethod BUILD(Renderer:D :$!renderer!) {}
multi submethod BUILD(Renderer::SQL:U :$renderer = Renderer::SQL, *%arguments) {
	$!renderer = $renderer.new(|%arguments);
}

method select(Source:D(Any:D) $source, |arguments) {
	my $select = $source.select(|arguments);
	$!renderer.render($select);
}

method insert(Table:D(Any:D) $target, |arguments) {
	my $insert = $target.insert(|arguments);
	$!renderer.render($insert);
}

method update(Table:D(Any:D) $target, |arguments) {
	my $update = $target.update(|arguments);
	$!renderer.render($update);
}

method delete(Table:D(Any:D) $target, |arguments) {
	my $delete = $target.delete(|arguments);
	$!renderer.render($delete);
}

method values(Rows:D $rows, OrderBy :$order-by, Limit :$limit, Offset :$offset) {
	my $values = Values.new(:$rows, :$order-by, :$limit, :$offset);
	$!renderer.render($values);
}

multi method table(Table:D(Any:D) $table) {
	$table;
}
multi method table(Table:D(Any:D) $table, Identifier:D(Cool:D) :$as!) {
	$table.as($as);
}
multi method table(Table:D(Any:D) $table, Pair :$as!) {
	$table.as($as.key, $as.value);
}

method value(Placeholder(Any) $value) {
	$value;
}

method begin() {
	'BEGIN', [];
}

method rollback() {
	'ROLLBACK',  [];
}

method commit() {
	'COMMIT', [];
}

=begin pod

=head1 Name

SQL::Abstract - Generate SQL from Raku data structures

=head1 Synopsis

=begin code :lang<raku>

use SQL::Abstract;

my $placeholders = SQL::Abstract::Placeholders::Postgres;
my $abstract = SQL::Abstract.new(:$placeholders);
my ($sql, @arguments) = $abstract.select('table', <foo bar>, :id(3));
my $result = $dbh.query($sql, @arguments);

my $join = $abstract.table('books').join('authors', :using<author_id>);
my ($sql, $arguments) = $abstract.select($join, [<book name>, <author name>], { <book cost> => { '<' => 10 }});

=end code

=head1 Description

SQL::Abstract abstracts the generation of SQL queries. Fundamentally its functionality is 

It should 

=head1 Argument types

SQL::Abstract uses

=head2 SQL::Abstract::Source

=begin item2
Str

This will select from the named table, e.g. C<"my_table">.
=end item2

=begin item2
List

This will select from the named table, with the elements of the list representing the components of the table name. E.g. C<< <bar baz> >> is equivalent to C< "bar.baz" >.
=end item2

=begin item2
Pair

This will select from the table in the pair value, and rename it to the key of the pair (C<value AS key>).
=end item2

It also has a C<select> method that works like C<SQL::Abstract>'s select

=head2 SQL::Abstract::Table

This class 

=head2 Identifiers

=begin item2
Str

This will fetch the given column
=end item2

=begin item2
List

This will fetch the given columns. The values can be anything in this 
=end item2

=begin item2
Whatever

This will fetch all columns
=end item2

=begin item2
Pair

This will fetch the given column, and if the value of the pair is not a boolean it will rename the column (C<value AS key>). If the value is boolean it's equivalent to the key as string.

=end item2

=begin item2
Hash

This will function as a list of pairs, fetch all keys as columns and rename them to their value if applicable.
=end item2

=head2 Expression::List

This works as a 

=head2 Conditions

=head1 Class SQL::Abstract

=head2 new(:$placeholders!)

This creates a new C<SQL::Abstract> object. It has one mandatory name argument, C<$placeholders>, which takes one of the following values:

=begin item1
SQL::Abstract::Placeholders::DBIish

This will use DBI style C<(?, ?)> for placeholders
=end item1

=begin item1
SQL::Abstract::Placeholders::Postgres

This will use Postgres style C<($1, $2)> for placeholders.
=end item1

=head2 select(Source() $source, Expression::List() $columns?, Conditions() $where?, Bool :$distinct, Expression::List() :$group-by, Conditions() :$having, OrderBy() :$order-by, Int :$limit, Int :$offset, :$locking)

This will generate a C<SELECT> query.

=begin item1
SQL::Abstract::Source(Any:D) $source

The source of the selection. If it's not already a C<Source> object (e.g. a C<Table> or C<Join> object) it will convert from the following types:

=end item1

=begin item1
SQL::Abstract::Expression::List(Any:D) $columns = *

This will contain the requested columns. It 

=end item1

=begin item1
Conditions(Any) $where?

This will be the C<WHERE> conditions.

=end item1

=begin item1
Bool :$distinct


=end item1

=begin item1
SQL::Abstract::Expression::List(Any) :$group-by,

Group by the listed columns. They're interpreted the same as C<$columns> is interpreted.
=end item1

=begin item1
Conditions(Any) :$having?, 

This will be the C<HAVING> conditions. it works described below in the Conditions section. This only makes sense to use when using C<$group-by>.
=end item1

=begin item1
OrderBy(Any) :$order-by,

This sorts the rows by the listed columns. This is interpreted the same as C<$columns>, except C<*> isn't allowed, simple numbers may be interpreted as column numbers by your database, and it also allows one to use C<SQL::Abstract::Order::Modifier::Asc>/C<SQL::Abstract::Order::Desc> to specify the sorting order.
=end item1

=begin item1
Int :$limit

Limit the number of rows returned by the database.
=end item1

=begin item1
Int :$offset

Sets an offset for the returned values. This only makes sense if C<$limit> is also set.
=end item1

=begin item1
SQL::Abstract::Locking(Any) :$locking

This sets the locking clause of the query. Reasonably portable values for this include:

=item2 C<'update'>/C<SQL::Abstract::Locking::Update>
=item2 C<'share'>/C<SQL::Abstract::Locking::Share>

=end item1

=head2 update(Table(Cool) $target, Assigns(Cool) $set, Conditions(Any) $where?, Expression::List(Any) :$returning) {

=begin item1
Table(Cool) $target

This works
=end item1

=begin item1
Assigns(Cool) $set

=end item1

=begin item1
Conditions(Any) $where?, 

=end item1

=begin item1
Expression::List(Any) :$returning

=end item1

=head2 insert(Table(Cool) $target, Expression::List(Any) $fields, Rows(Any) $rows, Expression::List(Any) :$returning)

=head2 insert(Table(Cool) $target, Assigns(Hash) $values, Expression::List(Any) :$returning) {

=head2 delete(Table(Cool) $target, Conditions(Any) $where?, Expression::List(Any) :$returning) {

=head1 Conditions

=head1 Author

Leon Timmermans <fawaka@gmail.com>

=head1 Copyright and License

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
