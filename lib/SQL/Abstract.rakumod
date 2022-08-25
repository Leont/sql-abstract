unit class SQL::Abstract;

use fatal;

enum Precedence <Rowlike Comma Not And Or Assignment Between In LoosePrefix Postfix Comparative Additive Multiplicative Concatlike TightPrefix Termlike>;

class Op::Unary::Not { ... }

role Expression {
	method precedence(--> Precedence) { ... }

	method negate(--> Op) {
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

role Value does Term {
}

class Integer does Value {
	has Int $.value;

	method new(Int $value) {
		self.bless(:$value);
	}
}

class Placeholder does Value {
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

class Value::Default does Constant['DEFAULT'] {}
class Value::True    does Constant['TRUE']    {}
class Value::False   does Constant['FALSE']   {}
class Value::Null    does Constant['NULL']    {}

my multi expand-expression(Expression $expression) {
	$expression;
}
my multi expand-expression(Any:D $value) {
	Placeholder.new($value);
}
my multi expand-expression(Capture $literal) {
	my ($sql, @arguments) = |$literal;
	Literal.new($sql, @arguments);
}

class Ident {
	has Str @.parts;

	proto method new(|) { * }
	multi method new(@parts) {
		self.bless(:@parts);
	}
	multi method new(Cool $name) {
		my @parts = $name.split('.');
		self.bless(:@parts);
	}

	method concat(Ident:D $other) {
		my @merged = |@!parts, |$other.parts;
		self.new(@merged);
	}

	my sub quote(Str $part) {
		'"' ~ $part.subst('"', '""', :g) ~ '"';
	}
	method Str() {
		@!parts.map(&quote).join('.');
	}
	method WHICH() {
		ValueObjAt.new("SQL::Abstract::Ident|{ self }");
	}
}

class Row does Expression {
	method precedence(--> Precedence::Rowlike) {}

	has Expression @.elements;

	method new(Expression @elements) {
		self.bless(:@elements);
	}

	method COERCE(@values) {
		my Expression @expressions = @values».&expand-expression;
		Row.new(@expressions);
	}
}

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

class Op::IsNotNull { ... }
class Op::IsNull does Op::Unary::Postfix['IS NULL'] {
	method negate() {
		Op::IsNotNull.new($!value);
	}
}
class Op::IsNotNull does Op::Unary::Postfix['IS NOT NULL'] {
	method negate() {
		Op::IsNull.value($!value);
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
	method operator(--> 'NOT') { ... }

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

role Op::Concatenative does Op::Binary {
	method precedence(--> Precedence::Concatlike) {}
}

class Op::Concat does Op::Concatenative {
	method operator(--> '||') {}
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

class Op::Binary::Additive does Op::Binary::Other[Precedence::Additive] { }
class Op::Binary::Multiplicative does Op::Binary::Other[Precedence::Multiplicative] { }

role Op::BetweenLike does Expression {
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
class Op::Or does Op::Logical['OR', Precedence::Or, Value::False] {}

role Op::ListCompare[Str $operator] does Op::List {
	method precedence(--> Precedence::In) {}
	method operator(--> Str) { $operator }

	has Expression:D $.left is required;

	method new(Expression $left, @elements) {
		self.bless(:$left, :@elements);
	}
}

class Op::NotIn { ... }
class Op::In does Op::ListCompare['IN'] {
	method negate() {
		Op::NotIn.new($!left, @!elements);
	}
}
class Op::NotIn does Op::ListCompare['NOT IN'] {
	method negate() {
		Op::In.new($!left, @!elements);
	}
}

class Op::Cast does Term {
	has Expression:D $.primary is required;
	has Str:D $.typename is required;

	method new(Expression:D $primary, Str:D $typename) {
		self.bless(:$primary, :$typename);
	}
}

class Rows {
	has Row @.elems;

	method COERCE(@input) {
		my Row(Any) @elems = @input;
		self.new(:@elems);
	}
}

class Column::Named { ... }
class Column::Renamed { ... }
class Column::All { ... }
class Column::Expression { ... }
class Column {
	multi submethod COERCE(Cool $column) {
		Column::Named.new($column);
	}
	multi submethod COERCE(Pair $pair) {
		Column::Renamed.COERCE($pair);
	}
	multi submethod COERCE(Pair $ (:$key, Bool :$value)) {
		Column::Named.new($key);
	}
	multi submethod COERCE(Whatever) {
		Column::All.new;
	}
	multi submethod COERCE(Expression $expression) {
		Column::Expression.new($expression);
	}
}

class Sorter::Modifier { ... }
class Sorter {
	multi submethod COERCE(Cool $ident) {
		Column::Named.new($ident);
	}
	multi submethod COERCE(Pair $pair) {
		Sorter::Modifier.COERCE($pair);
	}

	method asc() {
		Sorter::Modifier.new(self, 'asc');
	}
	method desc() {
		Sorter::Modifier.new(self, 'desc');
	}
}

class Column::Renamable is Column {
	method as(Column::Named(Cool) $alias) {
		Column::Renamed.new(self, $alias);
	}
}

class Column::Named is Column::Renamable is Sorter does Term {
	has Ident:D $.name is required handles<Str>;

	method new(Ident(Cool) $name) {
		self.bless(:$name);
	}

	method WHICH() {
		ValueObjAt.new("SQL::Abstract::Column::Named|{ self }");
	}
}

class Column::Renamed is Column is Sorter {
	has Column::Named:D $.alias is required;
	has Column:D $.source is required;

	proto method new(|) { * }
	multi method new(Column $source, Column::Named(Cool) $alias) {
		self.bless(:$source, :$alias);
	}
	multi method new(Column::Named(Cool) $source, Column::Named(Cool) $alias) {
		self.bless(:$source, :$alias);
	}

	method COERCE(Pair $ (Column::Named(Cool) :$key, Column(Any) :$value)) {
		Column::Renamed.new($value, $key);
	}
}

class Column::All is Column does Term {
	method WHICH() {
		ValueObjAt.new("Column::All|*");
	}
}
multi expand-expression(Whatever) {
	Column::All.new;
}

class Column::Expression is Column::Renamable is Sorter does Expression {
	has Expression:D $.expression is required handles<precedence>;

	method new(Expression $expression) {
		self.bless(:$expression);
	}
}

role Column::List {
	has Column:D @.elems is required;
}

class Column::Expressions does Column::List {
	method new(@elems) {
		self.bless(:@elems);
	}

	sub to-column(Column(Any) $column) {
		$column;
	}
	multi method COERCE(Column(Any) $column) {
		self.new([$column]);
	}
	multi method COERCE(@list) {
		my Column(Any) @columns = @list;
		self.new(@columns);
	}
	multi method COERCE(%columns) {
		samewith(%columns.sort(*.key));
	}

	multi method merge(Column::Expressions $other) {
		self.new(|@!elems, |$other.elems);
	}
}

class Column::Names does Column::List {
	method new(@elems) {
		self.bless(:@elems);
	}

	sub to-column(Column(Any) $column) {
		$column;
	}
	multi method COERCE(Column::Named(Any) $column) {
		self.new([$column]);
	}
	multi method COERCE(@list) {
		my Column::Named(Any) @columns = @list;
		self.new(@columns);
	}
	multi method COERCE(%columns) {
		samewith(%columns.sort(*.key));
	}

	multi method merge(Column::Names $other) {
		self.new(|@!elems, |$other.elems);
	}
}

class Sorter::Modifier is Sorter {
	my enum Order (:Asc<ASC> :Desc<DESC>);
	has Column:D $.column is required;
	has Order:D $.order is required;

	proto method new(|) { * }
	multi method new(Column(Cool) $column, Order $order) {
		self.bless(:$column, :$order);
	}
	multi method new(Column(Cool) $column, Str $order-name) {
		my $order = Order.WHO{$order-name.lc.tc};
		self.bless(:$column, :$order);
	}

	method COERCE(Pair $pair) {
		self.new($pair.key, $pair.value);
	}
}

class Sorters {
	has Sorter:D @.elems is required;

	method new(@elems) {
		self.bless(:@elems);
	}

	multi method COERCE(Sorter:D(Any:D) $sorter) {
		Sorters.new([$sorter]);
	}
	multi method COERCE(@list) {
		my Sorter(Any) @sorters = @list;
		Sorters.new(@sorters);
	}
}

class Window::Definition {
	has Column::Named $.existing;
	has Column::Expressions $.columns;
	has Sorters $.order-by;
}

class Column::Window is Column::Renamable {
	has Function:D $.function is required;
	has Window::Definition:D $.window is required;

	method new(Function:D(Cool:D) $function, Column::Expressions(Any) :$columns, Sorters(Any) :$order-by, Column::Named :$existing) {
		my $window = Window::Definition.new(:$existing, :$columns, :$order-by);
		self.bless(:$function, :$window);
	}
}

class Window {
	has Column::Named:D $.name is required;
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

class Assigns {
	has Pair @.pairs;

	sub transform-pair(Pair $ (:$key, :$value)) {
		my $column = Column::Named.new($key);
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
			Op::Comperative::Custom.new($key, $left, $expression);
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
		Op::And.pack(@partials».negate);
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

	my multi expand-condition(Pair $ (Column::Named(Cool) :$key, Mu :$value)) {
		expand-partial($key, $value);
	}
	my multi expand-condition(Pair $ (Expression :$key, Mu :$value)) {
		expand-partial($key, $value);
	}

	multi method COERCE(Expression $expression) {
		Conditions.bless(:$expression);
	}
	multi method COERCE(Pair $pair) {
		Conditions.new(expand-condition($pair));
	}
	multi method COERCE(@list) {
		Conditions.new(@list».&expand-condition);
	}
	multi method COERCE(%hash) {
		samewith(%hash.sort);
	}
}

class Table { ... }
class Join::On { ... }
class Join::On::Conditions { ... }
class Join::Using { ... }
class Source::Renamed { ... }
class Select { ... }

class Source does Expression {
	method precedence(--> Precedence::Rowlike) {}

	multi submethod COERCE(Cool $pre-source) {
		Table.new($pre-source);
	}
	multi submethod COERCE(Pair $pair) {
		Source::Renamed.COERCE($pair);
	}

	multi method join(Source:D(Any:D) $right, Table(Cool) :$as, Join::On::Conditions(Any) :$on!, *%arguments) {
		my $new-right = $as ?? $right.as($as) !! $right;
		Join::On.new(self, $new-right, $on, |%arguments);
	}
	multi method join(Source:D(Any:D) $right, Table(Cool) :$as, Column::Names(Any) :$using!, *%arguments) {
		my $new-right = $as ?? $right.as($as) !! $right;
		Join::Using.new(self, $new-right, $using, |%arguments);
	}

	method select(Column::Expressions:D(Any:D) $columns = *, Conditions(Any) $where?, *%arguments) {
		Select.new(:$columns, :source(self), :$where, |%arguments);
	}

	method as(Table:D(Cool:D) $alias, Column::Names(Any) $columns?) {
		Source::Renamed.new(self, $alias, $columns);
	}
}

class Insert::Values { ... }
class Insert::Select { ... }
class Insert::Defaults { ... }
class Update::Pairwise { ... }
class Update::Row      { ... }
class Update::Select { ... }
class Delete { ... }

class Table is Source {
	has Ident:D $.name is required handles<Str>;

	method new(Ident(Cool:D) $name) {
		self.bless(:$name);
	}

	multi method update(Assigns:D(Any:D) $set, Conditions(Any) $where?, *%arguments) {
		Update::Pairwise.new(:target(self), :$set, :$where, |%arguments);
	}
	multi method update(Column:D(Any:D) $columns, Row $row, Conditions(Any) $where?, *%arguments) {
		Update::Row.new(:target(self), :$columns, :expressions($row.elements), :$where, |%arguments);
	}
	multi method update(Column::Names:D(Any:D) $columns, Select:D $select, Conditions(Any) $where?, *%arguments) {
		Update::Select.new(:target(self), :$columns, :$select, :$where, |%arguments);
	}

	multi method insert(Column::Names(Any) $fields, Rows:D(List:D) $rows, *%arguments) {
		Insert::Values.new(:target(self), :$fields, :$rows, |%arguments);
	}
	multi method insert(Assigns:D(Cool:D) $values, *%arguments) {
		samewith($values.keys, [$values.values,], |%arguments);
	}
	multi method insert(Column::Names(Any) $fields, Select $select, *%arguments) {
		Insert::Select.new(:target(self), :$fields, :$select, |%arguments);
	}
	multi method insert(Value::Default, *%arguments) {
		Insert::Defaults.new(:target(self), |%arguments);
	}

	method delete(Conditions(Any) $where?, *%arguments) {
		Delete.new(:target(self), :$where, |%arguments);
	}

	multi method concat(Column::Named $column) {
		Column::Named.new($!name.concat($column.name));
	}
	multi method concat(Column::Named @names) {
		@names.map: { self.concat($^name) };
	}

	method WHICH() {
		ValueObjAt.new("SQL::Abstract::Table|{ self }");
	}
}

class Source::Renamed is Source {
	has Table:D $.alias is required;
	has Source:D $.source is required;
	has Column::Names $.columns;

	method new(Source:D $source, Table:D(Cool:D) $alias, Column::Names(Any) $columns?) {
		self.bless(:$source, :$alias, :$columns);
	}

	method COERCE(Pair $ (:$key, Source(Any) :$value)) {
		Source::Renamed.new($value, $key);
	}
}

class Join is Source {
	enum Type <Inner Left Right Outer>;

	has Source:D $.left is required;
	has Source:D $.right is required;
	has Type:D $.type = Type::Inner;
	has Bool $.lateral = False;

}

class Join::On::Conditions does Conditional {
	sub expand(Pair $pair (Column::Named(Cool) :$key, Column::Named(Cool) :$value)) {
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

	method new(Source(Any) $left, Source(Any) $right, Join::On::Conditions(Any:D) $on, Join::Type :$type = Join::Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :$on, :$type, :$lateral);
	}
}

class Join::Using is Join {
	has Column::Names $.using;
	method new(Source(Any) $left, Source(Any) $right, Column::Names(Any) $using, Join::Type :$type = Join::Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :$using, :$type, :$lateral);
	}
}

class Locking {
	enum Strength (:Update<UPDATE> :NoKeyUpdate('NO KEY UPDATE') :Share<SHARE> :KeyShare('KEY SHARE'));

	has Strength:D $.strength is required;
	has Table $.table;
	has Bool $.no-wait;
	has Bool $.skip-locked;

	method new(Strength:D $strength, Table(Cool) $table?, Bool :$no-wait, Bool :$skip-locked) {
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

role Keyword does Expression {
	method precedence() { Precedence::Rowlike }
}

class Common { ... }

role Command does Keyword {
	has Common(Any)      $.common-tables;
}

class Values does Keyword {
	has Rows:D  $.rows is required;
	has Sorters $.order-by;
	has Limit   $.limit;
	has Offset  $.offset;
}

class SubQuery is Source {
	has Command:D       $.command is required;
	has Table:D(Cool:D) $.alias is required;

	method new(Command:D $command, Table:D(Cool:D) $alias) {
		self.bless(:$command, :$alias);
	}
}

class Common {
	class Rename {
		has Table:D $.alias is required;
		has Keyword:D $.keyword is required;
		method new(Table:D(Cool:D) $alias, Keyword:D $keyword) {
			self.bless(:$alias, :$keyword);
		}
		method COERCE(Pair $pair) {
			self.new($pair.key, $pair.value);
		}
	}
	has Rename @.tables;
	has Bool $.recursive;

	method new(Source::Renamed @tables, Bool :$recursive) {
		self.bless(:@tables, :$recursive);
	}

	method COERCE(@pairs) {
		my Rename(Pair) @tables = @pairs;
		self.bless(:@tables);
	}
}

class Distinct::All { ... }
class Distinct::Expressions { ... }

class Distinct {
	multi submethod COERCE(Bool $value) {
		$value ?? Distinct::All.new !! Distinct;
	}
	multi submethod COERCE(Column::Names(Cool) $columns) {
		my Expression @expressions = $columns.elems;
		Distinct::Expressions.new(@expressions);
	}
}

class Distinct::All is Distinct {}

class Distinct::Expressions is Distinct {
	has Expression @.expressions;

	method new(Expression @expressions) {
		self.bless(:@expressions);
	}
	
}

class Select does Command does Expression {
	has Column::Expressions:D(Any:D) $.columns is required;
	has Distinct(Any)    $.distinct;
	has Source:D(Any:D)  $.source is required;
	has Conditions(Any)  $.where;
	has Column::Expressions(Any)     $.group-by;
	has Conditions(Any)  $.having;
	has Windows          $.windows;
	has Compound(Pair)   $.compound;

	has Sorters(Any)     $.order-by;
	has Limit(Any)       $.limit;
	has Offset(Any)      $.offset;
	has Locking(Any)     $.locking;

	method as(Table:D(Cool:D) $alias) {
		SubQuery.new(self, $alias);
	}
}

class Conflict::Nothing { ... }
class Conflict::Update  { ... }
class Conflict {
	role Target {
	}
	class Target::Columns does Target {
		has Column::Names:D $.columns is required;
		has Conditions      $.where;

		method new(Column::Names:D(Any) $columns, Conditions(Any) $where?) {
			self.bless(:$columns, :$where);
		}
	}
	class Target::Constraint does Target {
		has Column::Named:D $.name is required;
		method new(Column::Named:D $name) {
			self.bless(:$name);
		}
	}
	has Target $.target;

	multi submethod COERCE(Str $string where $string.lc eq 'nothing') {
		Conflict::Nothing.new;
	}
}

class Conflict::Nothing is Conflict {
	method new(Conflict::Target $target?) {
		self.bless(:$target);
	}
}

class Conflict::Update is Conflict {
	has Assigns:D(Cool:D) $.assigns is required;
	has Conditions(Any)   $.where;
	multi method new(Conflict::Target:D $target, Assigns:D(Cool:D) $assigns, Conditions(Any) $where?) {
		self.bless(:$target, :$assigns, :$where);
	}
	multi method new(Conflict::Target::Columns:D(Any) $target, Assigns:D(Cool:D) $assigns, Conditions(Any) $where?) {
		self.bless(:$target, :$assigns, :$where);
	}
}

role Insert does Command {
	has Table:D(Cool:D)    $.target is required;
	has Table(Cool)        $.as;
	has Column::Names(Any) $.fields;
	has Conflict(Any)      $.conflict;
	has Column::Expressions(Any)       $.returning;
}

class Insert::Values does Insert {
	has Rows:D(List:D)     $.rows is required;
}

class Insert::Defaults does Insert {
}

class Insert::Select does Insert {
	has Select:D $.select is required;
}

role Update does Command {
	has Table:D(Cool:D)   $.target is required;
	has Bool              $.only;
	has Table(Cool)       $.as;
	has Source(Any)       $.from;
	has Conditions(Any)   $.where;
	has Column::Expressions(Any)      $.returning;
}

class Update::Pairwise does Update {
	has Assigns:D(Cool:D) $.set is required;
}

class Update::Row does Update {
	has Column::Names:D(Any) $.columns;
	has Row(Any)             $.row is required;
}

class Update::Select does Update {
	has Column::Names:D(Any) $.columns;
	has Select:D             $.select is required;
}

class Delete does Command {
	has Table:D(Cool:D) $.target is required;
	has Bool            $.only;
	has Table(Cool)     $.as;
	has Source(Any)     $.using;
	has Conditions(Any) $.where;
	has Column::Expressions(Any)    $.returning;
}

role Placeholders {
	has @.values;
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

role Renderer {
	method render-keyword() { ... }
}

class Renderer::SQL { ... }

role Expression::Custom does Expression {
	method render-sql(Renderer::SQL $renderer) { ... }
}

class Renderer::SQL does Renderer {
	has Placeholders:U $.placeholders is required;
	has Bool:D $.quoting = False;

	my sub parenthesize-if(Str $input, Bool() $parenthese --> Str) {
		$parenthese ?? "($input)" !! $input;
	}

	method render-list(Placeholders $arguments, @elements, Bool $parenthesize --> Str) {
		my $precedence = @elements > 1 ?? Precedence::Comma !! Precedence::Rowlike;
		my @elems = @elements.map: { self.render-expression($arguments, $^element, $precedence) };
		parenthesize-if(@elems.join(', '), $parenthesize);
	}

	method quote-identifier(Str $identifier --> Str) {
		$!quoting ?? '"' ~ $identifier.subst('"', '""', :g) ~ '"' !! $identifier;
	}

	method render-identifier(Ident $ident --> Str) {
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
	multi method render-expression(Placeholders $placeholders, Row $row, Precedence --> Str) {
		self.render-list($placeholders, $row.elements, True);
	}
	multi method render-expression(Placeholders $placeholders, Column $column, Precedence $outer --> Str) {
		self.render-column($placeholders, $column);
	}
	multi method render-expression(Placeholders $placeholders, Op::Cast $cast, Precedence $ --> Str) {
		"CAST({ self.render-expression($placeholders, $cast.primary, Precedence::Comma) } AS $cast.type())";
	}
	multi method render-expression(Placeholders $placeholders, Function $function, Precedence --> Str) {
		$function.name ~ self.render-list($placeholders, $function.arguments, True);
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
		my $candidates = self.render-list($placeholders, $in.elements, True);
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
	multi method render-expression(Placeholders $placeholders, Command $command, Precedence $outer --> Str) {
		self.render-keyword-expression($placeholders, $command);
	}

	multi method render-distinct(Placeholders $placeholders, Distinct::Expressions:D $source) {
		'DISTINCT ON', self.render-list($placeholders, $source.expressions, True);
	}
	multi method render-distinct(Placeholders $placeholders, Distinct::All:D $source) {
		'DISTINCT';
	}
	multi method render-distinct(Placeholders $placeholders, Distinct:U $source) {
		Empty;
	}

	method render-window-definition(Placeholders $placeholders, Window::Definition $definition) {
		my @inner;
		@inner.push: self.render-column($placeholders, $definition.existing) with $definition.existing;
		@inner.push: self.render-columns($placeholders, $definition.columns, 'PARTITION BY');
		@inner.push: self.render-order-by($placeholders, $definition.order-by);
		"({ @inner.join(' ') })";
	}

	multi method render-column(Placeholders $placeholders, Column::Named $column --> Str) {
		self.render-identifier($column.name);
	}
	multi method render-column(Placeholders $placeholders, Column::Renamed $column --> Str) {
		my $input = self.render-column($placeholders, $column.source);
		my $output = self.render-column($placeholders, $column.alias);
		"$input AS $output";
	}
	multi method render-column(Placeholders $placeholders, Column::All --> '*') {}
	multi method render-column(Placeholders $placeholders, Column::Expression $ (:$expression) --> Str) {
		self.render-expression($placeholders, $expression, Precedence::Comma);
	}
	multi method render-column(Placeholders $placeholders, Column::Window $column --> Str) {
		my $window-function = self.render-expression($placeholders, $column.function, Precedence::Comma);
		my $window-definition = self.render-window-definition($placeholders, $column.window);
		"$window-function OVER $window-definition";
	}

	multi method render-columns(Placeholders $placeholders, Column::List:D $columns, Str $prefix?, Bool :$parenthesize --> List) {
		my @elems = $columns.elems.map: { self.render-column($placeholders, $^column) };
		my $results = parenthesize-if(@elems.join(', '), $parenthesize);
		$prefix ?? ($prefix, $results) !! ($results,);
	}
	multi method render-columns(Placeholders $placeholders, Column::List:U $columns, Str $prefix?, Bool :$parenthesize --> List) {
		Empty;
	}

	method render-table(Table $source --> Str) {
		self.render-identifier($source.name);
	}

	multi method render-source(Placeholders $, Table $table --> Str) {
		self.render-table($table);
	}
	multi method render-source(Placeholders $placeholders, SubQuery $query --> Str) {
		my $rendered = self.render-keyword-expression($placeholders, $query.query);
		"($rendered) AS ", self.render-table($query.alias);
	}
	multi method render-source(Placeholders $placeholders, Source::Renamed $renamed--> Str) {
		my $source = self.render-source($placeholders, $renamed.source);
		my $alias  = self.render-table($renamed.alias);
		"$source AS $alias";
	}
	multi method render-join-condition(Placeholders $placeholders, Join::On $join) {
		self.render-conditions($placeholders, $join.on, 'ON');
	}
	multi method render-join-condition(Placeholders $placeholders, Join::Using $join) {
		self.render-columns($placeholders, $join.using, 'USING', :parenthesize);
	}
	multi method render-source(Placeholders $placeholders, Join $join --> List) {
		my $left    = self.render-source($placeholders, $join.left);
		my $right   = self.render-source($placeholders, $join.right);
		my $type    = $join.type.uc;
		my @lateral = $join.lateral ?? 'LATERAL' !! Empty;
		my @on      = self.render-join-condition($placeholders, $join);
		$left, $type, 'JOIN', |@lateral, $right, |@on;
	}

	method render-common-table(Placeholders $placeholders, Common::Rename $rename --> List) {
		my $alias      = self.render-table($rename.alias);
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

	method render-row(Placeholders $placeholders, Row $row --> Str) {
		self.render-list($placeholders, $row.elements, True);
	}

	multi method render-conditions(Placeholders $placeholders, Conditional:D $conditions, Str $type = 'WHERE' --> List) {
		$type, self.render-expression($placeholders, $conditions.expression, Precedence::And);
	}
	multi method render-conditions(Placeholders $placeholders, Conditional:U $conditions, Str $type = Str --> List) {
		Empty;
	}

	multi method render-sorter(Placeholders $placeholders, Column $column --> Str) {
		self.render-column($placeholders, $column);
	}
	multi method render-sorter(Placeholders $placeholders, Sorter::Modifier $column --> Str) {
		"{ self.render-column($placeholders, $column.column) } $column.order()";
	}

	multi method render-group-by(Placeholders $placeholders, Column::Expressions:D $columns, Conditions $conditions --> List) {
		my @group-by = self.render-columns($placeholders, $columns, 'GROUP BY');
		my @having   = self.render-conditions($placeholders, $conditions, 'HAVING');
		|@group-by, |@having;
	}
	multi method render-group-by(Placeholders $placeholders, Column::Expressions:U $columns, Conditions $having --> List) {
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

	multi method render-order-by(Placeholders $placeholders, Sorters:D $sorters --> List) {
		my @elems = $sorters.elems.map: { self.render-sorter($placeholders, $^column) };
		'ORDER BY', @elems.join(', ');
	}
	multi method render-order-by(Placeholders $placeholders, Sorters:U $sorters --> List) {
		Empty;
	}

	multi method render-limit-offset(Placeholders $placeholders, Limit $limit, Offset $offset --> List) {
		my @result;
		@result.push: 'LIMIT',  self.render-expression($placeholders, $limit.value,  Precedence::Termlike) with $limit;
		@result.push: 'OFFSET', self.render-expression($placeholders, $offset.value, Precedence::Termlike) with $offset;
		@result;
	}

	multi method render-locking(Locking:D $locking --> List) {
		my @result = 'FOR', $locking.strength;
		@result.push('OF', self.render-table($locking.table)) with $locking.table;
		@result.push('NOWAIT') if $locking.no-wait;
		@result.push('SKIP LOCKED') if $locking.skip-locked;
		@result;
	}
	multi method render-locking(Locking:U --> List) {
		Empty;
	}

	method render-query(Placeholders $placeholders, Select $select --> Str) {
		my @parts;
		@parts.append: 'SELECT';
		@parts.append: self.render-distinct($placeholders, $select.distinct);
		@parts.append: self.render-columns($placeholders, $select.columns);
		@parts.append: self.render-from($placeholders, $select.source);
		@parts.append: self.render-conditions($placeholders, $select.where);
		@parts.append: self.render-group-by($placeholders, $select.group-by, $select.having);
		@parts.append: self.render-compound($placeholders, $select.compound);
		@parts.append: self.render-windows($placeholders, $select.windows);
		@parts.join(' ');
	}

	multi method render-keyword-expression(Placeholders $placeholders, Select $select --> Str) {
		my @parts;
		@parts.append: self.render-common-tables($placeholders, $select.common-tables);

		@parts.append: self.render-query($placeholders, $select);

		@parts.append: self.render-order-by($placeholders, $select.order-by);
		@parts.append: self.render-limit-offset($placeholders, $select.limit, $select.offset);
		@parts.append: self.render-locking($select.locking);
		@parts.join(' ');
	}

	multi method render-as(Table:D $alias --> List) {
		'AS', self.render-table($alias);
	}
	multi method render-as(Table:U $alias --> List) {
		Empty;
	}

	method render-set(Placeholders $placeholders, Assigns:D $assigns) {
		my @expressions = $assigns.pairs.map: -> (:$key, :$value) { Op::Assign.new($key, $value) }
		my $set         = self.render-list($placeholders, @expressions, False);
		'SET', $set
	}

	multi method render-update-values(Placeholders $placeholders, Update::Pairwise $update) {
		self.render-set($placeholders, $update.set);
	}
	multi method render-update-values(Placeholders $placeholders, Update::Row $update) {
		my $columns = self.render-columns($placeholders, $update.columns, True);
		my $expression = self.render-row($placeholders, $update.row);
		"SET $columns = $expression";
	}
	multi method render-update-values(Placeholders $placeholders, Update::Select $update) {
		my $columns = self.render-columns($placeholders, $update.columns, :parenthesize);
		my $select  = self.render-expression($placeholders, $update.select, Precedence::Assignment);
		"SET $columns = $select";
	}

	multi method render-from(Placeholders $placeholders, Source:D $source, Str $prefix = 'FROM') {
		$prefix, self.render-source($placeholders, $source);
	}
	multi method render-from(Placeholders $placeholders, Source:U $source, Str = Str) {
		Empty;
	}

	multi method render-returning(Placeholders $placeholders, Column::Expressions:D $returning --> List) {
		self.render-columns($placeholders, $returning, 'RETURNING');
	}
	multi method render-returning(Placeholders $placeholders, Column::Expressions:U $returning --> List) {
		Empty;
	}

	multi method render-keyword-expression(Placeholders $placeholders, Update $update) {
		my @common       = self.render-common-tables($placeholders, $update.common-tables);
		my $target       = self.render-table($update.target);
		my @only         = $update.only ?? 'ONLY' !! Empty;
		my @as           = self.render-as($update.as);
		my @set          = self.render-update-values($placeholders, $update);
		my @from         = self.render-from($placeholders, $update.from);
		my @where        = self.render-conditions($placeholders, $update.where);
		my @returning    = self.render-returning($placeholders, $update.returning);

		(@common, 'UPDATE', @only, $target, @as, @set, @from, @where, @returning).flat.join(' ');
	}

	method render-rows(Placeholders $placeholders, Rows $rows) {
		my @values = $rows.elems.map({ self.render-row($placeholders, $^row) });
		@values.join(', ');
	}

	multi method render-insert-content(Placeholders $placeholders, Insert::Values $insert) {
		'VALUES', self.render-rows($placeholders, $insert.rows);
	}
	multi method render-insert-content(Placeholders $placeholders, Insert::Select $insert) {
		self.render-keyword-expression($placeholders, $insert.select);
	}
	multi method render-insert-content(Placeholders $placeholders, Insert::Defaults $insert) {
		'DEFAULT VALUES';
	}

	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target::Columns:D $target) {
		my $columns = self.render-columns($placeholders, $target.columns, :parenthesize);
		my $where = self.render-conditions($placeholders, $target.where);
		$columns, |@$where;
	}
	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target::Constraint:D $target) {
		'ON CONSTRAINT', self.render-column($target.name);
	}
	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target:U $conflict) {
		Empty;
	}

	multi method render-conflict(Placeholders $placeholders, Conflict::Update:D $conflict) {
		my @result = 'ON CONFLICT';
		@result.push: self.render-conflict-target($placeholders, $conflict.target);
		@result.push: 'DO UPDATE';
		@result.push: self.render-set($placeholders, $conflict.assigns);
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

	multi method render-keyword-expression(Placeholders $placeholders, Insert $insert) {
		my @common       = self.render-common-tables($placeholders, $insert.common-tables);
		my $target       = self.render-table($insert.target);
		my @as           = self.render-as($insert.as);
		my @fields       = self.render-columns($placeholders, $insert.fields, :parenthesize);
		my $content      = self.render-insert-content($placeholders, $insert);
		my @conflict     = self.render-conflict($placeholders, $insert.conflict);
		my @returning    = self.render-returning($placeholders, $insert.returning);

		(@common, 'INSERT INTO', $target, @as, @fields, $content, @conflict, @returning).flat.join(' ');
	}

	multi method render-keyword-expression(Placeholders $placeholders, Delete $delete) {
		my @common       = self.render-common-tables($placeholders, $delete.common-tables);
		my @only         = $delete.only ?? 'ONLY' !! Empty;
		my $target       = self.render-table($delete.target);
		my @as           = self.render-as($delete.as);
		my @using        = self.render-from($placeholders, $delete.using);
		my @where        = self.render-conditions($placeholders, $delete.where);
		my @returning    = self.render-returning($placeholders, $delete.returning);

		(@common, 'DELETE FROM', @only, $target, @as, @using, @where, @returning).flat.join(' ');
	}

	multi method render-keyword-expression(Placeholders $placeholders, Values $values) {
		my $rows     = $values.render-rows($placeholders, $values.rows);
		my @order-by = self.render-order-by($placeholders, $values.order-by);
		my @limits   = self.render-limit-offset($placeholders, $values.limit, $values.offset);

		('VALUES', $rows, @order-by, @limits).flat.join(' ');
	}

	method render-keyword(Command $command) {
		my $placeholders = self.placeholders.new;
		my $sql          = self.render-keyword-expression($placeholders, $command);
		$sql, $placeholders.values;
	}
}

has Renderer:D $.renderer is required;

multi submethod BUILD(Renderer:D :$!renderer!) {}
multi submethod BUILD(Renderer::SQL:U :$renderer = Renderer::SQL, *%arguments) {
	$!renderer = $renderer.new(|%arguments);
}

method select(Source:D(Any:D) $source, |arguments) {
	my $select = $source.select(|arguments);
	$!renderer.render-keyword($select);
}

method insert(Table:D(Cool:D) $target, |arguments) {
	my $insert = $target.insert(|arguments);
	$!renderer.render-keyword($insert);
}

method update(Table:D(Cool:D) $target, |arguments) {
	my $update = $target.update(|arguments);
	$!renderer.render-keyword($update);
}

method delete(Table:D(Cool:D) $target, |arguments) {
	my $delete = $target.delete(|arguments);
	$!renderer.render-keyword($delete);
}

multi method table(Table:D(Cool:D) $table) {
	$table;
}
multi method table(Table:D(Cool:D) $table, Table:D(Cool:D) :$as!) {
	$table.as($as);
}
multi method table(Table:D(Cool:D) $table, Pair :$as!) {
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

=end code

=head1 Description

SQL::Abstract abstracts the generation of SQL queries. Fundamentally its functionality is 

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

=head2 select($source, $columns?, $where?, :$distinct, :$group-by, :$having, :$order-by, :$limit, :$offset, :$locking)

This will generate a C<SELECT> query.

=begin item1
SQL::Abstract::Table(Any:D) $source

=begin item2
Str

This will select from the named table
=end item2

=begin item2
List

This will select from the named table

=end item2

=begin item2
Pair

This will select from the table in the pair value, and rename it to the key of the pair (C<value AS key>).
=end item2

=end item1

=begin item1
SQL::Abstract::Column::Expressions(Any:D) $columns = *

This will contain the requested columns. It 

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

=begin item2
SQL::Abstract::Column

This will fetch the given column.
=end item2

=end item1

=begin item1
Conditions(Any) $where?

This will be the C<WHERE> conditions, it works as described below in the Conditions section.

=end item1

=begin item1
Bool :$distinct

=end item1

=begin item1
SQL::Abstract::Column::Expressions(Any) :$group-by,

Group by the listed columns. They're interpreted the same as C<$columns> is interpreted.
=end item1

=begin item1
Conditions(Any) :$having?, 

This will be the C<HAVING> conditions. it works described below in the Conditions section. This only makes sense to use when using C<$group-by>.
=end item1

=begin item1
Sorters(Any) :$order-by,

This sorts the rows by the listed columns. This is interpreted the same as C<$columns>, except C<*> isn't allowed, simple numbers may be interpreted as column numbers by your database, and it also allows one to use C<SQL::Abstract::Sorter::Modifier::Asc>/C<SQL::Abstract::Sorter::Desc> to specify the sorting order.
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

This sets the locking clause of the query. Portable values for this include:

=item2 C<'update'>/C<SQL::Abstract::Locking::Update>
=item2 C<'share'>/C<SQL::Abstract::Locking::Share>

=end item1

=head2 update(Table(Cool) $target, Assigns(Cool) $set, Conditions(Any) $where?, Column::Expressions(Any) :$returning) {

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
Column::Expressions(Any) :$returning

=end item1

=head2 insert(Table(Cool) $target, Column::Expressions(Any) $fields, Rows(Any) $rows, Column::Expressions(Any) :$returning)

=head2 insert(Table(Cool) $target, Assigns(Hash) $values, Column::Expressions(Any) :$returning) {

=head2 delete(Table(Cool) $target, Conditions(Any) $where?, Column::Expressions(Any) :$returning) {

=head1 Conditions

=head1 Author

Leon Timmermans <fawaka@gmail.com>

=head1 Copyright and License

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
