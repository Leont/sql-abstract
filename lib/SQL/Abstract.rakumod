unit class SQL::Abstract;

use fatal;

enum Precedence <Rowlike Comma Not And Or Assignment Between In Prefix Comparative Additive Multiplicative Postfix TermLike>;

class Op::Unary::Not { ... }
role Expression {
	method precedence(--> Precedence) { ... }

	method negate(--> Op) {
		Op::Unary::Not.new(self);
	}
}

role Term does Expression {
	method precedence(--> Precedence::TermLike) {}
}

class Literal does Expression {
	has Precedence $.precedence = Precedence::TermLike;
	has Str:D $.payload is required;
	has Any:D @.arguments;

	method new(Str:D $payload, @arguments) {
		self.bless(:$payload, :@arguments);
	}
}

class Value does Term {
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
	Value.new($value);
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

	sub quote(Str $part) {
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

	method new(@elements) {
		self.bless(:@elements);
	}

	method COERCE(@values) {
		Row.new(@values».&expand-expression);
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

role Op::Unary::Prefix does Op::Unary {
	method precedence(--> Precedence::Prefix) {}
	method operator(--> Str:D) { ... }
}

class Op::Unary::Not does Op::Unary::Prefix {
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

role Op::Logical[Str $operator, Precedence $precedence] does Op::List {
	method precedence(--> Precedence) { $precedence }
	method operator(--> Str) { $operator }
	method empty(--> Expression) { ... }

	method new(@elements) {
		self.bless(:@elements);
	}
}
class Op::And does Op::Logical['AND', Precedence::And] {
	method empty() { Value::True }
}
class Op::Or does Op::Logical['OR', Precedence::Or] {
	method empty() { Value::False }
}

role Op::ListCompare[Str $operator] does Op::List {
	method precedence(--> Precedence::In) {}
	method operator(--> Str) { $operator }

	has Expression:D $.left is required;

	method new(Expression $left, Expression @elements) {
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

class Table { ... }
class Join { ... }
class Source::Renamed { ... }
class Source does Expression {
	method precedence(--> Precedence::Rowlike) {}

	multi submethod COERCE(Cool $pre-source) {
		Table.new($pre-source);
	}
	multi submethod COERCE(Pair $ (:$key, Source(Any) :$value)) {
		Source::Renamed.new($value, $key);
	}

	method join(|arguments) {
		Join.new(self, |arguments);
	}
}

class Source::Singular is Source {
	method as(Table:D(Cool:D) $alias) {
		Source::Renamed.new(self, $alias);
	}
}

class Table is Source::Singular {
	has Ident:D $.name is required handles<Str>;

	method new(Ident(Cool:D) $name) {
		self.bless(:$name);
	}
	method WHICH() {
		ValueObjAt.new("SQL::Abstract::Table|{ self }");
	}
}

class Source::Renamed is Source {
	has Table:D $.alias is required;
	has Source::Singular:D $.source is required;

	method new(Source::Singular $source, Table:D(Cool:D) $alias) {
		self.bless(:$source, :$alias);
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
	multi submethod COERCE(Pair $ (Column::Named(Cool) :$key, Column(Any) :$value)) {
		Column::Renamed.new($value, $key);
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

role Command {}

class Sorter::Modifier { ... }
class Sorter {
	multi submethod COERCE(Cool $ident) {
		Column::Named.new($ident);
	}
	multi submethod COERCE(Pair $pair) {
		Sorter::Modifier.new($pair.key, $pair.value);
	}
}

class Column::Renamable is Column {
	method as(Column::Named(Cool) $alias) {
		Column::Renamed.new(self, $alias);
	}
}

class Column::Named is Column::Renamable is Sorter does Term {
	has Ident:D $.ident is required handles<Str>;

	method new(Ident(Cool) $ident) {
		self.bless(:$ident);
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

class Columns {
	has Column:D @.elems is required;

	method new(@elems) {
		self.bless(:@elems);
	}

	sub to-column(Column(Any) $column) {
		$column;
	}
	multi method COERCE(Column(Any) $column) {
		Columns.new([$column]);
	}
	multi method COERCE(@list) {
		my Column(Any) @columns = @list;
		Columns.new(@columns);
	}
	multi method COERCE(%columns) {
		samewith(%columns.sort(*.key));
	}

	multi method merge(Columns $other) {
		self.new(|@!elems, |$other.elems);
	}
}

class Sorter::Modifier is Sorter {
	my enum Order <Asc Desc>;
	has Column:D $.column is required;
	has Order:D $.order is required;

	multi method new(Column(Cool) $column, Order $order) {
		self.bless(:$column, :$order);
	}
	multi method new(Column(Cool) $column, Str $order-name) {
		my $order = Order.WHO{$order-name.lc.tc};
		self.bless(:$column, :$order);
	}

	method modifier() { $!order.uc }
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

class Column::Window is Column::Renamable {
	has Function:D $.function is required;
	has Columns $.columns;
	has Sorters $.order-by;

	method new(Function:D(Cool:D) $function, Columns(Any) :$columns, Sorters(Any) :$order-by) {
		self.bless(:$function, :$columns, :$order-by);
	}
}

my sub pack-expressions(Op::Logical ::Class, @expressions) {
	given @expressions.elems {
		when 1  { @expressions[0] }
		when 0  { Class.empty.new ; }
		default { Class.new(@expressions) }
	}
}

sub unpack-expression(Op::Logical ::Class, Expression $expression) {
	given $expression {
		when Class       { $expression.elements }
		when Class.empty { Empty }
		default          { $expression }
	}
}

role Conditional {
	has Expression:D $.expression is required;

	proto method new(|) { * }
	multi method new(@expressions) {
		my $expression = pack-expressions(Op::And, @expressions);
		self.bless(:$expression);
	}
	multi method new(Expression $expression) {
		self.bless(:$expression);
	}

	multi method merge(Conditional:D: Conditional:D $other) {
		my @left = unpack-expression(Op::And, $!expression);
		my @right = unpack-expression(Op::And, $other.expression);
		my $expression = pack-expressions(Op::And, flat(@left, @right));
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
	role Partial {
		method resolve(Column::Named:D $name) { ... }
	}
	class Partial::Simple does Partial {
		has Expression:U $.class is required;
		has Capture:D $.arguments is required;

		method new(Expression:U $class, |capture) {
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

	my multi expand-partial(Any:D $expression) {
		Partial::Simple.new(Op::Equals, expand-expression($expression));
	}
	my multi expand-partial(Any:U $expression) {
		Partial::Simple.new(Op::IsNull);
	}

	my multi expand-pair('isnull', $value) {
		Partial::Simple.new(Op::IsNull);
	}
	my multi expand-pair('isnotnull', $value) {
		Partial::Simple.new(Op::IsNotNull);
	}
	my $binary-like = any(%binary-op-for.keys);
	my multi expand-pair(Str $key where $key eq $binary-like, $value) {
		Partial::Simple.new(%binary-op-for{$key}, expand-expression($value));
	}
	my multi expand-partial(Pair $ (:$key, :$value)) {
		expand-pair($key, $value);
	}

	my multi expand-partial(%hash) {
		%hash.sort(*.key)».&expand-partial;
	}
	my multi expand-partial(Range $range) {
		my $min = Value.new($range.min);
		my $max = Value.new($range.max);
		Partial::Simple.new(Op::Between, $min, $max);
	}

	subset PartialEquals of Partial::Simple where .class ~~ Op::Equals && .arguments.elems == 1;

	my multi expand-junction('any', @partials) {
		Partial::Custom.new: -> $name { pack-expressions(Op::Or, @partials».resolve($name)) };
	}
	my multi expand-junction('any', @partials where all(@partials) ~~ PartialEquals) {
		my Expression @values = @partials».arguments»[0];
		Partial::Simple.new(Op::In, @values);
	}
	my multi expand-junction('all', @partials) {
		Partial::Custom.new: -> $name { pack-expressions(Op::And, @partials».resolve($name)) }
	}
	my multi expand-junction('none', @partials) {
		Partial::Custom.new: -> $name { pack-expressions(Op::And, @partials».resolve($name)».negate) }
	}
	my multi expand-junction('none', @partials where all(@partials) ~~ PartialEquals) {
		my Expression @values = @partials».arguments»[0];
		Partial::Simple.new(Op::NotIn, @values);
	}
	my multi expand-junction('one', @partials) {
		Partial::Custom.new: -> $name {
			my @comparisons = @partials».resolve;
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
	multi method COERCE(Pair $pair) {
		Conditions.new(expand-condition($pair));
	}
	multi method COERCE(@list) {
		Conditions.new(@list».&expand-condition.flat);
	}
	multi method COERCE(%hash) {
		Conditions.new(%hash.sort».&expand-condition.flat);
	}
}

class Join is Source {
	enum Type <Inner Left Right Outer>;

	class On does Conditional {
		sub expand(Pair $pair (Column::Named(Cool) :$key, Column::Named(Cool) :$value)) {
			Op::Equals.new($key, $value);
		}
		multi method COERCE(Conditions $conditions) {
			On.new($conditions.expression);
		}
		multi method COERCE(%hash) {
			On.new(%hash.sort(*.key)».&expand);
		}
	}

	has Source:D $.left is required;
	has Source:D $.right is required;
	has On $.on;
	has Columns $.using;
	has Type:D $.type = Type::Inner;
	has Bool $.lateral;

	proto method new(|) { * }
	multi method new(Source(Any) $left, Source(Any) $right, Columns(Any) :$using!, Type :$type = Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :$using, :$type, :$lateral);
	}
	multi method new(Source(Any) $left, Source(Any) $right, On(Any:D) :$on!, Type :$type = Type::Inner, Bool :$lateral = False) {
		self.bless(:$left, :$right, :$on, :$type, :$lateral);
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

	multi method COERCE(Str $name) {
		my $strength = Strength.WHO{$name.lc.tc};
		self.new($strength);
	}
	multi method COERCE(Pair $ (:$key, :$value)) {
		my $strength = Strength.WHO{$key.lc.tc};
		self.new($strength, $value);
	}
}

class Common {
	has Source::Renamed @.tables;
	has Bool $.recursive;

	method new(Source::Renamed @tables) {
		self.bless(:@tables);
	}

	method COERCE(@pairs) {
		my Source(Pair) @tables = @pairs;
		self.bless(:@tables);
	}
}

class Select { ... }

class Union {
	has Bool $.all;
	has Select $.next;

	method new(Select $next, Bool $all = False) {
		self.bless(:$next, :$all);
	}
}

class Select is Source::Singular does Command does Expression {
	has Common(Any)      $.common-tables;
	has Columns:D(Any:D) $.columns = *;
	has Bool             $.distinct;
	has Source:D(Any:D)  $.source is required;
	has Conditions(Any)  $.where;
	has Columns(Any)     $.group-by;
	has Conditions(Any)  $.having;
	has Union(Select)    $.union;
	has Sorters(Any)     $.order-by;
	has Int              $.limit;
	has Int              $.offset;
	has Locking(Any)     $.locking;
}

class Insert does Command {
	has Table:D(Cool:D) $.target is required;
	has Columns(Any)    $.fields;
	has Rows(Any)       $.rows;
	has Columns(Any)    $.returning;
}

class Update does Command {
	has Table:D(Cool:D) $.target is required;
	has Assigns:D(Hash:D) $.set is required;
	has Conditions(Any) $.where;
	has Columns(Any) $.returning;
}

class Delete does Command {
	has Table:D(Cool:D) $.target is required;
	has Conditions(Any) $.where;
	has Columns(Any) $.returning;
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
	multi method new(Str:D $identifier) {
		self.bless(:$identifier);
	}
	multi method new(Str:D $identifier, Any:U :$type!) {
		self.bless(:$identifier, :$type);
	}
	multi method new(Str:D $identifier, Any :$default!, Any:U :$type = $default.WHAT) {
		self.bless(:$identifier, :$type, :$default, :has-default);
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
	method render-select() { ... }
	method render-insert() { ... }
	method render-update() { ... }
	method render-delete() { ... }
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
		my @elems = @elements.map: { self.render-expression($arguments, $^element, Precedence::Comma) };
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
	multi method render-expression(Placeholders $placeholders, Literal $ (:$payload, :@arguments, :$precedence), Precedence --> Str) {
		$payload.subst(/ <!after '\\'> '?'/, { $placeholders.bind(@arguments.shift) }, :g);
	}
	multi method render-expression(Placeholders $placeholders, Value $ (:$value), Precedence --> Str) {
		$placeholders.bind($value);
	}
	multi method render-expression(Placeholders $placeholders, Constant $constant, Precedence --> Str) {
		$constant.keyword;
	}
	multi method render-expression(Placeholders $placeholders, Row $ (:@elements), Precedence --> Str) {
		self.render-list($placeholders, @elements, True);
	}
	multi method render-expression(Placeholders $placeholders, Function $ (:$name, :@arguments), Precedence --> Str) {
		$name ~ self.render-list($placeholders, @arguments, True);
	}
	multi method render-expression(Placeholders $placeholders, Op::Unary::Postfix $op, Precedence --> Str) {
		my $value = self.render-expression($placeholders, $op.value, Precedence::Postfix);
		"$value $op.postfix()";
	}
	multi method render-expression(Placeholders $placeholders, Op::Unary::Prefix $op, Precedence --> Str) {
		my $primary = self.render-expression($placeholders, $op.value, Precedence::Prefix);
		"$op.operator() $primary";
	}
	multi method render-expression(Placeholders $placeholders, Op::Binary $op, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $op.left, $op.precedence);
		my $right = self.render-expression($placeholders, $op.right, $op.precedence);
		"$left $op.operator() $right";
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
	multi method render-expression(Placeholders $placeholders, Op::Cast $cast, Precedence $ --> Str) {
		"CAST({ self.render-expression($placeholders, $cast.primary, Precedence::Comma) } AS $cast.type())";
	}
	multi method render-expression(Placeholders $placeholders, Select $select, Precedence $outer --> Str) {
		"({self.render-select-expression($placeholders, $select)})";
	}
	multi method render-expression(Placeholders $placeholders, Column $column, Precedence $outer --> Str) {
		self.render-column($placeholders, $column);
	}

	multi method render-column(Placeholders $placeholders, Column::Named $ (:$ident)--> Str) {
		self.render-identifier($ident);
	}
	multi method render-column(Placeholders $placeholders, Column::Renamed $ (:$source, :$alias) --> Str) {
		my $input = self.render-column($placeholders, $source);
		my $output = self.render-column($placeholders, $alias);
		"$input AS $output";
	}
	multi method render-column(Placeholders $placeholders, Column::All --> '*') {}
	multi method render-column(Placeholders $placeholders, Column::Expression $ (:$expression) --> Str) {
		self.render-expression($placeholders, $expression, Precedence::Comma);
	}
	multi method render-column(Placeholders $placeholders, Column::Window $ (:$function, :$columns, :$order-by) --> Str) {
		my $window-function = self.render-expression($placeholders, $function, Precedence::Comma);
		my @inner;
		@inner.push: 'PARTITION BY', self.render-columns($placeholders, $columns) with $columns;
		@inner.push: self.render-order-by($placeholders, $order-by);
		"$window-function OVER ({ @inner.join(' ') })";
	}

	method render-columns(Placeholders $placeholders, Columns $columns, Bool $parenthesized = False --> Str) {
		my @elems = $columns.elems.map: { self.render-column($placeholders, $^column) };
		parenthesize-if(@elems.join(', '), $parenthesized);
	}

	multi method render-sorter(Placeholders $placeholders, Column $column --> Str) {
		self.render-column($placeholders, $column);
	}
	multi method render-sorter(Placeholders $placeholders, Sorter::Modifier $column --> Str) {
		"{ self.render-column($placeholders, $column.column) } $column.modifier()";
	}

	method render-table(Table $source --> Str) {
		self.render-identifier($source.name);
	}

	multi method render-source(Placeholders $, Table $table --> Str) {
		self.render-table($table);
	}
	multi method render-source(Placeholders $placeholders, Select $command --> Str) {
		"({ self.render-select-expression($placeholders, $command) })";
	}
	multi method render-source(Placeholders $placeholders, Source::Renamed $renamed--> Str) {
		my $source = self.render-source($placeholders, $renamed.source);
		my $alias = self.render-table($renamed.alias);
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

	method render-row(Placeholders $placeholders, Row $row --> Str) {
		self.render-list($placeholders, $row.elements, True);
	}

	method render-values(Placeholders $placeholders, Rows $rows --> Str) {
		$rows.elems.map({ self.render-row($placeholders, $^row) }).join(', ');
	}

	multi method render-conditions(Placeholders $placeholders, Conditional:D $conditions, Str $type = 'WHERE' --> Str) {
		"$type " ~ self.render-expression($placeholders, $conditions.expression, Precedence::And);
	}
	multi method render-conditions(Placeholders $placeholders, Conditional:U $conditions, Str $type = Str --> List) {
		Empty;
	}

	multi method render-order-by(Placeholders $placeholders, Sorters:D $sorters --> List) {
		my @elems = $sorters.elems.map: { self.render-sorter($placeholders, $^column) };
		'ORDER BY', @elems.join(', ');
	}
	multi method render-order-by(Placeholders $placeholders, Sorters:U $sorters --> List) {
		Empty;
	}

	multi method render-group-by(Placeholders $placeholders, Columns:D $columns, Conditions $conditions --> List) {
		my $group-by = self.render-columns($placeholders, $columns);
		my @having   = self.render-conditions($placeholders, $conditions, 'HAVING');
		'GROUP BY', $group-by, |@having;
	}
	multi method render-group-by(Placeholders $placeholders, Columns:U $columns, Conditions $having --> List) {
		Empty;
	}

	multi method render-returning(Placeholders $placeholders, Columns:D $returning --> List) {
		'RETURNING', self.render-columns($placeholders, $returning);
	}
	multi method render-returning(Placeholders $placeholders, Columns:U $returning --> List) {
		Empty;
	}

	multi method render-limit(Int:D $limit, Int:D $offset --> Str) {
		"LIMIT $limit OFFSET $offset";
	}
	multi method render-limit(Int:D $limit, Int:U $offset --> Str) {
		"LIMIT $limit";
	}
	multi method render-limit(Int:U $limit, Int $offset --> List) {
		Empty;
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

	method render-common-table(Placeholders $placeholders, Source::Renamed $rename) {
		my $alias      = self.render-table($rename.alias);
		my $expression = self.render-source($placeholders, $rename.source);
		"$alias AS $expression";
	}

	multi method render-common-tables(Placeholders $placeholders, Common:D $common --> List) {
		my @recursive   = $common.recursive ?? 'RECURSIVE' !! Empty;
		my @expressions = $common.tables.map({ self.render-common-table($placeholders, $^table) });
		'WITH', |@recursive, @expressions.join(', ');
	}
	multi method render-common-tables(Placeholders $placeholders, Common:U --> List) {
		Empty;
	}

	multi method render-union(Placeholders $placeholders, Union:D $union --> List) {
		my $type = $union.all ?? 'ALL' !! 'DISTINCT';
		my $next = self.render-select-expression($placeholders, $union.next, :!allow-order);
		"UNION", $type, parenthesize-if($next, $union.next.order-by);
	}
	multi method render-union(Placeholders $placeholders, Union:U --> List) {
		Empty;
	}

	multi method render-select-expression(Placeholders $placeholders, Select $select --> Str) {
		my @parts;
		@parts.push: self.render-common-tables($placeholders, $select.common-tables);
		@parts.push: 'SELECT';
		@parts.push: 'DISTINCT' if $select.distinct;
		@parts.push: self.render-columns($placeholders, $select.columns);
		@parts.push: 'FROM';
		@parts.push: self.render-source($placeholders, $select.source);
		@parts.push: self.render-conditions($placeholders, $select.where);
		@parts.push: self.render-group-by($placeholders, $select.group-by, $select.having);
		@parts.push: self.render-union($placeholders, $select.union);
		@parts.push: self.render-order-by($placeholders, $select.order-by);
		@parts.push: self.render-limit($select.limit, $select.offset);
		@parts.push: self.render-locking($select.locking);
		@parts.join(' ');
	}

	method render-select(Select $command) {
		my $placeholders = self.placeholders.new;
		my $sql          = self.render-select-expression($placeholders, $command);
		$sql, $placeholders.values;
	}

	method render-insert(Insert $insert) {
		my $placeholders = self.placeholders.new;
		my $target       = self.render-table($insert.target);
		my $fields       = self.render-columns($placeholders, $insert.fields, True);
		my $values       = self.render-values($placeholders, $insert.rows);
		my @returning    = self.render-returning($placeholders, $insert.returning);
		my $sql          = ('INSERT INTO', $target, $fields, 'VALUES', $values, |@returning).join(' ');
		$sql, $placeholders.values;
	}

	method render-update(Update $update) {
		my $placeholders = self.placeholders.new;
		my $target       = self.render-table($update.target);
		my @expressions  = $update.set.pairs.map: -> (:$key, :$value) { Op::Assign.new($key, $value) }
		my $set          = self.render-list($placeholders, @expressions, False);
		my @where        = self.render-conditions($placeholders, $update.where);
		my @returning    = self.render-returning($placeholders, $update.returning);
		my $sql          = ('UPDATE', $target, 'SET', $set, |@where, |@returning).join(' ');
		$sql, $placeholders.values;
	}

	method render-delete(Delete $delete) {
		my $placeholders = self.placeholders.new;
		my $target       = self.render-table($delete.target);
		my @where        = self.render-conditions($placeholders, $delete.where);
		my @returning    = self.render-returning($placeholders, $delete.returning);
		my $sql          = ('DELETE FROM', $target, @where, @returning).flat.join(' ');
		$sql, $placeholders.values;
	}
}

class Renderer::SQL::Postgres is Renderer::SQL {
	multi method render-expression(Placeholders $placeholders, Op::Cast $cast --> Str) {
		self.render-expression($placeholders, $cast.primary, Precedence::TermLike) ~ '::' ~ $cast.type;
	}
}

role Renderer::Bare does Renderer {
}

class Renderer::Noop does Renderer::Bare {
	method render-select(Select $select --> Select) { $select }
	method render-insert(Insert $insert --> Insert) { $insert }
	method render-update(Update $update --> Update) { $update }
	method render-delete(Delete $delete --> Delete) { $delete }
}

has Renderer:D $.renderer is required;

multi submethod BUILD(Renderer:D :$!renderer!) {}
multi submethod BUILD(Placeholders:U :$placeholders!, Renderer::SQL:U :$renderer = Renderer::SQL, *%arguments) {
	$!renderer = $renderer.new(:$placeholders, |%arguments);
}
multi submethod BUILD(Renderer::Bare:U :$renderer!, *%arguments) {
	$!renderer = $renderer.new(|%arguments);
}

method select(Source:D(Any:D) $source, Columns:D(Any:D) $columns = *, Conditions(Any) $where?, Bool :$render = True, *%arguments) {
	my $select = Select.new(:$columns, :$source, :$where, |%arguments);
	$render ?? $!renderer.render-select($select) !! $select;
}

multi method insert(Table:D(Cool:D) $target, Columns:D(Any:D) $fields, Rows:D(Any:D) $rows, Columns(Any) :$returning) {
	my $insert = Insert.new(:$target, :$fields, :$rows, :$returning);
	$!renderer.render-insert($insert);
}
multi method insert(Table:D(Cool:D) $target, Assigns:D(Hash:D) $values, Columns(Any) :$returning) {
	my Columns(List) $fields = $values.keys;
	my Rows(List) $rows = [$values.values,];
	samewith($target, $fields, $rows, :$returning);
}

method update(Table:D(Cool:D) $target, Assigns:D(Hash:D) $set, Conditions(Any) $where?, Columns(Any) :$returning) {
	my $update = Update.new(:$target, :$set, :$where, :$returning);
	$!renderer.render-update($update);
}

method delete(Table:D(Cool:D) $target, Conditions(Any) $where?, Columns(Any) :$returning) {
	my $delete = Delete.new(:$target, :$where, :$returning);
	$!renderer.render-delete($delete);
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

SQL::Abstract abstracts the generation of SQL queries.

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

=head2 select($source, $columns?, $where?, :$distinct, :$group-by, :$having?, :$order-by, :$limit, :$offset, :$locking)

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
SQL::Abstract::Columns(Any:D) $columns = *

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
SQL::Abstract::Columns(Any) :$group-by,

Group by the listed columns. They're interpreted the same as C<$columns> is interpreted.
=end item1

=begin item1
Conditions(Any) :$having?, 

This will be the C<HAVING> conditions. it works described below in the Conditions section. This only makes sense to use when using C<$group-by>.
=end item1

=begin item1
Sorters(Any) :$order-by,

This sorts the rows by the listed columns. This is interpreted the same as C<$columns>, except C<*> isn't allowed, simple numbers may be interpreted as column numbers by your database, and it also allows one to use C<SQL::Abstract::Sorter::Asc>/C<SQL::Abstract::Sorter::Desc> to specify the sorting order.
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

=head2 update(Table(Cool) $target, Assigns(Hash) $set, Conditions(Any) $where?, Columns(Any) :$returning) {

=begin item1
Table(Cool) $target

This works
=end item1

=begin item1
Assigns(Hash) $set

=end item1

=begin item1
Conditions(Any) $where?, 

=end item1

=begin item1
Columns(Any) :$returning

=end item1

=head2 insert(Table(Cool) $target, Columns(Any) $fields, Rows(Any) $rows, Columns(Any) :$returning)

=head2 insert(Table(Cool) $target, Assigns(Hash) $values, Columns(Any) :$returning) {

=head2 delete(Table(Cool) $target, Conditions(Any) $where?, Columns(Any) :$returning) {

=head1 Conditions

=head1 Author

Leon Timmermans <fawaka@gmail.com>

=head1 Copyright and License

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
