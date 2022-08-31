unit class SQL::Abstract;

use fatal;

enum Precedence <Rowlike Comma Assignment And Or Not Between In Postfix Comparative Bitwise Additive Multiplicative Concatlike Prefix Termlike>;

class Op::Not { ... }

role Expression {
	method precedence(--> Precedence) { ... }

	method negate(--> Expression) {
		Op::Not.new(self);
	}
}

role Term does Expression {
	method precedence(--> Precedence::Termlike) {}
}

class Literal does Expression {
	has Precedence $.precedence;
	has Str:D $.payload is required;
	has Any:D @.arguments;

	method new(Str:D $payload, @arguments?, :$precedence = Precedence::Termlike) {
		self.bless(:$payload, :@arguments, :$precedence);
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
my multi expand-expression(Any $value) {
	Placeholder.new($value);
}
my multi expand-expression(Capture $literal) {
	my ($sql, @arguments, *%args) = |$literal;
	Literal.new($sql, @arguments, |%args);
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

	method concat(Identifier $other) {
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

	method new(Expression $source, Identifier $alias) {
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
	has Identifier:D @.elements is required;

	method new(@elements) {
		self.bless(:@elements);
	}

	multi method COERCE(Identifier(Cool) $column) {
		self.new([$column]);
	}
	multi method COERCE(@list) {
		my Identifier(Cool) @columns = @list;
		self.new(@columns);
	}

	multi method merge(Identifiers $other) {
		self.new(|@!elements, |$other.elements);
	}
}

class Expression::List does Expression {
	method precedence(--> Precedence::Rowlike) {}

	has Expression:D @.elements is required;

	method new(@elements) {
		self.bless(:@elements);
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
		self.new($columns.elements);
	}

	multi method merge(Expression::List $other) {
		self.new(|@!elements, |$other.elements);
	}
}

role Op::Unary does Expression {
	has Expression:D $.value is required;

	method new(Expression $value) {
		self.bless(:$value);
	}
}

role Op::Postfix does Op::Unary {
	method precedence(--> Precedence::Postfix) {}

	method postfix(--> Str) { ... }

	method new(Expression $value) {
		self.bless(:$value);
	}
}

class Op::IsNull does Op::Postfix {
	has Bool $.negated;
	method postfix() {
		$!negated ?? 'IS NOT NULL' !! 'IS NULL';
	}
	method negate {
		Op::IsNull.new($!value, :negated(!$!negated));
	}
}

role Op::Prefix[Str $operator] does Op::Unary {
	method precedence(--> Precedence::Prefix) {}
	method operator(--> Str:D) { $operator }
}

class Op::Positive does Op::Prefix['+'] {}
class Op::Negative does Op::Prefix['-'] {}
class Op::Complement does Op::Prefix['~'] {}

class Op::Not does Op::Unary {
	method precedence(--> Precedence::Not) {}

	method negate(--> Bool) {
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

role Op::Comperative[Str $operator] does Op::Binary {
	method precedence(--> Precedence::Comparative) {}
	method operator(--> Str) { $operator }
}

class Op::Unequals does Op::Comperative['<>'] {}
class Op::Equals does Op::Comperative['='] {
	method negate() {
		Op::Unequals.new($!left, $!right);
	}
}

class Op::LessThan does Op::Comperative['<'] {}
class Op::GreaterOrEqual does Op::Comperative['>='] {}

class Op::LessOrEqual does Op::Comperative['<='] {}
class Op::GreaterThan does Op::Comperative['>'] {}

class Op::Distinct does Op::Comperative['IS DISTINCT FROM'] {}
class Op::NotDistinct does Op::Comperative['IS NOT DISTINCT FROM'] {}

role Op::Liking[Bool $positive] does Expression {
	method precedence(--> Precedence::Comparative) {}
	method negated() { !$positive }

	has Expression:D $.left is required;
	has Expression:D $.right is required;
	has Expression $.escape;

	method new(Expression:D $left, Expression:D $right, Expression $escape?) {
		self.bless(:$left, :$right, :$escape);
	}
}
class Op::Like does Op::Liking[True] {}
class Op::NotLike does Op::Liking[False] {}

class Op::Assign does Op::Binary {
	method precedence(--> Precedence::Assignment) {}

	method operator(--> '=') {}
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

role Op::Additive[Str $operator] does Op::Binary {
	method precedence(--> Precedence::Additive) {}
	method operator() { $operator }
}
class Op::Plus does Op::Additive['+'] {}
class Op::Minus does Op::Additive['-'] {}

role Op::Multiplicative[Str $operator] does Op::Binary {
	method precedence(--> Precedence::Multiplicative) {}
	method operator() { $operator }
}
class Op::Times does Op::Multiplicative['*'] {}
class Op::Divide does Op::Multiplicative['/'] {}
class Op::Rest does Op::Multiplicative['%'] {}

role Op::Bitwise[Str $operator] does Op::Binary {
	method precedence(--> Precedence::Bitwise) {}
	method operator() { $operator }
}
class Op::Bitwise::And does Op::Bitwise['&'] {}
class Op::Bitwise::Or does Op::Bitwise['&'] {}
class Op::Bitwise::Left does Op::Bitwise['<<'] {}
class Op::Bitwise::Right does Op::Bitwise['>>'] {}

my %binary-op-for =
	'||'           => Op::Concat,
	'->'           => Op::JSON::GetElem,
	'->>'          => Op::JSON::GetElemText,
	'#>'           => Op::JSON::GetPath,
	'#>>'          => Op::JSON::GetPathText,
	'*'            => Op::Times,
	'/'            => Op::Divide,
	'%'            => Op::Rest,
	'+'            => Op::Plus,
	'-'            => Op::Minus,
	'&'            => Op::Bitwise::And,
	'|'            => Op::Bitwise::Or,
	'<<'           => Op::Bitwise::Left,
	'>>'           => Op::Bitwise::Right,
	'='            => Op::Equals,
	'!='           => Op::Unequals,
	'<>'           => Op::Unequals,
	'<'            => Op::LessThan,
	'<='           => Op::LessOrEqual,
	'>'            => Op::GreaterThan,
	'>='           => Op::GreaterOrEqual,
	'like'         => Op::Like,
	'not-like'     => Op::NotLike,
	'distinct'     => Op::Distinct,
	'not-distinct' => Op::NotDistinct;

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

role Op::Logical[Str $operator, Precedence $precedence, Constant $empty] does Expression {
	has Expression @.elements is required;

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
class Op::Or  does Op::Logical['OR' , Precedence::Or, Value::False] {}

role Op::In does Expression {
	method precedence(--> Precedence::In) {}
	method expression() { ... }

	has Expression:D $.left is required;
	has Bool $.negated;
}

class Op::In::List does Op::In {
	has Expression::List:D $.expression is required handles<elements>;

	method new(Expression $left, @elements, Bool :$negated = False) {
		my $expression = Expression::List.new(@elements);
		self.bless(:$left, :$expression, :$negated);
	}

	method negate() {
		self.bless(:$!left, :$!expression, :negated(!$!negated));
	}
}

role Query { ... }

class Op::In::Query does Op::In {
	has Query:D $.expression is required;

	method new(Expression $left, Query $expression, Bool :$negated = False) {
		self.bless(:$left, :$expression, :$negated);
	}

	method negate() {
		self.new($!left, $!expression, :negated(!$!negated));
	}
}

class Op::Exists does Expression {
	method precedence(--> Precedence::In) {}

	has Expression:D $.left is required;
	has Query:D $.query is required;
	has Bool $.negated;

	method new(Expression $left, Query $query, Bool :$negated = False) {
		self.bless(:$left, :$query, :$negated);
	}

	method negate() {
		self.new($!left, $!query, :negated(!$!negated));
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

	method new(@whens, Expression $else?, Expression :$left) {
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
		Op::IsNull.new($left, :negated);
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
		my $min = expand-expression($range.min);
		my $max = expand-expression($range.max);
		Op::Between.new($left, $min, $max);
	}

	my multi expand-junction(Expression $left, 'any', @partials) {
		Op::Or.pack(@partials);
	}
	my multi expand-junction(Expression $left, 'any', @partials where @partials > 0 && all(@partials) ~~ Op::Equals && all(@partials).left === $left) {
		Op::In::List.new($left, @partials».right);
	}
	my multi expand-junction(Expression $left, 'all', @partials) {
		Op::And.pack(@partials);
	}
	my multi expand-junction(Expression $left, 'none', @partials) {
		Op::Or.pack(@partials).negate;
	}
	my multi expand-junction(Expression $left, 'none', @partials where @partials > 0 && all(@partials) ~~ Op::Equals && all(@partials).left === $left) {
		Op::In::List.new($left, @partials».right, :negated);
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
		self.new(@list.map(&expand-condition));
	}
	multi method COERCE(%hash) {
		samewith(%hash.sort);
	}
}

class Order::Modifier does Expression {
	method precedence(--> Precedence::Comma) {}

	enum Sorting (:Asc<asc> :Desc<desc>);
	enum Nulls (:First<first>, :Last<last>);
	has Expression:D $.column is required;
	has Sorting $.order is required;
	has Nulls $.nulls;

	proto method new(|) { * }
	multi method new(Expression $column, Sorting $order?, Nulls $nulls?) {
		self.bless(:$column, :$order, :$nulls);
	}

	multi method COERCE(Pair $ (Expression:D :$key, Sorting(Str) :$value)) {
		self.new($key, $value);
	}
	multi method COERCE(Pair $ (Identifier:D(Cool:D) :$key, Sorting(Str) :$value)) {
		self.new($key, $value);
	}
	multi method COERCE(Map $ (Identifier:D(Cool:D) :$column, Sorting(Str) :$order, Nulls(Str) :$nulls)) {
		self.new($column, $order, $nulls)
	}
}

class OrderBy {
	has Expression:D @.elements is required;

	method new(@elements) {
		self.bless(:@elements);
	}

	my multi to-sorter(Expression:D $expression) {
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
		my Expression @expressions = @values.map(&expand-expression);
		self.new(@expressions);
	}
}

class Rows {
	has Row @.elements;

	method COERCE(@input) {
		my Row(Any) @elements = @input;
		self.new(:@elements);
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
		my @pairs = @list.unique(:as(*.key)).map(&transform-pair);
		self.bless(:@pairs);
	}
	multi method COERCE(%set) {
		my @pairs = %set.sort(*.key).map(&transform-pair);
		self.bless(:@pairs);
	}

	method keys() {
		@!pairs.map(*.key);
	}
	method values() {
		@!pairs.map(*.value);
	}
}

enum Quantifier (:All<all> :Distinct<distinct>);

package Window {
	class Function { ... }
	class Definition { ... }
}
class Source::Function { ... }

class Function does Expression {
	has Str:D $.name is required;
	has Quantifier $.quantifier;
	has Expression::List:D $.arguments is required;
	has OrderBy $.order-by;
	has Conditions $.filter;

	method new(Str $name, Expression::List $arguments, Conditions :$filter, Quantifier :$quantifier, OrderBy :$order-by) {
		self.bless(:$name, :$arguments, :$filter, :$quantifier, :$order-by);
	}

	method precedence() {
		$!filter ?? Precedence::Between !! Precedence::Termlike;
	}

	method over(*%arguments) {
		my $window = Window::Definition.COERCE(%arguments);
		Window::Function.new(self, $window);
	}

	method as(Identifier:D(Cool:D) $alias, Identifiers(Any) $columns?, Bool :$lateral, Bool :$ordinal) {
		Source::Function.new(self, $alias, $columns, :$lateral, :$ordinal);
	}
}

package Window {
	enum Exclusion (:Current('current row'), :Group<group>, :Ties<ties>, :NoOthers('no others'));
	enum Mode (:Rows<rows> :Range<range> :Groups<groups>);

	class Boundary::Preceding::Unbounded { ... }
	class Boundary::Preceding::Bounded { ... }
	class Boundary::CurrentRow { ... }
	class Boundary::Following::Bounded { ... }
	class Boundary::Following::Unbounded { ... }

	role Boundary {
#		method tag(--> Str) { ... }
		multi method COERCE('preceding') { Boundary::Preceding::Unbounded.new }
		multi method COERCE('current') { Boundary::CurrentRow.new }
		multi method COERCE('following') { Boundary::Following::Unbounded.new }
		multi method COERCE(Pair $ (Str :$key where 'preceding', Any :$value)) {
			my $offset = expand-expression($value);
			Boundary::Preceding::Bounded.new(:$offset);
		}
		multi method COERCE(Pair $ (Str :$key where 'following', Any :$value)) {
			my $offset = expand-expression($value);
			Boundary::Following::Bounded.new(:$offset);
		}
	}
	role Boundary::Unbounded[Str $tag] does Boundary {
		method tag() { $tag }
	}
	role Boundary::Bounded[Str $tag] does Boundary {
		method tag() { $tag }

		has Expression:D $.offset is required;
	}
	class Boundary::Preceding::Unbounded does Boundary::Unbounded['UNBOUNDED PRECEDING'] { }
	class Boundary::Preceding::Bounded does Boundary::Bounded['PRECEDING'] { }
	class Boundary::CurrentRow does Boundary::Unbounded['CURRENT ROW'] {}
	class Boundary::Following::Bounded does Boundary::Bounded['FOLLOWING'] { }
	class Boundary::Following::Unbounded does Boundary::Unbounded['UNBOUNDED FOLLOWING'] {}

	class Frame {
		has Window::Mode:D $.mode is required;
		has Boundary:D $.from is required;
		has Boundary $.to is required;
		has Exclusion $.exclude;

		method new(Mode $mode, Boundary $from, Boundary $to?, Exclusion :$exclude?) {
			self.bless(:$mode, :$from, :$to, :$exclude);
		}

		method COERCE(Map $ (Boundary(Any) :$from, Boundary(Any) :$to, Exclusion(Str) :$exclude, Mode(Str) :$mode = Mode::Rows)) {
			self.new($mode, $from, $to, :$exclude);
		}
	}

	class Definition {
		has Identifier $.existing;
		has Expression::List $.partition-by;
		has OrderBy $.order-by;
		has Frame $.frame;

		method is-simple() {
			$!existing && !$!partition-by && !$!order-by;
		}

		method COERCE(Map (Expression::List(Any) :$partition-by, OrderBy(Any) :$order-by, Identifier(Cool) :$existing, Window::Frame(Map) :$frame)) {
			self.new(:$existing, :$partition-by, :$order-by, :$frame);
		}
	}

	class Clause {
		has Identifier:D         $.name is required;
		has Window::Definition:D $.definition is required;
	}

	class Clauses {
		has Window::Clause @.windows;

		multi method COERCE(@windows) {
			self.bless(:@windows);
		}
		multi method COERCE(Window::Clause $window) {
			self.bless(:windows[ $window ]);
		}
	}
}

class Window::Function does Expression {
	method precedence(--> Precedence::Comma) {}

	has Function:D $.function is required;
	has Window::Definition:D $.window is required;

	method new(Function:D $function, Window::Definition $window) {
		self.bless(:$function, :$window);
	}
}

class Locking {
	enum Strength (:Update<update> :NoKeyUpdate('no key update') :Share<share> :KeyShare('key share'));

	has Strength:D $.strength is required;
	has Identifier $.table;
	has Bool $.no-wait;
	has Bool $.skip-locked;

	method new(Strength:D $strength, Identifier $table?, Bool :$no-wait, Bool :$skip-locked) {
		self.bless(:$strength, :$table, :$no-wait, :$skip-locked);
	}

	multi method COERCE(Strength(Str) $strength) {
		self.new($strength);
	}
	multi method COERCE(Pair $ (Strength(Str) :$key, Identifier(Cool) :$value)) {
		self.new($key, $value);
	}
}

class Select { ... }

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
		my $type = Type.WHO{$key.lc.tc};
		self.new($type, $pair.value, $all);
	}
}

class Limit {
	class All does Constant['ALL'] {}

	has Expression $.value;

	method new(Expression $value) {
		self.bless(:$value);
	}

	multi method COERCE(Expression $value) {
		self.new($value);
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

	multi method COERCE(Expression $value) {
		self.new($value);
	}
	multi method COERCE(Int $value) {
		self.new(Integer.new($value));
	}
}

class Source::Query { ... }
role Query does Expression {
	method precedence(--> Precedence::Rowlike) {}

	method as(Identifier:D(Cool:D) $alias, Bool :$lateral) {
		Source::Query.new(self, $alias, :$lateral);
	}
}

class Values does Query {
	has Rows:D  $.rows is required;
	has OrderBy $.order-by;
	has Limit   $.limit;
	has Offset  $.offset;

	method values(Rows:D(List:D) $rows, OrderBy :$order-by, Limit :$limit, Offset :$offset) {
		self.bless(:$rows, :$order-by, :$limit, :$offset);
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
		has Query:D $.query is required;
		method new(Name:D $alias, Query:D $query) {
			self.bless(:$alias, :$query);
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

role Query::Common does Query {
	has Common(Any)      $.common-tables;
}

class Distinction::Full { ... }
class Distinction::Columns { ... }

class Distinction {
	multi submethod COERCE(Bool $value) {
		$value ?? Distinction::Full.new !! Distinction;
	}
	multi submethod COERCE(Quantifier $value) {
		$value ?? Distinction::Full.new !! Distinction;
	}
	multi submethod COERCE(Expression::List(Cool) $columns) {
		Distinction::Columns.new($columns);
	}
}

class Distinction::Full is Distinction {}

class Distinction::Columns is Distinction {
	has Expression::List $.columns;

	method new(Expression::List $columns) {
		self.bless(:$columns);
	}
	
}

class GroupBy {
	has Expression::List:D $.columns is required handles<elements>;
	has Bool               $.all;

	method new(Expression::List:D(Any:D) $columns, Bool $all = False) {
		self.bless(:$columns, :$all);
	}
}

role Source { ... }
role Table { ... }

class Select does Query::Common {
	has Expression::List:D $.columns is required;
	has Distinction        $.distinct;
	has Source             $.source;
	has Conditions         $.where;
	has GroupBy            $.group-by;
	has Conditions         $.having;
	has Window::Clauses    $.windows;
	has Compound           $.compound;

	has OrderBy            $.order-by;
	has Limit              $.limit;
	has Offset             $.offset;
	has Locking            $.locking;
}

role Update does Query::Common {
	has Table:D          $.target is required;
	has Source           $.from;
	has Conditions       $.where;
	has Expression::List $.returning;
}

role Updater::Pairwise {
	has Assigns:D $.assigns is required;
}
class Update::Pairwise does Update does Updater::Pairwise {}

role Updater::Row {
	has Identifiers:D $.columns is required;
	has Row           $.row is required;
}
class Update::Row does Update does Updater::Row {}

role Updater::Select {
	has Identifiers:D $.columns is required;
	has Select:D      $.select is required;
}
class Update::Select does Update does Updater::Select {}

enum Overriding (:System<system>, :User<user>);

class Conflict::Target::Columns { ... }
class Conflict::Target::Constraint { ... }

role Conflict::Target {
	multi method COERCE(Pair $ (:$key where 'columns', Identifiers:D(Any:D) :$value)) {
		Conflict::Target::Columns.new($value);
	}
	multi method COERCE(Pair $ (:$key where 'constraint', Identifier:D(Cool:D) :$value)) {
		Conflict::Target::Constraint.new($value);
	}
}

class Conflict::Target::Columns does Conflict::Target {
	has Identifiers:D $.columns is required;
	has Conditions    $.where;

	method new(Identifiers:D $columns, Conditions $where?) {
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

role Conflict::Update { ... }
role Conflict {
	has Conflict::Target $.target;

	multi submethod COERCE(Str $string where $string.lc eq 'nothing') {
		Conflict::Nothing.new;
	}
	multi submethod COERCE(Pair $ (Conflict::Target(Pair) :$key, :$value where 'nothing')) {
		Conflict::Nothing.new($key);
	}
	multi submethod COERCE(Pair $ (Conflict::Target(Pair) :$key, :$value)) {
		Conflict::Update.COERCE(($key => $value));
	}
}

class Conflict::Nothing does Conflict {
	method new(Conflict::Target $target?) {
		self.bless(:$target);
	}
}

class Conflict::Update::Pairwise { ... }
class Conflict::Update::Row { ... }
class Conflict::Update::Select { ... }
role Conflict::Update does Conflict {
	has Conditions $.where;

	multi submethod COERCE(Pair $ (:$key, Assigns:D(Cool:D) :$value)) {
		Conflict::Update::Pairwise.new($key, $value);
	}
	multi submethod COERCE(Pair $ (:$key, Pair:D :$value (:key($columns), :value($row)))) {
		Conflict::Update::Row.new($key, $columns, $row);
	}
	multi submethod COERCE(Pair $ (:$key, Select:D :$value)) {
		Conflict::Update::Select.new($key, $value);
	}
}

class Conflict::Update::Pairwise does Conflict::Update does Updater::Pairwise {
	method new(Conflict::Target:D $target, Assigns:D $assigns, Conditions $where?) {
		self.bless(:$target, :$where, :$assigns);
	}
}

class Conflict::Update::Row does Conflict::Update does Updater::Row {
	method new(Conflict::Target:D $target, Identifiers:D $columns, Row:D $row, Conditions $where?) {
		self.bless(:$target, :$where, :$columns, :$row);
	}
}

class Conflict::Update::Select does Conflict::Update does Updater::Select {
	method new(Conflict::Target:D $target, Select $select, Conditions $where?) {
		self.bless(:$target, :$where, :$select);
	}
}

role Insert does Query::Common {
	has Table:D          $.target is required;
	has Identifiers      $.fields;
	has Overriding       $.overriding;
	has Conflict         $.conflict;
	has Expression::List $.returning;
}

class Insert::Values does Insert {
	has Rows:D $.rows is required;
}

class Insert::Select does Insert {
	has Select:D $.select is required;
}

class Insert::Defaults does Insert {
}

class Delete does Query::Common {
	has Table:D          $.target is required;
	has Source           $.using;
	has Conditions       $.where;
	has Expression::List $.returning;
}

class Join::On { ... }
class Join::Conditions { ... }
class Join::Using { ... }
class Table::Simple { ... }
class Table::Renamed { ... }

enum Join::Type (:Inner<inner>, :Left<left>, :Right<right>, :Outer<outer>);

role Source {
	multi submethod COERCE(Identifier:D(Cool:D) $pre-source) {
		Table::Simple.new($pre-source);
	}
	multi submethod COERCE(Pair $ (Identifier:D(Cool:D) :$key, Table(Cool) :$value)) {
		Table::Renamed.new($value, $key);
	}
	multi submethod COERCE(Pair $ (Identifier:D(Cool:D) :$key, Query:D :$value)) {
		Source::Query.new($value, $key);
	}
	multi submethod COERCE(Pair (Identifier:D(Cool:D) :$key, Function:D :$value)) {
		Source::Function.new($value, $key);
	}

	multi method join(Source:D(Any:D) $right, Join::Conditions(Any) :$on!, Join::Type(Str) :$type = Join::Type::Inner) {
		Join::On.new(:left(self), :$right, :$on, :$type);
	}
	multi method join(Source:D(Any:D) $right, Identifiers(Any) :$using!, Join::Type(Str) :$type = Join::Type::Inner) {
		Join::Using.new(:left(self), :$right, :$using, :$type);
	}
	multi method join(Source:D(Any:D) $right, Bool :$natural!, Join::Type(Str) :$type = Join::Type::Inner) {
		Join::Natural.new(:left(self), :$right, :$type);
	}
	multi method join(Source:D(Any:D) $right, Bool :$cross!) {
		Join::Cross.new(:left(self), :$right);
	}

	method select(Expression::List:D(Any:D) $columns = *, Conditions(Any) $where?, Common(Any) :$common-tables, Distinction(Any) :$distinct, GroupBy(Any) :$group-by, Conditions(Any) :$having, Window::Clauses :$windows, Compound(Pair) :$compound, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset, Locking(Any) :$locking) {
		Select.new(:$common-tables, :$distinct, :$columns, :source(self), :$where, :$group-by :$having, :$windows, :$compound, :$order-by, :$limit, :$offset, :$locking);
	}
}

role Table does Source {
	has Identifier:D $.name is required;
	has Bool         $.only;

	multi method update(Assigns:D(Any:D) $assigns, Conditions(Any) $where?, Source(Any) :$from, Expression::List(Any) :$returning) {
		Update::Pairwise.new(:target(self), :$assigns, :$where, :$from, :$returning);
	}
	multi method update(Identifiers:D(Any:D) $columns, Row $row, Conditions(Any) $where?, Source(Any) :$from, Expression::List(Any) :$returning) {
		Update::Row.new(:target(self), :$columns, :$row, :$where, :$from, :$returning);
	}
	multi method update(Identifiers:D(Any:D) $columns, Select:D $select, Conditions(Any) $where?, Source(Any) :$from, Expression::List(Any) :$returning) {
		Update::Select.new(:target(self), :$columns, :$select, :$where, :$from, :$returning);
	}

	multi method insert(Identifiers(Any) $fields, Rows:D(List:D) $rows, Overriding :$overriding, Conflict(Any) :$conflict, Expression::List(Any) :$returning) {
		Insert::Values.new(:target(self), :$fields, :$rows, :$overriding, :$conflict, :$returning);
	}
	multi method insert(Assigns:D(Cool:D) $values, Overriding :$overriding, Conflict(Any) :$conflict, Expression::List(Any) :$returning) {
		samewith($values.keys, [$values.values,], :$overriding, :$conflict, :$returning);
	}
	multi method insert(Identifiers(Any) $fields, Select $select, Overriding :$overriding, Conflict(Any) :$conflict, Expression::List(Any) :$returning) {
		Insert::Select.new(:target(self), :$fields, :$select, :$overriding, :$conflict, :$returning);
	}
	multi method insert(Value::Default, Overriding :$overriding, Conflict(Any) :$conflict, Expression::List(Any) :$returning) {
		Insert::Defaults.new(:target(self), :$overriding, :$conflict, :$returning);
	}

	method delete(Conditions(Any) $where?, Source(Any) :$using, Expression::List(Any) :$returning) {
		Delete.new(:target(self), :$where, :$using :$returning);
	}

	multi method concat(Identifier $column) {
		Identifier.new($!name.concat($column));
	}
	multi method concat(Identifiers $columns) {
		$columns.elements.map: { self.concat($^column) };
	}
}

class Table::Simple does Table does Query {
	method new(Identifier:D $name, Bool :$only) {
		self.bless(:$name, :$only);
	}

	proto method as(|) { * }
	multi method as(Identifier:D(Cool:D) $alias) {
		Table::Renamed.new(self, $alias);
	}
	multi method as(Identifier:D(Cool:D) $alias, Identifiers(Any) $columns) {
		Table::Renamed.new(self, $alias, $columns);
	}
	multi method as(Pair $pair) {
		Table::Renamed.new(self, $pair.key, $pair.value);
	}
}

role Source::Aliased does Source {
	has Identifier:D $.alias is required;
	has Identifiers $.columns;
}

class Table::Renamed does Table does Source::Aliased {
	method new(Table:D $table, Identifier:D $alias, Identifiers $columns?) {
		self.bless(:name($table.name), :only($table.only), :$alias, :$columns);
	}
}

class Join does Source {
	has Source:D $.left is required;
	has Source:D $.right is required;
}

class Join::Conditions does Conditional {
	sub expand(Pair $ (Identifier(Cool) :$key, Identifier(Cool) :$value)) {
		Op::Equals.new($key, $value);
	}

	multi method COERCE(Conditions $conditions) {
		self.new($conditions.expression);
	}
	multi method COERCE(Pair $pair) {
		self.new([ expand($pair) ]);
	}
	multi method COERCE(@list) {
		self.new(@list.map(&expand));
	}
	multi method COERCE(%hash) {
		samewith(%hash.sort(*.key));
	}
}
class Join::On is Join {
	has Join::Type:D $.type is required;
	has Join::Conditions $.on;
}

class Join::Using is Join {
	has Join::Type:D $.type is required;
	has Identifiers $.using;
}

class Join::Natural is Join {
	has Join::Type:D $.type is required;
}

class Join::Cross is Join {}

role Source::Nested does Source does Source::Aliased {
	has Bool $.lateral;
}

class Source::Query does Source::Nested {
	has Query:D $.query is required;

	multi method new(Query:D $query, Identifier:D $alias, Identifiers $columns?, Bool :$lateral) {
		self.bless(:$query, :$alias, :$columns, :$lateral);
	}
}

class Source::Function does Source::Nested {
	has Function:D $.function is required;
	has Bool       $.ordinal;

	multi method new(Function:D $function, Identifier:D $alias, Identifiers $columns?, Bool :$lateral, Bool :$ordinal) {
		self.bless(:$function, :$alias, :$columns, :$lateral, :$ordinal);
	}
}

role Placeholders {
	has @.values is built(False);
	method bind(Any $value) { ... }
}

class Placeholders::DBI does Placeholders {
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

class Result {
	has Str:D $.sql is required;
	has @.arguments is required;

	method new(Str:D $sql, @arguments?) {
		self.bless(:$sql, :@arguments);
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
	multi resolve-value(%replacements, Delayed $delayed) {
		if %replacements{$delayed.identifier}:exists {
			%replacements{$delayed.identifier};
		} elsif $delayed.has-default {
			$delayed.default;
		} else {
			die "No value given for delayed value '$delayed.identifier()'"
		}
	}

	method resolve(%replacements?) {
		@!arguments.map: { resolve-value(%replacements, $^element) };
	}

	method identifiers() {
		@!arguments.grep(Delayed)».identifier;
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
	has Bool:D $.quoting = False;

	my sub parenthesize-if(Str $input, Bool() $parenthese --> Str) {
		$parenthese ?? "($input)" !! $input;
	}
	my sub parenthesize-list(@items, Str $with = " ") {
		"({ @items.join($with) })";
	}

	method render-expressions(Placeholders $placeholders, @elements, Precedence $precedence = Precedence::Comma, Str $with = ', ') {
		@elements.map({ self.render-expression($placeholders, $^element, $precedence) }).join($with);
	}

	method render-expression-list(Placeholders $placeholders, @elements, Bool $parenthesize --> Str) {
		my $result = self.render-expressions($placeholders, @elements, Precedence::Comma);
		parenthesize-if($result, $parenthesize);
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
		~$constant;
	}
	multi method render-expression(Placeholders $placeholders, Expression::List $parentheses, Precedence --> Str) {
		self.render-expression-list($placeholders, $parentheses.elements, False);
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
		@result.append: self.render-expression($placeholders, $case.left, Precedence::Comma) with $case.left;
		@result.append: $case.whens.map: { self.render-when($placeholders, $^when) };
		@result.append: 'ELSE', self.render-expression($placeholders, $case.else, Precedence::Comma) with $case.else;
		@result.append: 'END';
		@result.join(' ');
	}
	multi method render-expression(Placeholders $placeholders, Op::Prefix $op, Precedence --> Str) {
		my $primary = self.render-expression($placeholders, $op.value, Precedence::Prefix);
		$op.operator ~ $primary;
	}
	multi method render-expression(Placeholders $placeholders, Op::Binary $op, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $op.left, $op.precedence);
		my $right = self.render-expression($placeholders, $op.right, $op.precedence);
		"$left $op.operator() $right";
	}
	sub null-if(Bool $bool) {
		$bool ?? 'NOT' !! Empty
	}
	multi method render-expression(Placeholders $placeholders, Op::Liking $liking, Precedence --> Str) {
		my @result;
		@result.push: self.render-expression($placeholders, $liking.left, $liking.precedence);
		@result.push: null-if($liking.negated), $liking.operator();
		@result.push: self.render-expression($placeholders, $liking.right, $liking.precedence);
		@result.push: 'ESCAPE', self.render-expression($placeholders, $liking.escape, $liking.precedence) with $liking.escape;
		@result.join(' ');
	}
	multi method render-expression(Placeholders $placeholders, Op::Postfix $op, Precedence --> Str) {
		my $value = self.render-expression($placeholders, $op.value, Precedence::Postfix);
		"$value $op.postfix()";
	}
	multi method render-expression(Placeholders $placeholders, Op::Not $op, Precedence --> Str) {
		my $primary = self.render-expression($placeholders, $op.value, Precedence::Not);
		"NOT $primary";
	}
	multi method render-expression(Placeholders $placeholders, Op::In $in, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $in.left, $in.precedence);
		my $candidates = self.render-expression($placeholders, $in.expression, Precedence::Comma);
		my $operator = $in.negated ?? 'NOT IN' !! 'IN';
		"$left $operator $candidates";
	}
	multi method render-expression(Placeholders $placeholders, Op::Exists $in, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $in.left, $in.precedence);
		my $candidates = self.render-expression($placeholders, $in.query, Precedence::Comma);
		my $operator = $in.negated ?? 'NOT EXISTS' !! 'EXISTS';
		"$left $operator $candidates";
	}
	multi method render-expression(Placeholders $placeholders, Op::BetweenLike $between, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $between.left, Precedence::Between);
		my $min = self.render-expression($placeholders, $between.min, Precedence::Between);
		my $max = self.render-expression($placeholders, $between.max, Precedence::Between);
		my @not = null-if($between.negated);
		($left, |@not, 'BETWEEN', $min, 'AND', $max).join(' ');
	}
	multi method render-expression(Placeholders $placeholders, Op::Logical $logical, Precedence --> Str) {
		self.render-expressions($placeholders, $logical.elements, $logical.precedence, " $logical.operator() ");
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

	multi method render-quantifier(Quantifier:D $quantifier) {
		$quantifier.uc;
	}
	multi method render-quantifier(Quantifier:U) {
		Empty;
	}

	multi method render-expression(Placeholders $placeholders, Function $function, Precedence --> Str) {
		my @expressions = self.render-quantifier($function.quantifier);
		@expressions.append: self.render-column-expressions($placeholders, $function.arguments);
		@expressions.append: self.render-order-by($placeholders, $function.order-by);
		my $expression = @expressions.join(' ');
		my @result = "{ $function.name() }($expression)";
		@result.push: 'FILTER', parenthesize-list(self.render-conditions($placeholders, $function.filter, 'WHERE')) with $function.filter;
		@result.join(' ');
	}

	multi method render-window-boundary(Placeholders $placeholders, Window::Boundary::Unbounded $boundary) {
		$boundary.tag;
	}
	multi method render-window-boundary(Placeholders $placeholders, Window::Boundary::Bounded $boundary) {
		self.render-expression($placeholders, $boundary.offset, Precedence::Between), $boundary.tag;
	}

	multi method render-window-frame(Placeholders $placeholders, Window::Frame:D $frame) {
		my $mode = $frame.mode.uc;
		my @exclusion = $frame.exclude.defined ?? ('EXCLUDE', $frame.exclude.uc) !! Empty;
		my $from = self.render-window-boundary($placeholders, $frame.from);
		$mode, $from, |@exclusion;
	}
	multi method render-window-frame(Placeholders $placeholders, Window::Frame:D $frame where .to.defined) {
		my $mode = $frame.mode.uc;
		my $from = self.render-window-boundary($placeholders, $frame.from);
		my $to = self.render-window-boundary($placeholders, $frame.to);
		my @exclusion = $frame.exclude.defined ?? ('EXCLUDE', $frame.exclude.uc) !! Empty;
		$mode, 'BETWEEN', $from, 'AND', $to, |@exclusion;
	}
	multi method render-window-frame(Placeholders $placeholders, Window::Frame:U $frame) {
		Empty;
	}

	multi method render-window-definition(Placeholders $placeholders, Window::Definition $definition --> Str) {
		my @inner;
		@inner.push: self.render-identifier($definition.existing) with $definition.existing;
		@inner.push: self.render-column-expressions($placeholders, $definition.partition-by, 'PARTITION BY');
		@inner.push: self.render-order-by($placeholders, $definition.order-by);
		@inner.push: self.render-window-frame($placeholders, $definition.frame);
		parenthesize-list(@inner);
	}
	multi method render-window-definition(Placeholders $placeholders, Window::Definition $definition where .is-simple --> Str) {
		self.render-identifier($definition.existing);
	}

	multi method render-expression(Placeholders $placeholders, Window::Function $column, Precedence --> Str) {
		my $function = self.render-expression($placeholders, $column.function, Precedence::Comma);
		my $definition = self.render-window-definition($placeholders, $column.window);
		"$function OVER $definition";
	}

	multi method render-distinct(Placeholders $placeholders, Distinction::Columns:D $source --> List) {
		'DISTINCT ON', self.render-expression-list($placeholders, $source.columns.elements, True);
	}
	multi method render-distinct(Placeholders $placeholders, Distinction::Full:D $source, --> Str) {
		'DISTINCT';
	}
	multi method render-distinct(Placeholders $placeholders, Distinction:U $source --> List) {
		Empty;
	}

	multi method render-column-expressions(Placeholders $placeholders, Expression::List:D $columns, Str $prefix? --> List) {
		my $results = self.render-expressions($placeholders, $columns.elements);
		$prefix ?? ($prefix, $results) !! ($results,);
	}
	multi method render-column-expressions(Placeholders $placeholders, Expression::List:U $columns, Str $prefix? --> List) {
		Empty;
	}

	multi method render-identifiers(Identifiers:D $columns --> Str) {
		parenthesize-list($columns.elements.map({ self.render-identifier($^column) }), ', ');
	}
	multi method render-identifiers(Identifiers:U $columns --> List) {
		Empty;
	}

	multi method render-source-alias(Source::Aliased $source) {
		my $alias = self.render-identifier($source.alias);
		my @columns = self.render-identifiers($source.columns);
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
		self.render-expression($placeholders, $query.query, Precedence::Comma);
	}
	multi method render-source-nested(Placeholders $placeholders, Source::Function $function --> Str) {
		my $result = self.render-expression($placeholders, $function.function, Precedence::Rowlike);
		$function.ordinal ?? "($result WITH ORDINALITY)" !! "($result)";
	}

	multi method render-source(Placeholders $, Table $table --> List) {
		self.render-table($table);
	}
	multi method render-source(Placeholders $placeholders, Source::Nested $nested --> List) {
		my @lateral = $nested.lateral ?? 'LATERAL' !! Empty;
		my $rendered = self.render-source-nested($placeholders, $nested);
		|@lateral, $rendered, self.render-source-alias($nested);
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
		'USING', self.render-identifiers($join.using);
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
		my @columns    = self.render-identifiers($rename.alias.columns);
		my $expression = self.render-expression($placeholders, $rename.query, Precedence::Comma);
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
		my @result = self.render-expression($placeholders, $column.column, Precedence::Comma);
		@result.push: $column.order.uc if $column.order;
		@result.push: 'NULLS', $column.nulls.uc if $column.nulls;
		@result.join(' ');
	}

	multi method render-group-by(Placeholders $placeholders, GroupBy:D $group-by, Conditions $conditions --> List) {
		my @result = 'GROUP BY';
		@result.push: 'ALL' if $group-by.all;
		@result.push: self.render-expression-list($placeholders, $group-by.elements, False);
		@result.push: self.render-conditions($placeholders, $conditions, 'HAVING');
		@result;
	}
	multi method render-group-by(Placeholders $placeholders, GroupBy:U $columns, Conditions $having --> List) {
		Empty;
	}

	method render-window(Placeholders $placeholders, Window::Clause $window --> Str) {
		"WINDOW $window.name() AS { self.render-window-definition($window.definition) }";
	}

	multi method render-windows(Placeholders $placeholders, Window::Clauses:D $windows --> List) {
		$windows.windows.map({ self.render-window($placeholders, $^window) }).join(', ');
	}
	multi method render-windows(Placeholders $placeholders, Window::Clauses:U $windows --> List) {
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
		'ORDER BY', self.render-expressions($placeholders, $sorters.elements);
	}
	multi method render-order-by(Placeholders $placeholders, OrderBy:U $sorters --> List) {
		Empty;
	}

	multi method render-limit-offset(Placeholders $placeholders, Limit $limit, Offset $offset --> List) {
		my @result;
		@result.push: 'LIMIT',  self.render-expression($placeholders, $limit.value,  Precedence::Between) with $limit;
		@result.push: 'OFFSET', self.render-expression($placeholders, $offset.value, Precedence::Between) with $offset;
		@result;
	}

	multi method render-locking(Locking:D $locking --> List) {
		my @result = 'FOR', $locking.strength.uc;
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
		my $columns = self.render-identifiers($update.columns);
		my $expression = self.render-expression-list($placeholders, $update.row.elements, True);
		"SET $columns = $expression";
	}
	multi method render-update-values(Placeholders $placeholders, Updater::Select $update) {
		my $columns = self.render-identifiers($update.columns);
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

	method render-values(Placeholders $placeholders, Rows $rows) {
		my @values = $rows.elements.map({ self.render-expression-list($placeholders, $^row.elements, True) });
		'VALUES', @values.join(', ');
	}

	multi method render-insert-content(Placeholders $placeholders, Insert::Values $insert) {
		self.render-values($placeholders, $insert.rows);
	}
	multi method render-insert-content(Placeholders $placeholders, Insert::Select $insert) {
		self.render-expression($placeholders, $insert.select, Precedence::Rowlike);
	}
	multi method render-insert-content(Placeholders $placeholders, Insert::Defaults $insert) {
		'DEFAULT VALUES';
	}

	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target::Columns:D $target) {
		my $columns = self.render-identifiers($target.columns);
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
		my @fields     = self.render-identifiers($insert.fields);
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
		my @rows     = $values.render-values($placeholders, $values.rows);
		my @order-by = self.render-order-by($placeholders, $values.order-by);
		my @limits   = self.render-limit-offset($placeholders, $values.limit, $values.offset);

		(@rows, @order-by, @limits).flat.join(' ');
	}

	multi method render-expression(Placeholders $placeholders, Table $table, Precedence --> Str) {
		"TABLE { self.render-table($table) }";
	}

	method render(Expression $expression) {
		my $placeholders = self.placeholders.new;
		my $sql          = self.render-expression($placeholders, $expression, Precedence::Rowlike);
		Result.new($sql, $placeholders.values);
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

multi method values(Rows:D $rows, OrderBy :$order-by, Limit :$limit, Offset :$offset) {
	my $values = Values.new(:$rows, :$order-by, :$limit, :$offset);
	$!renderer.render($values);
}

method begin() {
	Result.new('BEGIN');
}

method rollback() {
	Result.new('ROLLBACK');
}

method commit() {
	Result.new('COMMIT');
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

method identifier(Identifier(Cool) $ident) {
	$ident;
}

method binary(Str $operator, Expression $left, Expression $right, *%args) {
	my $class = %binary-op-for{$operator.lc}:exists ?? %binary-op-for{$operator.lc} !! die "No such operator '$operator'";
	$class.new($left, $right, |%args);
}

method logical(Str $operator, @expressions) {
	my $class = $operator.lc eq 'and' ?? Op::And !! $operator.lc eq 'or' ?? Op::Or !! die "No such operator '$operator'";
	$class.new(@expressions);
}

method not(Expression $expression) {
	Op::Not.new($expression)
}

method function(Str $name, Expression::List:D(Any:D) $arguments = (), Conditions(Any) :$filter, Quantifier(Str) :$quantifier, OrderBy(Any) :$order-by) {
	Function.new($name, $arguments, :$filter, :$quantifier, :$order-by);
}

method value(Any $value) {
	expand-expression($value);
}

method delayed(|arguments) {
	Delayed.new(|arguments);
}

multi method conflict(Any:D :$do where 'nothing') {
	Conflict.COERCE($do);
}
multi method conflict(Identifiers(Any) :$columns!, Any:D :$do) {
	Conflict.COERCE(:$columns => $do);
}
multi method conflict(Identifier(Cool) :$constraint!, Any:D :$do) {
	Conflict.COERCE(:$constraint => $do);
}

method null() {
	Value::Null.new;
}
method default() {
	Value::Default.new;
}
method true() {
	Value::True.new;
}
method false() {
	Value::False.new;
}

=begin pod

=head1 Name

SQL::Abstract - Generate SQL from Raku data structures

=head1 Synopsis

=begin code :lang<raku>

use SQL::Abstract;

my $placeholders = SQL::Abstract::Placeholders::Postgres;
my $abstract = SQL::Abstract.new(:$placeholders);
my $query = $abstract.select('table', <foo bar>, :id(3));
my $result = $dbh.query($result.sql, $result.arguments);

my $join = $abstract.table('books').join('authors', :using<author_id>);
my ($sql, $arguments) = $abstract.select($join, [<book name>, <author name>], { <book cost> => { '<' => 10 }});

=end code

=head1 Description

SQL::Abstract abstracts the generation of SQL queries. Fundamentally its functionality has three components

=item1 An AST to represent SQL queries
=item1 A set of helpers to convert standard raku datatypes into such an AST
=item1 A renderer to turn that AST into an SQL query

It should be able to represent any C<SELECT>, C<UPDATE>, C<INSERT>, or C<DELETE> query that is valid in both Postgresql and SQLite. This subset should be generally portable to other databases as well (

=head1 Helper types

SQL::Abstract uses various helper types to aid

=head2 SQL::Abstract::Source

A source is source of data, usually a table or a join. If not passed as a C<Source> object it will upconvert from the following types:

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

This will rename the table in the value to the name in the key.
=end item2

It also has a C<select> method that works like C<SQL::Abstract>'s select except returns a C<Select> object instead of a rendered query, and it doesn't take a C<Source> as first argument.

=head2 SQL::Abstract::Table does SQL::Abstract::Source

This class takes the same 

=head2 SQL::Abstract::Identifier

=begin item2
Str

This will fetch the given column
=end item2

=begin item2
List

This will fetch the given columns, with the elements of the list representing the components of the column name. E.g. C<< <bar baz> >> is equivalent to C< "bar.baz" >.
=end item2

=head2 SQL::Abstract::Identifiers

This takes either a list of C<Identifier()>, or a single C<Identifier()>. Note that a single list will be interpreted will be interpreted as a list of string identifiers, if one wants to pass a single list-from identifier the list must be nested (e.g. C<[ <table column>,]>).

=head2 SQL::Abstract::Expression::List

This is a list much like C<Identifiers>, however it will accept not just identifiers but any expression (e.g. comparisons, function calls, etc…). If given a pair it will rename the value to the key (C<value AS key>). A whatever-star will represent all columns.

=head2 SQL::Abstract::Conditions

This is a pair, a list of pairs, a hash or an C<Expression>. In the former three cases, the key (called left in the rest of this section) shall be an C<Identifier()> designating a column name, or an C<Expression>. The right hand side can be one of several types:

=head3 Any:U

This will check if the left expression is C<NULL>; C<:left(Any)> equals C<left IS NULL>.

=head3 Pair

This will use the key as operator to compare left against another value or expression. E.g. C<:left('<' => 42)> renders like C<< left < 42 >>. The following keys are known:

=item C<=>
=item C<!=>
=item C<< <> >>
=item C<< < >>
=item C<< <= >>
=item C<< > >>
=item C<< >= >>
=item C<like>
=item C<not-like>
=item C<distinct>
=item C<not-distinct>
=item C<||>
=item C<< -> >>
=item C<<< ->> >>>
=item C<< #> >>
=item C<<< #>> >>>
=item C<*>
=item C</>
=item C<%>
=item C<+>
=item C<->
=item C<&>
=item C<|>
=item C<<< << >>>
=item C<<< >> >>>

=head3 Hash

This will be interpreted as a conjunction of the hash pairs. E.g. C<:left{ '>' => 3, '<' => 42 }> will render like C<< left > 3 AND left < 42 >>.

=head3 Range

This will check if a value is in a certain range. E.g. C<:left(1..42)> will render like C<left BETWEEN 1 AND 42>.

=head3 Junction

This will check against the values in the function. E.g. C<:left(1|2|4)> will render like C<left IN (1, 2, 4)>.

=head3 Any

If none of the above options match, the value will be compared to as is (as a placeholder). C<:left(2)> will render equivalent to C<left = 2>.

=head2 SQL::Abstract::Assigns

=head2 SQL::Abstract::OrderBy

This takes a list of things to sort by. Much like C<Expression::List> this accepts identifiers and expressions, but C<*> isn't allowed and pair values are interpreted as order modifier (e.g. C<:column<desc>>).

=head1 Class SQL::Abstract

=head3 new(:$placeholders!)

This creates a new C<SQL::Abstract> object. It has one mandatory name argument, C<$placeholders>, which takes one of the following values:

=begin item1
SQL::Abstract::Placeholders::DBI

This will use DBI style C<(?, ?)> for placeholders
=end item1

=begin item1
SQL::Abstract::Placeholders::Postgres

This will use Postgres style C<($1, $2)> for placeholders.
=end item1

=head3 select(Source() $source, Expression::List() $columns?, Conditions() $where?, Bool :$distinct, Expression::List() :$group-by, Conditions() :$having, OrderBy() :$order-by, Int :$limit, Int :$offset, Locking :$locking)

This will generate a C<SELECT> query.

=begin item1
Source(Any:D) $source

=end item1

=begin item1
Expression::List(Any:D) $columns = *

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
Expression::List(Any) :$group-by,

Group by the listed columns. They're interpreted the same as C<$columns> is interpreted.
=end item1

=begin item1
Conditions(Any) :$having?, 

This will be the C<HAVING> conditions. it works described below in the Conditions section. This only makes sense to use when using C<$group-by>.
=end item1

=begin item1
OrderBy(Any) :$order-by,

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
Locking(Any) :$locking

This sets the locking clause of the query. Reasonably portable values for this include:

=item2 C<'update'>/C<SQL::Abstract::Locking::Strength::Update>
=item2 C<'share'>/C<SQL::Abstract::Locking::Strength::Share>

=end item1

=head3 update(Table(Cool) $target, Assigns(Cool) $set, Conditions(Any) $where?, Expression::List(Any) :$returning) {

=head3 insert(Table(Cool) $target, Expression::List(Any) $fields, Rows(Any) $rows, Expression::List(Any) :$returning)

=head3 insert(Table(Cool) $target, Assigns(Hash) $values, Expression::List(Any) :$returning) {

=head3 delete(Table(Cool) $target, Conditions(Any) $where?, Expression::List(Any) :$returning) {

=head1 Author

Leon Timmermans <fawaka@gmail.com>

=head1 Copyright and License

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
