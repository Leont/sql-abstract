unit class SQL::Abstract;

use fatal;

use SQL::Placeholders;

enum Precedence <Rowlike Comma Assignment And Or Not Between In Postfix Comparative Bitwise Additive Multiplicative Concatlike Prefix Termlike>;

class Op::Not { ... }
class Identifier { ... }
class Expression::Renamed { ... }

role Expression {
	method precedence(--> Precedence) { ... }

	method negate(--> Expression) {
		Op::Not.new(:value(self));
	}
	method as(Identifier(Cool) $alias) {
		Expression::Renamed.new(:source(self), :$alias);
	}
}

role Term does Expression {
	method precedence(--> Precedence::Termlike) {}
}

class Literal does Expression {
	has Precedence $.precedence = Precedence::Termlike;
	has Str:D $.payload is required;
	has Any:D @.arguments;

	method COERCE(Capture $capture) {
		my ($payload, @arguments, *%args) = |$capture;
		self.new(:$payload, :@arguments, |%args);
	}
}

class Integer does Term {
	has Int $.value;

	method COERCE(Int $value) {
		self.new(:$value);
	}
}

class Placeholder does Term {
	has Any $.value;

	method COERCE(Any $value) {
		self.bless(:$value);
	}
}

role Constant[Str $keyword] does Term {
	method Str() {
		$keyword;
	}
	method WHICH() {
		ValueObjAt.new("SQL::Abstract::Constant|$keyword");
	}
}

class Value::Default does Constant['DEFAULT'] {}
class Value::True    does Constant['TRUE']    {}
class Value::False   does Constant['FALSE']   {}
class Value::Null    does Constant['NULL']    {}

multi expand-expression(Expression $expression) {
	$expression;
}
multi expand-expression(Any:D $value) {
	Placeholder.new(:$value);
}
multi expand-expression(Any:U) {
	Value::Null.new;
}
multi expand-expression(Literal(Capture:D) $literal) {
	$literal;
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

	sub quote(Str $part) {
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

	method COERCE(Pair (Identifier(Cool) :key($alias), Expression :value($source))) {
		self.new(:$alias, :$source);
	}

	method as(Identifier(Cool) $alias) {
		Expression::Renamed.new(:$!source, :$alias);
	}
}

multi expand-expression(Whatever) {
	Identifier.new('*');
}

role Value::List does Expression {
	method precedence(--> Precedence::Rowlike) {}

	method elements() { ... }

	multi method merge(::?CLASS $other) {
		my @elements = |self.elements, |$other.elements:
		self.new(:@elements);
	}
}

class Identifiers does Value::List {
	has Identifier:D @.elements is required;

	multi method COERCE(Identifier(Cool) $column) {
		self.new(:elements[$column]);
	}
	multi method COERCE(@list) {
		my Identifier(Cool) @elements = @list;
		self.new(:@elements);
	}
	multi method COERCE(List:U $list) {
		Identifiers;
	}
}

class Function { ... }

class Column::List does Value::List {
	has Expression:D @.elements is required;

	our proto to-column(|) { * }
	multi to-column(Expression $column) {
		$column;
	}
	multi to-column(Identifier(Str) $column) {
		$column;
	}
	multi to-column(Identifier(List) $column) {
		$column;
	}
	multi to-column(Pair $ (:$key, :$value)) {
		Expression::Renamed.COERCE($key => to-column($value));
	}
	multi to-column(Pair (:$key, Bool :$value)) {
		Identifier.new($key);
	}
	multi to-column(Whatever) {
		Identifier.new('*');
	}
	multi to-column(Map $map (Str :$function!, :%over!, *%args)) {
		Function.COERCE({ :name($function), |%args }).over(|%over);
	}
	multi to-column(Map $map (Str :$function!, *%args)) {
		Function.COERCE({ :name($function), |%args });
	}

	multi method COERCE(Any $column) {
		self.new(:elements[to-column($column)]);
	}
	multi method COERCE(@list) {
		my @elements = @list.map(&to-column);
		self.new(:@elements);
	}
	multi method COERCE(Identifiers $elements) {
		self.new(:elements($elements.elements));
	}
}

role Op::Unary does Expression {
	has Expression:D $.value is required;
}

role Op::Postfix does Op::Unary {
	method precedence(--> Precedence::Postfix) {}

	method postfix(--> Str) { ... }
}

class Op::IsNull does Op::Postfix {
	has Bool $.negated;
	method postfix() {
		$!negated ?? 'IS NOT NULL' !! 'IS NULL';
	}
	method negate {
		Op::IsNull.new(:$!value, :negated(!$!negated));
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
}

class Op::NotBetween does Op::BetweenLike[False] {}
class Op::Between does Op::BetweenLike[True] {
	method negate() {
		Op::NotBetween.new(:$!left, :$!min, :$!max);
	}
}

role Op::Logical[Str $operator, Precedence $precedence, Constant $empty] does Expression {
	has Expression @.elements is required;

	method precedence(--> Precedence) { $precedence }
	method operator(--> Str) { $operator }

	method pack(*@elements) {
		given @elements.elems {
			when 1  { @elements[0] }
			when 0  { $empty.new }
			default { self.new(:@elements) }
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
	has Value::List:D $.expression is required handles<elements>;

	submethod BUILD(Expression :$!left, :@elements, Bool :$!negated = False) {
		$!expression = Column::List.new(:@elements);
	}

	method negate() {
		self.bless(:$!left, :$!expression, :negated(!$!negated));
	}
}

role Query { ... }

class Op::In::Query does Op::In {
	has Query:D $.query is required;
	method expression() { $!query }

	method negate() {
		self.new($!left, $!query, :negated(!$!negated));
	}
}

class Op::Exists does Expression {
	method precedence(--> Precedence::In) {}

	has Expression:D $.left is required;
	has Query:D $.query is required;
	has Bool $.negated;

	method negate() {
		self.new(:$!left, :$!query, :negated(!$!negated));
	}
}

class Op::Cast does Term {
	has Expression:D $.primary is required;
	has Str:D $.typename is required;
}

class Op::Case does Expression {
	method precedence(--> Precedence::Between) {}

	has Expression $.left;
	class When {
		has Expression:D $.condition is required;
		has Expression:D $.value is required;
	}
	has When:D @.whens;
	has Expression $.else;
}

role Conditional {
	has Expression:D $.expression is required;

	method new(:@expressions) {
		my $expression = Op::And.pack(@expressions);
		self.bless(:$expression);
	}

	multi method merge(Conditional:D: Conditional:D $other) {
		my $expression = Op::And.merge($!expression, $other.expression);
		self.bless(:$expression);
	}
	multi method merge(Conditional:U: Conditional:D $other) {
		self.bless(:expression($other.expression));
	}
	multi method merge(Conditional: Conditional:U $other) {
		self;
	}
}

class Conditions does Conditional {
	multi expand-partial(Expression $left, Any:D $expression) {
		Op::Equals.new(:$left, :right(expand-expression($expression)));
	}
	multi expand-partial(Expression $value, Any:U $) {
		Op::IsNull.new(:$value);
	}
	multi expand-partial(Expression $left, Constant:U $constant) {
		Op::Equals.new(:$left, :right($constant.new));
	}
	multi expand-partial(Expression $value, Constant $constant where Value::Null) {
		Op::IsNull.new(:$value);
	}

	multi expand-pair(Expression $value, 'isnull', $) {
		Op::IsNull.new(:$value);
	}
	multi expand-pair(Expression $value, 'isnotnull', $) {
		Op::IsNull.new(:$value, :negated);
	}
	multi expand-pair(Expression $left, Str $key, Any:D $value) {
		my $right = expand-expression($value);
		if %binary-op-for{$key}:exists {
			%binary-op-for{$key}.new(:$left, :$right);
		} else {
			Op::Comperative[$key].new(:$left, :$right);
		}
	}
	multi expand-partial(Expression $left, Pair (:$key, :$value)) {
		expand-pair($left, $key, $value);
	}

	multi expand-partial(Expression $left, %hash) {
		%hash.sort(*.key).map: { expand-partial($left, $^pair) };
	}
	multi expand-partial(Expression $left, Range $range) {
		my $min = expand-expression($range.min);
		my $max = expand-expression($range.max);
		Op::Between.new(:$left, :$min, :$max);
	}

	multi expand-junction(Expression $left, 'any', @partials) {
		Op::Or.pack(@partials);
	}
	multi expand-junction(Expression $left, 'any', @partials where @partials > 0 && all(@partials) ~~ Op::Equals && all(@partials).left === $left) {
		Op::In::List.new(:$left, :elements(@partials».right));
	}
	multi expand-junction(Expression $left, 'all', @partials) {
		Op::And.pack(@partials);
	}
	multi expand-junction(Expression $left, 'none', @partials) {
		Op::Or.pack(@partials).negate;
	}
	multi expand-junction(Expression $left, 'none', @partials where @partials > 0 && all(@partials) ~~ Op::Equals && all(@partials).left === $left) {
		Op::In::List.new(:$left, :elements(@partials».right), :negated);
	}
	multi expand-junction(Expression $left, 'one', @partials) {
		my @comparisons = @partials.map: { Op::Cast.new(:$^primary, :typename<INTEGER>) };
		my $addition = @comparisons.reduce: { Op::Plus.new(:$^left, :$^right) };
		Op::Equals.new(:left($addition), :right(Integer.new(:1value)));
	}
	multi expand-partial(Expression $left, Junction $junction) {
		use nqp;
		my $type = nqp::box_s(nqp::getattr($junction, Junction, '$!type'), Str);
		my @eigenstates = nqp::getattr($junction, Junction, '$!eigenstates').List;
		expand-junction($left, $type, @eigenstates.map({ expand-partial($left, $^value) }));
	}

	multi expand-condition(Pair (Identifier(Cool) :$key, Mu :$value)) {
		expand-partial($key, $value);
	}
	multi expand-condition(Pair (Expression :$key, Mu :$value)) {
		expand-partial($key, $value);
	}

	multi method COERCE(Expression $expression) {
		self.bless(:$expression);
	}
	multi method COERCE(Pair $pair) {
		self.new(:expressions[expand-condition($pair)]);
	}
	multi method COERCE(@list) {
		self.new(:expressions(@list.map(&expand-condition)));
	}
	multi method COERCE(%hash) {
		samewith(%hash.sort);
	}
	multi method COERCE(Literal(Capture) $expression) {
		self.bless(:$expression);
	}
}

class Order::Modifier does Expression {
	method precedence(--> Precedence::Comma) {}

	enum Sorting (:Asc<asc> :Desc<desc>);
	enum Nulls (:First<first>, :Last<last>);
	has Expression:D $.column is required;
	has Sorting $.order is required;
	has Nulls $.nulls;

	multi method COERCE(Pair (Expression:D :key($column), Sorting(Str) :value($order))) {
		self.new(:$column, :$order);
	}
	multi method COERCE(Pair (Identifier:D(Cool:D) :key($column), Sorting(Str) :value($order))) {
		self.new(:$column, :$order);
	}
	multi method COERCE(Map (Expression:D :$column, Sorting(Str) :$order, Nulls(Str) :$nulls)) {
		self.new(:$column, :$order, :$nulls)
	}
	multi method COERCE(Map (Identifier:D(Cool:D) :$column, Sorting(Str) :$order, Nulls(Str) :$nulls)) {
		self.new(:$column, :$order, :$nulls)
	}
}

class OrderBy {
	has Expression:D @.elements is required;

	multi to-sorter(Expression:D $expression) {
		$expression;
	}
	multi to-sorter(Identifier:D(Cool:D) $ident) {
		$ident;
	}
	multi to-sorter(Order::Modifier:D(Pair:D) $modifier) {
		$modifier;
	}
	multi to-sorter(Order::Modifier:D(Map:D) $modifier) is default {
		$modifier;
	}

	multi method COERCE(Any:D $sorter) {
		self.new(:elements[to-sorter($sorter)]);
	}
	multi method COERCE(@list) {
		my @elements = @list.map(&to-sorter);
		self.new(:@elements);
	}
}

class Row does Value::List {
	has Expression @.elements is required;

	method COERCE(@values) {
		my Expression @elements = @values.map(&expand-expression);
		self.new(:@elements);
	}
}

class Rows {
	has Row @.elements is required;

	method COERCE(@input) {
		my Row(Any) @elements = @input;
		self.new(:@elements);
	}
}

class Assigns {
	has Pair @.pairs;

	multi transform-pair(Pair (Expression :$key, :$value)) {
		my $expression = expand-expression($value);
		$key => $expression;
	}
	multi transform-pair(Pair (Identifier(Cool) :$key, :$value)) {
		my $expression = expand-expression($value);
		$key => $expression;
	}

	multi method COERCE(@list) {
		my @pairs = @list.unique(:as(*.key)).map(&transform-pair);
		self.new(:@pairs);
	}
	multi method COERCE(%set) {
		my @pairs = %set.sort(*.key).map(&transform-pair);
		self.new(:@pairs);
	}
	multi method COERCE(Pair $pair) {
		my @pairs = transform-pair($pair);
		self.new(:@pairs);
	}

	method assignments() {
		@!pairs.map: { Op::Assign.new(:left($^pair.key), :right($^pair.value)) };
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
	has Value::List:D $.arguments is required;
	has OrderBy $.order-by;
	has Conditions $.filter;

	method COERCE(Map (Str :$name!, Column::List:D(Any:D) :$arguments = (), Conditions(Any) :$filter, Quantifier(Str) :$quantifier, OrderBy(Any) :$order-by)) {
		Function.new(:$name, :$arguments, :$filter, :$quantifier, :$order-by);
	}

	method precedence() {
		$!filter ?? Precedence::Between !! Precedence::Termlike;
	}

	method over(*%arguments) {
		my $window = Window::Definition.COERCE(%arguments);
		Window::Function.new(:function(self), :$window);
	}

	method as(Identifier:D(Cool:D) $alias, Identifiers(Any) $columns?, Bool :$lateral, Bool :$ordinal) {
		Source::Function.new(:function(self), :$alias, :$columns, :$lateral, :$ordinal);
	}
}

package Window {
	enum Exclusion (:CurrentRow('current row'), :Group<group>, :Ties<ties>, :NoOthers('no others'));
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
		multi method COERCE(Pair (Str :$key where 'preceding', Any :$value)) {
			my $offset = expand-expression($value);
			Boundary::Preceding::Bounded.new(:$offset);
		}
		multi method COERCE(Pair (Str :$key where 'following', Any :$value)) {
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
	class Boundary::Preceding::Unbounded does Boundary::Unbounded['UNBOUNDED PRECEDING'] {}
	class Boundary::Preceding::Bounded does Boundary::Bounded['PRECEDING'] {}
	class Boundary::CurrentRow does Boundary::Unbounded['CURRENT ROW'] {}
	class Boundary::Following::Bounded does Boundary::Bounded['FOLLOWING'] {}
	class Boundary::Following::Unbounded does Boundary::Unbounded['UNBOUNDED FOLLOWING'] {}

	class Frame {
		has Window::Mode:D $.mode is required;
		has Boundary:D $.from is required;
		has Boundary $.to is required;
		has Exclusion $.exclude;

		method COERCE(Map (Boundary(Any) :$from!, Boundary(Any) :$to, Exclusion(Str) :$exclude, Mode:D(Str) :$mode = Mode::Rows)) {
			self.new(:$mode, :$from, :$to, :$exclude);
		}
	}

	class Definition {
		has Identifier   $.existing;
		has Column::List $.partition-by;
		has OrderBy      $.order-by;
		has Frame        $.frame;

		method is-simple() {
			$!existing && !$!partition-by && !$!order-by;
		}

		method COERCE(Map (Column::List(Any) :$partition-by, OrderBy(Any) :$order-by, Identifier(Cool) :$existing, Window::Frame(Map) :$frame)) {
			self.new(:$existing, :$partition-by, :$order-by, :$frame);
		}
	}

	class Clause {
		has Identifier:D         $.name is required;
		has Window::Definition:D $.definition is required;

		method COERCE(Pair (Identifier(Cool) :$key, Window::Definition(Map) :$value)) {
			self.new(:name($key), :definition($value));
		}
	}

	class Clauses {
		has Window::Clause @.windows;

		multi method COERCE(@specs) {
			my Window::Clause(Pair) @windows = @specs;
			self.new(:@windows);
		}
		multi method COERCE(Window::Clause(Pair) $window) {
			self.new(:windows[ $window ]);
		}
	}
}

class Window::Function does Expression {
	method precedence(--> Precedence::Comma) {}

	has Function:D $.function is required;
	has Window::Definition:D $.window is required;
}

class Locking {
	enum Strength (:Update<update> :NoKeyUpdate('no key update') :Share<share> :KeyShare('key share'));

	has Strength:D $.strength is required;
	has Identifier $.table;
	has Bool $.no-wait;
	has Bool $.skip-locked;

	multi method COERCE(Strength(Str) $strength) {
		self.new(:$strength);
	}
	multi method COERCE(Pair (Strength(Str) :$key, Identifier(Cool) :$value)) {
		self.new(:strength($key), :table($value));
	}
}

class Select { ... }

class Compound {
	enum Type <Union Intersect Except>;
	has Type:D $.type = Union;
	has Bool $.all;
	has Select $.next;

	multi method COERCE(Pair $ (Str :$key, Select(Map) :$value)) {
		my $operation = $key.lc;
		my $all = so $operation ~~ s/ '-all' $ //;
		my $type = Type.WHO{$operation.lc.tc};
		self.new(:$type, :next($value), :$all);
	}
}

class Limit {
	class All does Constant['ALL'] {}

	has Expression:D $.value is required;

	multi method COERCE(Expression:D $value) {
		self.new(:$value);
	}
	multi method COERCE(Placeholder:D(Any:D) $value) {
		self.new(:$value);
	}
	multi method COERCE(Inf) {
		self.new(:value(All.new));
	}
}

class Offset {
	has Expression:D $.value is required;

	multi method COERCE(Expression:D $value) {
		self.new(:$value);
	}
	multi method COERCE(Placeholder:D(Any:D) $value) {
		self.new(:$value);
	}
}

class Source::Query { ... }
role Query does Expression {
	method precedence(--> Precedence::Rowlike) {}

	method create(|) { ... }

	method COERCE(%arguments) {
		self.create(|%arguments);
	}

	method as(Identifier:D(Cool:D) $alias, Bool :$lateral) {
		Source::Query.new(:query(self), :$alias, :$lateral);
	}
}

class Values does Query {
	has Rows:D  $.rows is required;
	has OrderBy $.order-by;
	has Limit   $.limit;
	has Offset  $.offset;

	method create(Rows:D(Any:D) :$rows!, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset) {
		self.new(:$rows, :$order-by, :$limit, :$offset);
	}
}

role Table { ... }

class Common {
	class Name {
		has Identifier:D $.name is required;
		has Identifiers $.columns;

		multi method COERCE(Identifier:D(Cool:D) $name) {
			self.new(:$name);
		}
		multi method COERCE(Pair $pair (Identifier(Cool) :key($name), Identifiers(Any) :value($columns))) {
			self.new(:$name, :$columns);
		}
	}
	class Rename {
		has Name:D $.alias is required;
		has Query:D $.query is required;

		multi method COERCE(Pair $pair (Name:D(Any:D) :key($alias), Select:D(Hash:D) :value($query))) {
			self.new(:$alias, :$query);
		}
		multi method COERCE(Pair $pair (Name:D(Any:D) :key($alias), Table:D(Str:D) :value($source))) {
			my $query = Select.create(:$source);
			self.new(:$alias, :$query);
		}
		multi method COERCE(Pair $pair (Name:D(Any:D) :key($alias), Query:D :value($query))) {
			self.new(:$alias, :$query);
		}
	}
	has Rename @.tables;
	has Bool $.recursive;

	method COERCE(@pairs) {
		my Rename(Pair) @tables = @pairs;
		self.new(:@tables);
	}
}

role Query::Common does Query {
	has Common(Any)      $.common-tables;
}

class Distinction::Full { ... }
class Distinction::Columns { ... }

role Distinction {
	multi method COERCE(Bool $value) {
		$value ?? Distinction::Full.new !! Distinction;
	}
	multi method COERCE(Column::List(Cool) $columns) {
		Distinction::Columns.new(:$columns);
	}
}

class Distinction::Full does Distinction {}

class Distinction::Columns does Distinction {
	has Column::List $.columns;
}

class GroupBy {
	has Expression @.elements;
	has Bool $.all;

	multi to-grouping(Any $column) {
		Column::List::to-column($column);
	}
	multi to-grouping(Column::List(List) $column) {
		$column;
	}
	multi to-grouping(Pair $pair) {
		Function.COERCE({ :name($pair.key), :arguments($pair.value) });
	}
	multi to-grouping(Pair $pair (:$key where $key.lc eq 'grouping sets', :$value)) {
		my @values = $value.map(&to-grouping);
		Function.COERCE({ :name($key), :arguments(@values) });
	}

	multi method COERCE(Any $grouping) {
		self.new(:elements[to-grouping($grouping)]);
	}
	multi method COERCE(@list) {
		my @elements = @list.map(&to-grouping);
		self.new(:@elements);
	}
	multi method COERCE(Identifiers $elements) {
		self.new(:elements($elements.elements));
	}
	multi method COERCE(Column::List $elements) {
		self.new(:elements($elements.elements));
	}
	multi method COERCE(Hash $ (Bool :$all, :@elements)) {
		self.new(:$all, :elements(@elements.map(&to-grouping)));
	}
}

role Source { ... }

class Select does Query::Common {
	has Column::List:D  $.columns is required;
	has Distinction     $.distinct;
	has Source          $.source;
	has Conditions      $.where;
	has GroupBy         $.group-by;
	has Conditions      $.having;
	has Window::Clauses $.windows;
	has Compound        $.compound;

	has OrderBy         $.order-by;
	has Limit           $.limit;
	has Offset          $.offset;
	has Locking         $.locking;

	method create(Source(Any) :$source, Column::List:D(Any:D) :$columns = *, Conditions(Any) :$where, Common(Any) :$common-tables, Distinction(Any) :$distinct, GroupBy(Any) :$group-by, Conditions(Any) :$having, Window::Clauses(Any) :$windows, Compound(Pair) :$compound, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset, Locking(Any) :$locking) {
		Select.new(:$common-tables, :$distinct, :$columns, :$source, :$where, :$group-by :$having, :$windows, :$compound, :$order-by, :$limit, :$offset, :$locking);
	}
}

class Update does Query::Common {
	has Table:D      $.target is required;
	has Op::Assign:D @.assignments;
	has Source       $.from;
	has Conditions   $.where;
	has Column::List $.returning;

	method create(Table(Cool) :$target!, Assigns:D(Any:D) :$assigns!, Source(Any) :$from, Conditions(Any) :$where, Column::List(Any) :$returning) {
		my @assignments = $assigns.assignments;
		Update.new(:$target, :@assignments, :$where, :$from, :$returning);
	}
}

enum Overriding (:System<system>, :User<user>);

class Conflict::Target::Columns { ... }
class Conflict::Target::Constraint { ... }

role Conflict::Target {
	multi method COERCE(Pair (:$key where 'columns', Identifiers:D(Any:D) :$value)) {
		Conflict::Target::Columns.new(:columns($value));
	}
	multi method COERCE(Pair (:$key where 'columns', Map :value($) (Identifiers:D(Any:D) :$columns, Conditions(Any) :$where))) {
		Conflict::Target::Columns.new(:$columns, :$where);
	}
	multi method COERCE(Pair (:$key where 'constraint', Identifier:D(Cool:D) :$value)) {
		Conflict::Target::Constraint.new(:constraint($value));
	}
}

class Conflict::Target::Columns does Conflict::Target {
	has Identifiers:D $.columns is required;
	has Conditions    $.where;
}

class Conflict::Target::Constraint does Conflict::Target {
	has Identifier:D $.constraint is required;
}

class Conflict::Nothing { ... }

class Conflict::Update { ... }

role Conflict {
	has Conflict::Target $.target;

	multi method COERCE('nothing') {
		Conflict::Nothing.new;
	}
	multi method COERCE(Map (Conflict::Target(Pair) :$target?, Str:D :$do! where 'nothing')) {
		Conflict::Nothing.new(:$target);
	}
	multi method COERCE(Map (Conflict::Target:D(Pair) :$target!, Assigns:D(Any:D) :$do!, Conditions(Any) :$where)) {
		my @assignments = $do.assignments;
		Conflict::Update.new(:$target, :@assignments, :$where);
	}
}

class Conflict::Nothing does Conflict {}

class Conflict::Update does Conflict {
	has Op::Assign @.assignments;
	has Conditions $.where;
}

role Insert does Query::Common {
	has Table:D      $.target is required;
	has Identifiers  $.columns;
	has Overriding   $.overriding;
	has Conflict     $.conflict;
	has Column::List $.returning;
}

class Insert::Values does Insert {
	has Rows:D $.rows is required;

	method create(Table(Any) :$target, Identifiers(Any) :$columns, Rows:D(List:D) :$rows, Overriding(Str) :$overriding, Conflict(Any) :$conflict, Column::List(Any) :$returning) {
		Insert::Values.new(:$target, :$columns, :$rows, :$overriding, :$conflict, :$returning);
	}
}

class Insert::Select does Insert {
	has Select:D $.select is required;

	method create(Table(Any) :$target, Identifiers(Any) :$columns, Select(Map) :$select, Overriding(Str) :$overriding, Conflict(Any) :$conflict, Column::List(Any) :$returning) {
		Insert::Select.new(:$target, :$columns, :$select, :$overriding, :$conflict, :$returning);
	}
}

class Insert::Defaults does Insert {
	method create(Table(Any) :$target,  Overriding(Str) :$overriding, Conflict(Any) :$conflict, Column::List(Any) :$returning) {
		Insert::Defaults.new(:$target, :$overriding, :$conflict, :$returning);
	}
}

class Delete does Query::Common {
	has Table:D      $.target is required;
	has Source       $.using;
	has Conditions   $.where;
	has Column::List $.returning;

	method create(Table(Cool) :$target!, Source(Any) :$using, Conditions(Any) :$where, Column::List(Any) :$returning) {
		Delete.new(:$target, :$where, :$using :$returning);
	}
}

class Join::On { ... }
class Join::Conditions { ... }
class Join::Using { ... }
class Join::Natural { ... }
class Join::Cross { ... }
class Table::Simple { ... }
class Table::Renamed { ... }

enum Join::Type (:Inner<inner>, :Left<left>, :Right<right>, :Full<full>);

role Source {
	multi method COERCE(Identifier:D(Str:D) $name) {
		Table::Simple.new(:$name);
	}
	multi method COERCE(Identifier:D(List:D) $name) {
		Table::Simple.new(:$name);
	}

	multi method COERCE(Pair (Identifier:D(Cool:D) :$key, Identifier(Cool) :$value)) {
		Table::Renamed.new(:name($value), :alias($key));
	}
	multi method COERCE(Pair (Identifier:D(Cool:D) :$key, Query:D :$value)) {
		Source::Query.new(:query($value), :alias($key));
	}
	multi method COERCE(Pair (Identifier:D(Cool:D) :$key, Function:D :$value)) {
		Source::Function.new(:function($value), :alias($key));
	}
	multi method COERCE(Pair (Identifier:D(Cool:D) :$key, Map:D :$value ( :function($name)!, *%args ))) {
		my Function(Map) $function = { :$name, |%args };
		Source::Function.new(:$function, :alias($key));
	}
	
	multi method COERCE(Map (Identifier:D(Cool:D) :$table!, Identifier() :$alias!, Identifiers() :$columns, Bool :$only)) {
		Table::Renamed.new(:$table, :$alias, :$columns, :$only);
	}
	multi method COERCE(Map (Identifier:D(Cool:D) :$table!, Bool :$only)) {
		Table::Simple.new(:$table, :$only);
	}
	multi method COERCE(Map (Function:D(Map) :$function!, Identifier() :$alias!, Identifiers() :$columns, Bool :$lateral, Bool :$ordinal)) {
		Source::Function.new(:$function, :$alias, :$columns, :$lateral, :$ordinal);
	}
	multi method COERCE(Map (Select(Map) :$query!, Identifier:D(Cool:D) :$alias!, Identifiers() :$columns, Bool :$lateral)) {
		Source::Query.new(:query($query), :$alias, :$columns, :$lateral);
	}
	multi method COERCE(Map (Source:D(Any:D) :$left!, Source:D(Any:D) :$right!, *%args)) {
		$left.join($right, |%args);
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
}

role Source::Named does Source {
	method alias() { ... }
	
	multi method concat(Identifier $column) {
		self.alias.concat($column);
	}
	multi method concat-all(Identifiers $columns) {
		$columns.elements.map: { self.concat($^column) };
	}
}

role Table does Source {
	has Identifier:D $.name is required;
	has Bool         $.only;
}

class Table::Simple does Table does Source::Named {
	method alias() { $!name }
}

role Source::Aliased does Source::Named {
	has Identifier:D $.alias is required;
	has Identifiers $.columns;
}

class Table::Renamed does Table does Source::Aliased {}

role Join does Source {
	has Source:D $.left is required;
	has Source:D $.right is required;
}

class Join::Conditions does Conditional {
	sub expand(Pair (Identifier(Cool) :key($left), Identifier(Cool) :value($right))) {
		Op::Equals.new(:$left, :$right);
	}

	multi method COERCE(Conditions $conditions) {
		self.bless(:expression($conditions.expression));
	}
	multi method COERCE(Pair $pair) {
		self.new(:expressions[ expand($pair) ]);
	}
	multi method COERCE(@list) {
		self.new(:expressions(@list.map(&expand)));
	}
	multi method COERCE(%hash) {
		samewith(%hash.sort(*.key));
	}

	method from-using(Source::Named $left, Source::Named $right, Identifiers $using) {
		my @expressions = $using.elements.map({ Op::Equals.new(:left($left.concat($^item)), :right($right.concat($^item))) });
		self.new(:@expressions);
	}
}
class Join::On does Join {
	has Join::Type:D $.type is required;
	has Join::Conditions $.on;
}

class Join::Using does Join {
	has Join::Type:D $.type is required;
	has Identifiers $.using;

	method to-join-on() {
		my $on = Join::Conditions.from-using($!left, $!right, $!using);
		Join::On.new(:$!left, :$!right, :$!type, :$on);
	}
}

class Join::Natural does Join {
	has Join::Type:D $.type is required;
}

class Join::Cross does Join {}

role Source::Nested does Source::Aliased {
	has Bool $.lateral;
}

class Source::Query does Source::Nested {
	has Query:D $.query is required;
}

class Source::Function does Source::Nested {
	has Function:D $.function is required;
	has Bool       $.ordinal;
}

class Delayed {
	has Str:D $.identifier is required;
	has Any:U $.type;
	has Bool:D $.has-default = False;
	has Any $.default;

	proto method new(|args) { * }
	multi method new(Str:D $identifier, Any:U :$type) {
		self.bless(:$identifier, :$type);
	}
	multi method new(Str:D $identifier, Any $default, Any:U :$type = $default.WHAT) {
		self.bless(:$identifier, :$type, :$default, :has-default);
	}
}

class Result {
	has Str:D $.sql is required;
	has @.arguments is required;

	multi type-of(Any $value) {
		$value.WHAT;
	}
	multi type-of(Delayed $value) {
		$value.type;
	}
	method type-hints() {
		@!arguments.map(&type-of);
	}

	multi resolve-value(Any $value, %replacements) {
		$value;
	}
	multi resolve-value(Delayed $delayed, %replacements) {
		if %replacements{$delayed.identifier}:exists {
			%replacements{$delayed.identifier};
		} elsif $delayed.has-default {
			$delayed.default;
		} else {
			die "No value given for delayed value '$delayed.identifier()'"
		}
	}

	method resolve(%replacements?) {
		@!arguments.map: { resolve-value($^element, %replacements) };
	}

	method identifiers() {
		@!arguments.grep(Delayed)».identifier;
	}

	method has-delayed(--> Bool) {
		so any(@!arguments) ~~ Delayed;
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

	sub parenthesize-if(Str $input, Bool() $parenthese --> Str) {
		$parenthese ?? "($input)" !! $input;
	}
	sub parenthesize-list(@items, Str $with = " ") {
		"({ @items.join($with) })";
	}

	method render-expressions(Placeholders $placeholders, @elements, Precedence $precedence = Precedence::Comma, Str $with = ', ') {
		@elements.map({ self.render-expression($placeholders, $^element, $precedence) }).join($with);
	}

	method render-expression-list(Placeholders $placeholders, @elements, Bool $parenthesize --> Str) {
		my $result = self.render-expressions($placeholders, @elements, Precedence::Comma);
		parenthesize-if($result, $parenthesize);
	}

	method quote-identifier-part(Str $identifier --> Str) {
		$!quoting || $identifier ~~ /<-[\w\*]>/ ?? '"' ~ $identifier.subst('"', '""', :g) ~ '"' !! $identifier;
	}

	method render-identifier(Identifier $ident --> Str) {
		$ident.parts.map({ self.quote-identifier-part($^id) }).join('.');
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
	multi method render-expression(Placeholders $placeholders, Value::List $parentheses, Precedence --> Str) {
		self.render-expression-list($placeholders, $parentheses.elements, False);
	}
	multi method render-expression(Placeholders $placeholders, Op::Cast $cast, Precedence $ --> Str) {
		"CAST({ self.render-expression($placeholders, $cast.primary, Precedence::Comma) } AS $cast.typename())";
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
	multi method render-expression(Placeholders $placeholders, Op::Liking $liking, Precedence --> Str) {
		my @result;
		@result.push: self.render-expression($placeholders, $liking.left, $liking.precedence);
		@result.push: $liking.negated ?? 'NOT LIKE' !! 'LIKE';
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
		my $operator = $between.negated ?? 'NOT BETWEEN' !! 'BETWEEN';
		my $min = self.render-expression($placeholders, $between.min, Precedence::Between);
		my $max = self.render-expression($placeholders, $between.max, Precedence::Between);
		"$left $operator $min AND $max";
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

	multi method render-column-expressions(Placeholders $placeholders, Column::List:D $columns, Str $prefix? --> List) {
		my $results = self.render-expressions($placeholders, $columns.elements);
		$prefix ?? ($prefix, $results) !! ($results,);
	}
	multi method render-column-expressions(Placeholders $placeholders, Column::List:U $columns, Str $prefix? --> List) {
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
		my $name = self.render-identifier($window.name);
		my $definition = self.render-window-definition($placeholders, $window.definition);
		"WINDOW $name AS $definition";
	}

	multi method render-windows(Placeholders $placeholders, Window::Clauses:D $windows --> Str) {
		$windows.windows.map({ self.render-window($placeholders, $^window) }).join(', ');
	}
	multi method render-windows(Placeholders $placeholders, Window::Clauses:U $windows --> List) {
		Empty;
	}

	multi method render-compound(Placeholders $placeholders, Compound:D $compound --> List) {
		my @type = $compound.all ?? 'ALL' !! Empty;
		my $next = self.render-select-core($placeholders, $compound.next);
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

	method render-select-core(Placeholders $placeholders, Select $select --> Str) {
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
		@parts.append: self.render-select-core($placeholders, $select);
		@parts.append: self.render-order-by($placeholders, $select.order-by);
		@parts.append: self.render-limit-offset($placeholders, $select.limit, $select.offset);
		@parts.append: self.render-locking($select.locking);
		@parts.join(' ');
	}

	multi method render-expression(Placeholders $placeholders, Update $update, Precedence --> Str) {
		my @common       = self.render-common-tables($placeholders, $update.common-tables);
		my @target       = self.render-table($update.target);
		my @set          = 'SET', self.render-expressions($placeholders, $update.assignments);
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
		my @where = self.render-conditions($placeholders, $target.where);
		$columns, |@where;
	}
	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target::Constraint:D $target) {
		'ON CONSTRAINT', self.render-identifier($target.constraint);
	}
	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target:U $conflict) {
		Empty;
	}

	multi method render-conflict(Placeholders $placeholders, Conflict::Nothing:D $conflict) {
		my @target = self.render-conflict-target($placeholders, $conflict.target);
		'ON CONFLICT', |@target, 'DO NOTHING';
	}
	multi method render-conflict(Placeholders $placeholders, Conflict::Update:D $conflict) {
		my @result = 'ON CONFLICT';
		@result.push: self.render-conflict-target($placeholders, $conflict.target);
		@result.push: 'DO UPDATE SET';
		@result.push: self.render-expressions($placeholders, $conflict.assignments);
		@result.push: self.render-conditions($placeholders, $conflict.where);
		@result;
	}
	multi method render-conflict(Placeholders $placeholders, Conflict:U $conflict) {
		Empty;
	}

	multi method render-expression(Placeholders $placeholders, Insert $insert, Precedence --> Str) {
		my @common     = self.render-common-tables($placeholders, $insert.common-tables);
		my @target     = self.render-table($insert.target);
		my @columns    = self.render-identifiers($insert.columns);
		my @overriding = self.render-overriding($insert.overriding);
		my @content    = self.render-insert-content($placeholders, $insert);
		my @conflict   = self.render-conflict($placeholders, $insert.conflict);
		my @returning  = self.render-column-expressions($placeholders, $insert.returning, 'RETURNING');

		(@common, 'INSERT INTO', @target, @columns, @overriding, @content, @conflict, @returning).flat.join(' ');
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
		my @rows     = self.render-values($placeholders, $values.rows);
		my @order-by = self.render-order-by($placeholders, $values.order-by);
		my @limits   = self.render-limit-offset($placeholders, $values.limit, $values.offset);

		(@rows, @order-by, @limits).flat.join(' ');
	}

	method render(Expression $expression) {
		my $placeholders = self.placeholders.new;
		my $sql          = self.render-expression($placeholders, $expression, Precedence::Rowlike);
		Result.new(:$sql, :arguments($placeholders.values));
	}
}

has Renderer:D $.renderer is required handles<render>;

multi submethod BUILD(Renderer:D :$!renderer!) {}
my %holder-for = (
	dbi      => SQL::Placeholders::DBI,
	postgres => SQL::Placeholders::Postgres,
);
multi submethod BUILD(Renderer::SQL:U :$renderer = Renderer::SQL, Str:D :placeholders($placeholder-str)!, *%arguments) {
	my $placeholders = %holder-for{$placeholder-str}:exists ?? %holder-for{$placeholder-str} !! die "No such placeholder style $placeholder-str";
	$!renderer = $renderer.new(:$placeholders, |%arguments);
}
multi submethod BUILD(Renderer::SQL:U :$renderer = Renderer::SQL, *%arguments) {
	$!renderer = $renderer.new(|%arguments);
}

method select(Source:D(Any:D) $source, Column::List:D(Any:D) $columns = *, Conditions(Any) $where?, Common(Any) :$common-tables, Distinction(Any) :$distinct, GroupBy(Any) :$group-by, Conditions(Any) :$having, Window::Clauses(Any) :$windows, Compound(Pair) :$compound, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset, Locking(Any) :$locking) {
	my $select = Select.new(:$common-tables, :$distinct, :$columns, :$source, :$where, :$group-by :$having, :$windows, :$compound, :$order-by, :$limit, :$offset, :$locking);
	$!renderer.render($select);
}

method update(Table:D(Any:D) $target, Assigns:D(Any:D) $assigns, Conditions(Any) $where?, Common(Any) :$common-tables, Source(Any) :$from, Column::List(Any) :$returning) {
	my @assignments = $assigns.assignments;
	my $update = Update.new(:$common-tables, :$target, :@assignments, :$where, :$from, :$returning);
	$!renderer.render($update);
}

multi method insert(Table:D(Any:D) $target, Identifiers(Any) $columns, Rows:D(List:D) $rows, Common(Any) :$common-tables, Overriding(Str) :$overriding, Conflict(Any) :$conflict, Column::List(Any) :$returning) {
	my $insert = Insert::Values.new(:$common-tables, :$target, :$columns, :$rows, :$overriding, :$conflict, :$returning);
	$!renderer.render($insert);
}
multi method insert(Table:D(Any:D) $target, Assigns:D(Any:D) $values, Common(Any) :$common-tables, Overriding(Str) :$overriding, Conflict(Any) :$conflict, Column::List(Any) :$returning) {
	samewith($target, $values.keys, [$values.values,], :$common-tables, :$overriding, :$conflict, :$returning);
}
multi method insert(Table(Any) $target, Identifiers(Any) $columns, Select(Map) $select, Common(Any) :$common-tables, Overriding(Str) :$overriding, Conflict(Any) :$conflict, Column::List(Any) :$returning) {
	my $insert = Insert::Select.new(:$common-tables, :$target, :$columns, :$select, :$overriding, :$conflict, :$returning);
	$!renderer.render($insert);
}
multi method insert(Table:D(Any:D) $target, Value::Default, Common(Any) :$common-tables, Overriding(Str) :$overriding, Conflict(Any) :$conflict, Column::List(Any) :$returning) {
	my $insert = Insert::Defaults.new(:$common-tables, :$target, :$overriding, :$conflict, :$returning);
	$!renderer.render($insert);
}

method delete(Table:D(Any:D) $target, Conditions(Any) $where, Common(Any) :$common-tables, Source(Any) :$using, Column::List(Any) :$returning) {
	my $delete = Delete.new(:$common-tables, :$target, :$where, :$using, :$returning);
	$!renderer.render($delete);
}

multi method values(Rows:D(List:D) $rows, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset) {
	my $values = Values.new(:$rows, :$order-by, :$limit, :$offset);
	$!renderer.render($values);
}

method begin() {
	Result.new(:sql<BEGIN>);
}

method rollback() {
	Result.new(:sql<ROLLBACK>);
}

method commit() {
	Result.new(:sql<COMMIT>);
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

method identifiers(Identifiers(Any) $idents) {
	$idents;
}

method binary(Str $operator, Expression $left, Expression $right, *%args) {
	my $class = %binary-op-for{$operator.lc}:exists ?? %binary-op-for{$operator.lc} !! die "No such operator '$operator'";
	$class.new(:$left, :$right, |%args);
}

method logical(Str $operator, @elements) {
	my $class = $operator.lc eq 'and' ?? Op::And !! $operator.lc eq 'or' ?? Op::Or !! die "No such operator '$operator'";
	$class.new(:@elements);
}

method not(Expression $expression) {
	Op::Not.new($expression)
}

method function(Str $name, Column::List:D(Any:D) $arguments = (), Conditions(Any) :$filter, Quantifier(Str) :$quantifier, OrderBy(Any) :$order-by) {
	Function.new(:$name, :$arguments, :$filter, :$quantifier, :$order-by);
}

method value(Any $value) {
	expand-expression($value);
}

method delay(|arguments) {
	Delayed.new(|arguments);
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

my $abstract = SQL::Abstract.new(:placeholders<dbi>);
my $query = $abstract.select('table', <foo bar>, :id(3));
my $result = $dbh.query($result.sql, $result.arguments);

my $join = { :left<books>, :right<authors>, :using<author_id> };
my $result = $abstract.select($join, ['books.name', 'authors.name'], { 'cost' => { '<' => 10 }});

=end code

=head1 Description

SQL::Abstract abstracts the generation of SQL queries. Fundamentally its functionality has three components

=item1 An AST to represent SQL queries
=item1 A set of helpers to convert standard raku datatypes into such an AST
=item1 A renderer to turn that AST into an SQL query

It should be able to represent any C<SELECT>, C<UPDATE>, C<INSERT>, or C<DELETE> query that is valid in both Postgresql and SQLite. This subset should be generally portable to other databases as well.

=head1 Helper types

SQL::Abstract uses various helper types:

=head2 SQL::Abstract::Identifier

This represents an identifier (typically a table name or column name, or an alias for such). It can be created from either:

=begin item1
Str

This will interpret a string (e.g. C<"foo"> or C<"foo.bar">) as an identifier.
=end item1

=begin item1
List

This will interpret the elements of the list representing the components of the name. E.g. C<< <bar baz> >> is equivalent to C< "bar.baz" >.
=end item1

=head2 SQL::Abstract::Identifiers

This takes either a list of C<Identifier()>, or a single C<Identifier()>. Note that a single list will be interpreted will be interpreted as a list of string identifiers, if one wants to pass a single list-from identifier the list must be nested (e.g. C<[ <table column>,]>).

=head2 SQL::Abstract::Source

A source is source of data, usually a table or a join. If not passed as a C<Source> object it will upconvert from the following types:

=begin item1
Str

This represents the named table, e.g. C<"my_table">.
=end item1

=begin item1
List

This represents the named table, with the elements of the list representing the components of the table name. E.g. C<< <bar baz> >> is equivalent to C< "bar.baz" >.
=end item1

=begin item1
Pair (Str => Identifier(Cool))

This will rename the table in the value to the name in the key.
=end item1

=begin item1
Pair (Str => Select(Map))

This will use the result of a subquery as if it's a table.
=end item1

=begin item1
Hash

This will join two `Source`s, named C<left> and C<right>, it requires one of the following entries to join them on:

=item3 Join::Conditions() :$on
=item3 Identifiers() :$conditions
=item3 Bool :$natural
=item3 Bool :$cross

e.g. C<< { :left<artist>, :right<album>, :using<album_id> } >> or C<< { :left<artist>, :right<album>, :on{'artist.id' => 'album.artist_id'} } >>

The first three joiners take an optional C<:$type> argument that can be any of C<"inner">/C<Join::Type::Inner>, C<"left">/C<Join::Type::Left>, C<"right">/C<Join::Type::Right> or C<"full">/C<Join::Type::Full>.

=end item1

=head2 SQL::Abstract::Table does SQL::Abstract::Source

This role takes the same conversions as C<Source>, but only the ones that represent a table. Unlike other sources, this can be used for mutating operations (update/insert/delete).

=head2 SQL::Abstract::Column::List

This is a list much like C<Identifiers>, however it will accept not just identifiers but any expression (e.g. comparisons, function calls, etc…). If given a pair it will rename the value to the key (C<value AS key>). A whatever-star will represent all columns.

=head2 SQL::Abstract::Conditions

This is a pair, a list of pairs, a hash or an C<Expression>. In the former three cases, the key (called left in the rest of this section) shall be an C<Identifier()> designating a column name, or an C<Expression>. The right hand side can be one of several types:

=head3 Expression

This will be used as-is

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

=head3 Range

This will check if a value is in a certain range. E.g. C<:left(1..42)> will render like C<left BETWEEN 1 AND 42>.

=head3 Map

This will be interpreted as a conjunction of the hash pairs. E.g. C<< :left{ '>' => 3, '<' => 42 } >> will render like C<< left > 3 AND left < 42 >>.

=head3 Junction

This will check against the values in the function. E.g. C<:left(1|2|4)> will render like C<left IN (1, 2, 4)>.

=head3 Capture

This will be used as a literal value. E.g. C<:left(\'NOW()')> will render like C<left = NOW()>.

=head3 Any

If none of the above options match, the value will be compared to as is (as a placeholder). C<:left(2)> will render equivalent to C<left = 2>.

=head2 SQL::Abstract::Assigns

This takes a list of pairs, or a hash. The keys shall be a value or an expression. E.g. C<< :name<author>, :id(SQL::Abstract::Values::Default), :timestamp(\'NOW()') >>

=head2 SQL::Abstract::OrderBy

This takes a list of things to sort by. Much like C<Column::List> this accepts identifiers and expressions, but C<*> isn't allowed and pair values are interpreted as order modifier (e.g. C<:column<desc>>). A hash element will be expanded as well (e.g. C<< { :column<column_name>, :order<desc>, :nulls<last> } >>)

=head1 Class SQL::Abstract

This is the main class of the 

=head3 new(:$placeholders!)

This creates a new C<SQL::Abstract> object. It has one mandatory name argument, C<$placeholders>, which takes one of the following values:

=begin item1
C<dbi>/C<SQL::Abstract::Placeholders::DBI>

This will use DBI style C<(?, ?)> placeholders
=end item1

=begin item1
C<postgres>/C<SQL::Abstract::Placeholders::Postgres>

This will use Postgres style C<($1, $2)> placeholders.
=end item1

=head3 select(Source() $source, Column::List() $columns = *, Conditions() $where?, Column::List() :$group-by, Conditions() :$having, OrderBy() :$order-by, Int :$limit, Int :$offset)

This will generate a C<SELECT> query. It will select C<$columns> from C<$source>, filtering by $conditions. 

=begin code :lang<raku>

my $join = { :left<books>, :right<authors>, :using<author_id> };
my $result = $abstract.select($join, ['books.name', 'authors.name'], { :cost{ '<' => 10 } });
# SELECT books.name, authors.name FROM books INNER JOIN authors USING (author_id) WHERE cost < 10

my @columns = [ 'name', :sum{ :function<count>, :arguments(*) } ];
$abstract.select('artists', @columns, { :name(:like('A%')) }, :group-by<name>, :order-by(:sum<desc>));
# SELECT name, COUNT(*) as sum FROM artists WHERE name LIKE 'A%' GROUP BY name ORDER BY sum DESC

=end code

=head3 update(Table(Cool) $target, Assigns(Hash) $set, Conditions() $where?, Source() :$from, Column::List() :$returning)

This will update C<$target> by assigning the columns and values from C<$set> if they match C<$where>, returning C<$returning>.

=head3 insert(Table(Cool) $target, Column::List() $columns, Rows(List) $rows, Column::List() :$returning)

Insert into C<$target>, assigning each of the values in Rows to a new row in the table.

=begin code :lang<raku>

$abstract.insert('artists', ['name'], [ [ 'Metallica'], ], :returning(*));
# INSERT INTO artists (name) VALUES ('Metallica') RETURNING *

$abstract.insert('artists', List, [ [ 'Metallica'], ], :returning(*));
# INSERT INTO artists VALUES ('Metallica') RETURNING *

=end code

=head3 insert(Table(Cool) $target, Assigns() $values, Column::List() :$returning)

Inserts the values in C<$values> into the table C<$target>, returning the columns in C<$returning>

=begin code :lang<raku>

$abstract.insert('artists', { :name<Metallica> }, :returning(*));
# INSERT INTO artists (name) VALUES ('Metallica') RETURNING *

=end code

=head3 insert(Table(Cool) $target, Identifiers() $columns, Select(Map) $values, Column::List() :$returning)

=begin code :lang<raku>

$abstract.insert('artists', 'name', { :source<new_artists>, :columns<name> }, :returning(*));
# INSERT INTO artists (name) SELECT name FROM new_artists RETURNING *

=end code

=head3 delete(Table(Cool) $target, Conditions() $where?, Column::List() :$returning)

=begin code :lang<raku>

$abstract.delete('artists', { :name<Madonna> });
# DELETE FROM artists WHERE name = 'Madonna'

=end code

=head1 Author

Leon Timmermans <fawaka@gmail.com>

=head1 Copyright and License

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
