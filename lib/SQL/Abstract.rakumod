unit class SQL::Abstract:ver<0.0.6>:auth<zef:leont>;

use fatal;

use SQL::Placeholders;
use SQL::Query;

enum Precedence <Rowlike Comma Assignment And Or Not Between In Postfix Comparative Bitwise Additive Multiplicative Concatlike Prefix Termlike>;

class Op::Not { ... }
class Identifier { ... }
class Expression::Renamed { ... }

our proto expand-capture(|) { * }
role Expression {
	method precedence(--> Precedence) { ... }

	method negate(--> Expression) {
		Op::Not.new(:value(self));
	}
	method as(Identifier(Any) $alias) {
		Expression::Renamed.new(:source(self), :$alias);
	}

	multi method COERCE(Capture $capture) {
		expand-capture(|$capture);
	}
}

our proto expand-expression(|) { * }
multi expand-expression(Expression $expression) {
	$expression;
}
multi expand-expression(Capture $capture) {
	expand-capture(|$capture);
}

role Term does Expression {
	method precedence(--> Precedence::Termlike) {}
}

class Literal does Expression {
	has Precedence $.precedence = Precedence::Termlike;
	has Str:D $.payload is required;
	has Any:D @.arguments;

	multi method COERCE(Capture $capture) {
		my ($payload, @arguments, *%args) = |$capture;
		self.new(:$payload, :@arguments, |%args);
	}
}

class Integer does Term {
	has Int $.value;

	multi method COERCE(Int $value) {
		self.new(:$value);
	}
}

class Placeholder does Term {
	has Any $.value;

	multi method COERCE(Any $value) {
		self.bless(:$value);
	}
}

multi expand-expression(Any:D $value) {
	Placeholder.new(:$value);
}
multi expand-capture(Placeholder(Any) :$bind!) {
	$bind;
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
multi expand-capture(Bool :$default!) { Value::Default.new }
class Value::True    does Constant['TRUE']    {}
multi expand-capture(Bool :$true!) { Value::True.new }
class Value::False   does Constant['FALSE']   {}
multi expand-capture(Bool :$false!) { Value::False.new }
class Value::Null    does Constant['NULL']    {}
multi expand-capture(Bool :$null!) { Value::Null.new }

multi expand-expression(Any:U) {
	Value::Null.new;
}
multi expand-capture(Str $payload, *@arguments, *%args) {
	Literal.new(:$payload, :@arguments, |%args);
}
multi expand-capture(:literal($) ($payload, *@arguments), *%args) {
	Literal.new(:$payload, :@arguments, |%args);
}

class Identifier does Term {
	has Str @.parts;

	method new(Cool $name) {
		my @parts = $name.split('.');
		self.bless(:@parts);
	}
	multi method new-from-list(@parts) {
		self.bless(:@parts);
	}

	method concat(Identifier $other) {
		self.new-from-list([|@!parts, |$other.parts]);
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

multi expand-capture(Identifier(Str) :$ident!) {
	$ident;
}
multi expand-capture(:@ident!) {
	Identifier.new-from-list(@ident);
}

class Expression::Renamed does Expression {
	method precedence(--> Precedence::Comma) {}

	has Identifier:D $.alias is required;
	has Expression:D $.source is required;

	multi method COERCE(Pair (Identifier(Any) :key($alias), Expression :value($source))) {
		self.new(:$alias, :$source);
	}

	method as(Identifier(Any) $alias) {
		Expression::Renamed.new(:$!source, :$alias);
	}
}

multi expand-capture(Bool :$star!) {
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

	multi method COERCE(Identifier(Any) $column) {
		self.new(:elements[$column]);
	}
	multi method COERCE(@list) {
		my Identifier(Any) @elements = @list;
		self.new(:@elements);
	}
	multi method COERCE(List:U $list) {
		Identifiers;
	}
}

multi expand-capture(Identifiers(Any) :$idents!) {
	$idents;
}

class Function { ... }

class Star does Constant['*'] {}

class Column::List does Value::List {
	has Expression:D @.elements is required;

	our proto to-column(|) { * }
	multi to-column(Expression $column) {
		$column;
	}
	multi to-column(Identifier(Str) $column) {
		$column;
	}
	multi to-column(Pair $ (:$key, Pair :$value)) {
		my $function = Function.COERCE({ :name($value.key), :arguments($value.value) });
		Expression::Renamed.COERCE($key => $function);
	}
	multi to-column(Pair $ (:$key, :$value)) {
		Expression::Renamed.COERCE($key => to-column($value));
	}
	multi to-column(Whatever) {
		Star.new;
	}
	multi to-column(Map $map (Str :$function!, :%over!, *%args)) {
		Function.COERCE({ :$function, |%args }).over(|%over);
	}
	multi to-column(Map $map (Str :$function!, Str :$over!, *%args)) {
		Function.COERCE({ :$function, |%args }).over($over);
	}
	multi to-column(Function(Map) $function) {
		$function;
	}
	multi to-column(Capture $capture) {
		expand-capture(|$capture);
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

	multi method COERCE(Capture $capture) {
		my $expression = expand-capture(|$capture);
		$expression ~~ Column::List ?? $expression !! self.new(:elements($expression));
	}
}

multi expand-capture(Column::List(Any) :$columns!) {
	$columns;
}

class Row does Value::List {
	has Expression @.elements is required;

	multi method COERCE(@values) {
		my Expression @elements = @values.map(&expand-expression);
		self.new(:@elements);
	}
}

multi expand-capture(Row(List) :$row!) {
	$row;
}

class Rows {
	has Row @.elements is required;

	multi method COERCE(@input) {
		my Row(Any) @elements = @input;
		self.new(:@elements);
	}
}

role Op::HasLeft does Expression {
	has Expression:D $.left is required;
}

role Op::Postfix does Op::HasLeft {
	method precedence(--> Precedence::Postfix) {}

	method postfix(--> Str) { ... }
}

class Op::IsNull does Op::Postfix {
	has Bool $.negated;
	method postfix() {
		$!negated ?? 'IS NOT NULL' !! 'IS NULL';
	}
	method negate {
		Op::IsNull.new(:$!left, :negated(!$!negated));
	}
}

role Op::Prefix[Str $operator] does Expression {
	has Expression:D $.value is required;
	method precedence(--> Precedence::Prefix) {}
	method operator(--> Str:D) { $operator }
}

class Op::Positive does Op::Prefix['+'] {}
class Op::Negative does Op::Prefix['-'] {}
class Op::Complement does Op::Prefix['~'] {}

class Op::Not does Op::Prefix['NOT'] {
	method precedence(--> Precedence::Not) {}

	method negate(--> Bool) {
		$!value;
	}
}

my %prefix-op-for = (
	'+' => Op::Positive,
	'-' => Op::Negative,
	'~' => Op::Complement,
	'not' => Op::Not,
);

multi expand-capture(:op(@) (Str $op where { %prefix-op-for{$op}:exists }, $expr)) {
	my $value = expand-expression($expr);
	%prefix-op-for{$op}.new(:$value);
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

multi expand-capture(:op(@) (Str $op where 'like'|'not-like', $left-expr, $right-expr, $escape-expr)) {
	my $left = expand-expression($left-expr);
	my $right = expand-expression($right-expr);
	my $escape = expand-expression($escape-expr);
	my $class = $op eq 'like' ?? Op::Like !! Op::NotLike;
	$class.new(:$left, :$right, :$escape);
}

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

multi expand-capture(:op(@) (Str $op where 'between'|'not-between', $column-expr, $left-expr, $right-expr)) {
	my $column = expand-expression($column-expr);
	my $left = expand-expression($left-expr);
	my $right = expand-expression($right-expr);
	Op::Between.new(:$left, :$right);
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

multi expand-capture(:@and) {
	my @elements = @and.map: { expand-expression($^item) };
	Op::And.pack(@elements);
}
multi expand-capture(:@or) {
	my @elements = @or.map: { expand-expression($^item) };
	Op::Or.pack(@elements);
}


role Op::InLike does Expression {
	method precedence(--> Precedence::In) {}
}

role Op::In does Op::InLike does Op::HasLeft {
	has Bool $.negated;
}

class Op::In::List does Op::In {
	has Row:D $.values is required handles<elements>;

	method create(Expression :$left, Row(List) :$values, Bool :$negated = False) {
		self.new(:$left, :$values, :$negated);
	}

	method negate() {
		self.bless(:$!left, :$!values, :negated(!$!negated));
	}
}

role Query { ... }
class Select { ... }

role Op::Subquery does Op::InLike {
	has Query:D $.query is required;
	method operator() { ... }
}

class Op::In::Query does Op::In does Op::Subquery {
	method operator { $!negated ?? 'NOT IN' !! 'IN' }

	method negate() {
		self.new($!left, $!query, :negated(!$!negated));
	}
}

multi expand-capture(:@op ('in', $left-expr, Capture (Select(Map) :$select!))) {
	my $left = expand-expression($left-expr);
	Op::In::Query.new(:$left, :query($select));
}
multi expand-capture(:@op ('in', $left-expr, *@values)) {
	my $left = expand-expression($left-expr);
	Op::In::List.create(:$left, :@values);
}
multi expand-capture(:@op ('not-in', $left-expr, Capture (Select(Map) :$select!))) {
	my $left = expand-expression($left-expr);
	Op::In::Query.new(:$left, :query($select), :negated);
}
multi expand-capture(:@op ('not-in', $left-expr, *@values)) {
	my $left = expand-expression($left-expr);
	Op::In::List.create(:$left, :@values, :negated);
}

class Op::Exists does Op::Subquery {
	has Bool $.negated;
	method operator() { $!negated ?? 'NOT EXISTS' !! 'EXISTS' }

	method negate() {
		self.new(:$!query, :negated(!$!negated));
	}
}

multi expand-capture(Select(Map) :$exists) {
	Op::Exists.new(:query($exists));
}
multi expand-capture(Select(Map) :$not-exists) {
	Op::Exists.new(:query($not-exists), :negated);
}

class Op::Cast does Term {
	has Expression:D $.primary is required;
	has Str:D $.typename is required;
}

multi expand-capture(:@op ('cast', Any $expression, Str $typename)) {
	my $primary = expand-expression($expression);
	Op::Cast.new(:$primary, $typename);
}

class Op::Case does Expression {
	method precedence(--> Precedence::Between) {}

	has Expression $.left;
	class When {
		has Expression:D $.condition is required;
		has Expression:D $.then is required;

		method COERCE(Pair $pair) {
			my $condition = expand-expression($pair.key);
			my $then = expand-expression($pair.value);
			Op::Case::When.new(:$condition, :$then);
		}
	}
	has When:D @.whens;
	has Expression $.else;
}

multi expand-capture(:@case is copy) {
	my $left = @case[0] !~~ Pair ?? expand-expression(@case.shift) !! Expression;
	my $else = @case[*-1] !~~ Pair ?? expand-expression(@case.pop) !! Expression;
	my Op::Case::When(Pair) @whens = @case;
	Op::Case.new(:$left, :@whens, :$else);
}
multi expand-capture(:op(@case) where @case[0] eq 'case') {
	my ($case, $left-expr, @args) = |@case;
	my $left = expand-expression($left-expr);
	my $else = @args[*-1] !~~ Pair ?? expand-expression(@args.pop) !! Expression;
	my Op::Case::When(Pair) @whens = @args;
	Op::Case.new(:$left, :@whens, :$else);
}

multi expand-capture(:op(@) (Str $key, $left-expr, $right-expr)) {
	my Op::Binary:U $class = %binary-op-for{$key}:exists ?? %binary-op-for{$key} !! Op::Comperative[$key];
	my $left = expand-expression($left-expr);
	my $right = expand-expression($right-expr);
	$class.new(:$left, :$right);
}

class Current does Expression {
	method precedence(--> Precedence::Between) {}
	has Identifier:D $.cursor is required;
}

multi expand-capture(Identifier(Any) :$current!) {
	Current.new(:cursor($current));
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
	multi expand-partial(Expression $left, Capture:D (:@op) ) {
		my ($op, *@args) = @op;
		my $capture = \(:op[ $op, $left, |@args ]);
		expand-capture(|$capture);
	}
	multi expand-partial(Expression $left, Capture:D $capture (Bool :$null)) {
		Op::IsNull.new(:$left, :negated(!$null));
	}
	multi expand-partial(Expression $left, Capture:D (:@and) ) {
		my @elements = @and.map: { expand-partial($left, $^item) };
		Op::And.pack(@elements);
	}
	multi expand-partial(Expression $left, Capture:D (:@or) ) {
		my @elements = @or.map: { expand-partial($left, $^item) };
		Op::Or.pack(@elements);
	}
	multi expand-partial(Expression $left, Capture:D $capture) {
		Op::Equals.new(:$left, :right(expand-capture(|$capture)));
	}
	multi expand-partial(Expression $left, Any:D $expression) {
		Op::Equals.new(:$left, :right(expand-expression($expression)));
	}
	multi expand-partial(Expression $left, Any:U $) {
		Op::IsNull.new(:$left);
	}
	multi expand-partial(Expression $left, Constant:U $constant) {
		Op::Equals.new(:$left, :right($constant.new));
	}
	multi expand-partial(Expression $left, Constant $constant where Value::Null) {
		Op::IsNull.new(:$left);
	}

	multi expand-pair(Expression $left, 'null', Bool $positive) {
		Op::IsNull.new(:$left, :negated(!$positive));
	}
	multi expand-pair(Expression $left, 'in', @values) {
		Op::In::List.create(:$left, :@values);
	}
	multi expand-pair(Expression $left, 'not-in', @values) {
		Op::In::List.create(:$left, :@values, :negated);
	}
	multi expand-pair(Expression $left, 'and', @and) {
		my @elements = @and.map: { expand-partial($left, $^item) };
		Op::And.pack(@elements);
	}
	multi expand-pair(Expression $left, 'or', @or) {
		my @elements = @or.map: { expand-partial($left, $^item) };
		Op::Or.pack(@elements);
	}
	multi expand-pair(Expression $left, Str $key, Any:D $value) {
		my $right = expand-expression($value);
		my $class = %binary-op-for{$key}:exists ?? %binary-op-for{$key} !! Op::Comperative[$key];
		$class.new(:$left, :$right);
	}
	multi expand-partial(Expression $left, Pair (:$key, :$value)) {
		expand-pair($left, $key, $value);
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
		Op::In::List.create(:$left, :values(@partials».right));
	}
	multi expand-junction(Expression $left, 'all', @partials) {
		Op::And.pack(@partials);
	}
	multi expand-junction(Expression $left, 'none', @partials) {
		Op::Or.pack(@partials).negate;
	}
	multi expand-junction(Expression $left, 'none', @partials where @partials > 0 && all(@partials) ~~ Op::Equals && all(@partials).left === $left) {
		Op::In::List.create(:$left, :values(@partials».right), :negated);
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

	multi expand-condition(Pair (Identifier(Any) :$key, Mu :$value)) {
		expand-partial($key, $value);
	}
	multi expand-condition(Pair (Expression :$key, Mu :$value)) {
		expand-partial($key, $value);
	}

	multi method COERCE(Expression $expression) {
		self.bless(:$expression);
	}
	multi method COERCE(Pair $pair) {
		my @expressions = expand-condition($pair);
		self.new(:@expressions);
	}
	multi method COERCE(@list) {
		my @expressions = @list.flatmap(&expand-condition);
		self.new(:@expressions);
	}
	multi method COERCE(%hash) {
		samewith(%hash.sort);
	}
	multi method COERCE(Capture $capture) {
		my $expression = expand-capture(|$capture);
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

	multi method COERCE(Pair (Expression :key($column), Sorting(Str) :value($order))) {
		self.new(:$column, :$order);
	}
	multi method COERCE(Pair (Identifier(Any:D) :key($column), Sorting(Str) :value($order))) {
		self.new(:$column, :$order);
	}
	multi method COERCE(Map (Expression :$column!, Sorting(Str) :$order, Nulls(Str) :$nulls)) {
		self.new(:$column, :$order, :$nulls);
	}
	multi method COERCE(Map (Identifier(Any) :$column!, Sorting(Str) :$order, Nulls(Str) :$nulls)) {
		self.new(:$column, :$order, :$nulls);
	}
}

class OrderBy {
	has Expression:D @.elements is required;

	multi to-sorter(Expression $expression) {
		$expression;
	}
	multi to-sorter(Identifier(Cool) $ident) {
		$ident;
	}
	multi to-sorter(Identifier(Capture) $ident) {
		$ident;
	}
	multi to-sorter(Order::Modifier(Pair) $modifier) {
		$modifier;
	}
	multi to-sorter(Order::Modifier(Map) $modifier) is default {
		$modifier;
	}

	multi method COERCE(Any $sorter) {
		self.new(:elements[to-sorter($sorter)]);
	}
	multi method COERCE(@list) {
		my @elements = @list.map(&to-sorter);
		self.new(:@elements);
	}
}

class Assigns {
	has Pair @.pairs;

	multi transform-pair(Pair (Expression :$key, :$value)) {
		my $expression = expand-expression($value);
		$key => $expression;
	}
	multi transform-pair(Pair (Capture :$key, :$value)) {
		my $ident = expand-expression($key);
		my $expression = expand-expression($value);
		$ident => $expression;
	}
	multi transform-pair(Pair (Identifier(Any) :$key, :$value)) {
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
	has Str:D $.function is required;
	has Quantifier $.quantifier;
	has Value::List:D $.arguments is required;
	has OrderBy $.order-by;
	has Conditions $.filter;

	multi method COERCE(Map (Str :$function!, Column::List(Any) :$arguments = (), Conditions(Any) :$filter, Quantifier(Str) :$quantifier, OrderBy(Any) :$order-by)) {
		Function.new(:$function, :$arguments, :$filter, :$quantifier, :$order-by);
	}
	multi method COERCE(Pair (Str :$key, Column::List(Any) :$value)) {
		Function.new(:function($key), :arguments($value));
	}

	method precedence() {
		$!filter ?? Precedence::Between !! Precedence::Termlike;
	}

	multi method over(Str $existing) {
		my $window = Window::Definition.COERCE(:$existing);
		Window::Function.new(:function(self), :$window);
	}
	multi method over(*%arguments) {
		my $window = Window::Definition.COERCE(%arguments);
		Window::Function.new(:function(self), :$window);
	}

	method as(Identifier(Any) $alias, Identifiers(Cool) $columns?, Bool :$lateral, Bool :$ordinal) {
		Source::Function.new(:function(self), :$alias, :$columns, :$lateral, :$ordinal);
	}
}

multi expand-capture(Function(Map) :$function!) {
	$function;
}
multi expand-capture(Map :function($)! (Str :$over, *%args)) {
	Function.COERCE(%args).over($over);
}
multi expand-capture(Map :function($)! (:%over, *%args)) {
	Function.COERCE(%args).over(|%over);
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
		has Boundary $.to;
		has Exclusion:D $.exclude is required;
	}

	class Definition {
		has Identifier   $.existing;
		has Column::List $.partition-by;
		has OrderBy      $.order-by;
		has Frame        $.frame;

		method is-simple() {
			$!existing && !$!partition-by && !$!order-by && !$!frame;
		}

		method COERCE(Map (Column::List(Any) :$partition-by, OrderBy(Any) :$order-by, Identifier(Any) :$existing = Identifier, Boundary(Any) :$from = Boundary::Preceding::Unbounded.new, Boundary(Any) :$to, Exclusion(Str) :$exclude = Exclusion::NoOthers, Mode(Str) :$mode = Mode::Range)) {
			my $default-frame = $from ~~ Boundary::Preceding::Unbounded && !$to && $mode === Mode::Range && $exclude === Exclusion::NoOthers;
			my $frame = $default-frame ?? Frame !! Frame.new(:$mode, :$from, :$to, :$exclude);
			self.new(:$existing, :$partition-by, :$order-by, :$frame);
		}
	}

	class Clause {
		has Identifier:D         $.name is required;
		has Window::Definition:D $.definition is required;

		method COERCE(Pair (Identifier(Any) :$key, Window::Definition(Map) :$value)) {
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

	class Clause {

		has Strength:D $.strength is required;
		has Identifiers $.tables;
		has Bool $.no-wait;
		has Bool $.skip-locked;

		multi method COERCE(Strength(Str) $strength) {
			self.new(:$strength);
		}
		multi method COERCE(Pair (Strength(Str) :$key, Identifiers(Cool) :$value)) {
			self.new(:strength($key), :tables($value));
		}
		multi method COERCE(Map (Strength(Str) :$strength, Identifiers(Cool) :$tables, Bool :$no-wait, Bool :$skip-locked?)) {
			self.new(:$strength, :$tables, :$no-wait, :$skip-locked);
		}
	}

	has Clause @.lockings;

	multi method COERCE(@specs) {
		my Clause(Any) @lockings = @specs;
		self.new(:@lockings);
	}
	multi method COERCE(Clause(Any) $locking) {
		self.new(:lockings[ $locking ]);
	}
}

class Compound {
	enum Type <Union Intersect Except>;
	has Type:D $.type = Type::Union;
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

	multi method COERCE(Expression $value) {
		self.new(:$value);
	}
	multi method COERCE(Placeholder(Any) $value) {
		self.new(:$value);
	}
	multi method COERCE(Inf) {
		self.new(:value(All.new));
	}
}

class Offset {
	has Expression:D $.value is required;

	multi method COERCE(Expression $value) {
		self.new(:$value);
	}
	multi method COERCE(Placeholder(Any) $value) {
		self.new(:$value);
	}
}

class Source::Query { ... }
role Query does Expression {
	method precedence(--> Precedence::Rowlike) {}

	method create(|) { ... }

	multi method COERCE(%arguments) {
		self.create(|%arguments);
	}

	method as(Identifier:D(Any:D) $alias, Bool :$lateral) {
		Source::Query.new(:query(self), :$alias, :$lateral);
	}
}

class Values does Query {
	has Rows:D  $.rows is required;
	has OrderBy $.order-by;
	has Limit   $.limit;
	has Offset  $.offset;

	method create(Rows(Any) :$rows!, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset) {
		self.new(:$rows, :$order-by, :$limit, :$offset);
	}
}

role Table { ... }

class Common {
	class Name {
		has Identifier:D $.name is required;
		has Identifiers $.columns;

		multi method COERCE(Identifier(Any) $name) {
			self.new(:$name);
		}
		multi method COERCE(Pair $pair (Identifier(Any) :key($name), Identifiers(Cool) :value($columns))) {
			self.new(:$name, :$columns);
		}
	}
	class Rename {
		has Name:D $.alias is required;
		has Query:D $.query is required;

		multi method COERCE(Pair $pair (Name(Any) :key($alias), Select(Map) :value($query))) {
			self.new(:$alias, :$query);
		}
		multi method COERCE(Pair $pair (Name(Any) :key($alias), Table(Str) :value($source))) {
			my $query = Select.create(:$source);
			self.new(:$alias, :$query);
		}
		multi method COERCE(Pair $pair (Name(Any) :key($alias), Query :value($query))) {
			self.new(:$alias, :$query);
		}
	}
	has Rename @.tables;
	has Bool $.recursive;

	multi method COERCE(Rename(Pair) $table) {
		self.new(:tables[$table]);
	}
	multi method COERCE(@pairs) {
		my Rename(Pair) @tables = @pairs;
		self.new(:@tables);
	}
}

role Query::Common does Query {
	has Common(Any)      $.common;
}

class Distinction::Full { ... }
class Distinction::Columns { ... }

role Distinction {
	multi method COERCE(Bool $value) {
		$value ?? Distinction::Full.new !! Distinction;
	}
	multi method COERCE(Column::List(Any) $columns) {
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
	multi to-grouping(Function(Pair) $function) {
		$function;
	}
	multi to-grouping(Pair $pair (:$key where $key.lc eq 'grouping sets', :$value)) {
		my @values = $value.map(&to-grouping);
		Function.COERCE(( $key => @values ));
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
	multi method COERCE(Map $ (Bool :$all, :@elements)) {
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

	method create(Source(Any) :$source!, Column::List(Any) :$columns = *, Conditions(Any) :$where, Common(Any) :$common, Distinction(Any) :$distinct, GroupBy(Any) :$group-by, Conditions(Any) :$having, Window::Clauses(Any) :$windows, Compound(Pair) :$compound, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset, Locking(Any) :$locking) {
		Select.new(:$common, :$distinct, :$columns, :$source, :$where, :$group-by :$having, :$windows, :$compound, :$order-by, :$limit, :$offset, :$locking);
	}
}

multi expand-capture(Select(Map) :$select!) {
	$select;
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
	multi method COERCE(Identifiers(Cool) $columns) {
		Conflict::Target::Columns.new(:$columns);
	}
	multi method COERCE(Pair (:$key where 'columns', Identifiers(Cool) :$value)) {
		Conflict::Target::Columns.new(:columns($value));
	}
	multi method COERCE(Pair (:$key where 'columns', Map :value($) (Identifiers(Cool) :$columns!, Conditions(Any) :$where))) {
		Conflict::Target::Columns.new(:$columns, :$where);
	}
	multi method COERCE(Pair (:$key where 'constraint', Identifier(Any) :$value)) {
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
	multi method COERCE(Pair (Conflict::Target(Any) :$key, Str :$value where 'nothing')) {
		Conflict::Noting.new(:target($key));
	}
	multi method COERCE(Pair (Conflict::Target(Any) :$key, Assigns(Any) :$value)) {
		my @assignments = $value.assignments;
		Conflict::Update.new(:target($key), :@assignments);
	}
	multi method COERCE(Map (Conflict::Target(Any) :$target?, Str :$do! where 'nothing')) {
		Conflict::Nothing.new(:$target);
	}
	multi method COERCE(Map (Conflict::Target(Any) :$target!, Assigns(Any) :$do!, Conditions(Any) :$where)) {
		my @assignments = $do.assignments;
		Conflict::Update.new(:$target, :@assignments, :$where);
	}
}

class Conflict::Nothing does Conflict {}

class Conflict::Update does Conflict {
	has Op::Assign @.assignments;
	has Conditions $.where;
}

class Conflicts {
	has Conflict @.conflicts;

	multi method COERCE(@specs) {
		my Conflict(Any) @conflicts = @specs;
		self.new(:@conflicts);
	}
	multi method COERCE(Conflict(Any) $conflict) {
		self.new(:conflicts[ $conflict ]);
	}
}

role Insert does Query::Common {
	has Table:D      $.target is required;
	has Identifiers  $.columns;
	has Overriding   $.overriding;
	has Conflicts    $.conflicts;
	has Column::List $.returning;
}

class Insert::Values does Insert {
	has Rows:D $.rows is required;

	method create(Table(Any) :$target!, Identifiers(Cool) :$columns, Rows(List) :$rows!, Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning) {
		Insert::Values.new(:$target, :$columns, :$rows, :$overriding, :$conflicts, :$returning);
	}
}

class Insert::Select does Insert {
	has Select:D $.select is required;

	method create(Table(Any) :$target!, Identifiers(Cool) :$columns, Select(Map) :$select!, Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning) {
		Insert::Select.new(:$target, :$columns, :$select, :$overriding, :$conflicts, :$returning);
	}
}

class Insert::Defaults does Insert {
	method create(Table(Any) :$target!, Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning) {
		Insert::Defaults.new(:$target, :$overriding, :$conflicts, :$returning);
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
	multi method COERCE(Identifier(Str) $name) is default {
		Table::Simple.new(:$name);
	}

	my subset FunctionMap of Map where .EXISTS-KEY('function');
	multi method COERCE(Pair (Identifier(Any) :$key, Identifier(Str) :$value)) {
		Table::Renamed.new(:name($value), :alias($key));
	}
	multi method COERCE(Pair (Identifier(Any) :$key, Query :$value)) {
		Source::Query.new(:query($value), :alias($key));
	}
	multi method COERCE(Pair (Identifier(Any) :$key, Select(Map) :$value)) {
		Source::Query.new(:query($value), :alias($key));
	}
	multi method COERCE(Pair (Identifier(Any) :$key, Function:D(FunctionMap) :$value)) {
		Source::Function.new(:function($value), :alias($key));
	}
	multi method COERCE(Pair (Identifier(Any) :$key, Function(Pair) :$value)) {
		Source::Function.new(:function($value), :alias($key));
	}
	multi method COERCE(Pair (Identifier(Any) :$key, Capture :$value)) {
		self.COERCE(($key => expand-capture(|$value)));
	}
	
	multi method COERCE(Map (Identifier(Any) :table($name), Identifier(Any) :$alias, Identifiers(Cool) :$columns, Bool :$only)) {
		Table::Renamed.new(:$name, :$alias, :$columns, :$only);
	}
	multi method COERCE(Map (Identifier(Any) :table($name)!, Bool :$only)) {
		Table::Simple.new(:$name, :$only);
	}
	multi method COERCE(Map (Function(Map) :$function, Identifier(Any) :$alias, Identifiers() :$columns, Bool :$lateral, Bool :$ordinal)) {
		Source::Function.new(:$function, :$alias, :$columns, :$lateral, :$ordinal);
	}
	multi method COERCE(Map (Select(Map) :$query, Identifier(Any) :$alias, Identifiers() :$columns, Bool :$lateral)) {
		Source::Query.new(:query($query), :$alias, :$columns, :$lateral);
	}
	multi method COERCE(Map (Source(Any) :$left!, Source(Any) :$right!, *%args)) {
		$left.join($right, |%args);
	}

	multi method join(Source(Any) $right, Join::Conditions(Any) :$on!, Join::Type(Str) :$type = Join::Type::Inner) {
		Join::On.new(:left(self), :$right, :$on, :$type);
	}
	multi method join(Source(Any) $right, Identifiers(Any) :$using!, Join::Type(Str) :$type = Join::Type::Inner) {
		Join::Using.new(:left(self), :$right, :$using, :$type);
	}
	multi method join(Source(Any) $right, Bool :$natural!, Join::Type(Str) :$type = Join::Type::Inner) {
		Join::Natural.new(:left(self), :$right, :$type);
	}
	multi method join(Source(Any) $right, Bool :$cross!) {
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
	sub expand(Pair (Identifier(Any) :key($left), Identifier(Cool) :value($right))) {
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
		my $then      = self.render-expression($placeholders, $when.then, Precedence::Comma);
		'WHEN', $condition, 'THEN', $then;
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
		my $value = self.render-expression($placeholders, $op.left, Precedence::Postfix);
		"$value $op.postfix()";
	}
	multi method render-expression(Placeholders $placeholders, Op::Not $op, Precedence --> Str) {
		my $primary = self.render-expression($placeholders, $op.value, Precedence::Not);
		"NOT $primary";
	}
	multi method render-expression(Placeholders $placeholders, Op::In::List $in, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $in.left, $in.precedence);
		my $candidates = self.render-expression($placeholders, $in.values, Precedence::Comma);
		my $operator = $in.negated ?? 'NOT IN' !! 'IN';
		"$left $operator $candidates";
	}
	multi method render-expression(Placeholders $placeholders, Op::Subquery $subquery, Precedence --> Str) {
		my $left = self.render-expression($placeholders, $subquery.left, $subquery.precedence);
		my $candidates = self.render-expression($placeholders, $subquery.query, Precedence::Comma);
		my $operator = $subquery.operator;
		"$left $operator $candidates";
	}
	multi method render-expression(Placeholders $placeholders, Op::Exists $in, Precedence --> Str) {
		my $candidates = self.render-expression($placeholders, $in.query, Precedence::Comma);
		my $operator = $in.negated ?? 'NOT EXISTS' !! 'EXISTS';
		"$operator $candidates";
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
	multi method render-expression(Placeholders $placeholders, Current $current, Precedence --> Str) {
		my $cursor = self.render-identifier($current.cursor);
		"CURRENT OF $cursor";
	}
	multi method render-expression(Placeholders $placeholders, Expression::Custom $custom, Precedence --> Str) {
		$custom.render-sql(self);
	}

	multi method render-expression(Placeholders $placeholders, Function $function, Precedence --> Str) {
		my @expressions = $function.quantifier ?? $function.quantifier.uc !! Empty;
		@expressions.append: self.render-column-expressions($placeholders, $function.arguments);
		@expressions.append: self.render-order-by($placeholders, $function.order-by);
		my $expression = @expressions.join(' ');
		my @result = "{ $function.function() }($expression)";
		@result.push: 'FILTER', parenthesize-list(self.render-conditions($placeholders, $function.filter, 'WHERE')) with $function.filter;
		@result.join(' ');
	}

	multi method render-window-boundary(Placeholders $placeholders, Window::Boundary::Unbounded $boundary) {
		$boundary.tag;
	}
	multi method render-window-boundary(Placeholders $placeholders, Window::Boundary::Bounded $boundary) {
		self.render-expression($placeholders, $boundary.offset, Precedence::Between), $boundary.tag;
	}

	multi method render-window-frame(Placeholders $placeholders, Window::Frame $frame) {
		my $mode = $frame.mode.uc;
		my @exclusion = $frame.exclude.defined ?? ('EXCLUDE', $frame.exclude.uc) !! Empty;
		my $from = self.render-window-boundary($placeholders, $frame.from);
		$mode, $from, |@exclusion;
	}
	multi method render-window-frame(Placeholders $placeholders, Window::Frame $frame where .to.defined) {
		my $mode = $frame.mode.uc;
		my $from = self.render-window-boundary($placeholders, $frame.from);
		my $to = self.render-window-boundary($placeholders, $frame.to);
		my @exclusion = $frame.exclude.defined ?? ('EXCLUDE', $frame.exclude.uc) !! Empty;
		$mode, 'BETWEEN', $from, 'AND', $to, |@exclusion;
	}

	multi method render-window-definition(Placeholders $placeholders, Window::Definition $definition --> Str) {
		my @inner;
		@inner.push: self.render-identifier($definition.existing) with $definition.existing;
		@inner.push: self.render-column-expressions($placeholders, $definition.partition-by, 'PARTITION BY');
		@inner.push: self.render-order-by($placeholders, $definition.order-by);
		@inner.push: self.render-window-frame($placeholders, $definition.frame) with $definition.frame;
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

	multi method render-column-expressions(Placeholders $placeholders, Column::List:D $columns, Str $prefix? --> List) {
		my $results = self.render-expressions($placeholders, $columns.elements);
		$prefix ?? ($prefix, $results) !! ($results,);
	}
	multi method render-column-expressions(Placeholders $placeholders, Column::List:U $columns, Str $prefix? --> List) {
		Empty;
	}

	multi method render-names(Identifiers:D $columns --> Str) {
		$columns.elements.map({ self.render-identifier($^column) }).join(', ');
	}

	multi method render-identifiers(Identifiers:D $columns --> Str) {
		parenthesize-list($columns.elements.map({ self.render-identifier($^column) }), ', ');
	}
	multi method render-identifiers(Identifiers:U $columns --> List) {
		Empty;
	}

	method render-source-alias(Source::Aliased $source) {
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

	method render-common-table(Placeholders $placeholders, Common::Rename $rename --> Str) {
		my $alias      = self.render-identifier($rename.alias.name);
		my @columns    = self.render-identifiers($rename.alias.columns);
		my $expression = self.render-expression($placeholders, $rename.query, Precedence::Comma);
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

	method render-group-by(Placeholders $placeholders, GroupBy $group-by, Conditions $conditions --> List) {
		my @result = 'GROUP BY';
		@result.push: 'ALL' if $group-by.all;
		@result.push: self.render-expression-list($placeholders, $group-by.elements, False);
		@result.push: self.render-conditions($placeholders, $conditions, 'HAVING');
		@result;
	}

	method render-window(Placeholders $placeholders, Window::Clause $window --> Str) {
		my $name = self.render-identifier($window.name);
		my $definition = self.render-window-definition($placeholders, $window.definition);
		"$name AS $definition";
	}

	method render-windows(Placeholders $placeholders, Window::Clauses $windows --> List) {
		'WINDOW', $windows.windows.map({ self.render-window($placeholders, $^window) }).join(', ');
	}

	method render-compound(Placeholders $placeholders, Compound $compound --> List) {
		my @type = $compound.all ?? 'ALL' !! Empty;
		my $next = self.render-select-core($placeholders, $compound.next);
		$compound.type.uc, |@type, $next;
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

	method render-locking(Locking::Clause $locking --> List) {
		my @result = 'FOR', $locking.strength.Str.uc;
		@result.push('OF', self.render-names($locking.tables)) with $locking.tables;
		@result.push('NOWAIT') if $locking.no-wait;
		@result.push('SKIP LOCKED') if $locking.skip-locked;
		@result;
	}

	method render-lockings(Locking $lockings --> Str) {
		$lockings.lockings.map({ self.render-locking($^locking) }).join(', ');
	}


	method render-select-core(Placeholders $placeholders, Select $select --> Str) {
		my @parts = 'SELECT';
		@parts.append: self.render-distinct($placeholders, $select.distinct) with $select.distinct;
		@parts.append: self.render-column-expressions($placeholders, $select.columns);
		@parts.append: self.render-from($placeholders, $select.source);
		@parts.append: self.render-conditions($placeholders, $select.where);
		@parts.append: self.render-group-by($placeholders, $select.group-by, $select.having) with $select.group-by;
		@parts.append: self.render-compound($placeholders, $select.compound) with $select.compound;
		@parts.append: self.render-windows($placeholders, $select.windows) with $select.windows;
		@parts.join(' ');
	}

	multi method render-expression(Placeholders $placeholders, Select $select, Precedence --> Str) {
		my @parts;
		@parts.append: self.render-common-tables($placeholders, $select.common);
		@parts.append: self.render-select-core($placeholders, $select);
		@parts.append: self.render-order-by($placeholders, $select.order-by);
		@parts.append: self.render-limit-offset($placeholders, $select.limit, $select.offset);
		@parts.append: self.render-lockings($select.locking) with $select.locking;
		@parts.join(' ');
	}

	multi method render-expression(Placeholders $placeholders, Update $update, Precedence --> Str) {
		my @common       = self.render-common-tables($placeholders, $update.common);
		my @target       = self.render-table($update.target);
		my @set          = 'SET', self.render-expressions($placeholders, $update.assignments);
		my @from         = self.render-from($placeholders, $update.from);
		my @where        = self.render-conditions($placeholders, $update.where);
		my @returning    = self.render-column-expressions($placeholders, $update.returning, 'RETURNING');

		(@common, 'UPDATE', @target, @set, @from, @where, @returning).flat.join(' ');
	}

	method render-overriding(Overriding $override) {
		'OVERRIDING', $override.uc, 'VALUE';
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

	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target::Columns $target) {
		my $columns = self.render-identifiers($target.columns);
		my @where = self.render-conditions($placeholders, $target.where);
		$columns, |@where;
	}
	multi method render-conflict-target(Placeholders $placeholders, Conflict::Target::Constraint $target) {
		'ON CONSTRAINT', self.render-identifier($target.constraint);
	}

	multi method render-conflict(Placeholders $placeholders, Conflict::Nothing $conflict) {
		my @target = $conflict.target ?? self.render-conflict-target($placeholders, $conflict.target) !! Empty;
		'ON CONFLICT', |@target, 'DO NOTHING';
	}
	multi method render-conflict(Placeholders $placeholders, Conflict::Update $conflict) {
		my @result = 'ON CONFLICT';
		@result.push: self.render-conflict-target($placeholders, $conflict.target) with $conflict.target;
		@result.push: 'DO UPDATE SET';
		@result.push: self.render-expressions($placeholders, $conflict.assignments);
		@result.push: self.render-conditions($placeholders, $conflict.where);
		@result;
	}

	method render-conflicts(Placeholders $placeholders, Conflicts $conflicts) {
		$conflicts.conflicts.flatmap: { self.render-conflict($placeholders, $^conflict) };
	}

	multi method render-expression(Placeholders $placeholders, Insert $insert, Precedence --> Str) {
		my @common     = self.render-common-tables($placeholders, $insert.common);
		my @target     = self.render-table($insert.target);
		my @columns    = self.render-identifiers($insert.columns);
		my @overriding = $insert.overriding ?? self.render-overriding($insert.overriding) !! Empty;
		my @content    = self.render-insert-content($placeholders, $insert);
		my @conflicts  = $insert.conflicts ?? self.render-conflicts($placeholders, $insert.conflicts) !! Empty;
		my @returning  = self.render-column-expressions($placeholders, $insert.returning, 'RETURNING');

		(@common, 'INSERT INTO', @target, @columns, @overriding, @content, @conflicts, @returning).flat.join(' ');
	}

	multi method render-expression(Placeholders $placeholders, Delete $delete, Precedence) {
		my @common    = self.render-common-tables($placeholders, $delete.common);
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

	method render(Expression:D $expression, Precedence:D :$precedence = Precedence::Rowlike) {
		my $placeholders = self.placeholders.new;
		my $sql          = self.render-expression($placeholders, $expression, $precedence);
		SQL::Query.new($sql, $placeholders.values);
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

our sub select(Source(Any) $source, Column::List(Any) $columns = *, Conditions(Any) $where?, Common(Any) :$common, Distinction(Any) :$distinct, GroupBy(Any) :$group-by, Conditions(Any) :$having, Window::Clauses(Any) :$windows, Compound(Pair) :$compound, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset, Locking(Any) :$locking) is export(:functions) {
	Select.new(:$common, :$distinct, :$columns, :$source, :$where, :$group-by :$having, :$windows, :$compound, :$order-by, :$limit, :$offset, :$locking);
}

method select(|args) {
	$!renderer.render(select(|args));
}

our sub update(Table(Any) $target, Assigns(Any) $assigns, Conditions(Any) $where?, Common(Any) :$common, Source(Any) :$from, Column::List(Any) :$returning) is export(:functions) {
	my @assignments = $assigns.assignments;
	Update.new(:$common, :$target, :@assignments, :$where, :$from, :$returning);
}

method update(|args) {
	$!renderer.render(update(|args));
}

proto insert(|) is export(:functions) { * }
multi insert(Table(Any) $target, Identifiers(Any) $columns, Rows(List) $rows, Common(Any) :$common, Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning) {
	Insert::Values.new(:$common, :$target, :$columns, :$rows, :$overriding, :$conflicts, :$returning);
}
multi insert(Table(Any) $target, Assigns(Any) $values, Common(Any) :$common, Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning) {
	samewith($target, $values.keys, [$values.values,], :$common, :$overriding, :$conflicts, :$returning);
}
multi insert(Table(Any) $target, Identifiers(Any) $columns, Select(Map) $select, Common(Any) :$common, Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning) {
	Insert::Select.new(:$common, :$target, :$columns, :$select, :$overriding, :$conflicts, :$returning);
}
multi insert(Table(Any) $target, Value::Default $, Common(Any) :$common, Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning) {
	Insert::Defaults.new(:$common, :$target, :$overriding, :$conflicts, :$returning);
}

method insert(|args) {
	$!renderer.render(insert(|args));
}

our sub delete(Table(Any) $target, Conditions(Any) $where, Common(Any) :$common, Source(Any) :$using, Column::List(Any) :$returning) is export(:functions) {
	Delete.new(:$common, :$target, :$where, :$using, :$returning);
}

method delete(|args) {
	$!renderer.render(delete(|args));
}

our sub values(Rows(List) $rows, OrderBy(Any) :$order-by, Limit(Any) :$limit, Offset(Any) :$offset) {
	Values.new(:$rows, :$order-by, :$limit, :$offset);
}

method values(|args) {
	$!renderer.render(values(|args));
}

method begin() {
	SQL::Query.new('BEGIN');
}

method rollback() {
	SQL::Query.new('ROLLBACK');
}

method commit() {
	SQL::Query.new('COMMIT');
}


proto table(|) is export(:functions) { * }
multi table(Any $table, Identifier(Any) :$as!, Identifiers(Any) :$columns = Identifiers, Bool :$only) {
	Table.COERCE({ :$table, :alias($as), :$columns, :$only });
}
multi table(Identifier(Any) $name) {
	Table::Simple.new(:$name);
}

proto identifier(|) is export(:functions) {*}
multi identifier(Identifier(Str) $ident) {
	$ident;
}
multi identifier(@ident) {
	Identifier.new-from-list(@ident);
}

our sub identifiers(Identifiers(Any) $idents) is export(:functions) {
	$idents;
}

our sub binary(Str $operator, Expression $left, Expression $right, *%args) is export(:functions) {
	my $class = %binary-op-for{$operator.lc}:exists ?? %binary-op-for{$operator.lc} !! die "No such operator '$operator'";
	$class.new(:$left, :$right, |%args);
}

our sub logical(Str $operator, @elements) is export(:functions) {
	my $class = $operator.lc eq 'and' ?? Op::And !! $operator.lc eq 'or' ?? Op::Or !! die "No such operator '$operator'";
	$class.new(:@elements);
}

our sub not(Expression $expression) is export(:functions) {
	Op::Not.new($expression)
}

our sub function(Str $function, Column::List(Any) $arguments = (), Conditions(Any) :$filter, Quantifier(Str) :$quantifier, OrderBy(Any) :$order-by) is export(:functions) {
	Function.new(:$function, :$arguments, :$filter, :$quantifier, :$order-by);
}

our sub value(Any $value) is export(:functions) {
	expand-expression($value);
}

our sub null() is export(:functions) {
	Value::Null.new;
}
our sub default() is export(:functions) {
	Value::Default.new;
}
our sub true() is export(:functions) {
	Value::True.new;
}
our sub false() is export(:functions) {
	Value::False.new;
}

our sub current(Identifier(Cool) $cursor) is export(:functions) {
	Current.new(:$cursor);
}

my package EXPORT::functions {
	our &delegate       = &SQL::Query::delegate;
	our &delegate-pair  = &SQL::Query::delegate-pair;
	our &delegate-pairs = &SQL::Query::delegate-pairs;
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
my $result = $abstract.select($join, ['books.name', 'authors.name'], { :cost('<' => 10) });

=end code

=head1 Description

SQL::Abstract abstracts the generation of SQL queries. Fundamentally its functionality has three components

=item1 An AST to represent SQL queries
=item1 A set of helpers to convert standard raku datatypes into such an AST
=item1 A renderer to turn that AST into an SQL query

It should be able to represent any C<SELECT>, C<UPDATE>, C<INSERT>, or C<DELETE> query that is valid in both Postgresql and SQLite. This subset should be generally portable to other databases as well.

=head1 Class SQL::Abstract

This is the main class of the module.

=head3 new(:$placeholders!, Bool :$quoting)

This creates a new C<SQL::Abstract> object. It has one mandatory name argument, C<$placeholders>, which takes one of the following values:

=begin item1
C<'dbi'>/C<SQL::Abstract::Placeholders::DBI>

This will use DBI style C<(?, ?)> placeholders
=end item1

=begin item1
C<'postgres'>/C<SQL::Abstract::Placeholders::Postgres>

This will use Postgres style C<($1, $2)> placeholders.
=end item1

It also takes an optional argument C<:$quoting>, if enabled table and column names will always be quoted.

=head2 select

=begin code :lang<raku>

method select(Source(Any) $source, Column::List(Any) $columns = *, Conditions(Any) $where?, Common(Any) :$common,
Distinction(Any) :$distinct, GroupBy(Any) :$group-by, Conditions(Any) :$having, Window::Clauses(Any) :$windows,
Compound(Pair) :$compound, OrderBy(Any) :$order-by, Limit :$limit, Offset :$offset, Locking(Any) :$locking)

=end code

This will generate a C<SELECT> query. It will select C<$columns> from C<$source>, filtering by $conditions.

=begin code :lang<raku>

my $join = { :left<books>, :right<authors>, :using<author_id> };
my $result = $abstract.select($join, ['books.name', 'authors.name'], { :cost{ '<' => 10 } });
# SELECT books.name, authors.name FROM books INNER JOIN authors USING (author_id) WHERE cost < 10

my @columns = [ 'name', :sum{ :function<count>, :arguments(*) } ];
my $counts = $$abstract.select('artists', @columns, { :name(like => 'A%') }, :group-by<name>, :order-by(:sum<desc>));
# SELECT name, COUNT(*) as sum FROM artists WHERE name LIKE 'A%' GROUP BY name ORDER BY sum DESC

=end code

=head2 update

=begin code :lang<raku>

method update(Table(Any) $target, Assigns(Any) $assigns, Conditions(Any) $where?,
Common(Any) :$common, Source(Any) :$from, Column::List(Any) :$returning)

=end code

This will update C<$target> by assigning the columns and values from C<$set> if they match C<$where>, returning C<$returning>.

=begin code :lang<raku>

$abtract.update('artists', { :name('The Artist (Formerly Known as Prince)') }, { :name<Prince> });
# UPDATE artists SET name = 'The Artist (Formerly Known as Prince)' WHERE name = 'Prince'

=end code

=head2 insert

=head3 Map insertion

=begin code :lang<raku>

method insert(Table(Any) $target, Assigns(Any) $values, Common(Any) :$common,
Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning)

=end code

Inserts the values in C<$values> into the table C<$target>, returning the columns in C<$returning>

=begin code :lang<raku>

$abstract.insert('artists', { :name<Metallica> }, :returning(*));
# INSERT INTO artists (name) VALUES ('Metallica') RETURNING *

=end code

=head3 List insertions

=begin code :lang<raku>

method insert(Table(Any) $target, Identifiers(Any) $columns, Rows(List) $rows, Common(Any) :$common,
Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning)

=end code

Insert into C<$target>, assigning each of the values in Rows to a new row in the table. This way one can insert a multitude of rows into a table.

=begin code :lang<raku>

$abstract.insert('artists', ['name'], [ ['Metallica'], ['Motörhead'] ], :returning(*));
# INSERT INTO artists (name) VALUES ('Metallica'), ('Motörhead') RETURNING *

$abstract.insert('artists', List, [ [ 'Metallica'], ], :returning<id>);
# INSERT INTO artists VALUES ('Metallica') RETURNING id

=end code

=head3 Select insertion

=begin code :lang<raku>

method insert(Table(Any) $target, Identifiers(Any) $columns, Select(Map) $select, Common(Any) :$common,
Overriding(Str) :$overriding, Conflicts(Any) :$conflicts, Column::List(Any) :$returning)

=end code

This selects from a (usually different) table, and inserts the values into the table.

=begin code :lang<raku>

$abstract.insert('artists', 'name', { :source<new_artists>, :columns<name> }, :returning(*));
# INSERT INTO artists (name) SELECT name FROM new_artists RETURNING *

=end code

=head2 delete

=begin code :lang<raku>

method delete(Table(Any) $target, Conditions(Any) $where, Common(Any) :$common,
Source(Any) :$using, Column::List(Any) :$returning)

=end code

This deletes rows from the database, optionally returning their values.

=begin code :lang<raku>

$abstract.delete('artists', { :name<Madonna> });
# DELETE FROM artists WHERE name = 'Madonna'

=end code

=head1 Helper types

SQL::Abstract uses various helper types that will generally coerce from basic datastructures:

=head2 SQL::Abstract::Identifier

This represents an identifier (typically a table name or column name, or an alias for such). It can be coerced from a string (e.g. C<"foo"> or C<"foo.bar">).

=head2 SQL::Abstract::Identifiers

This takes either a list of C<Identifier()>, or a single C<Identifier()>.

=begin code :lang<raku>

my SQL::Abstract::Identifiers() $identifiers = <name email website>;

=end code

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
Pair (Str => Function(Map))

This will use the function as a subquery.
=end item1

=begin item1
Map

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

=begin code :lang<raku>

my Column::List() $columns = ('name', :number(:count(*)));
# name, COUNT(*) AS number;

=end code

This is a list of items representing a column. Each item can either be a:
much like C<Identifiers>, however it will accept not just identifiers but any expression (e.g. comparisons, function calls, etc…). If given a pair it will rename the value to the key (C<value AS key>). A whatever-star will represent all columns.

=head2 SQL::Abstract::Conditions

=begin code :lang<raku>

my Conditions() $conditions = { :name(:like<%leon%>), :age(25..45), :country('nl'|'be'|lu') };
# name LIKE '%leon%' AND AGE BETWEEN 25 AND 45 AND country IN('nl', 'be', 'lu')

=end code

This is a pair, a list of pairs, a hash or an C<Expression>. In the former three cases, the key (called left in the rest of this section) shall be an C<Identifier()> designating a column name, or an C<Expression>. The right hand side can be one of several types:

=head3 Expression

This will be used as-is

=head3 Any:U

This will check if the left expression is C<NULL>; C<:left(Any)> equals C<left IS NULL>.

=head3 Pair

This will use the key as operator to compare left against another value or expression. E.g. C<< :left('<' => 42) >> renders like C<< left < 42 >>. The following keys are known:

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

A few operators are not binary operators as such.

=begin item1
C<null>

will do C<IS NULL>, or C<IS NOT NULL> if its argument is false(C<:!null>).
=end item1

=begin item1
C<in>/C<not-in>

This takes a list of values that the column will be compared to. E.g. C<:in(1, 2, 3)>.
=end item1

=begin item1
C<and>/C<or>

The logical operators take a list or arguments, that are all expanded like pair values. So C<< :foo(:and('>' => 3, '<' => 42)) >> renders as C<< foo > 3 AND foo < 42 >>
=end item1

=head3 Range

This will check if a value is in a certain range. E.g. C<:left(1..42)> will render like C<left BETWEEN 1 AND 42>.

=head3 Junction

This will check against the values in the function. E.g. C<:left(1|2|4)> will render like C<left IN (1, 2, 4)>.

=head3 Capture

This will be expanded as a capture expression (. E.g. C<:left(\'NOW()')> will render like C<left = NOW()>. If it's an C<:op> expression, the column will be inserted as first/left operand: C<< :left(\(:op('<' => 42))) >> renders like C<< left < 42 >>.

=head3 Any

If none of the above options match, the value will be compared to as is (as a placeholder). C<:left(2)> will render equivalent to C<left = 2>.

=head2 SQL::Abstract::Assigns

This takes a list of pairs, or a hash. The keys shall be a value or an expression. E.g. C<< :name<author>, :id(SQL::Abstract::Values::Default), :timestamp(\'NOW()') >>

=head2 SQL::Abstract::OrderBy

This takes a list of things to sort by. Much like C<Column::List> this accepts identifiers and expressions, but C<*> isn't allowed and pair values are interpreted as order modifier (e.g. C<:column<desc>>). A hash element will be expanded as well (e.g. C<< { :column<column_name>, :order<desc>, :nulls<last> } >>)

=head2 SQL::Abstract::Common

This represents a common table expression. It converts from a pair or a list of pairs, with the keys being the name and the values being either a table name, a select hash or an C<SQL::Abstract::Query> object.

=begin code :lang<raku>

my Common() $cte = recent => { :source<users>, :columns('name', :count(:count(*)), :group-by(name) };
# WITH recent AS (SELECT name, COUNT(*) AS count FROM users GROUP BY name);

=end code

=head2 SQL::Abstract::Locking

This takes one or more locking clauses. A locking clause is usually taken ... strings: C<'update'>, C'<no key update'>, C<'share'>, C<'key share'>, but it can also take a pair of stregth

=head2 SQL::Abstract::GroupBy

This takes a list of grouping elements. Usually these are just columns, but they may also be arbitrary expressions (inclusing lists of columns). A pair is taken as a function call with the key as function name and the value as arguments.

=head2 SQL::Abstract::Conflicts

This represents one or more upsert clause. It can either be the string C<'nothing'>, or a pair with the columns as keys and an C<Assigns(Map)>.

=begin code :lang<raku>

my SQL::Abstract::Conflicts = <name organization> => { :$email };
# ON CONFLICT (name, organization) DO UPDATE email = ?

=end code

=head2 SQL::Abstract::Distinction

This takes C<True>for a distinct row, or a C<Column::List> for specific rows that have to be distinct.

=head2 Window::Definition

Window definiton converts from a map taking the following keys, all optional:

=begin item1
Identifier(Cool) :$existing

This takes the name of an existing window
=end item1

=begin item1
Column::List(Any) :$partition-by

This lists expressions to partition the rows by, somewhat similar to a C<GROUP BY>.
=end item1

=begin item1
OrderBy(Any) :$order-by

The order within a frame.
=end item1

=begin item1
Boundary(Any) :$from

This argument defines the starting boundary of the frame. This can be any of:

=item2 'preceding'
=item2 :preceding($amount)
=item2 'current'
=item2 :following($amount)
=end item1

It defaults to 'preceding'.

=begin item1
Boundary(Any) :$to

This optional argument defines the ending boundary of the frame, This can be any of:

=item2 :preceding($amount)
=item2 'current'
=item2 :following($amount)
=item2 'following'

=end item1

=begin item1
Mode:D(Str) :$mode = Mode::Range

The mode of the frame takes one of C<'rows'>, C<'range'> or C<'groups'>, defaulting to C<'range'>.
=end item1

=begin item1
Exclusion(Str) :$exclude

The exclusion of the frame, it takes one of C<'current row'>, C<'group'>, C<'ties'> or C<'no others'> (the default).
=end item1

=begin code :lang<raku>

my Window::Definition $d = { :partition-by<foo bar>, :from<current> :to(:following(5)), :exclude<ties> }
# PARTITION BY foo, bar RANGE BETWEEN CURRENT ROW AND 5 FOLLOWING EXCLUDE TIES

=end code

=head2 Window::Clauses

This takes one or more pairs, with the names being windows names and the values taking window definition maps.

=begin code :lang<raku>

my Windows::Clauses $clauses =
    over5 => { :frame{ :preceding(5) } },
    foo => { :partition-by<foo bar>, :mode<range>, :from<current> :to(:following(5)), :exclude<ties> };
# WINDOW
#   over5 AS (ROWS 5 PRECEDING),
#   foo as (PARTITION BY foo, bar RANGE BETWEEN CURRENT ROW AND 5 FOLLOWING EXCLUDE TIES)

=end code

=head2 SQL::Abstract::Limit / SQL::Abstract::Offset

These both take either an C<Int> or an C<Expression>.

=head1 Capture expressions

Captures can be used in most places where Expressions can be used. They allow for SQL expressions that can't be encoded using simpler values.

There are two kinds of capture expressions. The first kind has one or more named arguments; the first will be used as literal SQL, the rest will be arguments for the literal. E.g. C<\'NOW()'>.

The second kind takes a single named argument that may or may not contain a value. Currently supported are:

=begin item1
Any :$bind

This represents a placeholder variable with the given value
=end item1

=begin item1
Bool :$default

This represents the C<DEFAULT> keyword
=end item1

=begin item1
Bool :$true

This represents the C<TRUE> keyword
=end item1

=begin item1
Bool :$false

This represents the C<FALSE> keyword
=end item1

=begin item1
Bool :$null

This represents the C<NULL> keyword
=end item1

=begin item1
Str :$ident

This represents an identifier (typically a column or table name)
=end item1

=begin item1
Bool :$star

This represents a C<*>.
=end item1

=begin item1
Any :$idents

This represents a list of identifiers
=end item1

=begin item1
Any :$columns

This represents a list of column expressions
=end item1

=begin item1
:op(@) ('not'|'+'|'-'|'~', Any $expr)

This represents a unary operator (C<NOT>, C<+>, C<->, C<~>).
=end item1

=begin item1
:op(@) ('like'|'not-like', Any $left-expr, Any $right-expr, Any $escape-expr)

This represents a C<LIKE> operator, e.h. C<column LIKE '%foo?' ESCAPE '\\'>
=end item1

=begin item1
:op(@) ('between'|'not-between', Any $column-expr, Any $left-expr, Any $right-expr)

This represents a C<BETWEEN> expression.
=end item1

=begin item1
:@and

This represents an AND expresssion. Typically the contents of this will be further capture expressions.
=end item1

=begin item1
:@or

This represents an OR expresssion. Typically the contents of this will be further capture expressions.
=end item1
=begin item1
:@op ('in'|'not-in', Any $left-expr, Capture $ (SQL::Abstract::Select(Map) :$select!))

This represents an <IN> expression with subquery. E.g. C<foo in (SELECT bar from baz where baz.id = table.id)>.
=end item1

=begin item1
:@op ('in'|'not-in', Any $left-expr, *@args)

This represents an <IN> expression with list. E.g. C<foo in (1, 2, 3)>.
=end item1

=begin item1
Select(Map) :$exists

This represents an C<EXISTS> expression. E.g. C<\(:exists{ :source<table>, columns<1>, :where{ :id(\(:ident<outer.id>)) }> for C<EXISTS(SELECT 1 FROM table WHERE id = outer.id>.
=end item1

=begin item1
Select(Map) :$not-exists

This is like :exists, but negated.
=end item1

=begin item1
:@op ("cast", Any $expression, Str $typename)

This is a <CAST> expression. E.g. C<CAST(columns AS INTEGER)>.
=end item1

=begin item1
:op(@) (Str $key, Any $left-expr, Any $right-expr)

Any binary operator applied to two expressions.
=end item1

=begin item1
Identifier(Any) :$current

This represents a C<CURRENT FOR cursor_name> clause, typically used in an C<UPDATE> or C<DELETE> statement.
=end item1

=begin item1
List :$row

This represents a row expression, e.g. C<(a, b, c)>.
=end item1

=begin item1
Function(Map) :$function

This represents a function call.
=end item1

=begin item1
Select(Map) :$select

This represents a subquery expression.
=end item1

=head1 Author

Leon Timmermans <fawaka@gmail.com>

=head1 Copyright and License

Copyright 2022 Leon Timmermans

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
