use v6.d;

unit class SQL::Query:ver<0.0.7>:auth<zef:leont>;

class Delegate {
	has Str:D $.identifier is required;
	has Any $.default is default(Nil);
	has Any:U $.type;

	method resolve(%replacements) {
		if %replacements{$!identifier}:exists {
			%replacements{$!identifier};
		} elsif $!default !=== Nil {
			$!default;
		} else {
			die "No value given for delegate value '$!identifier'"
		}
	}
}

has Str:D $.sql is required;
has @.arguments is required;

method new(Str:D $sql, @arguments?) {
	self.bless(:$sql, :@arguments);
}

method type-hints() {
	@!arguments.map: { $^value ~~ Delegate ?? $value.type !! $value.WHAT };
}

method resolve(%replacements?) {
	@!arguments.map: { $^value ~~ Delegate ?? $^value.resolve(%replacements) !! $^value };
}

method identifiers() {
	@!arguments.grep(Delegate)».identifier;
}

method has-delegate(--> Bool) {
	so any(@!arguments) ~~ Delegate;
}

our sub delegate(Str:D $identifier, Any $default = Nil, Any:U :$type = $default.WHAT) is export(:delegate) {
	Delegate.new(:$identifier, :$default, :$type);
}
our sub delegate-pair(Str:D $identifier, Any $default = Nil, Any:U :$type = $default.WHAT) is export(:delegate) {
	$identifier => Delegate.new(:$identifier, :$default, :$type);
}
our sub delegate-pairs(*@identifiers) is export(:delegate) {
	@identifiers.map(&delegate-pair);
}

=begin pod

=head1 Name

SQL::Query - An SQL query with it's arguments

=head1 Synopsis

=begin code :lang<raku>

use SQL::Query :delegate;
my $query = SQL::Query.new('SELECT ...', [1, 2, delegate('foo')]);
my $sth = $dbh.prepare($query);
for 3..5 -> $foo {
	$sth.execute($query.resolve({ :$foo }));
}

=end code

=head1 Description

This abstracts over the combination of a query and its placeholder arguments.

=head1 SQL::Query

=head2 new(Str:D $sql, @arguments? --> SQL::Query)

This creates a new C<SQL::Query> object.

=head2 sql(--> Str:D)

This returns the SQL portion of the query.

=head2 arguments(--> List)

This returns the argument list without resolving any delegate values.

=head2 resolve(%replacements --> List)

This resolves the delegate arguments in the list using C<%replacements> and returns the result.

=head2 identifiers(--> List)

This returns all identifiers of delegate arguments.

=head2 type-hints(--> List)

This returns the expected types for the arguments.

=head2 has-delegate(--> Bool)

This returns true if any argument is delegate.

=head1 delegate values

This functionality is mainly useful when abstracting over prepared statements.
A delegate value can be using three helper functions:

=head2 delegate

This takes an identifier that is later used to map the value to its substitution. It optionally takes a default value in case no substitution is provided and a type for type-hinting.

=code delegate(Str $identifier, Any $default?, Any:U :$type --> SQL::Query::Delegate)

=head2 delegate-pair

=code delegate-pair(Str $identifier, Any $default?, Any:U :$type = $default.WHAT --> Pair)

This is much like C<delegate>, but returns a pair with the identifier as key and the delegate as value.

=head2 delegate-pairs

=code delegate-pairs(*@identifiers)

This returns a sequence of delegate pairs with the chosen identifiers.

=end pod
