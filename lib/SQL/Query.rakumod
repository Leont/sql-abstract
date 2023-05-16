use v6;

unit class SQL::Query;

class Delayed {
	has Str:D $.identifier is required;
	has Any $.default is default(Nil);
	has Any:U $.type;

	method has-default() {
		$!default !=== Nil;
	}
}

has Str:D $.sql is required;
has @.arguments is required;

method new(Str:D $sql, @arguments?) {
	self.bless(:$sql, :@arguments);
}

method type-hints() {
	@!arguments.map: { $^value ~~ Delayed ?? $value.type !! $value.WHAT };
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

our sub delay(Str:D $identifier, Any $default = Nil, Any:U :$type = $default.WHAT) is export(:delay) {
    Delayed.new(:$identifier, :$default, :$type);
}

=begin pod

=head1 Name

SQL::Query - An SQL query with it's arguments

=head1 Synopsis

=begin code :lang<raku>

use SQL::Query :delay;
my $query = SQL::Query.new('SELECT ...', [1, 2, delay('foo')]);
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

This returns the argument list without resolving any delayed values.

=head2 resolve(%replacements --> List)

This resolves the delayed arguments in the list using C<%replacements> and returns the result.

=head2 identifiers(--> List)

This returns all identifiers of delayed arguments.

=head2 type-hints(--> List)

This returns the expected types for the arguments.

=head2 has-delayed(--> Bool)

This returns true if any argument is delayed.

=head1 delayed values

A delayed value can be using the C<delay> function.

=head2 delay(Str:D $identifier, Any $default?, Any:U :$type --> SQL::Query::Delayed)

This takes an identifier that is later used to map the value to its substitution. It optionally takes a default value in case no substitution is provided.

This functionality is mainly useful when abstracting over prepared statements.

=end pod
