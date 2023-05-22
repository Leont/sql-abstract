unit package SQL;

use SQL::Query;

role Placeholders:ver<0.0.3>:auth<zef:leont> {
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
	has Str %!names;
	multi method bind(Any $value --> Str) {
		@!values.push($value);
		'$' ~ +@!values;
	}
	multi method bind(SQL::Query::Delegate $value --> Str) {
		if not $value.has-default and %!names{$value.identifier} -> $placeholder {
			$placeholder;
		} else {
			@!values.push($value);
			%!names{$value.identifier} = '$' ~ +@!values;
		}
	}
}

=begin pod

=head1 Name

SQL::Placeholders - pluggable placeholder management

=head1 Description

This package abstracts away the difference between different styles of placeholders. It's an interface with currently only two methods:

=head2 values(--> List)

This returns the values of all bound values.

=head2 bind($value --> Str)

This adds adds a bound value, and returns the string that should be inserted into the SQL in its place.

=head1 Implementations

It currently offers two implementations of the C<Placeholders> role.

=head2 C<Placeholders::DBI>

This is for DBI style placeholders, e.g. C<?, ?>.

=head2 C<Placeholders::Postgres>

This is for postgres style placeholders, e.g. C<$1, $2>.

=end pod
