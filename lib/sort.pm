package sort;

our $VERSION = '1.00';

$sort::hint_bits       = 0x00020000; # HINT_LOCALIZE_HH, really...

$sort::quicksort_bit   = 0x00000001;
$sort::mergesort_bit   = 0x00000002;
$sort::sort_bits       = 0x000000FF; # allow 256 different ones
$sort::stable_bit      = 0x00000100;

use strict;

sub import {
    shift;
    if (@_ == 0) {
	require Carp;
	Carp::croak("sort pragma requires arguments");
    }
    $^H |= $sort::hint_bits;
    local $_;
    no warnings 'uninitialized';	# $^H{SORT} bitops would warn
    while ($_ = shift(@_)) {
	if (/^_q(?:uick)?sort$/) {
	    $^H{SORT} &= ~$sort::sort_bits;
	    $^H{SORT} |=  $sort::quicksort_bit;
	} elsif ($_ eq '_mergesort') {
	    $^H{SORT} &= ~$sort::sort_bits;
	    $^H{SORT} |=  $sort::mergesort_bit;
	} elsif ($_ eq 'stable') {
	    $^H{SORT} |=  $sort::stable_bit;
	} else {
	    require Carp;
	    Carp::croak("sort: unknown subpragma '$_'");
	}
    }
}

sub current {
    my @sort;
    if ($^H{SORT}) {
	push @sort, 'quicksort' if $^H{SORT} & $sort::quicksort_bit;
	push @sort, 'mergesort' if $^H{SORT} & $sort::mergesort_bit;
	push @sort, 'stable'    if $^H{SORT} & $sort::stable_bit;
    }
    push @sort, 'mergesort' unless @sort;
    join(' ', @sort);
}

1;
__END__

=head1 NAME

sort - perl pragma to control sort() behaviour

=head1 SYNOPSIS

    use sort 'stable';		# guarantee stability
    use sort '_quicksort';	# use a quicksort algorithm
    use sort '_mergesort';	# use a mergesort algorithm

    use sort '_qsort';		# alias for quicksort

    my $current = sort::current();	# identify prevailing algorithm

=head1 DESCRIPTION

With the sort pragma you can control the behaviour of the builtin
sort() function.

In Perl versions 5.6 and earlier the quicksort algorithm was used to
implement sort(), but in Perl 5.8 a mergesort algorithm was also made
available, mainly to guarantee worst case O(N log N) behaviour:
the worst case of quicksort is O(N**2).  In Perl 5.8 and later,
quicksort defends against quadratic behaviour by shuffling large
arrays before sorting.

A stable sort means that for records that compare equal, the original
input ordering is preserved.  mergesort is stable, quicksort is not.
Stability will matter only if elements that compare equal can be
distinguished in some other way.  That means that simple numerical
and lexical sorts do not profit from stability, since equal elements
are indistinguishable.  However, with a comparison such as

   { substr($a, 0, 3) cmp substr($b, 0, 3) }

stability might matter because elements that compare equal on the
first 3 characters may be distinguished based on subsequent characters.
In Perl 5.8 and later, quicksort can be stabilized, but doing so will
add overhead, so it should only be done if it matters.

The best algorithm depends on many things.  On average, mergesort
does fewer comparisons than quicksort, so it may be better when
complicated comparison routines are used.  Mergesort also takes
advantage of pre-existing order, so it would be favored for using
sort to merge several sorted arrays.  On the other hand, quicksort
is often faster for small arrays, and on platforms with small memory
caches that are much faster than main memory.  You can force the
choice of algorithm with this pragma, but this feels heavy-handed,
so the subpragmas beginning with a C<_> may not persist beyond Perl 5.8.

=cut

