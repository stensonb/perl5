#!/usr/local/bin/perl

# Check whether there are naming conflicts when names are truncated
# to the DOSish case-ignoring 8.3 format

sub eight_dot_three {
    my ($dir, $base, $ext) = ($_[0] =~ m!^(?:(.+)/)?([^/.]+)(?:\.([^/.]+))?$!);
    $base = substr($base, 0, 8);
    $ext  = substr($ext,  0, 3) if defined $ext;
    if (defined $dir) {
	return ($dir, defined $ext ? "$dir/$base.$ext" : "$dir/$base");
    } else {
	return ('.', defined $ext ? "$base.$ext" : $base);
    }
}

my %dir;

if (open(MANIFEST, "MANIFEST")) {
    while (<MANIFEST>) {
	chomp;
	s/\s.+//;
	unless (-f) {
	    warn "$_: missing\n";
	    next;
	}
	if (tr/././ > 1) {
	    print "$_: more than one dot\n";
	    next;
	}
	my ($dir, $edt) = eight_dot_three($_);
	($dir, $edt) = map { lc } ($dir, $edt);
	push @{$dir{$dir}->{$edt}}, $_;
    }
} else {
    die "$0: MANIFEST: $!\n";
}

for my $dir (sort keys %dir) {
    for my $edt (keys %{$dir{$dir}}) {
	my @files = @{$dir{$dir}->{$edt}};
	if (@files > 1) {
	    print "@files: directory $dir conflict $edt\n";
	}
    }
}
