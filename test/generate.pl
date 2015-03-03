#!/usr/bin/env perl

use strict;
use warnings;

use Term::ANSIColor;

my $task = shift;

print colored ['yellow'], "Generating answers for $task\n";
while (glob "./test-files/$task/*.in") {
	my $out = $_;
	$out =~ s/in/out/;
	if (/disable/) {
	    print colored ['yellow'], "Ignoring $_\n";
	    next;
	}
	`rm -f $out`;
	if (system ("./$task.native <$_ >$out") != 0) {
		print colored ['red'], "$out generation error\n";
	} else {
		print colored ['green'], "$out generated\n";
	}
}
