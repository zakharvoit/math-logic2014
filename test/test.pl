#!/usr/bin/env perl

use strict;
use warnings;

use Term::ANSIColor;

my $task = shift;

print colored ['yellow'], "Testing $task\n";
while (glob "./test-files/$task/*.in") {
	my $out = $_;
	$out =~ s/in/out/;
	my $should_fail = 0;
	$should_fail = 1 if (/fail/);
	if (/disable/) {
	    print colored ['yellow'], "Ignored $_\n";
	    next;
	}
	if (system("./$task.native <$_ >buffer.txt") != 0) {
	    if ($should_fail) {
		print colored ['green'], "OK failed $_\n";
	    } else {
		print colored ['red'], "Error on $_\n"
	    }
	} elsif ($should_fail) {
	    print colored ['red'], "Should fail $_\n";
	} elsif (system("cmp buffer.txt $out") != 0) {
		print colored ['red'], "Wrong answer on $_\n";
	} else {
		print colored ['green'], "OK $_\n";
	}
	system("grep -ir 'Не доказано' buffer.txt");
}

`rm -f buffer.txt`
