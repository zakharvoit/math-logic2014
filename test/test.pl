#!/usr/bin/env perl

use strict;
use warnings;

use Term::ANSIColor;

my $task = shift;

print colored ['yellow'], "Testing $task\n";
while (glob "./test-files/$task/*.in") {
	my $out = $_;
	$out =~ s/in/out/;
	if (system("./$task.native <$_ >buffer.txt") != 0) {
		print colored ['red'], "Error on $_\n"
	} elsif (system("cmp buffer.txt $out") != 0) {
		print colored ['red'], "Wrong answer on $_\n";
	} else {
		print colored ['green'], "OK $_\n";
	}
	system("grep -ir 'Не доказано' buffer.txt");
}

`rm -f buffer.txt`
