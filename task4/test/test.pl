#!/usr/bin/env perl

use strict;
use warnings;

use Term::ANSIColor;

my $task = shift;

print colored ['yellow'], "Testing $task\n";
while (glob "./test-files/$task/*.in") {
	my $out = $_;
	$out =~ s/in$/out/;
	if (/disable/) {
	    print colored ['yellow'], "Ignored $_\n";
	    next;
	}
	my $should_fail = 0;
	if (/fail/) {
	    $should_fail = 1;
	}
	if (system("./$task.native <$_ >buffer.txt") != 0) {
	    print colored ['red'], "Error on $_\n"
	} elsif (system("cmp buffer.txt $out") != 0) {
		print colored ['red'], "Wrong answer on $_\n";
	} else {
		print colored ['green'], "OK $_\n";
	}
	my $correct = system("grep -ir 'Некорректен' buffer.txt >/dev/null");
	if ($should_fail != !$correct) {
	    print colored ['red'], "Error on $_ (failed)";
	}
}

`rm -f buffer.txt`
