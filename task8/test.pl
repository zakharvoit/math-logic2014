#!/usr/bin/env perl
use warnings;
use strict;

use Term::ANSIColor;

my $executable = shift;
$executable = "./task8.native" if !defined($executable);

while (glob "./test-files/*.in") {
    my $expected;
    if (/equal/) {
        $expected = 1;
    } elsif (/differ/) {
        $expected = 0;
    } else {
        print colored ['red'], "Bad name on $_\n";
        next;
    }

    my $output = `$executable <$_`;
    my $actual;
    if ($output =~ /^Равны/) {
        $output = 1;
    } elsif ($output =~ /^Не равны/) {
        $output = 0;
    } else {
        print colored ['red'], "Bad output on $_\n";
        next;
    }
    if (${^CHILD_ERROR_NATIVE} != 0) {
        print colored ['red'], "RE on $_\n";
    } elsif ($output == $expected) {
        print colored ['green'], "OK on $_\n";
    } else {
        print colored ['red'], "WA on $_\n";
    }
}
