#!/usr/bin/env perl
use POSIX qw/WNOHANG/;
use Time::HiRes qw/sleep/;

$SIG{CHLD} = 'IGNORE';
if (my $pid = fork) {
    print "$pid\n";
}
else {
    sleep 0.1; # time for the print to finish up
    close \*STDERR;
    close \*STDOUT;
    exec @ARGV;
}
