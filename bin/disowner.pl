#!/usr/bin/env perl
use POSIX qw/WNOHANG/;

$SIG{CHLD} = 'IGNORE';
open my $fh, '-|', @ARGV;
