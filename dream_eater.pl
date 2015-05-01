#!/usr/bin/perl
#
# Process text in Einstein's Dreams

use warnings;
use strict;

use Time::Piece;
use Data::Dumper;

open(my $fh, "<", 'ed.txt') or die "can't open file: $!";

my %book;
my $day = '';

while (<$fh>) {
  chomp;
  $_ =~ s/\d{1,2,3}\/203//g;
  $_ =~ s/^.*$//g;
  if (/^\d{1,2} (APRIL|April|May|June) 1905/){
    my $d = Time::Piece->strptime($_,"%d %B %Y");
    $day = $d->mdy;
    next;
  }
  push $_, @{ $book{$day} }; 
}

#print Dumper \%book;
#print $book{"05-29-1905"};
