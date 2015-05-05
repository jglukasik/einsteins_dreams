#!/usr/bin/perl
#
# Process text in Einstein's Dreams

use warnings;
use strict;

use HTML::Template;
use Time::Piece;
use Data::Dumper;

open(my $fh, "<", 'ed.txt') or die "can't open file: $!";

my %book;
my $day = '';
my $dream = '';
while (<$fh>) {
  if (/^• PROLOGUE/){
    last;
  }
}

while (<$fh>) {
  chomp;
  $_ =~ s/• //g;
  $_ =~ s/^.*$//g;
  if (/^(\d{1,2} (APRIL|April|May|June) 1905)|INTERLUDE|EPILOGUE/){

    if ($dream ne ''){
      $book{$day} = $dream;
      $dream = '';
    }

    if (/INTERLUDE|EPILOGUE/){
      $day = $_;
    } else {
      my $d = Time::Piece->strptime($_,"%d %B %Y");
      $day = $d->mdy;
    }

    $book{$day} = ();
    next;
  }
  if (!/^\s*$/){
    $dream = $dream . ' ' . $_;
  }
}

my $todays_dream;
if (defined $book{"$ARGV[0]-1905"}){
  $todays_dream = $book{"$ARGV[0]-1905"};
} else {
  $todays_dream = "No dream today...";
}

my $template = HTML::Template->new(filename => 'dream_catcher.html');
$template->param(DREAM => $todays_dream);
print $template->output;

