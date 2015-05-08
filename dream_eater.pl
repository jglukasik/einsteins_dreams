#!/usr/bin/perl
#
# Process text in Einstein's Dreams
#
# Call ./dream_eater mm-dd to print to STDOUT an html page, using dream_catcher
# as a template, containing the dream for the given day

use warnings;
use strict;

use HTML::Template;
use Time::Piece;
use Data::Dumper;

# Holds onto my phone number
use credentials;

# Open the Einstein's Dreams text file
open(my $fh, "<", 'ed.txt') or die "can't open file: $!";

my %book;
my $day = '';
my $dream = '';

# First, get past the index in the book
while (<$fh>) {
  if (/^• PROLOGUE/){
    last;
  }
}

# Then, loop through line by line once the book actually starts
while (<$fh>) {
  chomp;

  # Remove the form feed character that is at the start of a chapter title
  $_ =~ s/• //g;

  # These are the page number lines, get rid of them
  $_ =~ s/^.*$//g;

  # Check to see if a chapter just started
  if (/^(\d{1,2} (APRIL|April|May|June) 1905)|INTERLUDE|EPILOGUE/){

    # The dream we are currently holding on to, through in the hash for the
    # chapter preceding this one
    if ($dream ne ''){
      $book{$day} = $dream;
      $dream = '';
    }

    # Get the day for this current chapter, or just use the
    # interlude/epilogue key in the hash
    if (/INTERLUDE|EPILOGUE/){
      $day = $_; 
    } else {
      my $d = Time::Piece->strptime($_,"%d %B %Y");
      $day = $d->mdy;
    }

    $book{$day} = ();
    next;
  }

  # Get the text for the dream
  if (!/^\s*$/){
    $dream = $dream . ' ' . $_;
  }
}

my $today = $ARGV[0];  # In format: mm-dd
my $todays_dream;

# Prepare for sending out a text if there was a dream for today
my $phone_num = $credentials::phone_num;
my $message = "New dream at http://ed.jgl.me";
my $text_cmd = "curl -X POST http://textbelt.com/text -d number=$phone_num -d 'message=$message' ";

if (defined $book{"$today-1905"}){
  system($text_cmd);
  $todays_dream = $book{"$today-1905"};
} else {
  $todays_dream = "No dream today...";
}

# Create an html file with todays dream
my $template = HTML::Template->new(filename => 'dream_catcher.html');
$template->param(DATE => $today);
$template->param(DREAM => $todays_dream);
print $template->output;

