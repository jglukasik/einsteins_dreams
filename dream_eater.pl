#!/usr/bin/perl
#
# Process text in Einstein's Dreams
#
# Call ./dream_eater to populate the sqlite database dream_catcher.db

use warnings;
use strict;

use Data::Dumper;
use DateTime;
use DBI;
use Time::Piece;

# Open the Einstein's Dreams text file
open(my $fh, "<", "/home/jglukasik/einsteins_dreams/ed.txt") or die "can't open file: $!";

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
      $day = $d->datetime;
    }

    $book{$day} = ();
    next;
  }

  # Get the text for the dream
  if (!/^\s*$/){
    $dream = $dream . ' ' . $_;
  }
}

my $db_file = '/home/jglukasik/einsteins_dreams/dream_catcher.db';
my $dbh = DBI->connect("dbi:SQLite:dbname=$db_file", "", "", { RaiseError => 1})
  or die ("Coudn't connect to db:" . DBI->errstr);

my $create_sth = $dbh->prepare('CREATE TABLE IF NOT EXISTS "dream"("id" INTEGER PRIMARY KEY,"date" TIMESTAMP NOT NULL,"content" VARCHAR NOT NULL);');
$create_sth->execute();

my $insert_sth = $dbh->prepare_cached('INSERT INTO dream (date,content) VALUES (?, ?)');

foreach my $d (keys %book){
  next if ($d =~ m/INTERLUDE|EPILOGUE|^$/g);
  $insert_sth->execute($d, $book{$d});
}
