#!/usr/bin/perl
# $Id: subst-macros.perl,v 1.10 2021-06-01 19:23:40-07 - - $

# Example substituting macros in a command.

use strict;
use warnings;

my %MACROS = (
  'ABC' => {
    'LINE' => '12',
    'VALUE' => 'DEF'
  },
  'THERE' => {
    'LINE' => '14',
    'VALUE' => 'there is a thing'
  },
  'THING' => {
    'LINE' => '13',
    'VALUE' => 'echo what do we want'
  }
);

my $prereq = "hello.c";
my @commands = (
   'echo /${ABC}/${FOOBAR}/${THING}/',
   'gcc -c $<',
   'echo $$?',
);

for my $command (@commands) {
   print "Before: $command\n";
   $command =~ s/\${(.*?)}/$MACROS{$1}{VALUE}||""/ge;
   $command =~ s/\$</$prereq/;
   $command =~ s/\$\$/\$/;
   print "After: $command\n";
}

