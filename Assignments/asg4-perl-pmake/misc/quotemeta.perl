#!/usr/bin/perl
# $Id: quotemeta.perl,v 1.6 2021-06-01 19:28:58-07 - - $

@patterns = ('%.o', '%.cmi');

for $pat (@patterns) {
   my $regex = quotemeta $pat;
   print "line8='$regex'\n";
   $regex =~ s/\\%/(.*)/;
   $regex = "^$regex\$";
   $regexhash{$pat} = $regex;
};

for $key (keys %regexhash) {
   print "'$key' => '$regexhash{$key}'\n";
}

$target = "foobar.o";
$prereq = "%.c";

print "prereq=$prereq\n";
print "\nsearching '$target'\n";
for $pat (keys %regexhash) {
   my $regex = $regexhash{$pat};
   if ($target =~ $regex) {
      my $percent = $1;
      print "'$target' matches '$regex' and % is $percent\n";
      $prereq =~ s/%/$percent/;
      print "prereq=$prereq\n";
   }
}

