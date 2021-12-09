#!/usr/bin/perl
# $Id$

$value = "   foo   bar   baz   ";

@split = split '\s+', $value;
print "[$_]" for @split;
print "\n";

@split = split ' ', $value;
print "[$_]" for @split;
print "\n";

@split = split ' ', '';
print "[$_]" for @split;
print "\n";
