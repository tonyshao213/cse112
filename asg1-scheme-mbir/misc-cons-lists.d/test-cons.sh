#!/bin/sh
# $Id: test-cons.sh,v 1.1 2020-10-07 15:55:53-07 - - $
cid -is $0

make cons-lists --makefile=- <<__END__
cons-lists: cons-lists.cpp
	mkc cons-lists.cpp
__END__
[ $? -eq 0 ] || exit $?

cons-lists '     ( a )    ' <<__END__
((( a b ((c)) )))
(a (  b  )  c)
(a b (c) x )))) def

a b c
__END__

