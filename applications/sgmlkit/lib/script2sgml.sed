# Extract the documentation header (ie, between `#+' and `#-') 
# from a file with # comments (ie, a shell or Perl script.
# Between <dl>..</dl>, rewrite `A:B' as `<dt>A<dd>B'.
#
# $Id$

1,/^#+/d
/^#-/{s/^.*//;q;}
s/^# *//
/<dl>/,/<\/dl>/s/\([^:]*\): */<dt>\1<dd>/
