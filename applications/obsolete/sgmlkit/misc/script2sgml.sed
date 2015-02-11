# Extract the documentation header (ie, between `#+' and `#-') 
# from a file with # comments (ie, a shell or Perl script.
# Between <dl>..</dl>, rewrite `A:B' as `<dt>A<dd>B'.
# In <[ou]l>, replaces leading * with <li>
#
# $Id$

#1,/^#+/d
#/^#-/{s/^.*//;q;}
1s/.*/<routine>/
2,/^#+/d
/^#-/{s/^.*/<codebody empty>/;q;}
s/^# *//
#/<dl>/,/<\/dl>/s/\([^:]*\): */<dt>\1<dd>/
#/<[ou]l>/,/<\/[ou]l>/s/^ *\* */<li>/
