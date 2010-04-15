#!/bin/csh -f

# Find INCLUDE dependencies of each file

foreach i (sculib*.f sculib*.F sculib*.c )
   set incs = `cat $i | grep INCLUDE | awk -F\' '{print $2}' | sort | uniq | fmt -60`
   echo ${i:r}.o: $incs

end
