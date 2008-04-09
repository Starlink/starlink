#+
#  Name:
#    bch_print.awk
#
#  Purpose:
#    Output benchmarking statistics. The input systax is that produced
#    by the tcsh time command.
#
#  Language:
#    awk
#
#  Invocation:
#    echo <benchmark> <statistics> | awk -f bch_print.awk
#
#  Authors:
#    Tim Gledhill (tmg), University of Hertfordshire
#
#  History:
#    20-AUG-1994 (tmg):
#       Original version.
#-
  {
#
# The first field should be the benchmark name. This is passed unaltered.
# 
      benchmark = $1
#
# The second field is the user cpu time.
#
      split($2,a,"u")
      utime = a[1]
#
# The third field gives the system cpu time.
#
      split($3,a,"s")
      stime = a[1]
#
# The fourth field gives the elapsed time. This is passed unaltered.
#
      etime = $4
#
# Print the output.
#

      printf( "\n %15s%13.2f%14.2f%17s\n", benchmark, utime, stime, etime )

  }
        
