#+
#  Name:
#    bch_head.awk
#
#  Purpose:
#    Output column headings for benchmarking statistics.
#
#  Language:
#    awk
#
#  Invocation:
#    echo /dev/null | awk -f bch_head.awk
#
#  Authors:
#    Tim Gledhill (tmg), University of Hertfordshire
#
#  History:
#    30-AUG-1994 (tmg)
#      Original version.
#-
#
# Print the output.
#
END{      printf( "\n        Benchmark    ucpu (sec)    kcpu (sec)    Elapsed" \
              " (min:sec)" )
      printf( "\n        ---------    ----------    ----------    -------" \
              "----------" )
   }
