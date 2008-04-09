#+
#  Name:
#    bch_load.awk
#
#  Purpose:
#    Get the current system load from a string output by w -u.
#
#  Language:
#    awk
#
#  Invocation:
#    echo "`w -u`" | awk -f bch_load.awk
#
#  Authors:
#    Tim Gledhill (tmg), University of Hertfordshire
#
#  History:
#    15-SEP-1994 (tmg):
#       Original version.
#-
  {
#
# Get the system load.
#
      for ( i = 1; i <= NF; i++ ) 
      {
        if ( $i == "average:" ) 
        {
          split($(i + 1 ), a, ",")
          load = a[1]
        }
      }

  }

END { printf( "%3.2f", load ) }
