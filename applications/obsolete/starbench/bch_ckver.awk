#+
#  Name:
#    bch_chkver.awk
#
#  Purpose:
#    Parse a Starlink package version number to get the major, minor and
#    revision numbers.
#
#  Language:
#    awk
#
#  Invocation:
#    echo $version | awk -f bch_chkver.awk
#
#  Description:
#    Given a Starlink package version number of the form X.Y-Z this
#    script splits off and prints out the major (X), minor (Y) and
#    revision (Z) numbers. If any of these numbers are not defined 
#    then they are set to zero. 
#
#  Notes:
#
#  Authors:
#    Tim Gledhill (tmg), University of Hertfordshire
#
#  History:
#    28-OCT-1997 (tmg):
#       Original version.
#-
#
# Split the major, minor and revision keys and print them out.
{
   major = 0
   minor = 0
   revision = 0
   if (index($0,".") >= 0) {
      split($0,x,".")
      major=x[1]
      if (index(x[2],"-") != 0 ) { 
        split(x[2],y,"-")
        minor=y[1]
        revision=y[2]
      }
      else
         minor=x[2]
   }
   else
      major=$0
   print major" "minor" "revision
}
