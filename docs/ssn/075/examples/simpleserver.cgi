#!/usr/local/bin/perl
#+
#  Name:
#     simpleserver.cgi
#  Purpose:
#     Simple example HTTP server.
#  Language:
#     Perl 5 / CGI script.
#  Description:
#     Simple example HTTP server.  The server invokes to generate a
#     fake star field.  The field is specified by its central Right
#     Ascension and Declination and radius.  The list of stars in
#     the field is returned as a tab-separated table.
#  Algorithm:
#     Copy the query string sent by the remote client to the query
#     buffer.
#     Parse the Query buffer to yield the Right Ascension, Declination
#     and radius.
#     Invoke the program to generate the list of objects which lie
#     within the star field and read it back.
#     Report any error.
#     Terminate output.
#  Authors:
#     ACD: A C Davenhall (Edinburgh)
#  History:
#     23/2/00  (ACD): Original version.
#     23/2/00  (ACD): First stable version.
#  Bugs:
#     None known
#  Libraries required:
#     None.
#-

#
#  Edit the following line so that queryExe is set to wherever
#  program genfield has been installed.

    $queryExe = "/star/examples/ssn75/genfield";

#
#  Initialise the status.

    $status = 0;
    $statusMessage = "success.";

#
#  Copy the query string sent by the remote client to the query buffer.
#
#  Note: to run the script interactively with fake input, rather than
#  in response to a query from a remote client: comment out the line
#  setting $query to an element of $ENV and un-comment the line explicitly
#  setting it.
#
#  The query should be in one or the other of the following two forms"
#
#    "ra=10:00:0.00&dec=-30:00:0.00&radius=3"
#    "ra=10.00&dec=-30.0&radius=3"
#
#  That is, sexagesimal or decimal forms are permitted for the R.A. and
#  Dec.  Irrespective of which form is used the units are: R.A. - hours;
#  Dec. - degrees and radius - minutes of arc.

    $query = $ENV{'QUERY_STRING'};
#    $query = "ra=10.00&dec=-30.0&radius=3";
#    $query = "ra=10:30:00&dec=-30:30.0&radius=3";

#
#  Parse the Query buffer to yield the Right Ascension, Declination
#  and radius.

    $status = &ParseQuery;

#
#  Invoke the program to generate the list of objects which satisfy
#  the query and read it back.

    if ($status == 0)
    {  print "Content-type: text/plain\n\n\n";

       $tst = `$queryExe $ra $dec $radius`;
       print "$tst";
    }

#
#  Report any error.

    if ($status != 0)
    {  print "Failed to extract a selection from the catalogue.\n";
       print "Query: $query \n";
       print "Error: $statusMessage \n";
    }

#
#  Terminate output.  This line signals to the remote client generating
#  the request the end of the data being returned to it.

    print "[EOD]\n";


#
# Functions.........................................................


#
# Parse the query string sent by the client to yield the Right
# Ascension, Declination and (outer) radius of the query.
# The Right Ascension is in sexagesimal or decimal hours, the Declination
# in sexagesimal or decimal degrees and the radius in minutes of arc.
#
# The query string is expected to have one of the two formats:
#
#    "ra=10:00:0.00&dec=-30:00:0.00&radius=3"
#    "ra=10.00&dec=-30.0&radius=3"

sub ParseQuery
{  if ($status == 0)
   {

#
#    Right Ascension.

      $start = index($query, "ra=") + 3;
      $stop = index($query, "&", $start);
      $length = $stop - $start;
      $rastr = substr($query, $start, $length);

#
#    Ensure that sexagesimal values are converted to decimal.  Also
#    convert the angle from hours to degrees.

      $ra = &ConvAngle($rastr);
      $ra = $ra * 15.0;

#
#    Declination.

      $start = index($query, "dec=") + 4;
      $stop = index($query, "&", $start);
      $length = $stop - $start;
      $decstr = substr($query, $start, $length);

#
#    Ensure that sexagesimal values are converted to decimal.

      $dec = &ConvAngle($decstr);

#
#    Radius.

      $start = index($query, "radius=") + 7;
      $radius = substr($query, $start, 100);

#     print "values decoded: $ra $dec $radius \n";

#
#    Check for some simple error cases.

      if ($ra == " "  ||  $dec == " " || $radius == " ")
      {  $status = 1;
         $statusMessage = "invalid query.";
      }
   }
   $status;
}


#
# Convert an angle in decimal or sexagesimal format to decimal format.
# Up to two sexagesimal subdivisions are allowed.

sub ConvAngle
{  if ($status == 0)
   {  $true = 1;
      $false = 0;

#
#    Copy the input argument to a variable.

      $inpAngle = $_[0];

#
#    Check if a first colon exists and proceed if there is at least one
#    colon.

      $firstCol = index($inpAngle, ":");

      if ($firstCol > -1)
      {

#
#       Check for negative values.

         $minusSign = index($inpAngle, "-");

         if ($minusSign > -1)
         {  $minus = $true;
            $absAngle = substr($inpAngle, $minusSign + 1, 100);
         }
         else
         {  $minus = $false;
            $absAngle = $inpAngle;
         }

#
#       Extract the whole part of the angle and the remainder.

         $firstCol = index($absAngle, ":");
         $whole = substr($absAngle, 0, $firstCol);
         $remainder = substr($absAngle, $firstCol + 1, 100);

#
#       Check whether the remainder contains an additional colon and
#       proceed accordingly.

         $secCol = index($remainder, ":");

         if ($secCol > -1)
         {  $min = substr($remainder, 0, $secCol);
            $sec = substr($remainder, $secCol + 1, 100);

            $angle = $whole + ($min / 60.0) + ($sec / 3600.0);
         }
         else
         {  $angle = $whole + ($remainder / 60.0);
         }

#
#       Restore the sign of a negative angle.

         if ($minus == $true)
         {  $angle = - $angle;
         }
      }
      else
      {  $angle = $inpAngle;
      }
   }
   else
   {  $angle = 0.0;
   }

#
# Set the return value.

   $angle
}
