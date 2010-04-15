#!/stardev/bin/tclsh8.2

#+
#  querycat.tcl
#
#  Example script to perform a selection on a remote catalogue.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   18/5/01 (ACD): Original version.
#-

#
#  Set a variable to hold the name and location of the catremote script.

    set catremoteExe /home/acd/starbase/cursa/catremote/catremote

#
#  Set variables to hold the name of the catalogue to be queried
#  (usno@eso) and the details of the region to be searched (the area
#  within 10 minutes of arc of Right Ascension 12:15:00, Declination
#  30:30:00 (J2000)).

    set remoteQueryCat    "usno@eso"
    set remoteQueryRA     "12:15:00"
    set remoteQueryDec    "30:30:00"
    set remoteQueryRadius "10"

#
#  Set the remote server to one which includes the catalogue usno@eso.

    set env(CATREM_CONFIG) "http://dev.starlink.ac.uk/~pwd/catremote/cursa.cfg"

#
#  Attempt to execute catremote to submit the query and proceed if ok.
#  The output is written to temporary file QUERY_RESULT.

    set queryStatus [catch {
      exec $catremoteExe "query" $remoteQueryCat $remoteQueryRA \
        $remoteQueryDec $remoteQueryRadius > QUERY_RESULT } queryMsg]

    if {$queryStatus == 0} then {

#
#     Read back the output written to temporary file QUERY_RESULT and
#     copy it to standard output.  Reading continues until an empty
#     line is encountered.
#
#     If the query succeeded a single line will be returned which includes
#     the name of the catalogue of retrieved objects, whereas a failed
#     query will return a different message.  A typical application might
#     parse the line to check for success or failure and obtain the catalogue
#     name in the case of success, eg. so that the catalogue can be opened.
#     In this example the line is parsed and simple messages written.

       set crf [open "QUERY_RESULT" r]
       set more 1

       while {$more != 0} {
          gets $crf currentLine
          if {$currentLine != ""} then {
             puts stdout $currentLine

             set catLine [string first "!(Info.) Catalogue" $currentLine]

             if {$catLine >= 0} then {
                set catalogueTemp [string range $currentLine 19 end]
                set firstSpace [string first " " $catalogueTemp]
                set catalogueName [string range $catalogueTemp 0 $firstSpace]
                puts stdout "*** Created catalogue $catalogueName."
             } else {
                puts stdout "*** Query failed."
             }
          } else {
             set more 0
          }
       }

#
#     Close and delete the temporary file.

       close $crf

       set queryStatus [catch {exec rm -f QUERY_RESULT} queryMsg]
    }

#
#  Report any error.

    if {$queryStatus != 0} then {
       puts stdout "Failed to run catremote (to query remote catalogue)."
       puts stdout "$queryMsg"
    }
