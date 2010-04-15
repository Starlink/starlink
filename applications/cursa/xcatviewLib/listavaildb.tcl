#!/stardev/bin/tclsh8.2

#+
#  listavaildb.tcl
#
#  Example script to list the remote databases which are currently
#  available.
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
#  Set the remote server to the default CURSA list.

    set env(CATREM_CONFIG) "http://dev.starlink.ac.uk/~pwd/catremote/cursa.cfg"

#
#  Attempt to execute catremote to list the databases available and
#  proceed if ok.  The output is written to temporary file DB_LIST.

    set queryStatus [catch {
      exec $catremoteExe "list" > DB_LIST} queryMsg]

    if {$queryStatus == 0} then {

#
#     Read back the output written to temporary file DB_LIST and
#     copy it to standard output.  Reading continues until an empty
#     line is encountered.
#
#     A real application would probably check for error messages (which
#     begin with an exclamation mark ('!')) and store the list of databases
#     for future use.

       set crf [open "DB_LIST" r]
       set more 1

       while {$more != 0} {
          gets $crf currentLine
          if {$currentLine != ""} then {
             puts stdout $currentLine
          } else {
             set more 0
          }
       }

#
#     Close and delete the temporary file.

       close $crf

       set queryStatus [catch {exec rm -f DB_LIST} queryMsg]
    }

#
#  Report any error.

    if {$queryStatus != 0} then {
       puts stdout "Failed to run catremote (to list databases available)."
       puts stdout "$queryMsg"
    }
