#!/stardev/bin/tclsh8.2

#+
#  resolvename.tcl
#
#  Example script to find the coordinates a named object.
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
#  Set the name of the object.

    set objectName "ngc3379"

#
#  Set the remote server to one which includes the SIMBAD name resolver.

    set env(CATREM_CONFIG) "http://dev.starlink.ac.uk/~pwd/catremote/cursa.cfg"

#
#  Attempt to execute catremote to resolve the object name and proceed if
#  ok.  The output is written to temporary file OBJECT_NAME.

    set queryStatus [catch {
      exec $catremoteExe "name" "simbad_ns@eso" $objectName \
        > OBJECT_NAME } queryMsg]

    if {$queryStatus == 0} then {

#
#     Read back the output written to temporary file OBJECT_NAME and
#     copy it to standard output.  Reading continues until an empty
#     line is encountered.

       set crf [open "OBJECT_NAME" r]
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

       set queryStatus [catch {exec rm -f OBJECT_NAME} queryMsg]
    }

#
#  Report any error.

    if {$queryStatus != 0} then {
       puts stdout "Failed to run catremote (to access name resolver)."
       puts stdout "$queryMsg"
    }
