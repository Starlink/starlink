$!+
$!  Name:
$!     CCDBATFIL.COM
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Purpose:
$!     Writes a commmand file containing ADAM initialising batch commands.
$!
$!  Description:
$!     This procedure writes commands to a file CCDPACKB.COM which initialise
$!     the setup required to run a ADAM tasks from batch. It write a command
$!     to assign the adam_user logical name to the current adam_saved logical
$!     name. Adam_saved has probably been set up by CCDSAVE, which copies all
$!     the SDF files in the current adam_user directory making a copy of the
$!     current environment.
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     20-MAY-1991 (PDRAPER):
$!        Original Version.
$!     {enter_further_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$! Determine the type of the command file.
$!
$TYPE = F$PARSE(P1,,,"TYPE")
$!
$! Open the batch command file.
$!
$open/write batch_file ccdpackb.com
$!
$! Write the startup commands.
$!
$write batch_file "$!"
$write batch_file "$! CCDPACK batch job procedure."
$write batch_file "$!"
$write batch_file "$! Generated: ''f$time()'."
$write batch_file "$!"
$write batch_file "$! Set up batch job to run CCDPACK, restoring saved environment."
$write batch_file "$!"
$write batch_file "$@CCDPACK_DIR:CCDBSTART ''F$TRNLNM("NEW_ADAM_USER")'" + -
                                           "  ''F$ENVIRONMENT("DEFAULT")'"
$write batch_file "$!"
$!
$! Write invocation appropriate to command type.
$!
$IF TYPE .NES. ".ICL"
$   THEN
$      write batch_file "$! Execute user supplied procedure."
$      write batch_file "$!"
$      write batch_file "$@''P1'"
$   ELSE
$      write batch_file "$! Start up ICL"
$      write batch_file "$!"
$      write batch_file "$ ICL SYS$INPUT"
$      write batch_file "{"
$      write batch_file "{ Start up CCDPACK "
$      write batch_file "{"
$      write batch_file "CCDPACK"
$      write batch_file "{"
$      write batch_file "{ Load user supplied procedure "
$      write batch_file "{"
$      write batch_file "LOAD ''P1'"
$      write batch_file "EXIT"
$   ENDIF
$write batch_file "$!"
$write batch_file "$! Clear up the mess. Delete the saved environment"
$write batch_file "$! (remove the next command to retain the environment)."
$write batch_file "$!"
$write batch_file "$@CCDPACK_DIR:CCDBEND ''F$TRNLNM("NEW_ADAM_USER")' " + -
                  "''F$TRNLNM("NEW_ADAM_USER_TRAIL")'"
$write batch_file "$EXIT"
$!
$write sys$output "  Batch job file created: CCDPACKB.COM"
$!
$close batch_file
$exit
$! $Id$
