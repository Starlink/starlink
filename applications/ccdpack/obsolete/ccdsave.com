$!+
$!  Name:
$!     CCDSAVE.COM
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Purpose:
$!     Save an ADAM execution environment.
$!
$!  Description:
$!     This procedure save a current ADAM execution environment, saving
$!     the global and "current" parameter values from the present
$!     environment. It does this by copying the contents of the
$!     current ADAM_USER directory to a new directory and defining
$!     the logical name ADAM_SAVED to refer to the new directory. The
$!     new directory is created as a sub-directory of SYS$SCRATCH and
$!     is named either using parameter P1 or a random name based on
$!     the date..
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     28-MAY-1991 (PDRAPER):
$!        HEAVILY Based on a command procedure ADAMFORK written by
$!        Rodney Warren-Smith.
$!     20-SEP-1991 (PDRAPER):
$!        Removed call to ADAMFORK_COPY replaced it with DCL command
$!        CONVERT/SHARE.
$!     8-OCT-1992 (PDRAPER):
$!        Added set file/end_of_file fix for device cluster size changes.
$!     {enter_further_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!  Obtain the initial value of the ADAM_USER logical name.
$      OLDUSER = F$TRNLNM( "ADAM_USER" )
$!
$!  Obtain the value of SYS$SCRATCH.
$      SCRATCH = F$TRNLNM( "SYS$SCRATCH" )
$!
$!  Check P1 if it is set then create a directory sys$scratch.p2.
$      IF P1 .NES. ""
$      THEN
$          TRAIL = P1
$      ELSE
$!  Create an random name.
$          K = F$TIME()
$          TRAIL = F$EXTRACT(F$LOCATE(":",K)-3,F$LENGTH(K),K) -":"-":"-"."
$          TRAIL = "ADAM_" + F$EDIT(TRAIL, "COLLAPSE" )
$      ENDIF
$!
$!  Create the new directory name.
$      NEWUSER = SCRATCH - "]" + "." + TRAIL +"]"
$!
$!  Inform user of progress.
$!
$      WRITE SYS$OUTPUT -
       "  Saving current ADAM context in directory ''NEWUSER'."
$!
$!
$!  See if the required new directory file already exists in
$!  SYS$SCRATCH. If not, then create it.
$      IF ( F$SEARCH( SCRATCH+"''TRAIL'.DIR" ) .EQS. "" )
$      THEN
$         CREATE/DIRECTORY 'NEWUSER'
$!
$!  If it does exist, then see if it contains any files. If so, delete
$!  them.
$      ELSE
$         IF ( F$SEARCH( NEWUSER + "*.*;*" ) .NES. "" ) THEN -
             DELETE 'NEWUSER'*.*;*
$      ENDIF
$!
$!  Obtain the current SET MESSAGE state.
$      MESSTATE = F$ENVIRONMENT( "MESSAGE" )
$!
$!  Create a new copy of the SYS$SCRATCH:ICL.SDF file to avoid access
$!  conflicts with other jobs which may already be running and using the
$!  existing file. Use the CONVERT/SHARE command to do this, as it
$!  allows the file to be copied even if it is currently in use.
$      IF ( F$SEARCH( "SYS$SCRATCH:ICL.SDF" ) .NES. "" )
$      THEN
$         CONVERT/SHARE SYS$SCRATCH:ICL.SDF SYS$SCRATCH:ICL.SDF
$      ENDIF
$!
$!  Loop to copy all the .SDF files in the old ADAM_USER directory to
$!  the new directory. Find the name of each file to be copied and
$!  quit the loop when there are no more files.
$LOOP:
$      FILE = F$SEARCH( "''OLDUSER'*.SDF" )
$      IF ( FILE .EQS. "" ) THEN GOTO ENDLOOP
$!
$!  Extract the file name and type fields.
$      FILE = F$PARSE( FILE,,, "NAME" ) + F$PARSE( FILE,,, "TYPE" )
$!
$!  Attempt to COPY the file, suppressing error messages. If successful,
$!  restore the previous message state and return to find the next file.
$      ON ERROR THEN GOTO HDSCOPY
$      SET MESSAGE/NOFACILITY/NOIDENTIFICATION/NOSEVERITY/NOTEXT
$      COPY 'OLDUSER''FILE' 'NEWUSER''FILE'
$      SET MESSAGE 'MESSTATE'
$      ON ERROR THEN EXIT
$      GOTO LOOP
$!
$!  If COPY fails, restore the previous message state. Then try the
$!  CONVERT/SHARE command instead, as the file is probably in use.
$HDSCOPY:
$      SET MESSAGE 'MESSTATE'
$      ON ERROR THEN EXIT
$      CONVERT/SHARE 'OLDUSER''FILE' 'NEWUSER''FILE'
$      GOTO LOOP
$ENDLOOP:
$!
$!  Show which directory is has been written to
$!  logical name.
$      WRITE SYS$OUTPUT -
       "  Context saved."
$      DEFINE/JOB/NOLOG  NEW_ADAM_USER "''NEWUSER'"
$      DEFINE/JOB/NOLOG  NEW_ADAM_USER_TRAIL "''TRAIL'"
$!
$! Apply the fix to change the file size to a multiple of 
$! target device the cluster size
$!
$     SET FILE/END_OF_FILE 'NEWUSER'*.SDF
$!
$!  Exit.
$EXIT:
$! $Id$
