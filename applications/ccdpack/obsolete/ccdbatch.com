$!+
$!  Name:
$!     CCDBATCH

$!  Purpose:
$!     Prepares a CCDPACK command file for submission to batch.

$!  Language:
$!     DCL

$!  Type of Module:
$!     DCL command precedure

$!  Notes:
$!     - VAX specific.

$!  Invocation:
$!     @CCDBATCH command_procedure [subdirectory]

$!  Description:
$!     This routine saves the current ADAM execution environment
$!     by taking a copy of the SDF files in the ADAM_USER directory. It
$!     stores them in a directory which it creates as a subdirectory of
$!     SYS$SCRATCH, or writes them to a subdirectory of SYS$SCRATCH
$!     given as the parameter P2. It then writes a (DCL) command
$!     procedure CCDPACKB which contains commands for restoring the
$!     saved environment, executing the supplied command file (which 
$!     can be written as if using DCL or ICL) and removing the saved
$!     environment. CCDPACKB is suitable for direct submission to batch.


$!  Usage:
$!     CCDBATCH command_procedure [subdirectory]

$!  ADAM Parameters:
$!     P1 = STRING (Read)
$!        The name of the CCDPACK command file which is to be executed
$!        in batch. If no type is specified then .COM takes precedence
$!        over .ICL.
$!     P2 = STRING (Read)
$!        The name of the subdirectory of SYS$SCRATCH in which to place
$!        the copy of the current environment. If this value is not
$!        supplied then CCDSAVE generates a unique value and uses this.
$!        The created directory is assigned to the logical name
$!        NEW_ADAM_USER.
$!     OUTPUT = CCDPACKB.COM (File - Write)
$!        Output command procedure which contains the commands necessary
$!        to execute the command procedure (P1) in batch.

$!  Examples:
$!      CCDBATCH CCDPACK_BATCH
$!        In this example CCDBATCH saves the current ADAM parameter
$!        environment and writes commands which will enable the
$!        command file CCDPACK_BATCH to execute CCDPACK commands in batch.
$!        The type (DCL or ICL) of the procedure file CCDPACK_BATCH is
$!        determined before submission so that suitable initialisation
$!        commands are written.
$!
$!      CCDBATCH CCDPACK_BATCH.ICL  ADAM_BATCH
$!        In this example the current ADAM context is written to the
$!        sub-directory ADAM_BATCH of SYS$SCRATCH. A command procedure
$!        CCDPACKB.COM is produced suitable to execute the ICL procedure
$!        CCDPACK_BATCH.ICL.

$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     RFWS: Rodney Warren-Smith (STARLINK)
$!     {enter_new_authors_here}

$!  History:
$!     24-SEP-1991 (PDRAPER):
$!        Original version.
$!     21-NOV-1991 (PDRAPER+RFWS):
$!        Major changes from original.
$!     {enter_further_changes_here}

$!  Bugs:
$!     {note_any_bugs_here}
$!

$!-
$!
$! See if P1 has been defined.
$!
$IF P1.EQS."" THEN INQUIRE P1 "Name of command procedure"
$IF P1.EQS."" THEN EXIT
$!
$! Check that input file exists.
$FILENAME1=F$PARSE("''P1'",".COM",,,"SYNTAX_ONLY")
$NEWNAME = F$SEARCH("''FILENAME1'")
$IF NEWNAME.EQS.""
$  THEN
$!
$! Look for file with ICL type.
$!
$    FILENAME=F$PARSE("''P1'",".ICL",,,"SYNTAX_ONLY")
$    NEWNAME = F$SEARCH("''FILENAME'")
$    IF NEWNAME.EQS.""
$       THEN
$       WRITE SYS$OUTPUT "File ''FILENAME1' does not exist."
$    ENDIF
$    EXIT
$  ENDIF
$!
$! Save the current environment.
$!
$@CCDPACK_DIR:CCDSAVE 'P2'
$!
$! Write initialising log file.
$!
$@CCDPACK_DIR:CCDBATFIL 'NEWNAME'
$!
$exit
$! $Id$
