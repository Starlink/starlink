$ VERIFY = F$VERIFY( 0 )
$!+
$! Name:
$!    SPLIT_GENERIC.COM
$!
$! Purpose:
$!    Fragments expanded generic code into separate files.
$!
$! Type of Module:
$!    DCL command procedure.
$!
$! Invocation:
$!    @SPLIT_GENERIC [LIST]
$!
$! Description:
$!    This procedure fragments the generic code listed in a file into
$!    separate files. 
$!
$! Parameters:
$!    LIST
$!       The optional name of the file containing the list of generic
$!       routines to be expanded.  If this parameter is not supplied,
$!       then the file GENERIX.TMP is used.
$!
$! Output:
$!    Fortran modules named as each module found in the expanded generic
$!    code.
$!
$! Prior Requirements:
$!    -  Generic code to be expanded must exist in the current directory.
$!    -  It is assumed that the GENERIC command is available.
$!
$! Authors:
$!    Malcolm J. Currie (STARLINK)
$!    {enter_new_authors_here}
$!
$! History:
$!    1992 May 8 (MJC):
$!       Original version, using a tidied DLT's file-splitting procedure
$!       (BURSTF).
$!    1992 May 25 (MJC):
$!       Added prologue and parameters.
$!    {enter_further_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$    ON ERROR THEN GOTO CLEANUP
$!
$!  Assign default where necessary.
$!
$    IF P1 .EQS. "" THEN $ P1 = "GENERIX.TMP"
$!
$!  Open the file containing the list of generic routines.
$!
$    OPEN/READ GENLIST 'P1'
$!
$!  Different closedown point as the file has been opened.
$!
$    ON ERROR THEN GOTO ENDFILE
$!
$    NEXT_ROUTINE:
$!
$!  Read the next routine filename.
$!
$       READ/END_OF_FILE=ENDFILE GENLIST GENERIC_ROUTINE
$!
$!  Extract the routine name.
$!
$       ROUTINE = F$PARSE( GENERIC_ROUTINE, , , "NAME" )
$!
$!  Split the file into its constituent modules.
$!
$       CALL BURSTF 'ROUTINE'.FOR
$!
$!  Delete the unwanted concatenated file.
$!
$       DELETE 'ROUTINE'.FOR;*
$!
$!  Try to add a further routine.
$!
$       GOTO NEXT_ROUTINE
$!
$!  Close the file when the list of routines is exhausted.
$!
$    ENDFILE:
$    CLOSE GENLIST
$!
$    CLEANUP:
$!
$!  Exit the procedure.
$! 
$    IF ( VERIFY ) THEN SET VERIFY
$!
$    EXIT
$!
$!  BURSTF subroutine
$!  =================
$!
$    BURSTF: SUBROUTINE
$!
$!  Splits a file of FORTRAN modules into separate files with filenames
$!  matching the module name.
$!
$!  The following restrictions apply:
$!
$!     o The module name must be on the same line as the SUBROUTINE etc.
$!       keywords.
$!
$!     o The above keywords must not contain spaces.
$!
$!     o The END statement must appear on a line all by itself (except for
$!       trailing comments.
$!
$!     o END ... statments must not be continued so that END appears on
$!       a line by itself.
$!
$!     o Everything before the first module or between modules is
$!       thrown away.
$!
$!     o Non standard function types (eg. REAL*8) are not recognized
$!
$    P1 = F$PARSE( P1, ".FOR",,, )
$    FILE_EXT = F$PARSE( P1, ".FOR", ,"TYPE", )
$    OPEN/READ INPUT 'P1'
$ READ_LOOP:
$    READ/END=EOF INPUT RECORD
$    TEST = F$EDIT( RECORD, "TRIM,COMPRESS,UPCASE" )
$    IF F$EXTRACT( 0, 10, TEST ) .EQS. "SUBROUTINE" 
$    THEN
$       S = 11 
$       GOTO NEW_MODULE
$    ENDIF
$    IF F$EXTRACT( 0, 8, TEST ) .EQS. "FUNCTION"
$    THEN
$       S = 9
$       GOTO NEW_MODULE
$    ENDIF
$    IF F$EXTRACT( 0, 16, TEST ) .EQS. "INTEGER FUNCTION"
$    THEN
$       S = 17
$       GOTO NEW_MODULE
$    ENDIF
$    IF F$EXTRACT( 0, 16, TEST ) .EQS. "LOGICAL FUNCTION"
$    THEN
$       S = 17
$       GOTO NEW_MODULE
$    ENDIF
$    IF F$EXTRACT( 0, 13, TEST ) .EQS. "REAL FUNCTION"
$    THEN
$       S = 14
$       GOTO NEW_MODULE
$    ENDIF
$    IF F$EXTRACT( 0, 25, TEST ) .EQS. "DOUBLE PRECISION FUNCTION"
$    THEN
$       S = 26
$       GOTO NEW_MODULE
$    ENDIF
$    IF F$EXTRACT( 0, 10, TEST) .EQS. "BLOCK DATA"
$    THEN
$       S = 11
$       GOTO NEW_MODULE
$    ENDIF
$    GOTO READ_LOOP
$ NEW_MODULE:
$    E = F$LOCATE( "(", TEST )
$    MODULE = F$EXTRACT( S, E-S, TEST ) + FILE_EXT
$    MODULE = F$EDIT( MODULE, "COLLAPSE" )
$    OPEN/WRITE OUTPUT 'MODULE' 
$    WRITE OUTPUT RECORD
$ NEXT_REC:
$    READ INPUT RECORD
$    WRITE OUTPUT RECORD
$    IF F$EDIT( RECORD, "COLLAPSE,UPCASE,UNCOMMENT") .NES. "END" -
                                        THEN $GOTO NEXT_REC
$    CLOSE OUTPUT
$    GOTO READ_LOOP
$ EOF:
$    CLOSE INPUT
$    EXIT
