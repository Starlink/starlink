$ VERIFY = F$VERIFY( 0 )
$!+
$! Name:
$!    GENERIC_EXPAND
$!
$! Purpose:
$!    Expands a list of Generic files into Fortran.
$!
$! Language:
$!    DCL
$!
$! Type of Module:
$!    Command procedure.
$!
$! Invocation:
$!    @GENERIC_EXPAND EXPANSION [LIST]
$!
$! Description:
$!    This procedure expands the generic routines from a list
$!    provided in a file.  The file should contain one routine name
$!    per line.  A lookup table of expansion data types is given in
$!    another file.  If the generic routine is not listed in the
$!    lookup table, the type NUMERIC is assumed.
$!
$! Parameters:
$!    EXPANSION
$!       The file containing the list of expansions for the generic
$!       routines.  If the parameter is not given the file
$!       CONVERT_DIR:CON_GENERIX.LIS is used.  The file has one
$!       line per routine formatted as follows:
$!          Routine_name  :types
$!    LIST
$!       The optional name of the file containing the list of generic
$!       routines to be expanded.  If this parameter is not supplied,
$!       then the file GENERIX.TMP is used.
$!
$! Output:
$!    -  The expanded source.
$!
$! Prior Requirements:
$!    -  Generic code to be expanded must exist in the current directory.
$!    -  It is assumed that the GENERIC command is available.
$!
$! Authors:
$!    MJC: Malcolm J. Currie (STARLINK)
$!    {enter_new_authors_here}
$!
$! History:
$!    1992 February 20 (MJC):
$!       Original version.
$!    1992 May 25 (MJC):
$!       Added prologue and parameters.
$!    {enter_further_changes_here}
$!
$! Bugs:
$!    {note_any_bugs_here}
$!
$!-
$!
$    ON ERROR THEN GOTO CLEANUP
$!
$!  Assign defaults where necessary.
$!
$    IF P1 .EQS. "" THEN $ P1 = "CONVERT_DIR:CON_GENERIX.LIS"
$    IF P2 .EQS. "" THEN $ P2 = "GENERIX.TMP"
$!
$!  Open the file containing the list of generic routines.
$!
$    OPEN/READ GENLIST 'P2'
$!
$!  Different closedown point as the files have been opened.
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
$!  Initialise the search flag.
$!
$       FOUND_GEN = 0
$!
$!  Find the supported types for the generic routine.
$!
$       STYPE = "NUMERIC"
$!
$!  Open the file containing the generic data types for the generic 
$!  routines.
$!
$       OPEN/READ GENERIX 'P1'
$!
$!  Read the file of generic routines and their allowed types.
$!
$       ROUTINE_LOOP:
$          READ/END_OF_FILE=ENDLOOP GENERIX GENERIC_TYPE
$          FPOS = F$LOCATE( ROUTINE, GENERIC_TYPE )
$          IF FPOS .EQ. F$LENGTH( GENERIC_TYPE ) THEN GOTO ROUTINE_LOOP
$!
$!  Record that the search has been successful.
$!
$          FOUND_GEN = 1
$!
$!  The routine has been located in the list.  Extract its type
$!  specification.  CON_GENERIX.LIS has one line per routine formatted
$!  as follows:
$!     Routine_name  :types
$!
$          STYPE = F$ELEMENT( 1, ":", GENERIC_TYPE )
$       ENDLOOP:
$       CLOSE GENERIX
$!
$!  Report the error.
$!
$       IF FOUND_GEN .EQ. 0
$       THEN
$          WRITE SYS$OUTPUT "''GENERIC_ROUTINE' does have not an entry in ''P1'. Using NUMERIC."
$       ENDIF
$!
$!  Make the source file containing the appropriate data types.
$!
$       GENERIC 'GENERIC_ROUTINE' /TYPES=('STYPE') /NOCOMPILE
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
