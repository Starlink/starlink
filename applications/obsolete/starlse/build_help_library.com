$!+
$!  Name:
$!     BUILD_HELP_LIBRARY.COM
$!
$!  Purpose:
$!     Build an internal help library as part of a STARLSE release.
$!
$!  Type of Module.
$!     DCL command procedure.
$!
$!  Invocation:
$!     @BUILD_HELP_LIBRARY <source_file>
$!
$!  Description:
$!     This procedure builds a help library from a file containing the help
$!     source.
$!
$!  Parameters:
$!     <source_file>
$!        The name of the file containing the help source for the library.
$!
$!  Authors:
$!     RFWS: R.F. Warren-Smith (STARLINK)
$!
$!  History:
$!     7-JUN-1990 (RFWS):
$!        Original version.
$!     {enter_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!  Ensure that the name of the source file is known.
$      IF ( P1 .EQS. "" ) THEN INQUIRE P1 "Source file for help information?"
$!
$!  Extract the file name field.
$      NAME = F$PARSE( P1, , , "NAME" )
$!
$!  Create a help library with an appropriate name.
$      LIBRARY/CREATE/HELP 'NAME'.HLB
$!
$!  Insert the help information.
$      LIBRARY/HELP 'NAME'.HLB 'P1'
$!
$!  Compress the library.
$      LIBRARY/COMPRESS/HELP 'NAME'.HLB
$      PURGE 'NAME'.HLB
$      SET FILE/TRUNCATE 'NAME'.HLB
$!
$!  Exit procedure.
$      EXIT
