      SUBROUTINE SHL_TRNVAR( LIBNAM, ISENV, LIBRAY, STATUS )
*+
*  Name:
*     SHL_ADAM

*  Purpose:
*     Gives help about specified application

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL SHL_ADAM( LIBNAM, STATUS )

*  Arguments:
*     LIBNAM = CHARACTER (Given)
*        Name of environment variable to use to obtain the location
*        of the required help library. _HELP is appended to the library
*        name, if not present, before translating environment variable.
*        Must translate to an actual help library file (the .shl
*        extension is optional).
*     ISENV = LOGICAL (Given)
*        Indicates whether LIBNAM refers to an environment variable
*        that must be translated to obtain the actual name of the
*        help file (.TRUE.) or an actual help file with path. (.FALSE.)
*     LIBRAY = CHARACTER (Returned)
*        Name of the translated file name, including the .shl. Must
*        be large enough to receive the string. Bad status is set
*        if the string is truncated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Translates the supplied argument to a help library file.
*     If ISENV is true, the argument can either be the name of an
*     environment variable that translates to a help file, or the
*     root of an environment variable requiring a _HELP suffix.

*     IF ISENV is false, it is assumed to be the name of the help
*     file with or without the .shl extension.

*     Either way, a .shl is added to the help file name if none is present.


*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28 July 2004 (TIMJ):
*        Refactored from shl_adam

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions

*  Arguments Given:
      CHARACTER * (*) LIBNAM   ! Name of help environment variable
      LOGICAL ISENV            ! Is LIBNAM a file or an env var?

*  Arguments Returned:
      CHARACTER * (*)  LIBRAY ! Name of help file

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER
     :  CHR_LEN                ! Length of character strings ignoring
                               ! trailing blanks

*  Local variables:
      CHARACTER * 256 PATH     ! Library path (without extension)
      CHARACTER *  80 ENVVAR   ! Name of environment variable to read
      INTEGER NC               ! Number of characters in the help string
      INTEGER IPOSN            ! Position in string

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the environment variable if we have one
      IF ( ISENV ) THEN

*  Copy environment variable name into local storage
*  since we may need to modify it
         ENVVAR = LIBNAM

*  See if the envvar name include _HELP., appending if necessary
*  Start searching from the end of the string
         IPOSN = CHR_LEN( ENVVAR )
         CALL CHR_FIND( ENVVAR, '_HELP', .FALSE., IPOSN )
         IF (IPOSN .EQ. 0 ) THEN
*  Not found
            IPOSN = CHR_LEN( ENVVAR )
            CALL CHR_APPND( '_HELP', ENVVAR, IPOSN )
         END IF

*  Translate the environment variable/logical name.
         CALL PSX_GETENV( ENVVAR, PATH, STATUS )
      ELSE

*     Assume it is a help file directly
         PATH = LIBNAM

      END IF

*  Now we need to form the library name by making sure there is a .shl
*  on the end
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Form the full library name. Appending .shl if not present already
         NC = CHR_LEN( PATH )
         LIBRAY = PATH( :NC )

*  Do we have .shl extension? Append one if not.
*  Start searching from the end of the string backwards
         IPOSN = NC
         CALL CHR_FIND( PATH, '.shl', .FALSE., IPOSN)
         IF (IPOSN .EQ. 0) THEN
*     Not found
            CALL CHR_APPND( '.shl', LIBRAY, NC )
         END IF

      END IF

      END

