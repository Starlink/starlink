      SUBROUTINE ICL_WRITEA( STRING )
*+
*  Name:
*     ICL_WRITEA

*  Purpose:
*     UNIX replacement for original routine to write a line to the
*     user interface splitting it into multiple lines of 80 characters
*     if necessary.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ICL_WRITEA( STRING )

*  Description:
*     The used length of the string is found. If it is zero a length
*     of 1 is assumed. The string is then printed LSIZE characters
*     per line.

*  Arguments:
*     STRING = CHARACTER*(*) (Given)
*        The string to be printed

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-1991 (AJC):
*        Original version.
*     14-DEC-1995 (AJC):
*        Remove spurious comma for Linux
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER*(*) STRING

*  External References:
      INTEGER CHR_LEN            ! Used length of string

*  Local Constants:
      INTEGER LSIZE              ! Line length
      PARAMETER ( LSIZE = 80 )

*  Local Variables:
      INTEGER LENGTH             ! Total message length
      INTEGER I                  ! Loop counter
*.


*  Get the used length of the string, or 1 if the string is blank
      LENGTH = MAX( CHR_LEN( STRING ), 1 )

*  Now print the string LSIZE characters at a time
      DO 10, I = 1, LENGTH, LSIZE
         WRITE( *, '(A)') STRING(I:MIN(I+LSIZE,LENGTH))
10    CONTINUE

      END
