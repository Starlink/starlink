      SUBROUTINE CHR_FILL( CVALUE, STRING )
*+
*  Name:
*     CHR_FILL

*  Purpose:
*     Fill a string with a given character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_FILL( CVALUE, STRING )

*  Description:
*     The given character string is filled with the specified character.

*  Arguments:
*     CVALUE = CHARACTER (Given)
*        The character specified to fill the string.
*     STRING = CHARACTER * ( * ) (Returned)
*        The string to be filled.

*  Algorithm:
*     Portable version:
*        Get the declared size of the given string.
*        Fill the whole string with the specified character.
*     VMS-specific version:
*        This routine may be implemented using the VAX VMS Run Time
*        Library routine STR$DUPL_CHAR, it then becomes VAX/VMS 
*        dependent.

*  Authors:
*     ASOC5: Dave Baines (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     DLT: D.L. Terrett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1984 (ASOC5):
*        Original version.
*     1-SEP-1988 (AJC):
*        Use LEN instead of CHR_SIZE.
*        Remove INCLUDE 'SAE_PAR'.
*     22-JAN-1990 (DLT):
*        Add portable version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER CVALUE

*  Arguments Returned:
      CHARACTER * ( * ) STRING

*  Portable version.
*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER SIZE               ! Declared size of given string

*.

*  Get the declared size of given string.
      SIZE = LEN( STRING )

*  Duplicate character into every element in the string.
      DO 10 I = 1, SIZE
         STRING( I : I ) = CVALUE
 10   CONTINUE

*  VMS-specific version. 
*  Local Variables:
*     INTEGER SIZE               ! Declared size of given string

*.

*  Get the declared size of given string.
*     SIZE = LEN( STRING )

*     CALL STR$DUPL_CHAR( STRING, SIZE, %REF( CVALUE ) )

      END
