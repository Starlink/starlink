      SUBROUTINE ARY1_SETC( VALUE, TOKEN )
*+
*  Name:
*     ARY1_SETC

*  Purpose:
*     Assign a character value to a message token.


*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_SETC( VALUE, TOKEN )

*  Description:
*     The routine assigns a character value to a message token by
*     calling the routine MSG_SETC. It exists solely to reverse the
*     argument order of MSG_SETC so that mapped character strings may
*     be passed as the VALUE argument on UNIX systems.

*  Arguments:
*     VALUE = CHARACTER * ( * ) (Given)
*        Character value to me assigned.
*     TOKEN = CHARACTER * ( * ) (Given)
*        Message token name.

*  Algorithm:
*     -  Assign the token value.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     20-JAN-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) VALUE
      CHARACTER * ( * ) TOKEN

*.

*  Assign the token value.
      CALL MSG_SETC( TOKEN, VALUE )

      END
