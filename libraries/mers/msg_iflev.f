      SUBROUTINE MSG_IFLEV( FILTER )
*+
*  Name:
*     MSG_IFLEV

*  Purpose:
*     Return the current filter level for conditional message output.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_IFLEV( FILTER )

*  Description:
*     The value of the current filtering level set for conditional
*     message output is returned.

*  Arguments:
*     FILTER = INTEGER (Returned)
*        The current message filtering level.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'MSG_CMN'          ! Message output filter level

*  Arguments Returned:
      INTEGER FILTER

*.

*  Return the current value of the message output filter level.
      FILTER = MSGINF

      END
