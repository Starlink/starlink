      SUBROUTINE MSG1_KTOK
*+
*  Name:
*     MSG1_KTOK

*  Purpose:
*     Clear the message token table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_KTOK

*  Description:
*     Clear all the message tokens at the current context level.

*  Arguments:
*     None

*  Algorithm:
*     Call EMS_EXPND with bad status - just kills tokens

*  Authors:
*     AJC: A.J.Chipperfield  (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     22-FEB-2001 (AJC):
*        Original version - to avoid use of EMS internal EMS1_KTOK
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER STRLEN             ! Dummy string length

      CHARACTER * 2 STRING       ! Dummy string

*.

*  Set bad status and call EMS_EXPND
      ISTAT = SAI__ERROR
      CALL EMS_EXPND( ' ', STRING, STRLEN, ISTAT ) 

      END
