      SUBROUTINE SUBPAR_BASCOPY ( INSIZE, INVEC, OUTVEC, STATUS )
*+
*  Name:
*     SUBPAR_BASCOPY

*  Purpose:
*     Copies from one byte vector to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_BASCOPY ( INSIZE, INVEC, OUTVEC, STATUS )

*  Description:
*     Copies the contents of a byte vector.

*  Arguments:
*     INSIZE=INTEGER (given)
*        number of elements to be copied
*     INVEC=BYTE(*) (given)
*        input vector
*     OUTVEC=BYTE(*) (returned)
*        output vector
*     STATUS=INTEGER

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1984 (BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER INSIZE

      BYTE INVEC(*)


*  Arguments Returned:
      BYTE OUTVEC(*)


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER J


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      DO J = 1, INSIZE
         OUTVEC(J) = INVEC(J)
      ENDDO

      END
