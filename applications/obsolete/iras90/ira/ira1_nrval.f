      SUBROUTINE IRA1_NRVAL( VAL, NUM, ARRY, NERVAL, STATUS )
*+
*  Name:
*     IRA1_NRVAL

*  Purpose:
*     Find a nearest in an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_NRVAL( VAL, NUM, ARRY, NERVAL, STATUS )

*  Description:
*     This routine search an array find a value among the elements which
*     is nearest to the given value.

*  Arguments:
*     VAL = REAL (Given)
*        The value to which the nearest array element is to be found.
*     NUM = INTEGER (Given)
*        The number of elements in the array.
*     ARRY( NUM ) = REAL (Given)
*        The array whose elements are to be searched.
*     NERVAL = REAL (Returned)
*        The found nearest value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      REAL VAL
      INTEGER NUM
      REAL ARRY( NUM )

*  Arguments Returned:
      REAL NERVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      REAL DIST                  ! Dist between give value and array el.
      INTEGER MINDST             ! Index of the min. distant element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the value is bad, set the status, report and exit.
      IF ( VAL .EQ. VAL__BADR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRA1_NRVAL_ERR1',
     : 'IRA1_NRVAL: Unable to find the nearest datum to a bad value',
     :                 STATUS )
         GOTO 999
      END IF

*  Initialise the distance between given value and array elements.
      DIST = ABS( VAL - ARRY( 1 ) )
      MINDST = 1

*  Search the array to find the index of the min. distance element.
      DO I = 2, NUM
         IF ( ARRY( I ) .NE. VAL__BADR ) THEN
            IF ( DIST .GT. ABS( VAL - ARRY( I ) ) ) THEN
               DIST = ABS( VAL - ARRY( I ) )
               MINDST = I
            END IF
         END IF
      END DO

*  Get the found value.
      NERVAL = ARRY( MINDST )

 999  CONTINUE

      END
