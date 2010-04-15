      SUBROUTINE IRM1_SETIP( EL, DATA, IP, NGOOD, STATUS )
*+
*  Name:
*     IRM1_SETIP

*  Purpose:
*     Set up an array of pixel pointers for IRM_MEDN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM1_SETIP( EL, DATA, IP, NGOOD, STATUS )

*  Description:
*     Each element in the input data array is checked in turn. If it is
*     valid, then its corresponding index into the data array is added
*     to the end of the list of index values in the array IP. If the
*     data value is bad its index is not added into IP.

*  Arguments:
*     EL = INTEGER (Given)
*        The size of the data array.
*     DATA( EL ) = REAL (Given)
*        The data array.
*     IP( EL ) = INTEGER (Returned)
*        The array of pixel indices.
*     NGOOD = INTEGER (Returned)
*        The total number of good pixels whose indices are stored in IP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      INTEGER EL
      REAL DATA( EL)

*  Arguments Returned:
      INTEGER IP( EL )
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of good pixels to zero.
      NGOOD = 0

*  Loop round the input data array.
      DO I = 1, EL

*  If it is a good pixel, increment the number of good pixels and add
*  its index into the array of indices.
         IF( DATA( I ) .NE. VAL__BADR ) THEN
            NGOOD = NGOOD + 1
            IP( NGOOD ) = I
         END IF

      END DO

      END
