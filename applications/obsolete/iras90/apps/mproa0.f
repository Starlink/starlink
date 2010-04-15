      SUBROUTINE MPROA0( N1, N2, DATIN, AX1IN, AX2IN, DATOUT, AX1OUT,
     :                   AX2OUT, STATUS )
*+
*  Name:
*     MPROA0

*  Purpose:
*     Store in-scan detector response data in the output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MPROA0( N1, N2, DATIN, AX1IN, AX2IN, DATOUT, AX1OUT, AX2OUT,
*                  STATUS )

*  Description:
*     The supplied data is stored in the output arrays.

*  Arguments:
*     N1 = INTEGER (Given)
*        The size of the first axis.
*     N2 = INTEGER (Given)
*        The size of the second axis.
*     DATIN( N1, N2 ) = REAL (Given)
*        The input data.
*     AX1IN( N1 ) = REAL (Given)
*        The axis data for axis 1.
*     AX2IN( N2 ) = INTEGER (Given)
*        The axis data for axis 2.
*     DATOUT( N1, N2 ) = REAL (Returned)
*        The output data.
*     AX1OUT( N1 ) = REAL (Returned)
*        The axis data for axis 1.
*     AX2OUT( N2 ) = REAL (Returned)
*        The axis data for axis 2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N1
      INTEGER N2
      REAL DATIN( N1, N2 )
      REAL AX1IN( N1 )
      INTEGER AX2IN( N2 )

*  Arguments Returned:
      REAL DATOUT( N1, N2 )
      REAL AX1OUT( N1 )
      REAL AX2OUT( N2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER J                  ! Loop count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the first axis data.
      DO I = 1, N1
         AX1OUT( I ) = AX1IN( I )
      END DO

*  Copy the rest of the data.
      DO J = 1, N2
         AX2OUT( J ) = REAL( AX2IN( J ) )

         DO I = 1, N1
            DATOUT( I, J ) = DATIN( I, J )
         END DO

      END DO

      END
