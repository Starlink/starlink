      SUBROUTINE POINC1( NSMP, PROF, SPSQ, SIP, SP, SISQ, SI, S1, V,
     :                   STATUS )
*+
*  Name:
*     POINC1

*  Purpose:
*     Calculate correlation constants from ideal point source profile

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINC1( NSMP, PROF, SPSQ, SIP, SP, SISQ, SI, S1, V, STATUS )

*  Description:
*     This subroutine is used to calculate the ideal point source profile
*     correalation coefficients later used in correlating real data segment
*     with the idealised point source template.

*  Arguments:
*     NSMP = INTEGER (Given)
*        Number of samples in the given point source profile.
*     PROF( NSMP ) = REAL (Given)
*        Point source profile.
*     SPSQ = DOUBLE PRECISION (Returned)
*        Sum of squared profile samples.
*     SIP = DOUBLE PRECISION (Returned)
*        Sum of sample indices times  profile samples.
*     SP = DOUBLE PRECISION (Returned)
*        Sum of profile samples.
*     SISQ =DOUBLE PRECISION (Returned)
*        Sum of squared sample indices.
*     SI = DOUBLE PRECISION (Returned)
*        Sum of sample indices.
*     S1 =DOUBLE PRECISION (Returned)
*        Sum of constant 1.
*     V = DOUBLE PRECISION (Returned)
*        Determinant of the symmatric matrix formed by SPSQ, SIP, SP,
*        SISQ, SI, S1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     4-MAR-1993 (WG):
*        Original version.
*     6-OCT-1994 (DCP):
*        Incorperated as is in new version of pointcrdd
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NSMP
      REAL PROF( NSMP )

*  Arguments Returned:
      DOUBLE PRECISION SPSQ, SIP, SP, SISQ, SI, S1, V

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Sample index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the sums.
      SPSQ = 0.0D0
      SIP = 0.0D0
      SP = 0.0D0
      SISQ = DBLE( NSMP * ( NSMP + 1 ) * ( 2 * NSMP + 1 ) ) / 6.0D0
      SI = DBLE( NSMP * ( NSMP + 1 ) ) / 2.0D0
      S1 = DBLE( NSMP )

*  Calculate sums.
      DO I = 1, NSMP
         SPSQ = SPSQ + DBLE( PROF( I ) * PROF( I ) )
         SIP = SIP + DBLE( REAL( I ) * PROF( I ) )
         SP = SP + DBLE( PROF( I ) )
      END DO

*  Calculate the determinant of the symmatric matrixs formed by SPSQ,
*  SIP, SP, SISQ, SI, S1.
      V = SPSQ * SISQ * S1 + 2.0D0 * SIP * SI * SP
      V = V - SP * SP * SISQ - SI * SI * SPSQ - SIP * SIP * S1

      END
