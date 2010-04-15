      SUBROUTINE IRM_SQFLT( NSMP, INDAT, OUTDAT, STATUS )
*+
*  Name:
*     IRM_SQFLT

*  Purpose:
*     Square wave filter a data series.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_SQFLT( NSMP, INDAT, OUTDAT, STATUS )

*  Description:
*     This subroutine filters an input data series with an eight-point
*     zero-sum square-wave filter which is defined as:
*
*     Y(i) = - X(i) - X(i+1) + X(i+2) + X(i+3)
*            + X(i+4) + X(i+5) - X(i+6) - X(i+7)
*
*     All samples in the input series should be valid..

*  Arguments:
*     NSMP = INTEGER (Given)
*        Number of samples in the data series.
*     INDAT( NSMP ) = REAL (Given)
*        Input data series.
*     OUTDAT( NSMP ) = REAL(Returned)
*        Filtered output series.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1993 (WG):
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
      INTEGER NSMP
      REAL INDAT( NSMP )

*  Arguments Returned:
      REAL OUTDAT( NSMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop indices

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The input data series should have at least 8 samples.
      IF ( NSMP .LT. 8 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_SQFLT_ERR1', 'IRM_SQFLT: Input data series'/
     :               /' does not have enough samples', STATUS )
         GOTO 999
      END IF

*  Get the first filtered data sample.
      OUTDAT( 1 ) = - INDAT( 1 ) - INDAT( 2 )
     :              + INDAT( 3 ) + INDAT( 4 ) + INDAT( 5 ) + INDAT( 6 )
     :              - INDAT( 7 ) - INDAT( 8 )

*  Calculate the following filtered data sample recursively.
      DO I = 1, NSMP - 8
         OUTDAT( I + 1 ) = OUTDAT( I ) + INDAT( I ) -
     :                     2.0 * INDAT( I + 2 ) + 2.0 * INDAT( I + 6 ) -
     :                     INDAT( I + 8 )
      END DO

*  The final 7 samples are non-filterable, assign them the last
*  filtered sample value.
      DO I = NSMP - 6, NSMP
         OUTDAT( I ) = OUTDAT( I - 1 )
      END DO

 999  CONTINUE

      END
