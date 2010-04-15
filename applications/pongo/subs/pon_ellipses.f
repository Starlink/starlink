      SUBROUTINE PON_ELLIPSES( DRAWAX, X, Y, CORR, SX, SY, NDAT, SIZE,
     :                         STATUS )
*+
*  Name:
*     PON_ELLIPSES

*  Purpose:
*     Draw error ellipses.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_ELLIPSES( DRAWAX, X, Y, CORR, SX, SY, NDAT, SIZE,
*    :                   STATUS )

*  Description:
*     Draw a series of error ellipses at the positions (X,Y) with
*     errors (SX,SY) and covariances CORR. The confidence level of the
*     ellipse can be determined by setting the value of SIZE suitably,
*     some examples are listed below:
*
*        SIZE       Confidence level
*        1.00            46%
*        2.30            68.3%
*        4.61            90%
*        9.21            99%

*  Arguments:
*     DRAWAX = LOGICAL (Given)
*        If TRUE, the principal axes of the ellipses will be drawn.
*     X( NDAT ) = DOUBLE PRECISION (Given)
*        X co-ordinates of the centres of the ellipses.
*     Y( NDAT ) = DOUBLE PRECISION (Given)
*        Y co-ordinates of the centres of the ellipses.
*     CORR( NDAT ) = REAL (Given)
*        Covariances of the errors.
*     SX( NDAT ) = REAL (Given)
*        Standard error of X.
*     SY( NDAT ) = REAL (Given)
*        Standard error of Y.
*     NDAT = INTEGER (Given)
*        Number of data points passed.
*     SIZE = REAL (Given)
*        Square of scaling factor for the error ellipse (see
*        the description of routine for full explanation).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-APR-1990 (JBVAD::PAH):
*        Original version (from the stand-alone PONGO).
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'    ! Standard SAE constants
      INCLUDE 'PONGO_PAR'  ! PONGO global constants

*  Arguments Given:
      LOGICAL DRAWAX

      INTEGER NDAT

      DOUBLE PRECISION X( NDAT )
      DOUBLE PRECISION Y( NDAT )

      REAL CORR( NDAT )
      REAL SX( NDAT )
      REAL SY( NDAT )
      REAL SIZE

*  Status:
      INTEGER STATUS

*  External References:
      REAL PON_EL1

*  Local Variables:
      INTEGER I ! Loop index

      REAL A ! Semi-major axis
      REAL B ! Semi-minor axis
      REAL DR
      REAL PSI
      REAL R
      REAL T

*.

*  Check the inherited status.
      IF ( STATUS.NE.SAI__OK ) RETURN

      DO I = 1, NDAT

*     Find the orientation of ellipse.
         IF ( SX( I ).NE.SY( I ) ) THEN
            PSI = ATAN( 2.0*CORR( I )*SX( I )*SY( I )/
     :                  ( SX( I )**2 - SY( I )**2 ) )/2.0
         ELSE
            PSI = SPI/4.0
         END IF

*     Find semi major and minor axes.
         A = PON_EL1( PSI, CORR( I ), SX( I ), SY( I ), SIZE )
         B = PON_EL1( PSI+SPI/2, CORR( I ), SX( I ), SY( I ), SIZE )

*     Draw the axes if necessary.
         IF ( DRAWAX ) THEN
            CALL PGMOVE( REAL( X( I ) )+A*COS( PSI ),
     :                   REAL( Y( I ) )+A*SIN( PSI ) )
            CALL PGDRAW( REAL( X( I ) )+A*COS( PSI+SPI ),
     :                   REAL( Y( I ) )+A*SIN( PSI+SPI ) )
            CALL PGMOVE( REAL( X( I ) )+B*COS( PSI+SPI/2.0 ),
     :                   REAL( Y( I ) )+B*SIN( PSI+SPI/2.0 ) )
            CALL PGDRAW( REAL( X( I ) )+B*COS( PSI+SPI/2.0+SPI ),
     :                   REAL( Y( I ) )+B*SIN( PSI+SPI/2.0+SPI ) )
         END IF

*     Find the desired circumferential step.
         DR = SPI*( A+B )/100.0
         T = 0

*     Now draw the ellipse.
         R = PON_EL1( 0.0, CORR( I ), SX( I ), SY( I ), SIZE )
         CALL PGMOVE( REAL( X( I ) )+R, REAL( Y( I ) ) )

         DO WHILE ( T.LE.S2PI )
            T = T + DR/R
            R = PON_EL1( T, CORR( I ), SX( I ), SY( I ), SIZE )
            CALL PGDRAW( REAL( X( I ) )+R*COS( T ),
     :                   REAL( Y( I ) )+R*SIN( T ) )
         END DO
      END DO

      END
* $Id$
