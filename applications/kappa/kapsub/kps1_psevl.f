      SUBROUTINE KPS1_PSEVL( AXISR, THETA, FWHM, GAMMA, LBND1, UBND1,
     :                       LBND2, UBND2, PX, PY, ARRAY, STATUS )
*+
*  Name:
*     KPS1_PSEVL

*  Purpose:
*     Evaluates a model 2-d point-spread function over an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PSEVL( AXISR, THETA, FWHM, GAMMA, LBND1, UBND1,
*                      LBND2, UBND2, PX, PY, ARRAY, STATUS )

*  Description:
*     This routine evaluates within an array a point-spread function
*     of the following radial form:
*        D =  A exp(-0.5 * (r/sigma) ** gamma )
*     where r is calculated from the true radial distance from the star
*     centre allowing for image ellipticity, sigma is the Gaussian
*     precision constant or profile width.  The point-spread function
*     may be oriented at an arbitrary angle.  The centre of the
*     point-spread function is at the supplied psoition (PX,PY).

*  Arguments:
*     AXISR = REAL (Given)
*        The axis ratio of the point-spread function.
*     THETA = REAL (Given)
*        The orientation of the major axis of the point-spread function
*        to the x axis in radians (x through y positive).
*     FWHM = REAL (Given)
*        The full width at half maximum of the point-spread function in
*        the minor-axis direction.
*     GAMMA = REAL (Given)
*        The exponent in the radial point-spread function.
*     LBND1 = INTEGER (Given)
*        The lower bound on the first pixel axis.
*     UBND1 = INTEGER (Given)
*        The upper bound on the first pixel axis. 
*     LBND2 = INTEGER (Given)
*        The lower bound on the second pixel axis.
*     UBND2 = INTEGER (Given)
*        The upper bound on the second pixel axis.
*     PX = REAL (Given)
*        The X pixel co-ordinate at the centre of the model PSF.
*     PY = REAL (Given)
*        The Y pixel co-ordinate at the centre of the model PSF.
*     ARRAY( LBND1 : UBND1, LBND2 : UBND2 ) = REAL (Returned)
*        The point spread function evaluated at each element.
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 9 (MJC):
*        Original version.
*     1995 April 5 (MJC):
*        Used the modern style of variable declaration and comment
*        indentation.
*     21-SEP-1999 (DSB):
*        Re-written to allow for arbitrary pixel origin, and PSF centre.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL AXISR
      REAL FWHM
      REAL GAMMA
      REAL THETA
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      REAL PX
      REAL PY

*  Arguments Returned:
      REAL ARRAY( LBND1:UBND1, LBND2:UBND2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counters
      INTEGER J                  ! Loop counters
      REAL MAJOR                 ! Displacement of the pixel from
                                 ! the centre of the PSF along the
                                 ! major axis
      REAL MINOR                 ! Displacement of the pixel from
                                 ! the centre of the PSF along the
                                 ! minor axis
      REAL PHI                   ! Angle of pixel with respect to the
                                 ! centre of the PSF and its major axis
      REAL PSI                   ! Angle of pixel with respect to the
                                 ! centre of the PSF and x direction
      REAL RAD                   ! Distance from the centre of the PSF
      REAL SIGMA                 ! The Gaussian width along the minor
                                 ! axis
      REAL XD                    ! X distance from PSF centre to pixel centre
      REAL YD                    ! Y distance from PSF centre to pixel centre
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert from FWHM to sigma.
      SIGMA = ( 0.5 * FWHM ) / 1.38629 ** ( 1.0 / GAMMA )

*  Loop for each line.
      DO J = LBND2, UBND2

*  Find the vertical offset in pixels from the PSF centre to the centre of 
*  this line.
         YD = REAL( J ) - 0.5 - PY

*  Loop for each column. 
         DO I = LBND1, UBND1

*  Find the horizontal offset in pixels from the PSF centre to the centre of 
*  this column.
            XD = REAL( I ) - 0.5 - PX

*  The profile is 1.0 at the centre.
            IF( YD .EQ. 0.0 .AND. XD .EQ. 0.0 ) THEN
               ARRAY( I, J ) = 1.0

*  Otherwise,
            ELSE

*  Find the distance from the centre of the PSF to the centre of
*  this pixel.
               RAD = SQRT( XD * XD +  YD * YD )

*  Compute the co-ordinate angle with respect to the centre. 
               PSI = ATAN2( YD, XD )

*  The polar angular co-ordinate is the difference between the apparent
*  angle and the orientation of the major axis of the point-spread
*  function.
               PHI = PSI - THETA

*  Form the components along the major and minor axes.
               MAJOR = ABS( RAD * COS( PHI ) )
               MINOR = ABS( RAD * SIN( PHI ) )

*  Evaluate the PSF function allowing for the difference sigma along
*  the major axis.
               ARRAY( I, J ) = EXP( -0.5 * ( MINOR /
     :                         MAX( 0.001, SIGMA ) ) ** GAMMA ) *
     :                         EXP( -0.5 * ( MAJOR / AXISR /
     :                         MAX( 0.001, SIGMA ) ) ** GAMMA )
            END IF

         END DO

      END DO

      END
