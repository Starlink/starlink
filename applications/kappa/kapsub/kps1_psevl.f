      SUBROUTINE KPS1_PSEVL( AXISR, THETA, FWHM, GAMMA, DIM1, DIM2,
     :                       ARRAY, STATUS )
*+
*  Name:
*     KPS1_PSEVL

*  Purpose:
*     Finds the dimensions of a 2-d point-spread function array to a
*     given threshold.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PSEVL( AXISR, THETA, FWHM, GAMMA, DIM1, DIM2, ARRAY,
*                      STATUS )

*  Description:
*     This routine evaluates within an array a point-spread function
*     of the following radial form:
*        D =  A exp(-0.5 * (r/sigma) ** gamma )
*     where r is calculated from the true radial distance from the star
*     centre allowing for image ellipticity, sigma is the Gaussian
*     precision constant or profile width.  The point-spread function
*     may be oriented at an arbitrary angle.  The centre of the
*     point-spread function is at the centre of the array.

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
*     DIM1 = INTEGER (Given)
*        The first dimension of the array to be filled.  It should be
*        odd numbered.
*     DIM2 = INTEGER (Given)
*        The second dimension of the array to be filled.  It should be
*        odd numbered.
*     ARRAY( DIM1, DIM2 ) = REAL (Returned)
*        The point spread function evaluated at each element.
*        The centre of the PSF is within the central element.
*     STATUS = INTEGER (Given)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 9 (MJC):
*        Original version.
*     1995 April 5 (MJC):
*        Used the modern style of variable declaration and comment
*        indentation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      REAL AXISR
      REAL FWHM
      REAL GAMMA
      REAL THETA

      INTEGER DIM1
      INTEGER DIM2

*  Arguments Returned:
      REAL ARRAY( DIM1, DIM2 )

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
      REAL RADSQ                 ! Squared distance from the centre of
                                 ! the PSF
      REAL SIGMA                 ! The Gaussian width along the minor
                                 ! axis
      INTEGER XC                 ! X centre of the array
      INTEGER XD                 ! X distance of pixel from the centre
                                 ! of the array
      INTEGER YC                 ! Y centre of the array
      INTEGER YD                 ! Y distance of pixel from the centre
                                 ! of the array
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert from FWHM to sigma.
      SIGMA = ( 0.5 * FWHM ) / 1.38629 ** ( 1.0 / GAMMA )

*  Find the central pixel.
      XC = DIM1 / 2 + 1
      YC = DIM2 / 2 + 1

*  Loop for each line to the centre.  The remaining lines are filled
*  by symmetry.
      DO J = 1, YC

*  Find the relative position with respect to the centre of the array
*  in y.
         YD = YC - J

*  Loop for each pixel.
         DO I = 1, DIM1

*  Find the relative position with respect to the centre of the array
*  in x.
            XD = XC - I

*  Find the squared distance from the centre of the PSF.
            RADSQ = REAL( XD * XD +  YD * YD )

*  If we are at the centre the pixel value is by definition 1.
            IF ( RADSQ .LT. VAL__SMLR ) THEN
               ARRAY( I, J ) = 1.0
            ELSE

*  It is now safe to find the radius in polar co-ordinates.
               RAD = SQRT( RADSQ )

*  Compute the co-ordinate angle with respect to the centre
               PSI = ATAN2( REAL( YD ), REAL( XD ) )

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

*  Due to the symmetry the other pixel with the same value may be
*  assigned, thereby improving efficiency.  For the last line these
*  will be determined in full in order to make the algorithm easier to
*  follow.
               IF ( J .LT. YC ) THEN
                  ARRAY( XC + XD, YC + YD ) = ARRAY( I, J )
               END IF
            END IF
         END DO
      END DO

      END
