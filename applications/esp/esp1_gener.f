


      SUBROUTINE ELF1_GENER(RADIUS,ELLIP,COUNT,ANG,RAD,STATUS)
*+
*  Name:
*     ELF1_GENER

*  Purpose:
*     Generates the positions of the points making up an ellipse of the
*     radius and ellipticity required. The angles generated are relative
*     to an origin of 0,0.
*
*     The number of points generated is proportional to the radius of the
*     ellipse subject to a minimum of 50 and a maximum defined within the
*     INCLUDE file elf_par.FOR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_GENER(RADIUS,ELLIP,COUNT,ANG,RAD,STATUS)

*  Description:
*     A number (N) is generated determining how many ellipse points will
*     be in the output ellipse. Points are then generated at
*     different angles within the quadrant, the difference in angle between
*     adjacent points being defined linearly by the number of ellipse
*     points required.

*  Arguments:
*     RADIUS = REAL (Given)
*        Ellipse radius in pixels.
*     ELLIP = REAL (Given)
*        The ellipse ellipticity.
*     COUNT = INTEGER (Returned)
*        The number of 'fit' ellipse pixels generated.
*     ANG(ELF__MXPOI) = REAL (Returned)
*        Position angle of the 'fit' ellipse points.
*     RAD(ELF__MXPOI) = REAL (Returned)
*        Distance of the 'fit' ellipse points from the ellipse origin.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      REAL ELLIP                      ! Ellipticity of the required ellipse
      REAL RADIUS                     ! Radius of the required ellipse

*  Arguments Returned:
      INTEGER COUNT                   ! Number of ellipse points generated
      REAL ANG(ELF__MXPOI)            ! Angle of the untranslated ellipse
                                      ! points
      REAL RAD(ELF__MXPOI)            ! Distance of the untranslated ellipse
                                      ! points from the origin

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER N                       ! Number of ellipse points
      REAL ANGLE                      ! Angle of a given point relative to
                                      ! the origin (Y axis)
      REAL JUMP                       ! Angular increment used when
                                      ! calculating the values of angle for
                                      ! points around the ellipse (radians)
      REAL SEMRAD                     ! Semi major axis of ellipse
      REAL X                          ! X co-ordinate of point
      REAL Y                          ! Y co-ordinate of point
      REAL ZERO                       ! Zero
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set zero.
      ZERO=0.0

*   Calculate the approximate perimeter circumference for the
*   ellipse. Assumed to be a circle for simplicity.
      N=2.*ELF__PIVAL*RADIUS

*   Impose some upper and lower limits on the number of points to be
*   generated.
      IF (N.LT.40) N=50
      IF (N.GT.ELF__MXPOI) N=ELF__MXPOI

*   Determine the angular increment required for an even distribution
*   of points around a circle.
      JUMP=360./REAL(N)*ELF__PI2360

*   Set the points generated COUNTR to zero.
      COUNT=0

*   Look at each angle in turn.
      SEMRAD=ELLIP*RADIUS
      DO 10 I=1,N

*      Generate the current angle in radians.
         ANGLE=JUMP*I

*      Calculate the x co-ordinates relative to 0,0.
         X=SEMRAD*SIN(ANGLE)
         Y=RADIUS*COS(ANGLE)

*      Determine the final angle (in degrees).
         CALL ELF1_ANGLES(X,Y,ZERO,ZERO,ANGLE,STATUS)
         ANGLE=ANGLE*ELF__PI2360

*      Store the results.
         COUNT=COUNT+1
         RAD(COUNT)=SQRT(X*X+Y*Y)
         ANG(COUNT)=ANGLE

 10   CONTINUE

 9999 CONTINUE

      END



      SUBROUTINE ELP1_GENER(ELLIP,SEMIMAJOR,NUMPOI,ANGL,DIS,STATUS)
*+
*  Name:
*     ELP1_GENER
*
*  Purpose:
*     Generates the positions of the points making up an ellipse of the
*     semi-major axis and ellipticity required. The angles generated are
*     relative to an origin of 0,0.
*
*     The number of points generated is proportional to the radius of the
*     ellipse subject to a minimum of 32 and a maximum defined within the
*     INCLUDE file elp_par.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL ELP1_GENER(ELLIP,SEMIMAJOR,NUMPOI,ANGL,DIS,STATUS)
*
*  Description:
*     A number (N) is generated determining how many ellipse points will
*     be found in a given ellipse quadrant. This is then used to derive an
*     angular increment that will separate points within a given quadrant.
*
*     Points are then generated at different angles within the quadrant.
*     To increase the speed of execution, these angles are also used to
*     create similar points in the other 3 quadrants.
*
*  Arguments:
*     ELLIP = REAL (Given)
*        The ellipse ellipticity.
*     SEMIMAJOR = REAL (Given)
*        Ellipse semi-major axis in pixels.
*     NUMPOI = INTEGER (Returned)
*        The number of 'fit' ellipse pixels generated.
*     ANGL(ELP__MXPOI) = REAL (Returned)
*        Position angle of the 'fit' ellipse points.
*     DIS(ELP__MXPOI) = REAL (Returned)
*        Distance of the 'fit' ellipse points from the ellipse origin.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*     26-Mar-1993 (GJP)
*       (Original version)
*     7-Dec-1999 (NG)
*       Renamed parameter RADIUS to SEMIMAJOR, after doing the
*       archaeology to work out that this is what was meant.
*
*  Bugs:
*     None known.
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      REAL ELLIP                      ! Ellipticity of the required ellipse
      REAL SEMIMAJOR                  ! semi-major axis of the required ellipse.

*  Arguments Returned:
      INTEGER NUMPOI                  ! Number of ellipse points generated
      REAL ANGL(ELP__MXPOI)           ! Angle of the untranslated ellipse
                                      ! points
      REAL DIS(ELP__MXPOI)            ! Distance of the untranslated ellipse
                                      ! points from the origin

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER N                       ! Number of points within a quadrant
      REAL ANGLE                      ! Angle of a given point relative to
                                      ! the origin (Y axis)
      REAL DIST                       ! Ellipse origin-point distance
      REAL JUMP                       ! Angular increment used when
                                      ! calculating the values of angle for
                                      ! points within a quadrant (radians)
      REAL SEMIMINOR                  ! Semi-minor axis
      REAL X                          ! X co-ordinate of point
      REAL Y                          ! Y co-ordinate of point

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the number of points generated to zero.
      NUMPOI=0

*   Calculate the number of points to be defined in a quadrant of the ellipse.
      N=NINT(2.*ELP__PIVAL*SEMIMAJOR/4.*2.0)
      IF (N.LT.6) N=6
      IF (N.GT.ELP__MXPOI/4.-1.) N=ELP__MXPOI/4.-1.

*   Calculate the angular increment required in radians.
      JUMP=90./REAL(N)*ELP__PI2360

*   Calculate the semi-minor axis
      SEMIMINOR=ELLIP*SEMIMAJOR

*   Calculate the ellipse points.
      DO 10 I=1,N-1

*      Calculate angle and then X and Y co-ordinates.
         ANGLE=JUMP*I
         X=SEMIMINOR*SIN(ANGLE)
         Y=SEMIMAJOR*COS(ANGLE)

*      Calculate the origin/pixel distance.
         DIST=SQRT(X*X+Y*Y)

*      Determine the final angle in (degrees).
         CALL ELP1_ANGLES(X,Y,0.0,0.0,ANGLE,STATUS)
         ANGLE=ANGLE*ELP__PI2360

*      Quadrant 1.
         NUMPOI=NUMPOI+1
         ANGL(NUMPOI)=ANGLE
         DIS(NUMPOI)=DIST

*      Quadrant 2.
         NUMPOI=NUMPOI+1
         ANGL(NUMPOI)=ELP__PIVAL-ANGLE
         DIS(NUMPOI)=DIST

*      Quadrant 3.
         NUMPOI=NUMPOI+1
         ANGL(NUMPOI)=-ANGLE
         DIS(NUMPOI)=DIST

*      Quadrant 4.
         NUMPOI=NUMPOI+1
         ANGL(NUMPOI)=ELP__PIVAL+ANGLE
         DIS(NUMPOI)=DIST

 10   CONTINUE

 9999 CONTINUE

      END
