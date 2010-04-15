


      SUBROUTINE ELF1_ROTAT(XO,YO,THETA,COUNT,ANG,RAD,XCR,YCR,STATUS)
*+
*  Name:
*     ELF1_ROTAT

*  Purpose:
*     Takes the angle/and distance values provided and generates co-ordinates
*     for an ellipse of the same size/ellipticity if it had been
*     rotated about its origin and then translated away from a 0,0
*     origin to that required on the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_ROTAT(XO,YO,THETA,COUNT,ANG,RAD,XCR,YCR,STATUS)

*  Description:
*     For each of the ellipse points in turn it determines the new angle of
*     the point relative to the origin. This and the distance
*     from the origin, are employed to calculate the new position of the point
*     when translated/rotated about the new origin.

*  Arguments:
*     XO = REAL (Given)
*        X co-ordinate of the required ellipse centre. Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of the required ellipse centre. Units pixels.
*     THETA = REAL (Given)
*        Position angle of the ellipse required.
*     COUNT = INTEGER (Given)
*        Number of points in the 'fit' ellipse.
*     ANG(ELF__MXPOI) = REAL (Given)
*        Position angle of each of the ellipse points.
*     RAD(ELF__MXPOI) = REAL (Given)
*        Distance of the ellipse points from the origin.
*     XCR(ELF__MXPOI) = REAL (Returned)
*        X co-ordinate of the ellipse points after rotation/translation.
*     YCR(ELF__MXPOI) = REAL (Returned)
*        Y co-ordinate of the ellipse points after rotation/translation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-AUG-1993 (GJP)
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
      INTEGER COUNT                   ! The number of points in the current
                                      ! 'fit' ellipse
      REAL ANG(ELF__MXPOI)            ! Angle of the ellipse points
                                      ! before rotation
      REAL RAD(ELF__MXPOI)            ! Distance from the ellipse centre
      REAL THETA                      ! Position angle required for the
                                      ! ellipse
      REAL XO                         ! X co-ordinate to which the ellipse
                                      ! origin is to be shifted
      REAL YO                         ! Y co-ordinate to which the ellipse
                                      ! origin is to be shifted

*  Arguments Returned:
      REAL XCR(ELF__MXPOI)            ! X co-ord for the translated ellipse
      REAL YCR(ELF__MXPOI)            ! Y co-ord for the translated ellipse

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL CURANG                     ! Current position angle in radians.
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Convert the required angle to radians.
      CURANG=THETA*ELF__PI2360

*   Rotate each point in turn.
      DO 10 I=1,COUNT

*      Calculate the transformed/rotated co-ordinates.
         XCR(I)=XO+RAD(I)*SIN(ANG(I)+CURANG)
         YCR(I)=YO+RAD(I)*COS(ANG(I)+CURANG)

 10   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELP1_ROTAT(X,Y,DIS,ANGL,POSANG,NUMPOI,
     :                      XR,YR,STATUS)
*+
*  Name:
*     ELP1_ROTAT

*  Purpose:
*     Takes the angle/and distance values provided and generates co-ordinates
*     for an ellipse of the same size/ellipticity if it had been
*     rotated about its origin and then translated away from a 0,0
*     origin to that required on the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_ROTAT(X,Y,DIS,ANGL,POSANG,NUMPOI,XR,YR,STATUS)

*  Description:
*     For each of the ellipse points in turn it determines the new angle of
*     the point relative to the origin. This and the distance
*     from the origin, are employed to calculate the new position of the point
*     when translated/rotated about the new origin.

*  Arguments:
*     X = REAL (Given)
*        X co-ordinate of the required ellipse centre. Units pixels.
*     Y = REAL (Given)
*        Y co-ordinate of the required ellipse centre. Units pixels.
*     DIS(ELP__MXPOI) = REAL (Given)
*        Distance of the ellipse points from the origin.
*     ANGL(ELP__MXPOI) = REAL (Given)
*        Position angle of each of the ellipse points.
*     POSANG = REAL (Given)
*        Position angle of the ellipse required.
*     NUMPOI = INTEGER (Given)
*        Number of points in the 'fit' ellipse.
*     XR(ELP__MXPOI) = REAL (Returned)
*        X co-ordinate of the ellipse points after rotation/translation.
*     YR(ELP__MXPOI) = REAL (Returned)
*        Y co-ordinate of the ellipse points after rotation/translation.
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
      INCLUDE 'ELP_PAR'               ! ELLPRO constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER NUMPOI                  ! The number of points in the current
                                      ! 'fit' ellipse
      REAL ANGL(ELP__MXPOI)           ! Angle of the ellipse points
                                      ! before rotation
      REAL DIS(ELP__MXPOI)            ! Distance from the ellipse centre
      REAL POSANG                     ! Position angle required for the
                                      ! ellipse
      REAL X                          ! X co-ordinate to which the ellipse
                                      ! origin is to be shifted
      REAL Y                          ! Y co-ordinate to which the ellipse
                                      ! origin is to be shifted

*  Arguments Returned:
      REAL XR(ELP__MXPOI)             ! X co-ord for the translated ellipse
      REAL YR(ELP__MXPOI)             ! Y co-ord for the translated ellipse

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL ANGLER                     ! Angle of the point relative to
                                      ! origin after rotation
      REAL CURANG                     ! The position angle expressed in
                                      ! radians
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Setup useful constants.
      CURANG=POSANG*ELP__PI2360

*   For all the ellipse points in turn.
      DO 10 I=1,NUMPOI

*      Calculate the resultant angle.
         ANGLER=ANGL(I)+CURANG

*      Translate and apply rotation to the current point.
*      Save the result.
         XR(I)=X+DIS(I)*SIN(ANGLER)
         YR(I)=Y+DIS(I)*COS(ANGLER)

 10   CONTINUE

 9999 CONTINUE

      END
