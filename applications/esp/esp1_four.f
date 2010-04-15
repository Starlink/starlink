

      SUBROUTINE ELF1_FOUR(POINTS,BACK,VALIDP,PCV,XE,YE,XO,YO,
     :                     POSANG,ELLIP,RESULT,STATUS)
*+
*  Name:
*     ELF1_FOUR

*  Purpose:
*     Determines the values of the Fourier descriptors from the current
*     ellipse parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_FOUR(POINTS,BACK,VALIDP,PCV,XE,YE,XO,YO,
*                    POSANG,ELLIP,RESULT,STATUS)

*  Description:
*     Uses the ellipse parameters to normalise the isophotal pixel
*     positions to a unit circle. Then solves a set of simultaneous
*     equations to determine (consequtively) the amplitudes of
*     sin/cos factors that would be required to create the variations
*     in brightness found around the circle.
*
*     This route is chosen (rather than using actual pixel positions)
*     due to the small number of pixels involved at low radii.

*  Arguments:
*     POINTS = INTEGER (Given)
*        Number of fit ellipse pixels.
*     BACK = REAL (Given)
*        Image pixels background count value. Units pixels.
*     VALIDP = INTEGER (Given)
*        The number of ellipses for which parameters have been found.
*     PCV(ELF__MXPOI) = REAL (Given)
*        The ellipse pixel brightness values. Units counts.
*     XE(ELF__MXPOI) = REAL (Given)
*        Ellipse pixel X co-ordinates. Units pixels.
*     YE(ELF__MXPOI) = REAL (Given)
*        Ellipse pixel Y co-ordinates. Units pixels.
*     XO = REAL (Given)
*        X co-ordinate of the galaxy centre, Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of the galaxy centre, Units pixels.
*     POSANG = REAL (Given)
*        Position angle of the ellipse.
*     ELLIP = REAL (Given)
*        Ellipticity of the ellipse.
*     RESULT(17,ELF__RESUL) = REAL (Returned)
*        Fitted ellipse parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     20-Sep-1993 (GJP)
*     (Original version)
*     14-FEB-1996 (GJP)
*     Removed the NAG routine.
*     12-OCT-1996 (GJP)
*     Removed the remaining NAG routines.

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
      INTEGER POINTS                  ! Number of pixels in the current
                                      ! isophote
      INTEGER VALIDP                  ! Number of ellipses for which parameters
                                      ! have been determined
      REAL BACK                       ! Pixel count background value
      REAL ELLIP                      ! Ellipticity of the fitted ellipse
      REAL PCV(ELF__MXPOI)            ! Brightness of the pixels
      REAL POSANG                     ! Position angle of the ellipse
      REAL XE(ELF__MXPOI)             ! X/Y co-ords of the pixels
                                      ! in the ellipse
      REAL YE(ELF__MXPOI)             ! X/Y co-ords of the pixels
                                      ! in the ellipse
      REAL XO                         ! X co-ord of galaxy centre
      REAL YO                         ! Y co-ord of galaxy centre

*  Arguments Returned:
      REAL RESULT(17,ELF__RESUL)      ! Ellipse parameter results

*  Arguments Given and Returned:

*  Local variables:
      DOUBLE PRECISION C              ! Cosine amplitude
      DOUBLE PRECISION S              ! Sine amplitude
      DOUBLE PRECISION XV2(500)       ! Angle
      DOUBLE PRECISION YV2(500)       ! Brightness
      DOUBLE PRECISION CONT(4)        ! Contributions from the previous
                                      ! Fourier descriptor orders
      DOUBLE PRECISION CONTR          ! Sum of contributions from the previous
                                      ! FD orders
      DOUBLE PRECISION MEAN           ! Mean pixel count
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Array index storage
      INTEGER ORDER                   ! Fourier desc. order being calculated
      REAL ANG(ELF__MXPOI)            ! Transformed pixel angle
      REAL ANGLE                      ! Temporary storage
      REAL ANGLER                     ! Angle of pixel plus the position angle
      REAL DEGS                       ! Angle of pixel
      REAL RAD                        ! Distance of pixel from the origin
      REAL XV(ELF__MXPOI)             ! Transformed/rotated pixel co-ord
      REAL YV(ELF__MXPOI)             ! Transformed/rotated pixel co-ord
      REAL ZERO                       ! Zero
*.
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set variable.
      ZERO=0.0

*   Rotate the ellipse.

*   Rotate each point in turn.
*   Also, calculate the mean pixel count value.
      MEAN=0.0D0
      DO 5 I=1,POINTS

*      Find the distance from the centre.
         RAD=SQRT((XE(I)-XO)*(XE(I)-XO)+(YE(I)-YO)*(YE(I)-YO))

*      Find the angle of the current pixel relative to the origin.
         CALL ELF1_ANGLES(XE(I),YE(I),XO,YO,DEGS,STATUS)

*      Calc the resultant angle between the point and the origin.
*      Then convert to radians.
         ANGLER=(DEGS-POSANG)*ELF__PI2360

*      Calculate the transformed/rotated co-ordinates where the origin
*      is now at 0,0.
         XV(I)=RAD*SIN(ANGLER)
         YV(I)=RAD*COS(ANGLER)

*      Increase the X component of the position to convert the
*      ellipse into a circle.
         XV(I)=XV(I)/ELLIP

*      Recalculate the angles. The ellipse/circle transformation will
*      have modified these.

*      Find the angle of the current pixel relative to the origin.
         CALL ELF1_ANGLES(XV(I),YV(I),ZERO,ZERO,DEGS,STATUS)
         ANG(I)=DEGS*ELF__PI2360

*      Add to the mean summation.
         MEAN=PCV(I)+MEAN

 5    CONTINUE
      MEAN=MEAN/REAL(POINTS)

*   Set up the equations of intensity versus angle that must be solved.
*   Work out the coefficients for each order in turn.

*   For each order in turn.
      DO 20 ORDER=1,4

*      For each of the pixels in turn set up a simultaneous equation.
         DO 30 I=1,POINTS

*         First, calculate the contribution from each preceeding order.
            DO 25 J=1,ORDER-1
               ANGLE=ANG(I)*REAL(J)
               K=(J-1)*2+10.
               CONT(J)=RESULT(K,VALIDP)*SIN(ANGLE)
     :                +RESULT(K+1,VALIDP)*COS(ANGLE)
 25         CONTINUE

*         Sum the contributions from the preceeding orders.
*         Performed in this way to avoid rounding errors.
            IF (ORDER.EQ.1) CONTR=0.0D0
            IF (ORDER.EQ.2) CONTR=CONT(1)
            IF (ORDER.EQ.3) CONTR=CONT(1)+CONT(2)
            IF (ORDER.EQ.4) CONTR=CONT(1)+CONT(2)+CONT(3)

*         Residual pixel count, sine factor and then cosine factor.
            XV2(I)=REAL(ORDER)*ANG(I)
            YV2(I)=PCV(I)-MEAN-CONTR

  30      CONTINUE

*      Solve the equations using a shareware routine.
         CALL ELF1_SOLVE(POINTS,XV2,YV2,S,C,STATUS)

*      Store the un-normalised values.
         J=(ORDER-1)*2+10.
         RESULT(J,VALIDP)=  S
         RESULT(J+1,VALIDP)=C

 20   CONTINUE

*   Normalise the Fourier descriptors.
      DO 100 I=10,17

*      Only normalise them if it will not lead to a very large number.
         IF (ABS(MEAN-BACK).GT.ELF__VSMAL)
     :      RESULT(I,VALIDP)=RESULT(I,VALIDP)/(MEAN-BACK)

100   CONTINUE


 9999 CONTINUE

      END



      SUBROUTINE ELP1_FOUR(NUMPOI,BACK,VALIDP,XR,YR,VA,XO,YO,
     :                     POSANG,ELLIP,RESULT,STATUS)
*+
*  Name:
*     ELP1_FOUR

*  Purpose:
*     Determines the values of the Fourier descriptors for the current
*     ellipse parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_FOUR(NUMPOI,BACK,VALIDP,XR,YR,VA,XO,YO,POSANG,
*                    ELLIP,RESULT,STATUS)

*  Description:
*     Uses the ellipse parameters to normalise the fit ellipse pixel
*     positions to a unit circle. Then solves a set of simultaneous
*     equations to determine the amplitudes of sin/cos factors that would
*     be required to create the variations in pixel brightness
*     found around the circle. These are converted to the values
*     you would get from contour analysis.

*  Arguments:
*     NUMPOI = INTEGER (Given)
*        Number of ellipse pixels.
*     BACK = REAL (Given)
*        Background count value for the image. Units counts.
*     VALIDP = INTEGER (Given)
*        The number of ellipses for which parameters have been found.
*     XO = REAL (Given)
*        X co-ordinate of the galaxy centre, Units pixels.
*     XR(ELP__MXPOI) = REAL (Given)
*        Isophotal pixel X co-ordinates. Units pixels.
*     YR(ELP__MXPOI) = REAL (Given)
*        Isophotal pixel Y co-ordinates. Units pixels.
*     VA(ELP__MXPOI) = REAL (Given)
*        Pixel brightness. Units counts.
*     YO = REAL (Given)
*        Y co-ordinate of the galaxy centre, Units pixels.
*     ELLIP = REAL (Given)
*        Ellipticity of the ellipse.
*     POSANG = REAL (Given)
*        Position angle of the ellipse.
*     RESULT(ELP__NRES,ELP__RESUL) = REAL (Returned)
*        Fitted ellipse parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     27-AUG-1993 (GJP)
*     (Original version)
*     14-FEB-1996 (GJP)
*     Removed some NAG routines.
*     12-OCT-1996 (GJP)
*     Removed the last NAG routines.
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER NUMPOI                  ! Number of pixels in the current
                                      ! ellipse and hence the number
                                      ! of equations to solve
      INTEGER VALIDP                  ! Number of profiles for parameters
                                      ! has been determined
      REAL BACK                       ! Background count value
      REAL ELLIP                      ! Ellipticity of the fitted ellipse
      REAL VA(ELP__MXPOI)             ! Pixel brightness
      REAL POSANG                     ! Position angle of the ellipse
      REAL XR(ELP__MXPOI)             ! X/Y co-ords of the pixels
                                      ! in the fitted ellipse
      REAL YR(ELP__MXPOI)             ! X/Y co-ords of the pixels
                                      ! in the fitted ellipse
      REAL XO                         ! X co-ord of galaxy centre
      REAL YO                         ! Y co-ord of galaxy centre

*  Arguments Returned:
      REAL RESULT(ELP__NRES,ELP__RESUL)      ! Ellipse parameter results

*  Arguments Given and Returned:

*  Local variables:
      DOUBLE PRECISION CONT(4)        ! Contributions from the previous
                                      ! Fourier descriptor orders
      DOUBLE PRECISION CONTR          ! Sum of contributions from the previous
                                      ! FD orders
      DOUBLE PRECISION MEAN           ! Mean pixel count
      DOUBLE PRECISION C              ! Cosine amplitude
      DOUBLE PRECISION S              ! Sine amplitude
      DOUBLE PRECISION XV2(500)       ! Angle
      DOUBLE PRECISION YV2(500)       ! Brightness
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Array index storage
      INTEGER ORDER                   ! Fourier desc. order being calculated
      REAL ANG(ELP__MXPOI)            ! Transformed pixel angle
      REAL ANGLE                      ! Temporary storage
      REAL ANGLER                     ! Angle of pixel plus the position angle
      REAL DEGS                       ! Angle of pixel
      REAL RAD                        ! Distance of pixel from the origin
      REAL XV(ELP__MXPOI)             ! Transformed/rotated pixel co-ord
      REAL YV(ELP__MXPOI)             ! Transformed/rotated pixel co-ord
      REAL ZERO                       ! Zero value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Rotate the ellipse

*   Rotate each point in turn.
*   Also, calculate the mean pixel count value.
      ZERO=0.0
      MEAN=0.0D0
      DO 5 I=1,NUMPOI

*      Find the distance from the centre.
         RAD=SQRT((XR(I)-XO)*(XR(I)-XO)+(YR(I)-YO)*(YR(I)-YO))

*      Find the angle of the current pixel relative to the origin.
         CALL ELP1_ANGLES(XR(I),YR(I),XO,YO,DEGS,STATUS)

*      Calc the resultant angle between the point and the origin.
*      Then convert to radians.
         ANGLER=(DEGS-POSANG)*ELP__PI2360

*      Calculate the transformed/rotated co-ordinates where the origin
*      is now at 0,0.
         XV(I)=RAD*SIN(ANGLER)
         YV(I)=RAD*COS(ANGLER)

*      Increase the X component of the position to convert the
*      ellipse into a circle.
         XV(I)=XV(I)/ELLIP

*      Recalculate the angles for the ellipse/circle transformation will
*      have modified these.

*      Find the angle of the current pixel relative to the origin.
         CALL ELP1_ANGLES(XV(I),YV(I),ZERO,ZERO,DEGS,STATUS)
         ANG(I)=DEGS*ELP__PI2360

*      Add to the mean summation.
         MEAN=VA(I)+MEAN

 5    CONTINUE
      MEAN=MEAN/REAL(NUMPOI)

*   Set up the equations of intensity versus angle that must be solved.
*   Work out the coefficients for each order in turn.

*   For each order in turn.
      DO 20 ORDER=1,4

*      For each of the pixels in turn set up a simultaneous equation.
         DO 30 I=1,NUMPOI

*         First, calculate the contribution from each preceeding order.
            DO 25 J=1,ORDER-1
               ANGLE=REAL(J)*ANG(I)
               K=(J-1)*2+10.
               CONT(J)=RESULT(K,VALIDP)*SIN(ANGLE)
     :                       +RESULT(K+1,VALIDP)*COS(ANGLE)
 25         CONTINUE

*         Sum the contributions from the preceeding orders.
*         Performed in this way to avoid rounding errors.
            IF (ORDER.EQ.1) CONTR=0.0D0
            IF (ORDER.EQ.2) CONTR=CONT(1)
            IF (ORDER.EQ.3) CONTR=CONT(1)+CONT(2)
            IF (ORDER.EQ.4) CONTR=CONT(1)+CONT(2)+CONT(3)

*         Residual pixel count, sine factor and then cosine factor.
            XV2(I)=REAL(ORDER)*ANG(I)
            YV2(I)=VA(I)-MEAN-CONTR

 30      CONTINUE

*      Solve the equations using a shareware routine.
         CALL ELP1_SOLVE(NUMPOI,XV2,YV2,S,C,STATUS)

*      Store the un-normalised values.
         J=(ORDER-1)*2+10.
         RESULT(J,VALIDP)=  S
         RESULT(J+1,VALIDP)=C

 20   CONTINUE

*   Normalise the Fourier descriptors.
      DO 100 I=10,17

*      Only normalise them if it will not lead to a very large number.
         IF (ABS(MEAN-BACK).GT.ELP__VSMAL)
     :      RESULT(I,VALIDP)=RESULT(I,VALIDP)/(MEAN-BACK)

100   CONTINUE

 9999 CONTINUE

      END
