      SUBROUTINE SCULIB_CALC_FLATFIELD (BOLNAME, BOL_TYPE, IDIM, JDIM,
     :  MAP_DATA, MAP_VARIANCE, MAP_QUALITY, X, Y, VOLUME, VOLUME_VAR,
     :  X_CENTRE, Y_CENTRE, THETA, A, B, QUALITY, STATUS)
*+
*  Name:
*     SCULIB_CALC_FLATFIELD

*  Purpose:
*     calculate flat-field results for a bolometer

*  Description:
*     This routine calculates the image parameters from data taken as a
*     flat-field measurement of a SCUBA bolometer. It takes as input data a
*     map made of a point source with pixels on a square grid.
*
*        If status is good on entry, the routine will set default return values
*     to VAL__BADR except QUALITY set to 0. As progress is made through the
*     routine the various image quantities will be set to their derived values,
*     and QUALITY will be set to 1 if any problem is encountered.
*
*        An attempt will then be made to estimate the 0 level of the image by
*     averaging valid data points in the corners of the map area. If there are
*     any such points then the 0 level will be subtracted from the map.
*
*        Next the routine will calculate the 0th and 1st order moments of the
*     image on the map. Map pixels with bad quality are ignored.
*
*         0th order = sum [f(i,j)], where i,j are the pixel indices
*
*         1st order in x = sum [x * f(i,j)], where x is the x position of pixel
*                                            i,j
*
*         1st order in y = sum [y * f(i,j)], where y is the y position of pixel
*                                            i,j
*
*     If the 0th order moment of the image was 0, i.e. there is no image on
*     the map, then a warning message will be output and the routine will
*     return with good status and QUALITY set to 1. Otherwise, the x,y centre
*     of the image is calculated from:-
*
*         X_CENTRE = 1st order in x        Y_CENTRE = 1st order in y
*                   .--------------                  .--------------
*                      0th order                        0th order
*
*     With this information the image can be analysed further by one of
*     2 methods. The method used depends on the value of parameter
*     FLAT_ANALYSIS. If there is an error reading this parameter, or
*     its value is not either MOMENTS or FIT, then a warning message will
*     be issued and a value of MOMENTS will be assumed.
*
*     For a MOMENTS analysis the 2nd order moments can be calculated about the
*     centre of the image:-
*
*         Uxx = sum [(x-X_CENTRE)^2 x f(i,j)]
*         Uxy = sum [(x-X_CENTRE)*(y-Y_CENTRE) x f(i,j)]
*         Uyy = sum [(y-Y_CENTRE)^2 x f(i,j)]
*
*     The parameters of the weighted ellipse describing the data are then
*     (following `Analysis of Astronomical Images using Moments', R.Stobie,
*     J.B.I.S., 33, 323):-
*
*         tan (2 THETA) = 2 Uxy / (Uxx - Uyy)
*                   A^2 = 2 (Uxx + Uyy) + 2sqrt[(Uxx-Uyy)^2 + 4Uxy^2]
*                   B^2 = 2 (Uxx + Uyy) - 2sqrt[(Uxx-Uyy)^2 + 4Uxy^2]
*
*     For a FIT analysis, a 2-d Gaussian is fitted by a least-squares
*     routine to the data. SCULIB_FIT_ROUTINE is the routine that performs
*     the fit while SCULIB_GAUSSIAN_XISQ calculates the chi-squared of the
*     fit to the data. An error will be reported and QUALITY set to 1
*     if there are any problems with the fit process. The fitted function
*     is :-
*
*              P * exp (-(x-X_CENTRE)**2 - (y-Y_CENTRE)**2)
*                      -----------------------------------
*                                  SIGMA**2
*
*     where SIGMA is an ellipse with semi-axis lengths A and B, with THETA
*     the angle between the x axis and A (THETA in radians, measured
*     anti-clockwise).
*
*     If all is well still, the volume under the image and the variance on it
*     are calculated. The volume will be the sum of all map pixels under a
*     circle of 12 arcsec radius for short-wave array bolometers, or 25
*     arcsec for other types. If this area laps over the edges of the map, or
*     any pixels inside it have bad quality then a warning message will be
*     output but QUALITY will stay good.
*

*  Invocation:
*     CALL SCULIB_CALC_FLATFIELD (BOLNAME, BOL_TYPE, IDIM, JDIM, MAP_DATA,
*    :  MAP_VARIANCE, MAP_QUALITY, X, Y, VOLUME, VOLUME_VAR, X_CENTRE,
*    :  Y_CENTRE, THETA, A, B, QUALITY, STATUS)

*  Arguments:
*     BOLNAME                           = CHARACTER*(*) (Given)
*           the name of the bolometer being measured
*     BOL_TYPE                          = CHARACTER*(*) (Given)
*           the type of the bolometer being measured
*     IDIM                              = INTEGER (Given)
*           `I' dimension of map
*     JDIM                              = INTEGER (Given)
*           `J' dimension of map
*     MAP_DATA (IDIM, JDIM)             = REAL (Given)
*           map data values
*     MAP_VARIANCE (IDIM, JDIM)         = REAL (Given)
*           map variance
*     MAP_QUALITY (IDIM, JDIM)          = REAL (Given)
*           map quality
*     X (IDIM)                          = REAL (Given)
*           x axis of map
*     Y (JDIM)                          = REAL (Given)
*           y axis of map
*     VOLUME                            = REAL (Returned)
*           volume under image within MULT times the fitted ellipse
*     VOLUME_VAR                        = REAL (Returned)
*           variance on VOLUME_DATA
*     X_CENTRE                          = REAL (Returned)
*           x of image centre
*     Y_CENTRE                          = REAL (Returned)
*           y of image centre
*     THETA                             = REAL (Returned)
*           angle of tilt of ellipse (radians)
*     A                                 = REAL (Returned)
*           semi-major axis of ellipse
*     B                                 = REAL (Returned)
*           semi-minor axis of ellipse
*     STATUS                            = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     25-MAY-1993: Original version.
*     22-MAY-1994: 2nd try.
*      7-APR-1995: FIT option added.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                    ! for VAL__BADR
      INTEGER MAX_FIT_DATA                 ! maximum number of measurements
      PARAMETER (MAX_FIT_DATA = 512)

*  Arguments Given:
      CHARACTER*(*) BOLNAME
      CHARACTER*(*) BOL_TYPE
      INTEGER IDIM
      INTEGER JDIM
      REAL MAP_DATA (IDIM, JDIM)
      REAL MAP_VARIANCE (IDIM, JDIM)
      INTEGER MAP_QUALITY (IDIM, JDIM)
      REAL X (IDIM)
      REAL Y (JDIM)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL VOLUME
      REAL VOLUME_VAR
      REAL X_CENTRE
      REAL Y_CENTRE
      REAL THETA
      REAL A
      REAL B
      INTEGER QUALITY

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL SCULIB_GAUSSIAN_XISQ        ! routine to calculate chi-squared
                                           ! of fit

*  Global variables:
      INTEGER NDATA_FIT                    ! number of measurements
      REAL    X_FIT (MAX_FIT_DATA)         ! x position of measurements
      REAL    Y_FIT (MAX_FIT_DATA)         ! y position of measurements
      REAL    DATA_FIT (MAX_FIT_DATA)      ! value of measurements
      REAL    VARIANCE_FIT (MAX_FIT_DATA)  ! variance on measurements
      INTEGER QUALITY_FIT (MAX_FIT_DATA)   ! quality on measurements
      COMMON /SCULIB_GAUSSIAN_FIT_DATA/ NDATA_FIT, X_FIT, Y_FIT,
     :  DATA_FIT, VARIANCE_FIT, QUALITY_FIT

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION ALPHA (6,6)         ! scratch used by SCULIB_FIT_FUNCTION
      CHARACTER*30     ANALYSIS            ! method of image analysis; MOMENTS
                                           ! or FIT
      DOUBLE PRECISION BETA (6)            ! scratch used by SCULIB_FIT_FUNCTION
      DOUBLE PRECISION DA (6)              ! scratch used by SCULIB_FIT_FUNCTION
      DOUBLE PRECISION FIT (6)             ! fitted parameters
      INTEGER          I                   ! DO loop variable
      INTEGER          IK (6)              ! scratch used by SCULIB_FIT_FUNCTION
      INTEGER          ITEMP               ! scratch integer
      INTEGER          ITERATION           ! fit iteration
      INTEGER          J                   ! DO loop variable
      INTEGER          JK (6)              ! scratch used by SCULIB_FIT_FUNCTION
      INTEGER          K                   ! array index
      DOUBLE PRECISION LAMBDA              ! Marquardt fit parameter
      REAL             LIMIT               ! the radius of the circle within
                                           ! which measurements are to be
                                           ! integrated
      LOGICAL          LOOPING             ! T while iterating fit
      LOGICAL          MISSING_POINTS      ! .TRUE. if some data points
                                           ! required to calculate VOLUME
                                           ! are missing
      REAL             PEAK                ! peak value of fitted Gaussian
      REAL             RTEMP               ! scratch real
      REAL             RTEMP1              ! scratch real
      REAL             SUM_F               ! sum of F
      REAL             SUM_F_VAR           ! variance on SUM_F
      REAL             SUM_XF              ! sum of X * F
      REAL             SUM_XF_VAR          ! variance on SUM_XF
      REAL             SUM_YF              ! sum of Y * F
      REAL             SUM_YF_VAR          ! variance on SUM_YF
      REAL             SUM_XXF             ! sum of X * X * F
      REAL             SUM_XYF             ! sum of X * Y * F
      REAL             SUM_YYF             ! sum of Y * Y * F
      DOUBLE PRECISION XICUT               ! when an iteration produces an
                                           ! improvement in chi-squared below
                                           ! this limit no further iterations
                                           ! will be performed
      DOUBLE PRECISION XISQ                ! chi-squared of fit
      DOUBLE PRECISION XIOLD               ! chi-squared of fit
      REAL             X_CENTRE_VAR        ! variance on X_CENTRE
      REAL             Y_CENTRE_VAR        ! variance on Y_CENTRE

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  default returned values

      VOLUME = VAL__BADR
      VOLUME_VAR = VAL__BADR
      X_CENTRE = VAL__BADR
      Y_CENTRE = VAL__BADR
      THETA = VAL__BADR
      A = VAL__BADR
      B = VAL__BADR
      QUALITY = 0

*  read the method of analysis to be used

      CALL PAR_GET0C ('FLAT_ANALYSIS', ANALYSIS, STATUS)
      CALL CHR_UCASE (ANALYSIS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_OUT (' ', 'SCULIB_CALC_FLATFIELD: error reading '//
     :     'FLAT_ANALYSIS parameter - ^STATUS', STATUS)
         CALL MSG_OUT (' ', 'a MOMENTS analysis will be performed',
     :     STATUS)
         ANALYSIS = 'MOMENTS'
      ELSE IF ((ANALYSIS.NE.'MOMENTS') .AND.
     :         (ANALYSIS.NE.'FIT'))    THEN
         STATUS = SAI__WARN
         CALL ERR_OUT (' ', 'SCULIB_CALC_FLATFIELD: FLAT_ANALYSIS '//
     :     'parameter is neither MOMENTS nor FIT', STATUS)
         CALL MSG_OUT (' ', 'a MOMENTS analysis will be performed',
     :     STATUS)
         ANALYSIS = 'MOMENTS'
      END IF

      RTEMP = 0.0
      ITEMP = 0

*  estimate 0 level by averaging points in corners of image

      DO J = 1, JDIM
         DO I = 1, IDIM
            IF (SQRT ((REAL(I) - REAL(IDIM)/2.0)**2 +
     :        (REAL(J) - REAL(JDIM)/2.0)**2) .GT.
     :        MIN (REAL(IDIM)/2.0, REAL(JDIM)/2.0)) THEN
               IF (MAP_QUALITY(I,J) .EQ. 0) THEN
                  RTEMP = RTEMP + MAP_DATA(I,J)
                  ITEMP = ITEMP + 1
               END IF
            END IF
         END DO
      END DO

*  subtract 0 level

      IF (ITEMP .NE. 0) THEN
         RTEMP = RTEMP / REAL(ITEMP)

         CALL MSG_SETC ('BOL', BOLNAME)
         CALL MSG_SETR ('ZERO', RTEMP)
         CALL MSG_OUT (' ', 'SCULIB: subtracting ^ZERO from the '//
     :     'image for ^BOL', STATUS)

         DO J = 1, JDIM
            DO I = 1, IDIM
               IF (MAP_QUALITY(I,J) .EQ. 0) THEN
                  MAP_DATA (I,J) = MAP_DATA (I,J) - RTEMP
               END IF
            END DO
         END DO
      END IF

      SUM_XF = 0.0
      SUM_XF_VAR = 0.0
      SUM_YF = 0.0
      SUM_YF_VAR = 0.0
      SUM_F = 0.0
      SUM_F_VAR = 0.0
      SUM_XXF = 0.0
      SUM_XYF = 0.0
      SUM_YYF = 0.0

*  cycle through the pixels, calculate 0th and 1st moments

      DO J = 1, JDIM
         DO I = 1, IDIM
            IF (MAP_QUALITY(I,J) .EQ. 0) THEN
               SUM_XF = SUM_XF + X (I) * MAP_DATA (I,J)
               SUM_XF_VAR = SUM_XF_VAR + X(I) * X(I) *
     :           MAP_VARIANCE(I,J)
               SUM_YF = SUM_YF + Y (J) * MAP_DATA (I,J)
               SUM_YF_VAR = SUM_YF_VAR + Y(I) * Y(I) *
     :           MAP_VARIANCE(I,J)
               SUM_F = SUM_F + MAP_DATA (I,J)
               SUM_F_VAR = SUM_F_VAR + MAP_VARIANCE(I,J)
            END IF
         END DO
      END DO

      IF ((SUM_F .EQ. 0.0)   .OR.
     :    (SUM_XF .EQ. 0.0)  .OR.
     :    (SUM_YF .EQ. 0.0)) THEN
         QUALITY = 1
         STATUS = SAI__WARN
         CALL MSG_SETC ('BOL', BOLNAME)
         CALL MSG_SETR ('SUM_F', SUM_F)
         CALL MSG_SETR ('SUM_XF', SUM_XF)
         CALL MSG_SETR ('SUM_YF', SUM_YF)
         CALL ERR_OUT (' ', 'SCULIB_CALC_FLATFIELD: bad 0th or 1st '//
     :     'order moments for ^BOL - SUM_F=^SUM_F, SUM_XF=^SUM_XF, '//
     :     'SUM_YF=^SUM_YF', STATUS)
      END IF

*  calculate x,y of centroid

      IF (QUALITY .EQ. 0) THEN
         X_CENTRE = SUM_XF / SUM_F
         Y_CENTRE = SUM_YF / SUM_F

         X_CENTRE_VAR = X_CENTRE**2 * (SUM_XF_VAR / SUM_XF**2 +
     :     SUM_F_VAR / SUM_F**2)
         Y_CENTRE_VAR = Y_CENTRE**2 * (SUM_YF_VAR / SUM_YF**2 +
     :     SUM_F_VAR / SUM_F**2)

         CALL MSG_SETR ('X_CENTRE', X_CENTRE)
         CALL MSG_SETR ('Y_CENTRE', Y_CENTRE)
         CALL MSG_SETR ('X_VAR', SQRT(X_CENTRE_VAR))
         CALL MSG_SETR ('Y_VAR', SQRT(Y_CENTRE_VAR))
         CALL MSG_SETC ('BOL', BOLNAME)
         CALL MSG_OUT (' ', 'SCULIB: bolometer ^BOL centroid at '//
     :     'X=^X_CENTRE+-^X_VAR, Y=^Y_CENTRE+-^Y_VAR', STATUS)
      END IF

*  now calculate other parameters of image

      IF (QUALITY .EQ. 0) THEN

         IF (ANALYSIS .EQ. 'MOMENTS') THEN

*  calculate 2nd order moments

            DO J = 1, JDIM
               DO I = 1, IDIM
                  IF (MAP_QUALITY(I,J) .EQ. 0) THEN
                     SUM_XXF = SUM_XXF + (X(I)-X_CENTRE)**2 *
     :                 MAP_DATA(I,J)
                     SUM_XYF = SUM_XYF + (X(I)-X_CENTRE) *
     :                 (Y(J)-Y_CENTRE) * MAP_DATA(I,J)
                     SUM_YYF = SUM_YYF + (Y(J)-Y_CENTRE)**2 *
     :                 MAP_DATA(I,J)
                  END IF
               END DO
            END DO

            SUM_XXF = SUM_XXF / SUM_F
            SUM_XYF = SUM_XYF / SUM_F
            SUM_YYF = SUM_YYF / SUM_F

*  get theta, a and b of the ellipse

            IF ((SUM_XXF - SUM_YYF) .EQ. 0.0) THEN
               QUALITY = 1
            ELSE
               RTEMP = 2.0 * SUM_XYF / (SUM_XXF - SUM_YYF)
               RTEMP = ATAN (RTEMP)
               THETA = RTEMP / 2.0

               RTEMP1 = 2.0 * SQRT((SUM_XXF-SUM_YYF)**2 + 4.0 *
     :           SUM_XYF**2)
               RTEMP = 2.0 * (SUM_XXF + SUM_YYF) - RTEMP1

               IF (RTEMP .LT. 0.0) THEN
                  QUALITY = 1
               ELSE
                  B = SQRT (RTEMP)
                  RTEMP = 2.0 * (SUM_XXF + SUM_YYF) + RTEMP1
                  IF (RTEMP .GE. 0.0) THEN
                     A = SQRT (RTEMP)
                  END IF
               END IF
            END IF

            IF (QUALITY .EQ. 1) THEN
               STATUS = SAI__WARN
               CALL MSG_SETC ('BOL', BOLNAME)
               CALL ERR_OUT (' ', 'SCULIB_CALC_FLATFIELD: error '//
     :           'calculating 2nd order moments for ^BOL', STATUS)
            ELSE
               CALL MSG_SETR ('THETA', THETA)
               CALL MSG_SETR ('A', A)
               CALL MSG_SETR ('B', B)
               CALL MSG_OUT (' ', 'SCULIB: - theta=^THETA, a=^A, '//
     :           'b=^B', STATUS)
            END IF

         ELSE IF (ANALYSIS .EQ. 'FIT') THEN

*  put the data into common for SCULIB_GAUSSIAN_XISQ to access

            NDATA_FIT = IDIM * JDIM

            IF (NDATA_FIT .GT. MAX_FIT_DATA) THEN
               STATUS = SAI__WARN
               CALL MSG_SETI ('N', NDATA_FIT)
               CALL MSG_SETI ('M', MAX_FIT_DATA)
               CALL ERR_OUT (' ', 'SCULIB_CALC_FLATFIELD: more '//
     :           'data points (^N) than maximum allowed (^M)', STATUS)
               QUALITY = 1
            ELSE

               DO J = 1, JDIM
                  DO I = 1, IDIM
                     K = (J-1)*IDIM + I
                     X_FIT (K) = X (I)
                     Y_FIT (K) = Y (J)
                     DATA_FIT (K) = MAP_DATA (I,J)
                     VARIANCE_FIT (K) = MAP_VARIANCE (I,J)
                     QUALITY_FIT (K) = MAP_QUALITY (I,J)
                  END DO
               END DO

*  setup first guess - a and b axes must be different to allow chi-squared
*  to vary with theta (if it doesn't then SCULIB_FIT_FUNCTION will refuse to
*  work)

               FIT (1) = 0.0D0

               IF (BOL_TYPE .EQ. 'SHORT') THEN
                  FIT (2) = 1.2011D0 * 3.09D0
                  FIT (3) = FIT (2) + 0.1D0
               ELSE IF (BOL_TYPE .EQ. 'LONG') THEN
                  FIT (2) = 1.2011D0 * 6.18D0
                  FIT (3) = FIT (2) + 0.1D0
               ELSE IF (BOL_TYPE .EQ. 'P1100') THEN
                  FIT (2) = 1.2011D0 * 8.00D0
                  FIT (3) = FIT (2) + 0.1D0
               ELSE IF (BOL_TYPE .EQ. 'P1300') THEN
                  FIT (2) = 1.2011D0 * 9.45D0
                  FIT (3) = FIT (2) + 0.1D0
               ELSE IF (BOL_TYPE .EQ. 'P2000') THEN
                  FIT (2) = 1.2011D0 * 14.50D0
                  FIT (3) = FIT (2) + 0.1D0
               ELSE
                  FIT (2) = 1.2011D0 * 5.0D0
                  FIT (3) = FIT (2) + 0.1D0
               END IF

               FIT (4) = 0.0D0
               FIT (5) = DBLE (X_CENTRE)
               FIT (6) = DBLE (Y_CENTRE)

               LAMBDA = 0.001
               CALL SCULIB_GAUSSIAN_XISQ (XISQ, 6, FIT, STATUS)
               XICUT = MAX (0.01D0 * XISQ, 0.01D0)

*  now iterate

               LOOPING = .TRUE.
               ITERATION = 0

               DO WHILE (LOOPING)
                  CALL SCULIB_FIT_FUNCTION (SCULIB_GAUSSIAN_XISQ,
     :              XICUT, 6, FIT, LAMBDA, ALPHA, BETA, IK, JK, DA,
     :              STATUS)

                  ITERATION = ITERATION + 1

*  check for last iteration

                  IF (STATUS .NE. SAI__OK) THEN
                     LOOPING =.FALSE.
                     QUALITY = 1
                  ELSE IF (ABS(XIOLD-XISQ) .LT. XICUT) THEN
                     LOOPING = .FALSE.
                  ELSE IF (ITERATION .GT. 10) THEN
                     STATUS = SAI__WARN
                     CALL ERR_OUT (' ', 'SCULIB_CALC_FLATFIELD: '//
     :                 'Gaussian fit has failed to converge after '//
     :                 '10 iterations', STATUS)
                     LOOPING = .FALSE.
                     QUALITY = 1
                  END IF

                  XIOLD = XISQ
                  CALL SCULIB_GAUSSIAN_XISQ (XISQ, 6, FIT, STATUS)

*  report the iteration result

                  CALL MSG_SETI ('ITER', ITERATION)
                  CALL MSG_SETR ('CHISQ', REAL(XISQ))
                  CALL MSG_OUT (' ', 'iter=^ITER chisq=^CHISQ', STATUS)
               END DO

*  if status has gone bad in this section, reset it to be good but
*  set the quality of the result to bad

               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_FLUSH (STATUS)
                  QUALITY = 1
               END IF
            END IF

*  report result

            IF (QUALITY .EQ. 0) THEN

*  calculate error matrix

               LAMBDA = 0.0D0
               CALL SCULIB_FIT_FUNCTION (SCULIB_GAUSSIAN_XISQ, XICUT,
     :           6, FIT, LAMBDA, ALPHA, BETA, IK, JK, DA, STATUS)

               CALL MSG_SETC ('BOL', BOLNAME)

               PEAK = REAL (FIT(1))
               RTEMP = REAL (SQRT(MAX(0.0D0,ALPHA(1,1))))
               CALL MSG_SETR ('PEAK', PEAK)
               CALL MSG_SETR ('PEAK_ERR', RTEMP)

               A = REAL (FIT(2)) * 0.833
               RTEMP = REAL (SQRT(MAX(0.0D0,ALPHA(2,2))))
               CALL MSG_SETR ('A', A)
               CALL MSG_SETR ('A_ERR', RTEMP)

               B = REAL (FIT(3)) * 0.833
               RTEMP = REAL (SQRT(MAX(0.0D0,ALPHA(3,3))))
               CALL MSG_SETR ('B', B)
               CALL MSG_SETR ('B_ERR', RTEMP)

               THETA = REAL (FIT(4))
               RTEMP = REAL (SQRT(MAX(0.0D0,ALPHA(4,4))))
               CALL MSG_SETR ('THETA', THETA)
               CALL MSG_SETR ('THETA_ERR', RTEMP)

               X_CENTRE = REAL (FIT(5))
               RTEMP = REAL (SQRT(MAX(0.0D0,ALPHA(5,5))))
               CALL MSG_SETR ('XCEN', X_CENTRE)
               CALL MSG_SETR ('XCEN_ERR', RTEMP)

               Y_CENTRE = REAL (FIT(6))
               RTEMP = REAL (SQRT(MAX(0.0D0,ALPHA(6,6))))
               CALL MSG_SETR ('YCEN', Y_CENTRE)
               CALL MSG_SETR ('YCEN_ERR', RTEMP)

               CALL MSG_OUT (' ', 'SCULIB: fit results for ^BOL',
     :           STATUS)
               CALL MSG_OUT (' ', '- peak=^PEAK (^PEAK_ERR) a=^A '//
     :           '(^A_ERR) b=^B (^B_ERR) theta=^THETA (^THETA_ERR) '//
     :           'xcen=^XCEN (^XCEN_ERR) ycen=^YCEN (^YCEN_ERR)',
     :           STATUS)
            END IF
         END IF
      END IF

*  sum pixels within 25 arcsec of the ellipse centre for LONG bolometers
*  12 arsec for SHORT

      IF (QUALITY .EQ. 0) THEN
         IF ((BOL_TYPE.EQ.'SHORT') .OR. (BOL_TYPE.EQ.'LONG')) THEN
            VOLUME = 0.0
            VOLUME_VAR = 0.0
            MISSING_POINTS = .FALSE.

            IF (BOL_TYPE .EQ. 'SHORT') THEN
               LIMIT = 12.0
            ELSE
               LIMIT = 25.0
            END IF

            DO J = 1, JDIM
               DO I = 1, IDIM

                  RTEMP = (X(I)-X_CENTRE)**2 + (Y(J)-Y_CENTRE)**2
                  IF (RTEMP .LE. LIMIT**2) THEN

*  point lies inside integration area

                     IF (MAP_QUALITY(I,J) .EQ. 0) THEN
                        VOLUME = VOLUME + MAP_DATA (I,J)
                        VOLUME_VAR = VOLUME_VAR + MAP_VARIANCE (I,J)
                     ELSE
                        MISSING_POINTS = .TRUE.
                     END IF
                  END IF

               END DO
            END DO

*  see if integration circle extended beyond map boundary

            IF (IDIM .GT. 1) THEN
               DO J = 1, JDIM
                  RTEMP = X(1) - (X(2)-X(1))
                  RTEMP1 = (RTEMP-X_CENTRE)**2 + (Y(J)-Y_CENTRE)**2

                  IF (RTEMP1 .LE. LIMIT**2) THEN
                     MISSING_POINTS = .TRUE.
                  END IF

                  RTEMP = X(IDIM) + (X(IDIM)-X(IDIM-1))
                  RTEMP1 = (RTEMP-X_CENTRE)**2 + (Y(J)-Y_CENTRE)**2

                  IF (RTEMP1 .LE. LIMIT**2) THEN
                     MISSING_POINTS = .TRUE.
                  END IF
               END DO
            END IF

            IF (JDIM .GT. 1) THEN
               DO I = 1, IDIM
                  RTEMP = Y(1) - (Y(2)-Y(1))
                  RTEMP1 = (X(I)-X_CENTRE)**2 + (RTEMP-Y_CENTRE)**2

                  IF (RTEMP1 .LE. LIMIT**2) THEN
                     MISSING_POINTS = .TRUE.
                  END IF

                  RTEMP = Y(JDIM) + (Y(JDIM)-Y(JDIM-1))
                  RTEMP1 = (X(I)-X_CENTRE)**2 + (RTEMP-Y_CENTRE)**2

                  IF (RTEMP1 .LE. LIMIT**2) THEN
                     MISSING_POINTS = .TRUE.
                  END IF
               END DO
            END IF

            IF (MISSING_POINTS) THEN
               STATUS = SAI__WARN
               CALL MSG_SETC ('BOL', BOLNAME)
               CALL ERR_OUT (' ', 'SCULIB_CALC_FLATFIELD: warning - '//
     :           'missing pixel(s) in flat-field map for ^BOL', STATUS)
            END IF

         ELSE

*  don't need volumes for other bolometers

            VOLUME = 1.0
            VOLUME_VAR = 0.0
         END IF

         CALL MSG_SETR ('VOLUME', VOLUME)
         CALL MSG_SETR ('ERR', SQRT(VOLUME_VAR))
         CALL MSG_OUT (' ', 'SCULIB: volume=^VOLUME (^ERR)', STATUS)
      END IF

      END
