      SUBROUTINE SCULIB_FIT_2D_GAUSSIAN (IDIM, JDIM, MAP_DATA,
     :  MAP_VARIANCE, MAP_QUALITY, X, Y, PEAK, PEAK_VAR,
     :  PEAK_QUAL, X_CENTRE, Y_CENTRE, X_HWHM, Y_HWHM, STATUS)
*+
*  Name:
*     SCULIB_FIT_2D_GAUSSIAN

*  Purpose:
*     fit a 2D Gaussian to an image

*  Description:
*     This routine fits a 2D Gaussian to an image of a source. It takes as
*     input data a map made of the source with pixels on a square grid.
*        If status is good on entry, the routine will calculate the
*     0th and 1st order moments of the image on the map. Map pixels with
*     bad quality are ignored.
*
*         0th order = sum [f(i,j)],
*
*     where i,j are the pixel indices
*
*         1st order in x = sum [x * f(i,j)],
*
*     where x is the x position of pixel i,j
*
*         1st order in y = sum [y * f(i,j)],
*
*     where y is the y position of pixel i,j
*
*     If the 0th order moment of the image was 0, i.e. there is no image on
*     the map, then a warning message will be output and all image parameters
*     set to 0 or bad quality. Otherwise, the x,y centre of the image is
*     calculated from:-
*
*         X_CENTRE = 1st order in x
*                   .--------------
*                      0th order
*
*     Similarly for Y_CENTRE.
*
*     With this information the 2nd order moments can be calculated about the
*     centre of the image:-
*
*         2nd order in x = sum [x^2 x f(i,j)],

*     where x is now the distance of
*     pixel i,j from X_CENTRE
*
*         2nd order in y = sum [y^2 x f(i,j)],
*
*     where y is now the distance of
*     pixel i,j from Y_CENTRE
*
*     If the image had a 2D Gaussian profile, f = A exp -ax^2*x^2 exp -ay^2*y^2
*     then it can be shown that for x:-
*
*               a_x^2 =   0th order
*                       -------------
*                       2 * 2nd order
*
*     and the half-width at half-maximum is calculated from:-
*
*              X_HWHM = sqrt (ln(2)) / a_x
*
*        Lastly, the volume under the image and the variance on it are
*     calculated. These will be the sum of all map pixels and their variances
*     that lie within a radius of 2 * max(HWHM_x, HWHM_y) from X_CENTRE,
*     Y_CENTRE. If the HWHM in either axis is zero then a warning message will
*     be output and PEAK_QUAL set bad. Otherwise, PEAK will be set to
*     volume / (pi * HWHM_x * HWHM_y).
*

*  Invocation:
*     CALL SCULIB_FIT_2D_GAUSSIAN (IDIM, JDIM, MAP_DATA,
*    :  MAP_VARIANCE, MAP_QUALITY, X, Y, PEAK, PEAK_VAR,
*    :  PEAK_QUAL, X_CENTRE, Y_CENTRE, X_HWHM, Y_HWHM, STATUS)

*  Arguments:
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
*     PEAK                              = REAL (Returned)
*           peak height of Gaussian
*     PEAK_VAR                          = REAL (Returned)
*           variance on PEAK
*     PEAK_QUAL                         = INTEGER (Returned)
*           quality of PEAK
*     X_CENTRE                          = REAL (Returned)
*           x of image centre
*     Y_CENTRE                          = REAL (Returned)
*           y of image centre
*     X_HWHM                            = REAL (Returned)
*           half-width half-max of image in x-axis
*     Y_HWHM                            = REAL (Returned)
*           half-width half-max of image in y-axis
*     STATUS                            = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1994,1995,1996,1997 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Method:

*  Deficiencies:

*  Bugs:

*  History:
*     $Id$
*     9-MAR-1994: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                          ! for VAL__BADR

*  Arguments Given:
      INTEGER IDIM, JDIM
      REAL MAP_DATA (IDIM, JDIM)
      REAL MAP_VARIANCE (IDIM, JDIM)
      INTEGER MAP_QUALITY (IDIM, JDIM)
      REAL X (IDIM)
      REAL Y (JDIM)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL PEAK, PEAK_VAR
      INTEGER PEAK_QUAL
      REAL X_CENTRE, Y_CENTRE
      REAL X_HWHM, Y_HWHM

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      REAL PI
      PARAMETER (PI = 3.1415927)

*  Local variables:
      INTEGER I, J                          ! DO loop
      REAL SUM_Y2F                          ! sum of Y^2 * F
      REAL SUM_X2F                          ! sum of X^2 * F
      REAL SUM_YF                           ! sum of Y * F
      REAL SUM_XF                           ! sum of X * F
      REAL SUM_F                            ! sum of F
      REAL MAXHW                            ! max (X_HWHM, Y_HWHM)
      REAL R2                               ! square of distance from map pixel
                                            ! to X_CENTRE, Y_CENTRE

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise sums and fit quality

      SUM_X2F = 0.0
      SUM_Y2F = 0.0
      SUM_XF = 0.0
      SUM_YF = 0.0
      SUM_F = 0.0
      PEAK_QUAL = 0

*  cycle through the pixels

      DO J = 1, JDIM
         DO I = 1, IDIM

            IF (MAP_QUALITY(I,J) .EQ. 0) THEN

               SUM_X2F = SUM_X2F + X(I)**2 * MAP_DATA (I,J)
               SUM_Y2F = SUM_Y2F + Y(J)**2 * MAP_DATA (I,J)
               SUM_XF = SUM_XF + X (I) * MAP_DATA (I,J)
               SUM_YF = SUM_YF + Y (J) * MAP_DATA (I,J)
               SUM_F = SUM_F + MAP_DATA (I,J)

            END IF

         END DO
      END DO


      X_CENTRE = 0.0
      Y_CENTRE = 0.0
      X_HWHM = 0.0
      Y_HWHM = 0.0
      PEAK = 0.0
      PEAK_VAR = 0.0

      IF (SUM_F .LE. 0.0) THEN

         PEAK_QUAL = 1
         STATUS = SAI__WARN
         CALL ERR_REP (' ', 'SCULIB_FIT_2D_GAUSSIAN: no image on '//
     :     'flat-field map', STATUS)

      ELSE

*  calculate x,y of centroid

         X_CENTRE = SUM_XF / SUM_F
         Y_CENTRE = SUM_YF / SUM_F

*  and HWHM in x and y of Gaussian

         IF (SUM_X2F .NE. 0.0) THEN
            X_HWHM = SUM_F / (2.0 * SUM_X2F)
            IF (X_HWHM .LE. 0.0) THEN
               X_HWHM = 0.0
            ELSE
               X_HWHM = SQRT (LOG(2.0) / X_HWHM)
            END IF
         END IF

         IF (SUM_Y2F .NE. 0.0) THEN
            Y_HWHM = SUM_F / (2.0 * SUM_Y2F)
            IF (Y_HWHM .LE. 0.0) THEN
               Y_HWHM = 0.0
            ELSE
               Y_HWHM = SQRT (LOG(2.0) / Y_HWHM)
            END IF
         END IF

*  sum pixels within 2*HWHM of image centre, issue warning if one of the HWHM
*  is zero

         IF ((X_HWHM .EQ. 0.0) .OR. (Y_HWHM .EQ. 0.0)) THEN

            PEAK_QUAL = 1
            STATUS = SAI__WARN
            CALL ERR_OUT (' ', 'SCULIB_FIT_2D_GAUSSIAN: source has '//
     :        'bad half-width', STATUS)

         ELSE

            MAXHW = MAX (X_HWHM, Y_HWHM)

            DO J = 1, JDIM
               DO I = 1, IDIM

                  R2 = (X(I) - X_CENTRE)**2 + (Y(J) - Y_CENTRE)**2
                  IF (R2 .LE. (4.0 * MAXHW**2)) THEN
                     IF (MAP_QUALITY(I,J) .EQ. 0) THEN
                        PEAK = PEAK + MAP_DATA (I,J)
                        PEAK_VAR = PEAK_VAR + MAP_VARIANCE (I,J)
                     END IF
                  END IF

               END DO
            END DO

            PEAK = PEAK / (PI * X_HWHM * Y_HWHM)
            PEAK_VAR = PEAK_VAR / (PI * X_HWHM * Y_HWHM)**2

         END IF
      END IF

      END
