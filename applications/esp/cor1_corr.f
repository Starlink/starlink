      SUBROUTINE COR1_CORR(MULT,ELEMS,ARRAY,RADIUS,MODE,
     :                       XMAX,YMAX,USEALL,HIEST,STATUS,ARRAY2)
*+
*  Name:
*     COR1_CORR

*  Purpose:
*     Performs correlation calculations for a circular symmetrical
*     template of known diameter. It assumes a global mode value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COR1_CORR(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
*                    XMAX,YMAX,USEALL,HIEST,STATUS,%VAL(POINT2(1)))

*  Description:
*     Given a source image, the routine generates an equivalent image of
*     similar size that shows, at each pixel, the degree of correlation
*     that was present in the circular area around the equivalent point
*     in the source image. Correlation should normally be near zero for
*     a Gaussian (Normal) distribution. Large deviations from this
*     suggest regions either containing objects.
*
*     The calculation is carried out as follows:
*
*     The value for each pixel of the output image is determined
*     as follows. An imaginary circle is drawn about the pixel
*     and the count values for all pixels within that circle,
*     are stored.
*
*     The values then have the background value subtracted and are
*     multiplied by an elliptical factor depending on the distance of
*     pixel from the resident pixel.
*
*     The template size is set as 1.8x the size of the galaxy to be detected.
*     This is the optimal size.
*
*     The individual contributions from all the pixels within the user defined
*     radius are summed and then divided by the number found (non-bad) to
*     normalise the result.

*  Arguments:
*     MULT = REAL (Given)
*        The multiplying value applied to all the correlation values.
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     RADIUS = REAL (Given)
*        The radius of the circular template that is used to define
*        the region about a given pixel, that might contribute to the
*        correlation value that will be inserted into
*        the equivalent pixel in ARRAY2.
*     MODE = REAL (Given)
*        The global mode value supplied by the user. Units counts.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     USEALL = LOGICAL (Given)
*        Flag indicating whether or not pixels of too high a value will be
*        ignored.
*     HIEST = REAL (Given)
*        The highest pixel count value that will be used in the analysis.
*        All pixels with count values above this will be ignored. Units
*        counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the correlation 'image'.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-Oct-1992
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'COR_PAR'               ! CORR constants

*  Arguments Given:
      LOGICAL USEALL                  ! Is a high count cut out being
                                      ! used?
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Radius of the size of template
                                      ! being considered
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      REAL ARRAY(ELEMS)               ! Array containing NDF data
      REAL HIEST                      ! The count value above which
                                      ! a pixel will be ignored if
                                      ! USEALL is true.
      REAL MODE                       ! Global image mode value
      REAL MULT                       ! Multipliying factor

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! correlation results

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER ADD2                    ! Array element address
      INTEGER J                       ! Loop variable
      INTEGER N                       ! The number of valid pixels
                                      ! that were present about a given
                                      ! origin pixel
      INTEGER NPIX                    ! The maximum number of pixels
                                      ! about a given origin. Depends on the
                                      ! pixel size and template size
      INTEGER OFFSETS(COR1__PIXN)     ! Address offsets for the
                                      ! pixels in the circular
                                      ! template used
      INTEGER PERC                    ! Percentage of the calculations
                                      ! done so far
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index

      REAL CORR                       ! Temporary correlation sum
      REAL ELF(COR1__PIXN)            ! Elliptical multiplying factor
                                      ! for each of the pixels in the
                                      ! pixels in the circular
                                      ! template used
      REAL ELFSQ(COR1__PIXN)          ! Elliptical multiplying factor squared
      REAL RRADIUS                    ! Radius value
      REAL SUMSQ1                     ! Sum of squares for data
      REAL SUMSQ2                     ! Sum of squares for mask
      REAL TEMP                       ! Temporary storage
      REAL TRAD                       ! Temporary radius value
      REAL VALUE                      ! Temporary storage
      REAL VALUE1                     ! Temporary storage
      REAL XR                         ! Temporary storage
      REAL YR                         ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=VAL__BADR
 10   CONTINUE

*   Real value for the radius.
      RRADIUS=REAL(RADIUS)

*   Construct an array containing the memory address offsets of all the
*   pixels in the template relative to the memory address of the circle
*   centre.
*
      NPIX=0
      DO 30 X1=-RADIUS,RADIUS
         DO 20 Y1=-RADIUS,RADIUS

*         Calculate the distance to the origin pixel in the mask.
            XR=REAL(X1)
            YR=REAL(Y1)
            TRAD=SQRT(XR*XR+YR*YR)

*         Check that the pixel at pixel offsets X1,Y1 is
*         within a circle of the given radius and hence within the
*         required circular area.
            IF (TRAD.LT.RADIUS) THEN

*            Calculate the memory address offset.
               VALUE=Y1*XMAX+X1

*            Increment the address counter and store the
*            address offset.
               NPIX=NPIX+1

*            Check that there are not too many pixels.
               IF (NPIX.GT.COR1__PIXN) THEN
                  STATUS=SAI__ERROR
                  CALL ERR_REP(' ','Too many pixels are being'//
     :                              ' used.',STATUS)
                  GOTO 9999
               ELSE
                  OFFSETS(NPIX)=VALUE
                  ELF(NPIX)=EXP(-TRAD/RRADIUS)
                  ELFSQ(NPIX)=ELF(NPIX)*ELF(NPIX)
               END IF

            END IF

 20      CONTINUE

 30   CONTINUE

*   Consider all pixels within the template. Obviously, a point off the
*   side of the image is not valid. To increase speed of operation only
*   the parts of the image where this is not the case are considered.
*   This leads to the generation of a border of bad-valued points around
*   the image. The border width is the same as the radius of the circular
*   template in use.
*
*   Two nearly identical routines are used to maximise execution speed.
      IF (USEALL) THEN

*      Perform calculations for situation where there is no
*      high count cutoff.
         PERC=0
         DO 100 Y=RADIUS+1,YMAX-RADIUS-1

*         Indicate that something is happening.
            IF (Y.EQ.NINT(Y/50.)*50) THEN
               PERC=NINT((Y-RADIUS)*100./(YMAX-2.*RADIUS)+1.)
               CALL MSG_SETI('PERC',PERC)
               CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
            END IF

*          Calculate the first component of the central pixel address.
            ADD2=(Y-1)*XMAX

            DO 90 X=RADIUS+1,XMAX-RADIUS-1

*            Calculate the full address of the central pixel address.
*            Also set the initial count for the number of usable pixel
*            found.
               N=0
               I=ADD2+X


*            Consider all points found earlier. Clear counters first.
               SUMSQ1=0.0
               SUMSQ2=0.0
               CORR=0.0
               DO 40 J=1,NPIX

*               Get the value of the point but only uses if
*               it is not defined as bad.
                  VALUE1=ARRAY(I+OFFSETS(J))
                  IF (VALUE1.NE.VAL__BADR) THEN

*                  Calculate sums of squares required.
                     TEMP=VALUE1-MODE
                     CORR=CORR+ELF(J)*TEMP
                     SUMSQ1=SUMSQ1+TEMP*TEMP
                     SUMSQ2=SUMSQ2+ELFSQ(J)

                  END IF

 40            CONTINUE

*            Calculate correlation if some non-bad pixels were found.
               IF (SUMSQ1.NE.0.0) THEN
                  ARRAY2(I)=MULT*CORR/SQRT(SUMSQ1*SUMSQ2)
               END IF

 90         CONTINUE

 100     CONTINUE

      ELSE

*      Perform calculations for the situation where there is a high
*      count cutofff defined by HIEST
         PERC=0
         DO 1100 Y=RADIUS+1,YMAX-RADIUS-1

*         Indicate that something is happening.
            IF (Y.EQ.NINT(Y/50.)*50) THEN
               PERC=NINT((Y-RADIUS)*100./(YMAX-2.*RADIUS)+1.)
               CALL MSG_SETI('PERC',PERC)
               CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
            END IF

*          Calculate the first component of the central pixel address.
            ADD2=(Y-1)*XMAX

            DO 1090 X=RADIUS+1,XMAX-RADIUS-1

*            Calculate the full address of the central pixel address.
*            Also set the initial count for the number of usable pixel
*            found.
               N=0
               I=ADD2+X

*            Consider all points found earlier. Clear counters first.
               SUMSQ1=0.0
               SUMSQ2=0.0
               CORR=0.0
               DO 1040 J=1,NPIX

*               Get the value of the point but only use if
*               it is not defined as bad and is not above the
*               cutoff value.
                  VALUE1=ARRAY(I+OFFSETS(J))

                  IF ((VALUE1.NE.VAL__BADR).AND.(VALUE1.LT.HIEST)) THEN

*                  Calculate sums of squares required.
                     TEMP=VALUE1-MODE
                     CORR=CORR+ELF(J)*TEMP
                     SUMSQ1=SUMSQ1+TEMP*TEMP
                     SUMSQ2=SUMSQ2+ELF(J)*ELF(J)

                  END IF

 1040          CONTINUE

*            Calculate correlation if some non-bad pixels were found.
               IF (SUMSQ1.NE.0.0) THEN
                  ARRAY2(I)=MULT*CORR/SQRT(SUMSQ1*SUMSQ2)
               END IF

 1090       CONTINUE

 1100    CONTINUE

      END IF

 9999 CONTINUE

      END
