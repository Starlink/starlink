      SUBROUTINE SKE1_LOCAL(MULT,ELEMS,ARRAY,RADIUS,
     :                  XMAX,YMAX,STATUS,ARRAY2)
*+
*  Name:
*     SKE1_LOCAL

*  Purpose:
*     Performs skewness calculations for a circular symmetrical
*     template of known width. The mode value used is the average pixel
*     value found within the template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SKE1_LOCAL(MULT,ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
*                      XMAX,YMAX,STATUS,%VAL(POINT2(1)))

*  Description:
*     Given a source image, the routine generates an equivalent image of
*     similar size that shows, at each pixel, the degree of skewness
*     that was present in the circular area around the equivalent point
*     in the source image. Skewness should normally be near zero for
*     a Gaussian (Normal) distribution. Large deviations from this
*     suggest regions either containing objects or flatfielding flaws.
*
*     The calculation is carried out as follows:
*
*     The value for each pixel of the output image is determined
*     as follows. An imaginary circle is drawn about the pixel
*     and the count values for all pixels within that circle,
*     are stored.
*
*     The count values are then used to calculate the skewness using
*     a local mode value.

*  Arguments:
*     MULT = REAL (Given)
*        The multiplying value applied to all the skewness values
*        generated.
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     RADIUS = REAL (Given)
*        The radius of the circular template that is used to define
*        the region about a given pixel, that might contribute to the
*        skewness value that will be inserted into
*        the equivalent pixel in ARRAY2.
*     MODE = REAL (Given)
*        The global mode value supplied by the user. Units counts.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the skewness 'image'.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-Oct-1992
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'SKE_PAR'               ! SKEW constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Radius of the size of object
                                      ! being considered
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      REAL ARRAY(ELEMS)               ! Array containing NDF data
      REAL MODE                       ! Global image mode value
      REAL MULT                       ! Multiplying factor

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! skewness results

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER ADD2                    ! Array element address
      INTEGER J                       ! Loop variable
      INTEGER N                       ! The number of valid pixel
                                      ! that were present about a given
                                      ! origin pixel
      INTEGER NPIX                    ! The maximum number of pixels
                                      ! about a given origin. Depends on the
                                      ! pixel size and filter width.
      INTEGER OFFSETS(SKE1__PIXN)     ! Address offsets for the
                                      ! pixels in the template
      INTEGER PERC                    ! Percentage of the calculations
                                      ! done so far
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index

      REAL RN                         ! Number of pixels used
      REAL SKEW                       ! Temporary skewness sum
      REAL VALUE                      ! Temporary storage
      REAL VALUES(SKE1__PIXN)         ! Values of pixel counts
      REAL VALUE1                     ! Temporary storage
      REAL VARI                       ! Temporary variance sum
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=VAL__BADR
 10   CONTINUE

*   Construct an array containing the memory address offsets of all the
*   pixels in the circular template relative to the memory address of
*   the circle centre.
*
      I=RADIUS*RADIUS
      NPIX=0
       DO 30 Y1=-RADIUS,RADIUS
         DO 20 X1=-RADIUS,RADIUS

*         Check that the pixel at pixel offsets X1,Y1 is
*         within a circle of the given radius and hence within the
*         required circular area.
            IF (I.GT.(X1*X1+Y1*Y1)) THEN

*            Calculate the memory address offset.
               VALUE=Y1*XMAX+X1

*            Increment the address counter and store the
*            address offset.
               NPIX=NPIX+1

*            Check that there are not too many pixels.
               IF (NPIX.GT.SKE1__PIXN) THEN
                  STATUS=SAI__ERROR
                  CALL ERR_REP(' ','Too many pixels are being'//
     :                              'used.',STATUS)
                  GOTO 9999
               ELSE
                  OFFSETS(NPIX)=VALUE
               END IF

            END IF

 20      CONTINUE

 30   CONTINUE

*   Consider all pixels within the template. Obviously, a point off the side
*   of the image is not valid. To increase the speed of operation only parts
*   of the image where this is not the case are used. This leads to the
*   generation of a border of bad-valued points around the image.
*   The border width is the same as the radius of the circular filter in
*   use.
      PERC=0
      DO 100 Y=RADIUS+1,YMAX-RADIUS-1

*      Indicate that something is happening.
         IF (Y.EQ.NINT(Y/50.)*50) THEN
            PERC=NINT((Y-RADIUS)*100./(YMAX-2.*RADIUS)+1.)
            CALL MSG_SETI('PERC',PERC)
            CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
         END IF

*       Calculate the first component of the central pixel address.
         ADD2=(Y-1)*XMAX

         DO 90 X=RADIUS+1,XMAX-RADIUS-1

*         Calculate the full address of the central pixel address.
*         Also set the initial count for the number of usable pixel
*         found.
            N=0
            I=ADD2+X

*         Consider all points found earlier.
            DO 40 J=1,NPIX

*            Get the value of the point but only use if
*            it was not defined as bad.
               VALUE1=ARRAY(I+OFFSETS(J))
               IF (VALUE1.NE.VAL__BADR) THEN

*               Increment the number of usable pixels
*               found and retain the latest value.
                  N=N+1
                  VALUES(N)=VALUE1

               END IF

 40         CONTINUE

*         Consider all the pixels that were found.
            IF (N.GT.1) THEN

*            Calculate the local mode of the pixels that
*            were acceptable.
               MODE=0.0
               DO 41 J=1,N
                  MODE=MODE+VALUES(J)
 41            CONTINUE
               MODE=MODE/REAL(N)

*               Calculate the skewness and variance totals.
                  SKEW=0.0
                  VARI=0.0
                  DO 50 J=1,N
                     VALUE1=VALUES(J)-MODE
                     VARI=VARI+VALUE1*VALUE1
                     SKEW=SKEW+VALUE1*VALUE1*VALUE1
 50               CONTINUE

*               Return the skewness value.
                  IF (VARI.GT.0.0) THEN
                     RN=REAL(N)
                     ARRAY2(I)=MULT*SKEW/(RN*SQRT(VARI/(RN-1))**3)
                  END IF

            END IF

 90      CONTINUE

 100  CONTINUE

 9999 CONTINUE

      END
