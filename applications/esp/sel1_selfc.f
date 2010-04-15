      SUBROUTINE SEL1_SELFC(ELEMS,ARRAY,RADIUS,MODE,
     :                       XMAX,YMAX,USEALL,HIEST,STATUS,ARRAY2)
*+
*  Name:
*     SEL1_SELFC

*  Purpose:
*     Performs self-correlation calculations for a circular symmetrical
*     template of known width. It assumes a global mode value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEL1_GLOBAL(ELEMS,%VAL(POINT1(1)),RADIUS,MODE,
*                      XMAX,YMAX,USEALL,HIEST,STATUS,%VAL(POINT2(1)))

*  Description:
*     Given a source image, the routine generates an equivalent image of
*     similar size that shows, at each pixel, the degree of self-correlation
*     that was present in the circular area around the equivalent point
*     in the source image. Correlation may be both negative, indicating
*     that the object is below sky - probably due to poor flat fielding, or
*     may be positive indicating that there may be an object present.
*
*     The calculation is carried out as follows:
*
*     The value for each pixel of the output image is determined
*     as follows. An imaginary circle is drawn about the pixel
*     and all pixel pairs within that circle, that lie on opposite
*     sides of the centre from each other, are stored.
*
*     Each pair is then considered in turn and the modal count value
*     subtracted from each. They are then multiplied together. The
*     value resulting is added that found for all the pixel pairs.
*     When all the pixel pairs have been considered, the sum is
*     divided by the number of pixel pairs found and the square root
*     taken. The resultant value is some measure of the extent to
*     which points within that circle (about the current pixel)
*     are correlated.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     RADIUS = REAL (Given)
*        The radius of the circular template that is used to define
*        the region about a given pixel, that might contribute to the
*        self-correlation function value that will be inserted into
*        the equivalent pixel in ARRAY2.
*     MODE = REAL (Given)
*        The global mode value supplied by the user. Units counts.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     USEALL = LOGICAL (Given)
*        Flag indicating whether or not a pixel count cutoff threshold is
*        being used.
*     HIEST = REAL (Given)
*        The highest pixel count value that will be used in the analysis.
*        All pixels with count values above this will be ignored. Units
*        counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the self-correlation
*        'image'.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-MAY-1993
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'SEL_PAR'               ! SELFC constants

*  Arguments Given:
      LOGICAL USEALL                  ! Is a high count cut out being
                                      ! used?
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Radius of the size of object
                                      ! being considered
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      REAL ARRAY(ELEMS)               ! Array containing NDF data
      REAL HIEST                      ! The count value above which
                                      ! a pixel will be ignored if
                                      ! USEALL is true.
      REAL MODE                       ! Global image mode value

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! self-corrlelation results

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER ADD2                    ! Array element address
      INTEGER J                       ! Loop variable
      INTEGER N                       ! The number of valid pixel pairs
                                      ! that were present about a given
                                      ! origin pixel
      INTEGER NPIX                    ! The maximum number of pixel pairs
                                      ! about a given origin. Depends on the
                                      ! pixel size and filter width.
      INTEGER OFFSETS(SEL1__PIXN)     ! Address offsets for the
                                      ! pixels in one hemisphere
                                      ! of the circular area used
      INTEGER PERC                    ! Percentage of the calculations
                                      ! done so far
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index

      REAL R                          ! Temporary radius storage
      REAL TOTAL                      ! Summation of pixel pair
                                      ! calculation for a given origin
                                      ! pixel
      REAL VALUE                      ! Temporary storage
      REAL VALUES(SEL1__PIXN,2)       ! Values for pairs of pixels
                                      ! minus the mode value
      REAL VALUE1                     ! Temporary storage
      REAL VALUE2                     ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=VAL__BADR
 10   CONTINUE

*   Construct an array containing the memory address offsets of all the
*   pixels in one hemisphere of a circular area
*   relative to the memory address of the circle centre.
*
*   The hemisphere is chosen so that each pixel has a matching pixel
*   on the other side of the centre of the circle at a memory address offset
*   that is the same in magnitude, but with an opposite sign.
*
*   This allows the addresses of pairs of points, equidistantly
*   placed on opposite sides of the origin, to be found using only
*   one array.
      NPIX=0
      DO 30 X1=-RADIUS,RADIUS
         DO 20 Y1=-RADIUS,0

          R=SQRT(REAL(X1*X1+Y1*Y1))
*         Check that the pixel at pixel offsets X1,Y1 is
*         within a circle of the given radius and hence within the
*         required circular area.
            IF (R.LE.RADIUS) THEN

*            Calculate the memory address offset.
               VALUE=Y1*XMAX+X1

*            Ensure that only points in the correct hemisphere
*            are used. The point at the origin itself is ignored
*            to reduce the influence of cosmic rays.
               IF (VALUE.LT.0) THEN

*               Increment the address counter and store the
*               address offset.
                  NPIX=NPIX+1

*               Check that there are not too many pixels.
                  IF (NPIX.GT.SEL1__PIXN) THEN
                     STATUS=SAI__ERROR
                     CALL ERR_REP(' ','Too many pixels are being '//
     :                              'used.',STATUS)
                     GOTO 9999
                  ELSE

*                  Store array offset.
                     OFFSETS(NPIX)=VALUE

                  END IF

               END IF

            END IF

 20      CONTINUE

 30   CONTINUE

*   Consider all pixels where there is a legal pixel on the diametrically
*   opposite side of the circle centre. Obviously, a point off the side
*   of the image is not valid. This leads to the generation of a border of
*   bad-valued points around the image. The border width is the same as
*   the radius of the circular filter in use.
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
*            pairs found.
               N=0
               I=ADD2+X

*            Consider all points in the hemisphere of pixels generated
*            earlier.
               DO 40 J=1,NPIX

*               Get the values of the points and only use if
*               they are not defined as bad.
                  VALUE1=ARRAY(I+OFFSETS(J))
                  IF (VALUE1.NE.VAL__BADR) THEN

                     VALUE2=ARRAY(I-OFFSETS(J))
                     IF (VALUE2.NE.VAL__BADR) THEN

*                     Increment the number of usable pixels pairs
*                     found and retain their values.
                        N=N+1
                        VALUES(N,1)=VALUE1
                        VALUES(N,2)=VALUE2

                     END IF

                 END IF

 40            CONTINUE

*            Consider all the pixel pairs that were found.
               IF (N.GT.0) THEN

*               Multiply together the values of the pixels (-mode) on
*               opposite sides of the circle origin and sum over all
*               the usable pixels.
                  TOTAL=0.0
                  DO 50 J=1,N
                     TOTAL=TOTAL+(VALUES(J,1)-MODE)*(VALUES(J,2)-MODE)
 50               CONTINUE

*               Return the self-correlation magnitude with the
*               appropriate sign.
                  IF (TOTAL.GT.0.0) THEN
                     ARRAY2(I)=SQRT(TOTAL/REAL(N))
                  ELSE
                     ARRAY2(I)=-SQRT(ABS(TOTAL)/REAL(N))
                  END IF

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
*            pairs found.
               N=0
               I=ADD2+X

*            Consider all points in the hemisphere of pixels generated
*            earlier.
               DO 1040 J=1,NPIX

*               Get the values of the points and only use if
*               they are not defined as bad and are not above the
*               cutoff value.
                  VALUE1=ARRAY(I+OFFSETS(J))

                  IF ((VALUE1.NE.VAL__BADR).AND.(VALUE1.LT.HIEST)) THEN

                     VALUE2=ARRAY(I-OFFSETS(J))

                     IF ((VALUE2.NE.VAL__BADR).AND.
     :                   (VALUE2.LT.HIEST)) THEN

*                     Increment the number of usable pixels pairs
*                     found and retain their values.
                       N=N+1
                       VALUES(N,1)=VALUE1
                       VALUES(N,2)=VALUE2

                     END IF

                 END IF

 1040          CONTINUE

*            Consider all the pixel pairs that were found.
               IF (N.GT.0) THEN

*               Multiply together the values of the pixels (-mode) on
*               opposite sides of the circle origin and sum over all
*               the usable pixels.
                  TOTAL=0.0
                  DO 1050 J=1,N
                     TOTAL=TOTAL+(VALUES(J,1)-MODE)*(VALUES(J,2)-MODE)
 1050             CONTINUE

*               Return the self-correlation magnitude with the
*               appropriate sign.
                  IF (TOTAL.GT.0.0) THEN
                     ARRAY2(I)=SQRT(TOTAL/REAL(N))
                  ELSE
                     ARRAY2(I)=-SQRT(ABS(TOTAL)/REAL(N))
                  END IF

               END IF

 1090       CONTINUE

 1100    CONTINUE

      END IF

 9999 CONTINUE

      END
