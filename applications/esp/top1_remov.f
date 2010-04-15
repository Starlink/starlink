      SUBROUTINE TOP1_REMOV(ELEMS,ARRAY,NOISE,BACK,SIGMA,NSIGMA,RADIUS,
     :                      XMAX,YMAX,ARRAY2,STATUS)
*+
*  Name:
*     TOP1_REMOV

*  Purpose:
*     Remove pixels immediately surrounding bright pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TOP1_REMOV(ELEMS,ARRAY,NOISE,BACK,SIGMA,NSIGMA,RADIUS,
*                     XMAX,YMAX,ARRAY2,STATUS)

*  Description:
*     Given a source image, the routine generates an equivalent image of
*     wherein all pixels within a circular radius (RADIUS) about bright
*     pixels (ie count exceeding COUNT) have been set to bad.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     NOISE = LOGICAL (Given)
*        Whether or not a random value is to be assigned to points set
*        to the bad value.
*     BACK = REAL (Given)
*        The background count for the image. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the sky background count. Units counts.
*     NSIGMA = REAL (Given)
*        The number of standard deviations above sky at which the
*        pixel cutoff occurs.
*     RADIUS = INTEGER (Given)
*        The radius of the circular template that is used to define
*        the bad region about bright pixels.
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the returned 'image'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     27-Jan-1993
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'TOP_PAR'               ! TOPPED constants

*  Arguments Given:
      LOGICAL NOISE                   ! Random noise instead of bad
                                      ! points flag
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER RADIUS                  ! Radius of the masking template
                                      ! being used
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      REAL ARRAY(ELEMS)               ! Array containing NDF data
      REAL BACK                       ! Background sky count value
      REAL NSIGMA                     ! Pixel brightness cutoff value
      REAL SIGMA                      ! Standard deviation of the background count

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! output image

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER ADD2                    ! Array element address
      INTEGER J                       ! Loop variable
      INTEGER NPIX                    ! The maximum number of pixels
                                      ! about a given origin. Depends on the
                                      ! pixel size and filter width.
      INTEGER OFFSETS(3,TOP1__PIXN)   ! Address offsets for the
                                      ! pixels in the circular
                                      ! template used
      INTEGER PERC                    ! Percentage of the calculations
                                      ! done so far
      INTEGER TEMP                    ! temporary variable
      INTEGER X                       ! Pixel x axis index
      INTEGER X1                      ! Pixel x axis index offset
      INTEGER Y                       ! Pixel y axis index
      INTEGER Y1                      ! Pixel y axis index
      REAL LIMIT                      ! The count value above which
                                      ! a pixel will be erased.
      REAL VALUE                      ! Temporary storage

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Calculate the threshold value to be used.
      LIMIT=BACK+SIGMA*NSIGMA

*   Set up the initial value for the output array.
      DO 10 I=1,ELEMS
         ARRAY2(I)=ARRAY(I)
 10   CONTINUE

*   Construct an array containing the memory address offsets of all the
*   pixels in the template relative to the memory address of the circle
*   centre.
*
      I=RADIUS*RADIUS
      NPIX=0
      DO 30 X1=-RADIUS,RADIUS
         DO 20 Y1=-RADIUS,RADIUS

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
               IF (NPIX.GT.TOP1__PIXN) THEN
                  STATUS=SAI__ERROR
                  CALL ERR_REP(' ','Too many pixels are being'//
     :                              'used.',STATUS)
                  GOTO 9999
               ELSE
                  OFFSETS(1,NPIX)=VALUE
                  OFFSETS(2,NPIX)=X1
                  OFFSETS(3,NPIX)=Y1
               END IF

            END IF

 20      CONTINUE

 30   CONTINUE

*   Consider all pixels within the image. Obviously, a point off the
*   side of the image is not valid.

*   Perform calculations for the situation where there is a high
*   count cutofff defined by HIEST
      PERC=0
      DO 1100 Y=1,YMAX

*      Indicate that something is happening.
         IF (Y.EQ.NINT(Y/50.)*50) THEN
            PERC=NINT(Y*100./YMAX)
            CALL MSG_SETI('PERC',PERC)
            CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
         END IF

*       Calculate the first component of the central pixel address.
         ADD2=(Y-1)*XMAX

         DO 1090 X=1,XMAX

*         Calculate the full address of the central pixel address.
            I=ADD2+X

*         Check to see if the pixel value is higher than LIMIT.
*         If it is, set the surrounding points (offsets calculated
*         earlier) to bad.
            IF (ARRAY(I).GT.LIMIT) THEN

*            Consider all surrounding points.
               ARRAY2(I)=VAL__BADR
               DO 1040 J=1,NPIX

*               Check that the pixel is not off the edge of
*               the picture.
                  TEMP=X+OFFSETS(2,J)

                  IF ((TEMP.GT.0).AND.(TEMP.LT.XMAX+1)) THEN

*                  Check that the pixel is not off the edge of
*                  the picture.
                     TEMP=Y+OFFSETS(3,J)
                     IF ((TEMP.GT.0).AND.(TEMP.LT.YMAX+1)) THEN

*                     Set the pixel to bad.
                        ARRAY2(I+OFFSETS(1,J))=VAL__BADR

                     END IF

                  ENDIF

 1040          CONTINUE

            END IF

 1090    CONTINUE

 1100 CONTINUE

*   Indicate how many pixels have had their values modified.
      TEMP=0
      DO 2000 I=1,ELEMS
         IF (ARRAY2(I).EQ.VAL__BADR) TEMP=TEMP+1
 2000 CONTINUE

      CALL MSG_BLANK(STATUS)
      VALUE=REAL(TEMP)/REAL(ELEMS)*100.
      CALL MSG_SETR('VALUE',VALUE)
      CALL MSG_BLANK(STATUS)

*   Look through image array. If a bad data point is found then it is assigned
*   a random value from the normal distribution defined by the background and
*   standard deviation values supplied. Only done if NOISE is true.
      IF (NOISE) THEN

*      Initialise the random number generator.
         CALL TOP1_RAND1(2001,VALUE,STATUS)

*      Look through all the image points.
         DO 3000 I=1,ELEMS

*         Replace a bad point with noise.
            IF (ARRAY2(I).EQ.VAL__BADR) THEN
               CALL TOP1_RAND2(0,VALUE,STATUS)
               ARRAY2(I)=BACK+SIGMA*VALUE
            END IF

 3000    CONTINUE

         CALL MSG_OUT(' ','Percentage of pixels reset: ^VALUE',STATUS)
         CALL MSG_BLANK(STATUS)

      ELSE

         CALL MSG_OUT(' ','Percentage of pixels bad: ^VALUE',STATUS)
         CALL MSG_BLANK(STATUS)

      END IF

 9999 CONTINUE

      END
