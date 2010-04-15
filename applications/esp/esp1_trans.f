


      SUBROUTINE ELF1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)
*+
*  Name:
*     ELF1_TRANS

*  Purpose:
*     Transfer data from the mapped NDF to a dynamic memory array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)

*  Description:
*      Copies data from the currently mapped source NDF 'DATA' array and
*      transfers it into the PSX dynamic memory allocated for temporary
*      storage. All manipulation is then carried out on that. The routine
*      allows it to be refreshed whenever a new profile is required.
*

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF.
*     ARRAY0(ELEMS) = REAL (Given)
*        Array containing the mapped NDF data region.
*     ARRAY1(ELEMS) = REAL (Returned)
*        Array into which the mapped NDF will be transfered.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     22-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the NDF array
      REAL ARRAY0(ELEMS)              ! The mapped NDF data array

*  Arguments Returned:
      REAL ARRAY1(ELEMS)              ! Dynamic array into which the
                                      ! mapped NDF data region is copied

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Transfer data from the mapped NDF array to the dynamic memory array.
      DO 10 I=1,ELEMS
         ARRAY1(I)=ARRAY0(I)
 10   CONTINUE

 9999 CONTINUE

      END




      SUBROUTINE ELP1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)
*+
*  Name:
*     ELP1_TRANS
*
*  Purpose:
*     Transfer data from the mapped NDF to a dynamic memory array.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*      CALL ELP1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)
*
*  Description:
*      Copies data from the currently mapped source NDF 'DATA' array and
*      transfers it into the PSX dynamic memory allocated for temporary
*      storage. All manipulation is then carried out on that. The routine
*      allows it to be refreshed whenever a new profile is required.
*
*
*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF.
*     ARRAY0(ELEMS) = REAL (Given)
*        Array containing the mapped NDF data region..
*     ARRAY1(ELEMS) = REAL (Returned)
*        Array into which the mapped NDF will be transfered.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*
*  History:
*     22-FEB-1993 (GJP)
*       (Original version)
*
*  Bugs:
*     None known.
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the NDF array
      REAL ARRAY0(ELEMS)              ! The mapped NDF data array

*  Arguments Returned:
      REAL ARRAY1(ELEMS)              ! Dynamic array into which the
                                      ! mapped NDF data region is copied

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Transfer data from the mapped NDF array to the dynamic memory array.
      DO 10 I=1,ELEMS
         ARRAY1(I)=ARRAY0(I)
 10   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE GAU1_TRANS(NSOUR,BACK,SIGMA,NSIGMA,ELEMS,ARRAY1,
     :                      ARRAY2,XCO,YCO,RLIM,PRANGE,
     :                      UPIX,GUESS,STATUS)
*+
*  Name:
*     GAU1_TRANS

*  Purpose:
*     Establish those parts of the image that will contribute.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_TRANS(NSOUR,BACK,SIGMA,NSIGMA,ELEMS,ARRAY1,ARRAY2,
*                      XCO,YCO,RLIM,PRANGE,UPIX,GUESS,STATUS)

*  Description:
*      An image is constructed on which only the pixels which
*      will contribute to the test summation are present
*      as non-zero values.
*
*      This is done by setting the whole image to zero and then
*      making a pixel non-zero if one of the sources contribute
*      to it.
*
*      The image is then scanned through and those pixels that
*      found to contribute set to the value in the input image.

*  Arguments:
*     NSOUR = INTEGER (Given)
*        Number of sources.
*     BACK = REAL (Given)
*        The sky background count.
*     SIGMA = REAL (Given)
*        Standard deviation of the background.
*     NSIGMA = REAL (Given)
*        Number of std dev above sky at which pixels are signicant
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF.
*     ARRAY1(ELEMS) = REAL (Given)
*        Array containing the mapped NDF data region..
*     ARRAY2(ELEMS) = REAL (Given and Returned)
*        Array into which the mapped NDF will be transfered.
*     XCO(10,2) = REAL (Given)
*        X co-ordinate of the source.
*     YCO(10,2) = REAL (Given)
*        Y co-ordinate of the source.
*     RLIM(10) = REAL (Given)
*        Radius of the source.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image.
*     UPIX = INTEGER (Returned)
*        The number of good pixels.
*     GUESS(10,7) = REAL (Returned)
*        Current parameter estimates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     22-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the NDF array
      INTEGER NSOUR                   ! Number of sources
      INTEGER PRANGE(2)               ! Width of the image
      REAL ARRAY1(ELEMS)              ! The mapped NDF data array
      REAL BACK                       ! Sky background count
      REAL NSIGMA                     ! Number of sdev above sky
      REAL SIGMA                      ! Std. dev. of background
      REAL XCO(10,2)                  ! X coordinate of source
      REAL YCO(10,2)                  ! Y coordinate of source
      REAL RLIM(10)                   ! Radius of source

*  Arguments Returned:
      INTEGER UPIX                    ! Number of pixels used
      REAL ARRAY2(ELEMS)              ! Dynamic array into which the
                                      ! mapped NDF data region is copied
      REAL GUESS(10,7)                ! Initial source parameters

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER ADDR                    ! Image location pointer
      INTEGER ANG                     ! The angle
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER K                       ! Temporary loop variable
      INTEGER N                       ! Temporary counter
      INTEGER R                       ! Radius limit
      INTEGER X                       ! Pixel co-ordinate
      INTEGER Y                       ! Pixel co-ordinate

      REAL ANGLE                      ! Angle
      REAL MXP                        ! Comparison limit
      REAL PATR(20000)                ! Pixel value at a given radius
      REAL RADIUS                     ! Distance from source origin
      REAL RMAX                       ! MAximum radius
      REAL R1                         ! Temporary radius
      REAL R2                         ! Temporary radius

      REAL SQMAX                      ! Radius squared
      REAL THRESH                     ! Good/bad pixel threshold
      REAL TOTAL                      ! Summation
      REAL VALUE                      ! Pixel value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look around the centre of the object. Find the brightest non-bad pixel.
      DO 5 I=1,NSOUR

*   Set the comparison pixel value to low.
         MXP=-1E+20

*      Look around the current centre
         DO 7 Y=INT(YCO(I,1))-7,INT(YCO(I,1))+7
            DO 8 X=INT(XCO(I,1))-7,INT(XCO(I,1))+7

*            Check that the pixel is within the image.
               IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.(Y.GE.1)
     :            .AND.(Y.LE.PRANGE(2))) THEN

*               Get the pixel address.
                  ADDR=(Y-1)*PRANGE(1)+X

*               Get the pixel value.
                  VALUE=ARRAY1(ADDR)

*               Check that it was not a BAD pixel.
                  IF (VALUE.NE.VAL__BADR) THEN

*                  Check to see if its brightest.
                     IF(VALUE.GT.MXP) MXP=VALUE

                 END IF

               END IF

 8          CONTINUE
 7       CONTINUE

*      Normalise the peak value.
         IF (MXP.GT.-1E+19) THEN
            GUESS(I,4)=MXP-BACK
         ELSE
            GUESS(I,4)=-1E20
         END IF

 5    CONTINUE

*   Look at sources for which the radius has been defined as BAD and
*   try to determine a sensible estimate for RLIM.
      DO 2 I=1,NSOUR

*      Only work for undefined radii.
         IF(RLIM(I).EQ.VAL__BADR) THEN

            TOTAL=0.0
            N=0
            DO 4 ANG=1,359,4

*            Find maximum plausible radius.
               R1=ABS(PRANGE(1)-XCO(I,1))
               R2=ABS(PRANGE(2)-YCO(I,1))
               RMAX=MAX(R1,R2)

*            Look outward from it until the first bad pixel or
*            first pixels below .75 the brightness of the core are found.
               VALUE=GUESS(I,4)
               R=0.0
               DO WHILE ((VALUE.NE.VAL__BADR).AND.
     :                   (VALUE.GT.GUESS(I,4)*.75).AND.
     :                   (R.LE.RMAX))

*               Calculate the pixel position.
                  ANGLE=REAL(ANG)*3.1415926*2./360.
                  X=XCO(I,1)+R*COS(ANGLE)
                  Y=YCO(I,1)+R*SIN(ANGLE)

*               Avoid choosing an off image origin.
                  IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.
     :                (Y.GE.1).AND.(Y.LE.PRANGE(2))) THEN

*                  Calc the array offset.
                     ADDR=(INT(Y)-1)*PRANGE(1)+INT(X)

*                  Get the pixel value.
                     VALUE=ARRAY1(ADDR)-BACK

*                  Store the value.
                     PATR(INT(R)+1)=VALUE

                  END IF

*               Increment the radius.
                  R=R+1.

               END DO

*            Look through the array and find the point at which the pixel
*            value at a given radius started to increase again.
               IF (R.GT.2) THEN

*               Look through the array from outside edge looking for
*               the last time the outer pixel value was greater than
*               the inner pixel value.
                  J=INT(R)+1
                  DO 6 K=INT(R)+1,3,-1
                     IF(PATR(K).GT.PATR(K-2)+SIGMA) J=K
 6                CONTINUE
                  R=REAL(J)

               END IF

*            Increment the counter.
               TOTAL=TOTAL+R
               N=N+1

 4          CONTINUE

*         Calculate the average radius.
            RLIM(I)=TOTAL/REAL(N)

*         Correct the radius of the sources if necessary
            IF (RLIM(I).LT.2.) RLIM(I)=2.

*         Display the value used.
            IF (I.EQ.1) CALL MSG_BLANK(STATUS)
            CALL MSG_FMTR('X','F6.1',XCO(I,1))
            CALL MSG_FMTR('Y','F6.1',YCO(I,1))
            CALL MSG_FMTR('R','F6.1',RLIM(I))
            CALL MSG_OUT(' ','Radius of source at ^X, ^Y '//
     :                ' was set to ^R pixels.',STATUS)

         END IF

 2    CONTINUE

*   Set output image pixels to zeroes.
      DO 10 I=1,ELEMS
         ARRAY2(I)=0.0
 10   CONTINUE

*   For each source in turn mark pixels that are near a source.
      DO 105 I=1,NSOUR

*      Set the threshold.
         IF ((SIGMA*NSIGMA.GT.GUESS(I,4)/2.).AND.
     :       (GUESS(I,4).GT.-1E19)) THEN
            THRESH=GUESS(I,4)/2.+BACK
         ELSE
            THRESH=NSIGMA*SIGMA+BACK
         END IF

*      Set radius limit.
         R=RLIM(I)+1.

*      Set maximum radius limit.
         SQMAX=REAL(R)**2

*      All pixels within range of the origin.
         DO 30 Y=INT(YCO(I,1)-R),INT(YCO(I,1)+R)
            DO 30 X=INT(XCO(I,1)-R),INT(XCO(I,1)+R)

*            Avoid choosing an off image origin.
               IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.
     :             (Y.GE.1).AND.(Y.LE.PRANGE(2))) THEN

*               Find the distance from the source origin.
                  RADIUS=(REAL(X)-XCO(I,1))**2 +
     :                   (REAL(Y)-YCO(I,1))**2

*               Abort if radius too big.
                  IF (RADIUS.LT.SQMAX) THEN

*                  Calc the array offset.
                     ADDR=(Y-1)*PRANGE(1)+X

*                  Get the pixel value.
                     VALUE=ARRAY1(ADDR)

*                  Only allow pixels above the threshold.
                     IF(VALUE.GT.THRESH) ARRAY2(ADDR)=ARRAY2(ADDR)+1


                  END IF

               END IF

 20         CONTINUE

 30      CONTINUE

 105  CONTINUE

*   Set pixels to zero if they do not contribute to any of the sources
*   or are bad in the source image. Increment a good pixel counter.
      UPIX=0
      DO 1000 I=1,ELEMS

*      Use pixels near the sources.
         IF((ARRAY2(I).NE.0.0).AND.(ARRAY1(I).NE.VAL__BADR)) THEN
            ARRAY2(I)=ARRAY1(I)-BACK
            UPIX=UPIX+1
         ELSE
            ARRAY2(I)=VAL__BADR
         END IF

 1000 CONTINUE

*   Nreat spacing.
      CALL MSG_BLANK(STATUS)

 9999 CONTINUE

      END


      SUBROUTINE SEC1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)
*+
*  Name:
*     SEC1_TRANS

*  Purpose:
*     Transfer data from the mapped NDF to a dynamic memory array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)

*  Description:
*      Copies data from the currently mapped source NDF 'DATA' array and
*      transfers it into the PSX dynamic memory allocated for temporary
*      storage. All manipulation is then carried out on that. The routine
*      allows it to be refreshed whenever a new profile is required.
*

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF.
*     ARRAY0(ELEMS) = REAL (Given)
*        Array containing the mapped NDF data region..
*     ARRAY1(ELEMS) = REAL (Returned)
*        Array into which the mapped NDF will be transfered.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     22-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the NDF array
      REAL ARRAY0(ELEMS)              ! The mapped NDF data array

*  Arguments Returned:
      REAL ARRAY1(ELEMS)              ! Dynamic array into which the
                                      ! mapped NDF data region is copied

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Transfer data from the mapped NDF array to the dynamic memory array.
      DO 10 I=1,ELEMS
         ARRAY1(I)=ARRAY0(I)
 10   CONTINUE

 9999 CONTINUE

      END
