      SUBROUTINE ELF1_FIND(ELEMS,ARRAY,PRANGE,XO,YO,SEARCH,
     :                     LOW,HIGH,COUNTR,XE,YE,PCV,STATUS)
*+
*  Name:
*     ELF1_FIND

*  Purpose:
*     Locates pixels on the image that are within the pixel brightness
*     range LOW to HIGH.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF_FIND(ELEMS,ARRAY,PRANGE,XO,YO,SEARCH,
*                   LOW,HIGH,COUNTR,XE,YE,PCV,STATUS)

*  Description:
*      A circular area of the image (radius SEARCH) centred on the
*      data co-ordinates XO and YO is searched for non-bad pixels within the
*      pixel count range LOW to HIGH. If too many are found (number exceeds
*      storage array size) the range of pixel counts permitted is reduced
*      by a factor of two. If too many are still found only a fraction
*      of those found are retained.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image array.
*     ARRAY(ELEMS) = REAL (Given)
*        The image array.
*     PRANGE(2) = INTEGER (Given)
*        Length of the image x and y axes. Units pixels.
*     XO = REAL (Given)
*        X co-ordinate of the pixel about which the search is centred.
*     YO = REAL (Given)
*        Y co-ordinate of the pixel about which the search is centred.
*     SEARCH = REAL (Given)
*        Radius of the circle within which the pixels may be taken.
*        Units pixels.
*     LOW = REAL (Given)
*        Lower pixel count limit. Units counts.
*     HIGH = REAL (Given)
*        Upper pixel count limit. Unit counts.
*     COUNTR = INTEGER (Returned)
*        Number of pixels within the pixel count range LOW to HIGH.
*     XE(ELF_PIXEL) = REAL (Returned)
*        X co-ordinates of pixels found within the required range.
*     YE(ELF_PIXEL) = REAL (Returned)
*        Y co-ordinates of pixels found within the required range.
*     PCV(ELL_PIXEL) = REAL (Returned)
*        Brightness of the pixels for which co-ordinates have been retained.
*        Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER PRANGE(2)               ! Size of the x and y dimensions of the
      REAL ARRAY(ELEMS)               ! Image array
      REAL HIGH                       ! Higher brightness limit
      REAL LOW                        ! Lower brightness limit
      REAL SEARCH                     ! Radius within which the
                                      ! pixels should be taken (relative
                                      ! to XO and YO
      REAL XO                         ! X co-ord of the galaxy centre
      REAL YO                         ! Y co-ord of the galaxy centre
                                      ! image
*  Arguments Returned:
      INTEGER COUNTR                  ! Number of isophote pixels selected
      REAL PCV(ELF__PIXEL)            ! Pixel brightness
      REAL XE(ELF__PIXEL)             ! X co-ord of the selected pixels
      REAL YE(ELF__PIXEL)             ! Y co-ord of the selected pixels


*  Arguments Given and Returned:

*  Local variables:

      INTEGER FINISH                  ! Finished selecting some pixels flag
      INTEGER HIX                     ! Upper x limit of the image
                                      ! region to be considered
      INTEGER HIY                     ! Upper y limit of the image
                                      ! region to be considered
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Temporary storage
      INTEGER LOX                     ! Low x limit of the image
                                      ! region to be considered
      INTEGER LOY                     ! Low y limit of the image
                                      ! region to be considered
      INTEGER NUMBER                  ! Number of points found within the
                                      ! desired isophote
      INTEGER X                       ! X co-ordinate being considered
      INTEGER Y                       ! Y co-ordinate being considered
      REAL FRACT                      ! Fraction of the points found for the
                                      ! current isophote that may be used
                                      ! (due to array size limit)
      REAL MEAN                       ! Mean value of high and lOW
      REAL RND                        ! A random number
      REAL RSQLIM                     ! Upper limit for RSQ
      REAL V                          ! Pixel count value
      REAL VALUE                      ! Temporary value
      REAL RANGE                      ! Temporary value
      REAL XS                         ! Temporary value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the x axis limits for the part of the image to be examined.
      LOX=XO-SEARCH
      HIX=XO+SEARCH
      IF (LOX.LT.1) LOX=1
      IF (HIX.GT.PRANGE(1)) HIX=PRANGE(1)

*   Set the y axis limits for the part of the image to be examined.
      LOY=YO-SEARCH
      HIY=YO+SEARCH
      IF (LOY.LT.1) LOY=1
      IF (HIY.GT.PRANGE(2)) HIY=PRANGE(2)

*   Look at all the pixels within the circular radius SEARCH.
      RSQLIM=SEARCH*SEARCH
      FINISH=0
      DO WHILE (FINISH.EQ.0)

*      Find number of pixels with values in the range required.
         NUMBER=0
         DO 10 X=LOX,HIX

*         Avoid recalculating this value inside Y loop.
            XS=(X-XO)*(X-XO)

*         Avoid next loop if necessary.
            IF (XS.LT.RSQLIM) THEN

               DO 20 Y=LOY,HIY

*               Check that the pixel is within the specified distance of the
*               galaxy centre.
                  IF (XS+(Y-YO)*(Y-YO).LT.RSQLIM) THEN

*                  Get the pixel value.
                     V=ARRAY((Y-1)*PRANGE(1)+X)

*                  Check that it was not a BAD pixel.
                     IF (V.NE.VAL__BADR) THEN

*                     Check it is within the required range.
                        IF (V.GE.LOW) THEN
                           IF (V.LE.HIGH) NUMBER=NUMBER+1
                        END IF

                     END IF

                  END IF

 20            CONTINUE

            END IF

 10      CONTINUE

*      Narrow the range if too many were found.
         IF (NUMBER.GT.ELF__PIXEL) THEN

*         Set range and see if new range is too narrow.
            RANGE=(HIGH-LOW)/3.

            IF (RANGE.GT.0.5) THEN

*            Narrow the bounds.
               MEAN=(HIGH+LOW)/2.
               LOW=MEAN-RANGE
               HIGH=MEAN+RANGE

           ELSE

*            Set flag to show that there is no point narrowing the range
*            further.
               FINISH=2

            END IF

         ELSE

            FINISH=1

         END IF

      END DO

*   Calculate the fraction of the points within the brightness range required
*   that are to be retained.
      IF (FINISH.EQ.1) THEN
         FRACT=1.
      ELSE
         FRACT=REAL(ELF__PIXEL)/REAL(NUMBER)/2.
      END IF

*   Initialise the pixel COUNTR.
      COUNTR=0

*   Look through all columns.
      X=LOX
      DO WHILE (X.LE.HIX)

*      Avoid calculating this for all values of Y.
         XS=(X-XO)*(X-XO)

*         Avoid next loop if necessary.
            IF (XS.LT.RSQLIM) THEN

*         Look through all rows.
            Y=LOY
            DO WHILE (Y.LE.HIY)

*            Check that the pixel is within the specified distance of the
*            galaxy centre.
               IF (XS+(Y-YO)*(Y-YO).LT.RSQLIM) THEN

*               Get the current pixel value.
                  V=ARRAY((Y-1)*PRANGE(1)+X)

*               Check that it is within bounds.
                  IF (V.GE.LOW) THEN
                     IF (V.LE.HIGH) THEN

*                     Check that it was not a BAD pixel.
                        IF (V.NE.VAL__BADR) THEN

*                        Ensure that the right proportion of the acceptable
*                        pixels are retained.
                           CALL ELF1_RAND(1,0,RND,STATUS)
                           IF (RND.LT.FRACT) THEN

*                           Increment COUNTR.
                              COUNTR=COUNTR+1
                              IF (COUNTR.GT.ELF__PIXEL)
     :                            COUNTR=ELF__PIXEL

*                           Store the co-ordinates of the pixel.
                              XE(COUNTR)=X
                              YE(COUNTR)=Y
                              PCV(COUNTR)=V

                           END IF

                        END IF

                     END IF

                  END IF

               END IF

*            Increment the column COUNTR.
               Y=Y+1

            END DO

         END IF

*      Increment the row COUNTR.
         X=X+1

      END DO

*   Scramble the values so that they are not in X/Y order.
      DO 40 I=1,COUNTR

*      Swap the Ith and Jth pixel data.
         CALL ELF1_RAND(1,0,RND,STATUS)
         J=INT(RND*COUNTR+1)

*      X co-ordinates of pixels.
         VALUE=XE(J)
         XE(J)=XE(I)
         XE(I)=VALUE

*      Y co-ordinate of pixels.
         VALUE=YE(J)
         YE(J)=YE(I)
         YE(I)=VALUE

*      Pixel to be used flag.
         VALUE=PCV(J)
         PCV(J)=PCV(I)
         PCV(I)=VALUE

 40   CONTINUE

 9999 CONTINUE

      END
