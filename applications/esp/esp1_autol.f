

      SUBROUTINE ELF1_AUTOL(ELEMS,PRANGE,BACK,ARRAY,XCO,YCO,STATUS)

*+
*  Name:
*     ELF1_AUTOL

*  Purpose:
*     Looks at the pixels immediately surrounding the user defined galaxy
*     centre and then chooses a location that provides a good estimate
*     for its position.
*
*     Two methods are available. Centroiding and weighted mean.
*     Choice is made via parameter AUTOLT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_AUTOL(ELEMS,PRANGE,BACK,ARRAY,XCO,YCO,STATUS)

*  Description:
*     Examines the region of the image immediately surrounding the user
*     input value and calculates the centroid or weighted mean
*     co-ordinates.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     BACK = REAL (Given)
*        Background count value.
*     ARRAY(ELEMS) = REAL (Given and Returned)
*        The image array. Contains the count values for all the image pixels.
*        Units counts.
*     XCO = REAL (Given and Returned)
*        X index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     YCO = REAL (Given and Returned)
*        Y index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status value.

*  Authors:
*     GJP: Grant Privett (STARLINK)


*  History:
*     16-MAR-1993 (GJP)
*     (Original version)
*     14-FEB-1996 (GJP)
*     Added centroiding and Linux corrections.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of elements/pixels in the
                                      ! image array
      INTEGER PRANGE(2)               ! Length of the X and Y axes of the
                                      ! image
      REAL BACK                       ! Background count

*  Arguments Returned:
      INTEGER FLAG                    ! It was not possible to find the
                                      ! central pixel value flag

*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)               ! The image array contains the count
                                      ! values for all the image pixels
      REAL XCO                        ! X index of the galaxy centre/origin
                                      ! supplied by the user
      REAL YCO                        ! Y index of the galaxy centre/origin
                                      ! supplied by the user

*  Local variables:
      LOGICAL AUTOLT                  ! Type of estimation to be used
      INTEGER ADDRES                  ! Array address of the element
                                      ! corresponding to pixel indices X,Y
      INTEGER XMAX                    ! Highest image pixel X index examined
      INTEGER XMIN                    ! Lowest image pixel X index examined
      INTEGER YMAX                    ! Highest image pixel Y index examined
      INTEGER YMIN                    ! Lowest image pixel Y index examined
      REAL CENINC                     ! Amount that the galaxy centre
                                      ! co-ord is incremented by
      REAL CENRAD                     ! Radius of the area around the galaxy
                                      ! centre to be examined
      REAL CENTXS                     ! Centroid summation X
      REAL CENTYS                     ! Centroid summation Y

      REAL MAX                        ! Maximum weighted average pixel value
      REAL NEWX                       ! X value of pixel with highest
                                      ! weighted surrounding values
      REAL NEWY                       ! Y value of the pixel with the highest
                                      ! weighted surrounding values
      REAL TOTAL                      ! Weighted pixel count total
      REAL VALUE                      ! Current pixel count value
      REAL VTOTAL                     ! Pixel intensity total
      REAL WTOTAL                     ! Weighting total
      REAL WEIGHT                     ! Weighting value used when summing the
                                      ! pixel count values about a given point
      REAL X                          ! Current X index
      REAL X2                         ! Current estimate X index
      REAL Y                          ! Current Y index
      REAL Y2                         ! Current estimate Y index

      INTEGER IX                      ! Loop counter over X index
      INTEGER IY                      ! Loop counter over Y index
      INTEGER IXMAX                   ! Max index in loop X
      INTEGER IYMAX                   ! Max index in loop Y

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Get the type of estimation to use.
      CALL PAR_GET0L('AUTOLT',AUTOLT,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

      IF (.NOT.AUTOLT) THEN

*      Weighted mean method.

*      Set the radius of the area around the assigned galaxy centre
*      to be considered. Also set the increment between galaxy centre
*      estimates.
         CENRAD=3.0
         CENINC=0.1

*      Set a flag to indicate if the pixel count value could be determined.
         FLAG=0

*      Set up the minimum and maximum image limits.
         XMIN=1
         XMAX=PRANGE(1)
         YMIN=1
         YMAX=PRANGE(2)

*      Set up the initial indices for the pixel with the highest weighted value>
         NEWX=XCO
         NEWY=YCO
         MAX=VAL__MINR

*      Loop through all pixels nearby to the chosen origin.
         X=XCO-CENRAD
         DO WHILE (X.LE.XCO+CENRAD)
            Y=YCO-CENRAD
            DO WHILE (Y.LE.YCO+CENRAD)

*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN

*               Initialise the pixel total and its weighting sum.
                  TOTAL=0.0
                  WTOTAL=0.0

*               Look at the pixels immediately adjacent to the current pixel.
*               Also check that they are within the image bounds.
                  X2=X-2.0
                  DO WHILE (X2.LE.X+2.0)
                     Y2=Y-2.0
                     DO WHILE (Y2.LE.Y+2.0)

*                     Avoid using points that are outside the image.
                        IF ((INT(X2).GE.XMIN).AND.(INT(X2).LE.XMAX)
     :                      .AND.
     :                      (INT(Y2).GE.YMIN).AND.(INT(Y2).LE.YMAX))
     :                      THEN

*                        Find the address of one of the surrounding pixels.
                           ADDRES=(INT(Y2)-1)*XMAX+INT(X2)

*                        Find the pixel value.
                           VALUE=ARRAY(ADDRES)

*                        Check that the pixel is not bad.
                           IF (VALUE.NE.VAL__BADR) THEN

*                           Calculate the weighting value.
*                           An arbitrary method.
                              WEIGHT=1./(1.+SQRT(REAL((X2-X)*(X2-X)
     :                               +(Y2-Y)*(Y2-Y))))

*                           Add the weighted pixel value to the summation
*                           and then add the current weighting value to
*                           the sum of all the weights for the current
*                           X/Y location.
                              TOTAL=TOTAL+VALUE*WEIGHT
                              WTOTAL=WTOTAL+WEIGHT

                           END IF

                        END IF

*                     Increment the Y value of the pixels being used.
                        Y2=Y2+1.0

                     END DO

*                  Increment the X value of the pixels being used.
                     X2=X2+1.0

                  END DO

*               Check to see if any legal points were found.
                  IF (WTOTAL.GT.0.0) THEN

*                  Calculate the weighted mean pixel value surrounding the
*                  current X/Y value. Keep it and its co-ords if it is bigger
*                  than the biggest found so far.
                     IF (TOTAL/WTOTAL.GT.MAX) THEN
                        MAX=TOTAL/WTOTAL
                        NEWX=X
                        NEWY=Y
                     END IF

                  END IF

               END IF

*            Increment the current Y position to be considered.
               Y=Y+CENINC

            END DO

*         Increment the current X position to be considered.
            X=X+CENINC

         END DO

*      Transfer the new centre location to the XCO YCO variables. Also,
*      pass back the value of the pixel chosen.
         XCO=NEWX
         YCO=NEWY

      ELSE

*      Centroid method.

*      Set the radius of the area around the assigned galaxy centre
*      to be considered.
         CENRAD=3

*      Set a flag to indicate if the pixel count value could be determined.
         FLAG=0

*      Set up the minimum and maximum image limits.
         XMIN=1
         XMAX=PRANGE(1)
         YMIN=1
         YMAX=PRANGE(2)

*      Set up the default indices for the pixel centroid.
         NEWX=XCO
         NEWY=YCO

*      Intitialise summations.
         VTOTAL=0.0
         CENTXS=0.0
         CENTYS=0.0

*      Loop through all pixels nearby to the chosen origin.
*      We go through hoops to force an integer loop
         IXMAX = INT(XCO + CENRAD) - INT(XCO - CENRAD) + 1
         IYMAX = INT(YCO + CENRAD) - INT(YCO - CENRAD) + 1

         DO 100 IX = 1, IXMAX
            DO 200 IY = 1, IYMAX

*            Convert this loop index into an actual position
*            Note that the loop must increment by 1.0 each time
*            starting at XCO - CENRAD
               X = XCO - CENRAD + IX - 1
               Y = YCO - CENRAD + IY - 1

*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN

*               Find the address of one of the surrounding pixels.
                  ADDRES=(INT(Y)-1)*XMAX+INT(X)

*               Find the pixel value.
                  VALUE=ARRAY(ADDRES)

*               Check that the pixel is not bad.
                  IF (VALUE.NE.VAL__BADR) THEN

*                  Create values needed to calculate the
*                  centroid later.
                     VALUE=VALUE-BACK
                     VTOTAL=VTOTAL+VALUE
                     CENTXS=CENTXS+VALUE*X
                     CENTYS=CENTYS+VALUE*Y

                  END IF

               END IF

 200        CONTINUE
 100     CONTINUE

*      Calculate the centroid.
         IF ((VTOTAL.NE.0).AND.(CENTXS.NE.0).AND.
     :                     (CENTYS.NE.0)) THEN
            NEWX=CENTXS/VTOTAL
            NEWY=CENTYS/VTOTAL

         END IF

*      Setup the new values to return.
         XCO=NEWX
         YCO=NEWY

      END IF

 9999 CONTINUE

       END


      SUBROUTINE ELP1_AUTOL(BACK,ELEMS,PRANGE,ARRAY,
     :                      XCO,YCO,STATUS)
*+
*  Name:
*     ELP1_AUTOL

*  Purpose:
*     Looks at the pixels immediately surrounding the user defined galaxy
*     centre and then determines the centroid.
*
*     Two methods are available. Centroiding and weighted mean.
*     Choice is made via parameter AUTOLT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_AUTOL(BACK,ELEMS,PRANGE,ARRAY,XCO,YCO,STATUS)

*  Description:
*     Examines the region of the image immediately surrounding the user
*     input value and calculates the centroid or weighted mean
*     co-ordinates.

*  Arguments:
*     BACK = REAL (Given)
*        The image background count.
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     ARRAY(ELEMS) = REAL (Given and Returned)
*        The image array. Contains the count values for all the image pixels.
*        Units counts.
*     XCO = REAL (Given and Returned)
*        X index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     YCO = REAL (Given and Returned)
*        Y index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status value.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     16-MAR-1993 (GJP)
*     (Original version)
*     14-FEB-1996 (GJP)
*     Added centroiding and Linux fix.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of elements/pixels in the
                                      ! image array
      INTEGER PRANGE(2)               ! Length of the X and Y axes of the
                                      ! image
      REAL BACK                       ! Image background

*  Arguments Returned:
      INTEGER FLAG                    ! It was not possible to find the
                                      ! central pixel value flag

*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)               ! The image array contains the count
                                      ! values for all the image pixels
      REAL XCO                        ! X index of the galaxy centre/origin
                                      ! supplied by the user
      REAL YCO                        ! Y index of the galaxy centre/origin
                                      ! supplied by the user

*  Local variables:
      LOGICAL AUTOLT                  ! Type of estimation to be used
      INTEGER ADDRES                  ! Array address of the element
                                      ! corresponding to pixel indices X,Y
      INTEGER XMAX                    ! Highest image pixel X index examined
      INTEGER XMIN                    ! Lowest image pixel X index examined
      INTEGER YMAX                    ! Highest image pixel Y index examined
      INTEGER YMIN                    ! Lowest image pixel Y index examined
      REAL CENINC                     ! Amount that the galaxy centre
                                      ! co-ord is incremented by
      REAL CENRAD                     ! Radius of the area around the galaxy
                                      ! centre to be examined
      REAL CENTXS                     ! Centroid summation X
      REAL CENTYS                     ! Centroid summation Y

      REAL MAX                        ! Maximum weighted average pixel value
      REAL NEWX                       ! X value of pixel with highest
                                      ! weighted surrounding values
      REAL NEWY                       ! Y value of the pixel with the highest
                                      ! weighted surrounding values
      REAL TOTAL                      ! Weighted pixel count total
      REAL VALUE                      ! Current pixel count value
      REAL VTOTAL                     ! Pixel intensity total
      REAL WTOTAL                     ! Weighting total
      REAL WEIGHT                     ! Weighting value used when summing the
                                      ! pixel count values about a given point
      REAL X                          ! Current X index
      REAL X2                         ! Current estimate X index
      REAL Y                          ! Current Y index
      REAL Y2                         ! Current estimate Y index

      INTEGER IX                      ! Loop counter over X index
      INTEGER IY                      ! Loop counter over Y index
      INTEGER IXMAX                   ! Max index in loop X
      INTEGER IYMAX                   ! Max index in loop Y
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Get the type of estimation to use.
      CALL PAR_GET0L('AUTOLT',AUTOLT,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

      IF (.NOT.AUTOLT) THEN

*      Weighted mean method.

*      Set the radius of the area around the assigned galaxy centre
*      to be considered. Also set the increment between galaxy centre
*      estimates.
         CENRAD=3.0
         CENINC=0.1

*      Set a flag to indicate if the pixel count value could be determined.
         FLAG=0

*      Set up the minimum and maximum image limits.
         XMIN=1
         XMAX=PRANGE(1)
         YMIN=1
         YMAX=PRANGE(2)

*      Set up the initial indices for the pixel with the highest weighted value
         NEWX=XCO
         NEWY=YCO
         MAX=VAL__MINR

*      Loop through all pixels nearby to the chosen origin.
         X=XCO-CENRAD
         DO WHILE (X.LE.XCO+CENRAD)
            Y=YCO-CENRAD
            DO WHILE (Y.LE.YCO+CENRAD)

*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN

*               Initialise the pixel total and its weighting sum.
                  TOTAL=0.0
                  WTOTAL=0.0

*               Look at the pixels immediately adjacent to the current pixel.
*               Also check that they are within the image bounds.
                  X2=X-2.0
                  DO WHILE (X2.LE.X+2.0)
                     Y2=Y-2.0
                     DO WHILE (Y2.LE.Y+2.0)

*                     Avoid using points that are outside the image.
                        IF ((INT(X2).GE.XMIN).AND.(INT(X2).LE.XMAX)
     :                      .AND.
     :                      (INT(Y2).GE.YMIN).AND.(INT(Y2).LE.YMAX))
     :                      THEN

*                        Find the address of one of the surrounding pixels.
                           ADDRES=(INT(Y2)-1)*XMAX+INT(X2)

*                        Find the pixel value.
                           VALUE=ARRAY(ADDRES)

*                        Check that the pixel is not bad.
                           IF (VALUE.NE.VAL__BADR) THEN

*                           Calculate the weighting value.
*                           An arbitrary method.
                              WEIGHT=1./(1.+SQRT(REAL((X2-X)*(X2-X)
     :                               +(Y2-Y)*(Y2-Y))))

*                           Add the weighted pixel value to the summation
*                           and then add the current weighting value to
*                           the sum of all the weights for the current
*                           X/Y location.
                              TOTAL=TOTAL+VALUE*WEIGHT
                              WTOTAL=WTOTAL+WEIGHT

                           END IF

                        END IF

*                     Increment the Y value of the pixels being used.
                        Y2=Y2+1.0

                    END DO

*                  Increment the X value of the pixels being used.
                     X2=X2+1.0

                  END DO

*               Check to see if any legal points were found.
                  IF (WTOTAL.GT.0.0) THEN

*                  Calculate the weighted mean pixel value surrounding the
*                  current X/Y value. Keep it and its co-ords if it is bigger
*                  than the biggest found so far.
                     IF (TOTAL/WTOTAL.GT.MAX) THEN
                        MAX=TOTAL/WTOTAL
                        NEWX=X
                        NEWY=Y
                     END IF

                  END IF

               END IF

*            Increment the current Y position to be considered.
               Y=Y+CENINC

            END DO

*         Increment the current X position to be considered.
            X=X+CENINC

         END DO

*      Transfer the new centre location to the XCO YCO variables. Also,
*      pass back the value of the pixel chosen.
         XCO=NEWX
         YCO=NEWY
      ELSE

*      Centroid method.

*      Set the radius of the area around the assigned galaxy centre
*      to be considered.
         CENRAD=3

*      Set a flag to indicate if the pixel count value could be determined.
         FLAG=0

*      Set up the minimum and maximum image limits.
         XMIN=1
         XMAX=PRANGE(1)
         YMIN=1
         YMAX=PRANGE(2)

*      Set up the default indices for the pixel centroid.
         NEWX=XCO
         NEWY=YCO

*      Intitialise summations.
         VTOTAL=0.0
         CENTXS=0.0
         CENTYS=0.0

*      Loop through all pixels nearby to the chosen origin.
*      We go through hoops to force an integer loop
         IXMAX = INT(XCO + CENRAD) - INT(XCO - CENRAD) + 1
         IYMAX = INT(YCO + CENRAD) - INT(YCO - CENRAD) + 1

         DO 100 IX = 1, IXMAX
            DO 200 IY = 1, IYMAX

*            Convert this loop index into an actual position
*            Note that the loop must increment by 1.0 each time
*            starting at XCO - CENRAD
               X = XCO - CENRAD + IX - 1
               Y = YCO - CENRAD + IY - 1

*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN

*               Find the address of one of the surrounding pixels.
                  ADDRES=(INT(Y)-1)*XMAX+INT(X)

*               Find the pixel value.
                  VALUE=ARRAY(ADDRES)

*               Check that the pixel is not bad.
                  IF (VALUE.NE.VAL__BADR) THEN

*                  Create values needed to calculate the
*                  centroid later.
                     VALUE=VALUE-BACK
                     VTOTAL=VTOTAL+VALUE
                     CENTXS=CENTXS+VALUE*X
                     CENTYS=CENTYS+VALUE*Y

                  END IF

               END IF

 200        CONTINUE
 100     CONTINUE

*      Calculate the centroid.
         IF ((VTOTAL.NE.0).AND.(CENTXS.NE.0).AND.
     :                     (CENTYS.NE.0)) THEN
            NEWX=CENTXS/VTOTAL
            NEWY=CENTYS/VTOTAL

         END IF

*      Setup the new values to return.
         XCO=NEWX
         YCO=NEWY

      END IF

 9999 CONTINUE

      END


      SUBROUTINE GAU1_AUTOL(NSOUR,ELEMS,PRANGE,ARRAY,
     :                      XCO,YCO,STATUS)
*+
*  Name:
*     GAU1_AUTOL

*  Purpose:
*     Looks at the pixels immediately surrounding the user defined source
*     centre and then determines the centroid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_AUTOL(NSOUR,ELEMS,PRANGE,ARRAY,XCO,YCO,STATUS)

*  Description:
*     Examines the region of the image immediately surrounding the user
*     input values and calculates the centroid co-ordinates. The region
*     examined is 7 pixels by 7 pixels.

*  Arguments:
*     NSOUR = INTEGER (Given)
*        Number of sources.
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     ARRAY(ELEMS) = REAL (Given and Returned)
*        The image array. Contains the count values for all the image pixels.
*     XCO(10,2) = REAL (Given and Returned)
*        X index of the source centre/origin supplied by the user.
*        Units pixels.
*     YCO(10,2) = REAL (Given and Returned)
*        Y index of the source centre/origin supplied by the user.
*        Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status value.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     16-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of elements/pixels in the
                                      ! image array
      INTEGER NSOUR                   ! Number of sources
      INTEGER PRANGE(2)               ! Length of the X and Y axes of the
                                      ! image

*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)               ! The image array contains the count
                                      ! values for all the image pixels
      REAL XCO(10,2)                  ! X index of the source centre/origin
                                      ! supplied by the user
      REAL YCO(10,2)                  ! Y index of the source centre/origin
                                      ! supplied by the user

*  Local variables:
      INTEGER ADDRES                  ! Array address of the element
                                      ! corresponding to pixel indices X,Y
      INTEGER FLAG                    ! It was not possible to find the
                                      ! central pixel value flag
      INTEGER CENRAD                  ! Radius of the area around the source
                                      ! centre to be examined
      INTEGER I                       ! Loops variable
      INTEGER OFFS                    ! Offset
      INTEGER XMAX                    ! Highest image pixel X index examined
      INTEGER XMIN                    ! Lowest image pixel X index examined
      INTEGER YMAX                    ! Highest image pixel Y index examined
      INTEGER YMIN                    ! Lowest image pixel Y index examined
      REAL CENTXS                     ! Centroid summation X
      REAL CENTYS                     ! Centroid summation Y
      REAL NEWX                       ! X value of pixel with highest
                                      ! weighted surrounding values
      REAL NEWY                       ! Y value of the pixel with the highest
                                      ! weighted surrounding values
      REAL VALUE                      ! Current pixel count value
      REAL VTOTAL                     ! Pixel intensity total
      REAL X                          ! Current X index
      REAL Y                          ! Current Y index

      INTEGER IX                      ! Loop counter over X index
      INTEGER IY                      ! Loop counter over Y index
      INTEGER IXMAX                   ! Max index in loop X
      INTEGER IYMAX                   ! Max index in loop Y

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Infdorm the user what is happening.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','Finding a better origin.',STATUS)

*   Set up the minimum and maximum image limits.
      XMIN=1
      XMAX=PRANGE(1)
      YMIN=1
      YMAX=PRANGE(2)

*   Set the radius of the area around the assigned source centre
*   to be considered.
      CENRAD=3

*   Loop through all sources.
      DO 10 I=1,NSOUR

*      Set a flag to indicate if the pixel count value could be determined.
         FLAG=0

*      Set up the default indices for the pixel centroid.
         NEWX=XCO(I,1)
         NEWY=YCO(I,1)

*      Initialise summations.
         VTOTAL=0.0
         CENTXS=0.0
         CENTYS=0.0

*      Loop through all pixels nearby to the chosen origin.
*      We go through hoops to force an integer loop
         IXMAX = INT(XCO(I,1) + CENRAD) - INT(XCO(I,1) - CENRAD) + 1
         IYMAX = INT(YCO(I,1) + CENRAD) - INT(YCO(I,1) - CENRAD) + 1

         DO 200 IY = 1, IYMAX

*     Convert this loop index into an actual position
*     Note that the loop must increment by 1.0 each time
*     starting at XCO - CENRAD
            Y = YCO(I,1) - CENRAD + IY - 1

*         Address offset.
            OFFS=(INT(Y)-1)*XMAX

            DO 100 IX = 1, IXMAX

*       Calculate original X position
               X = XCO(I,1) - CENRAD + IX - 1

*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN

*               Find the address of one of the surrounding pixels.
                  ADDRES=OFFS+INT(X)

*               Find the pixel value.
                  VALUE=ARRAY(ADDRES)

*               Check that the pixel is not bad.
                  IF (VALUE.NE.VAL__BADR) THEN

*                  Create values needed to calculate the
*                  centroid later.
                     VTOTAL=VTOTAL+VALUE
                     CENTXS=CENTXS+VALUE*X
                     CENTYS=CENTYS+VALUE*Y

                  END IF

               END IF

 100        CONTINUE

 200     CONTINUE

*      Calculate the centroid.
         IF ((VTOTAL.NE.0.0).AND.(CENTXS.NE.0.0).AND.
     :                         (CENTYS.NE.0.0)) THEN
            NEWX=CENTXS/VTOTAL
            NEWY=CENTYS/VTOTAL

         END IF

*      Setup the new values to return.
         XCO(I,2)=XCO(I,1)
         YCO(I,2)=YCO(I,1)
         XCO(I,1)=NEWX
         YCO(I,1)=NEWY

 10   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE SEC1_AUTOL(AUTOL,ELEMS,PRANGE,OCOUNT,FLAG,
     :                      ARRAY,XCO,YCO,STATUS)
*+
*  Name:
*     SEC1_AUTOL

*  Purpose:
*     Looks at the pixels immediately surrounding the user defined galaxy
*     centre and then chooses a location that provides the biggest weighted
*     mean value.
*
*     Does not generate a value greater than 1 pixel accuracy. For better
*     accuracy PISA, FOCAS or the ESP profiling application is recommended.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_AUTOL(AUTOL,ELEMS,PRANGE,OCOUNT,FLAG,
*                     ARRAY,XCO,YCO,STATUS)

*  Description:
*     Searches for the highest weighted mean pixel value in the region of the
*     image immediately surrounding the user input value. The region examined
*     is 11 pixels by 11 pixels and is searched using a 5 by 5 square.

*  Arguments:
*     AUTOL = LOGICAL (Given)
*        Defines whether autolocate is to be used.
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     OCOUNT = REAL (Returned)
*        The count value found at the galaxy origin. Units counts.
*     FLAG = INTEGER (Returned)
*        It was not possible to find the central pixel value flag.
*     ARRAY(ELEMS) = REAL (Given and Returned)
*        The image array. Contains the count values for all the image pixels.
*        Units counts.
*     XCO = REAL (Given and Returned)
*        X index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     YCO = REAL (Given and Returned)
*        Y index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status value.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     1-Dec-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      LOGICAL AUTOL                   ! Defines whether autolocation of the
                                      ! object centre is to be used.
      INTEGER ELEMS                   ! Number of elements/pixels in the
                                      ! image array
      INTEGER PRANGE(2)               ! Length of the X and Y axes of the
                                      ! image

*  Arguments Returned:
      INTEGER FLAG                    ! It was not possible to find the
                                      ! central pixel value flag
      REAL OCOUNT                     ! The count value found at the pixel
                                      ! used as the galaxy origin

*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)               ! The image array contains the count
                                      ! values for all the image pixels
      REAL XCO                        ! X index of the galaxy centre/origin
                                      ! supplied by the user
      REAL YCO                        ! Y index of the galaxy centre/origin
                                      ! supplied by the user

*  Local variables:
      INTEGER ADDRES                  ! Array address of the element
                                      ! corresponding to pixel indices X,Y
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Temporary counter
      INTEGER X                       ! Current X index
      INTEGER XMAX                    ! Highest image pixel X index examined
      INTEGER XMIN                    ! Lowest image pixel X index examined
      INTEGER YMAX                    ! Highest image pixel Y index examined
      INTEGER YMIN                    ! Lowest image pixel Y index examined
      INTEGER Y                       ! Current Y index
      INTEGER ARRAYIDX                ! Array index
      REAL MAX                        ! Maximum weighted average pixel value
      REAL NEWX                       ! X value of pixel with highest
                                      ! weighted surrounding values
      REAL NEWY                       ! Y value of the pixel with the highest
                                      ! weighted surrounding values
      REAL TOTAL                      ! Weighted pixel count total
      REAL VALUE                      ! Current pixel count value
      REAL WTOTAL                     ! Weighting total
      REAL WEIGHT                     ! Weighting value used when summing the
                                      ! pixel count values about a given point
      INTEGER IX                      ! Loop counter over X index
      INTEGER IY                      ! Loop counter over Y index
      INTEGER IXMAX                   ! Max index in loop X
      INTEGER IYMAX                   ! Max index in loop Y
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set a flag to indicate if the pixel count value could be determined.
      FLAG=0

*   Set up the minimum and maximum image limits.
      XMIN=1
      XMAX=PRANGE(1)
      YMIN=1
      YMAX=PRANGE(2)

*   Use autolocation if selected.
      IF (AUTOL) THEN

*      Set up the initial indices for the pixel with the highest weighted value.
         NEWX=XCO
         NEWY=YCO
         MAX=VAL__MINR

*     Looping over +/-5 pixels (or 0..10)
         IXMAX = 11
         IYMAX = 11

*      Loop through all pixels nearby to the chosen origin.
         DO 10 IX=1,IXMAX

*     Recover the original float
            X = XCO - ( ( IXMAX - 1 ) / 2 )  + ( IX - 1 )

            DO 15 IY=1,IYMAX

               Y = YCO - ( ( IYMAX - 1 ) / 2 )  + ( IY - 1 )

*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN

*               Initialise the pixel total and its weighting sum.
                  TOTAL=0.0
                  WTOTAL=0.0

*               Look at the pixels immediately adjacent to the current pixel.
*               Also check that they are within the image bounds.
                  DO 20 I=X-2,X+2

                     DO 25 J=Y-2,Y+2

*                     Avoid using points that are outside the image.
                        IF ((I.GE.XMIN).AND.(I.LE.XMAX).AND.
     :                      (J.GE.YMIN).AND.(J.LE.YMAX)) THEN

*                        Find the address of one of the surrounding pixels.
                           ADDRES=(J-1)*XMAX+I

*                        Find the pixel value.
                           VALUE=ARRAY(ADDRES)

*                        Check that the pixel is not bad.
                           IF (VALUE.NE.VAL__BADR) THEN

*                           Calculate the weighting value.
*                           An arbitrary method.
                              WEIGHT=1./(1.+SQRT(REAL((I-X)*(I-X)
     :                               +(J-Y)*(J-Y))))

*                           Add the weighted pixel value to the summation
*                           and then add the current weighting value to
*                           the sum of all the weights for the current
*                           X/Y location.
                              TOTAL=TOTAL+VALUE*WEIGHT
                              WTOTAL=WTOTAL+WEIGHT

                           END IF

                        END IF

 25                  CONTINUE

 20               CONTINUE

*               Check to see if any legal points were found.
                  IF (WTOTAL.GT.0.0) THEN

*                  Calculate the weighted mean pixel value surrounding the
*                  current X/Y value. Keep it and its co-ords if it is bigger
*                  than the biggest found so far.
                     IF (TOTAL/WTOTAL.GT.MAX) THEN
                        MAX=TOTAL/WTOTAL
                        NEWX=X
                        NEWY=Y
                     END IF

                  END IF

               END IF

 15         CONTINUE

 10      CONTINUE

*      Transfer the new centre location to the XCO YCO variables. Also,
*      pass back the value of the pixel chosen.
         XCO=NEWX
         YCO=NEWY
         OCOUNT=ARRAY((NINT(YCO-1))*XMAX+NINT(XCO))

      ELSE

*      Obtain the value of the pixel at XCO,YCO if autolocate
*      is not selected.
         ARRAYIDX=(NINT(YCO-1))*XMAX+NINT(XCO)
         IF (ARRAYIDX .GT. ELEMS) THEN
            STATUS=SAI__ERROR
            CALL MSG_SETR('XCO',XCO)
            CALL MSG_SETR('YCO',YCO)
            CALL MSG_SETI('ELEMS',ELEMS)
            CALL ERR_REP(' ','AUTOL: pixel (^XCO,^YCO) out of range'
     :           //' for array size ^ELEMS',STATUS)
            OCOUNT=VAL__BADR
            GOTO 9999
         ELSE
            OCOUNT=ARRAY(ARRAYIDX)
         ENDIF

*      Correct and issue warning if the value is bad.
         IF (OCOUNT.EQ.VAL__BADR) THEN
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','The central pixel was bad.',STATUS)
            CALL MSG_BLANK(STATUS)
            OCOUNT=0.0
            FLAG=1
         END IF

      END IF

 9999 CONTINUE

      END
