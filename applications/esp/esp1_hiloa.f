

      SUBROUTINE HIS1_HILOA(ELEMS,ARRAY,
     :                      UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)
*+
*  Name:
*     HIS1_HILOA

*  Purpose:
*     Find the highest and lowest count values in an image array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_HILOA(ELEMS,ARRAY,
*                     UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)

*  Description:
*     Establish the highest and lowest count values found in the image
*     ARRAY. The mean value is found also as is the number of pixels
*     that are bad.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        An array containing the image pixel counts.
*     UNUPIX = INTEGER (Returned)
*        The number of unused pixels in the image. Units pixels.
*     HIGH = REAL (Returned)
*        The highest count value found in the image pixels. Units counts.
*     MEAN = DOUBLE PRECISION (Returned)
*        Mean of the values found in the image pixels. Units counts.
*     LOW = REAL (Returned)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     NUMBER = INTEGER (Returned)
*        The number of non-bad pixels. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'HIS_PAR'               ! HISTPEAK system variables

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      REAL ARRAY( ELEMS )             ! Image pixel counts

*  Arguments Returned:
      INTEGER NUMBER                  ! The number of pixels to be used
      INTEGER UNUPIX                  ! Number of unused pixels in the data
      REAL HIGH                       ! Highest value in the array
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION MEAN           ! Average value in the array

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL VALUE                      ! Value of current array element
      REAL UNUPXR                     ! Number of bad pixels (Real)
      DOUBLE PRECISION SUM            ! Sum of all non-bad pixels in
                                      ! the data array

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look through the array in search of highest, lowest values and
*   also take the sum (which allows the mean to be calculated) and count
*   the number of unused pixels in the image.
      MEAN=0.0
      HIGH=VAL__MINR
      LOW= VAL__MAXR
      UNUPIX=0
      UNUPXR=0.0
      SUM= 0.0

      DO 112 I=1,ELEMS
         VALUE=ARRAY(I)
         IF (VALUE.NE.VAL__BADR) THEN
            SUM=SUM+VALUE
            IF (VALUE.GT.HIGH) HIGH=VALUE
            IF (VALUE.LT.LOW) LOW=VALUE
         ELSE
            UNUPXR=UNUPXR+1.0
         END IF
 112  CONTINUE


*   Check that the range of pixel values is too big for the
*   current software version.
      IF (REAL(HIGH)-REAL(LOW).GT.REAL(VAL__MAXI)-2.) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Pixel range too large to handle.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

*   Check that there are not too many bad pixels.
      IF (UNUPXR.GE.REAL(VAL__MAXI)-2.) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too many data points are bad.',STATUS)
         GOTO 9999
      END IF

      UNUPIX=NINT(UNUPXR)
      NUMBER=ELEMS-UNUPIX

*   Check that enough data points are available.
      IF (NUMBER.LT.3) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few data points have been '//
     :                    'specified.',STATUS)
         GOTO 9999
      END IF

*   Calculate the average data array value.
      IF (NUMBER.GT.0) MEAN=SUM/REAL(NUMBER)

*   Check the number of unused data points present in the array and
*   set STATUS if less than 3 data points are present.
      IF (NUMBER.LT.3) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few points are present for an accurate'//
     :                    ' estimate of the mode to be made.',STATUS)
         GOTO 9999
      END IF

*   Check the likely width of the histogram.
      IF (HIGH-LOW.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The histogram is very narrow.'//
     :                    ' Please rescale your data.',STATUS)
         GOTO 9999
      END IF

*   Check if the range of values found is less than 3 and
*   set STATUS if true.
      IF (HIGH-LOW.LT.3.0) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The range of values found is less than 3!'//
     :                    ' Consequently, accurate modes are not'//
     :                    ' available.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END



      SUBROUTINE HSB1_HILOA(ELEMS,ARRAY,
     :                      STATUS,UNUPIX,HIGH,MEAN,LOW,NUMBER)
*+
*  Name:
*     HSB1_HILOA

*  Purpose:
*     Find the highest and lowest count values in an image array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_HILOA(ELEMS,ARRAY,
*                     STATUS,UNUPIX,HIGH,MEAN,LOW,NUMBER)

*  Description:
*     Establish the highest and lowest count values found in the image
*     ARRAY. The mean value is found also as is the number of pixels
*     that are bad.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        An array containing the image pixel counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     UNUPIX = INTEGER (Returned)
*        The number of unused pixels in the image. Units pixels.
*     HIGH = REAL (Returned)
*        The highest count value found in the image pixels. Units counts.
*     MEAN = DOUBLE PRECISION (Returned)
*        Mean of the values found in the image pixels. Units counts.
*     LOW = REAL (Returned)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     NUMBER = INTEGER (Returned)
*        The number of pixels non-bad. Units pixels.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'HSB_PAR'               ! HSUB system variables

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      REAL ARRAY( ELEMS )             ! Image pixel counts

*  Arguments Returned:
      INTEGER NUMBER                  ! The number of pixels non-bad
      INTEGER UNUPIX                  ! Number of unused pixels in the data
      REAL HIGH                       ! Highest value in the array
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION MEAN           ! Average value in the array

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL VALUE                      ! Value of current array element
      REAL UNUPXR                     ! Number of bad pixels (Real)
      DOUBLE PRECISION SUM            ! Sum of all non-bad pixels in
                                      ! the data array

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look through the array in search of highest, lowest values and
*   also take the sum (which allows the mean to be calculated) and count
*   the number of unused pixels in the image.
      MEAN=0.0
      HIGH=VAL__MINR
      LOW= VAL__MAXR
      UNUPIX=0
      UNUPXR=0.0
      SUM= 0.0

      DO 112 I=1,ELEMS
         VALUE=ARRAY(I)
         IF (VALUE.NE.VAL__BADR) THEN
            SUM=SUM+VALUE
            IF (VALUE.GT.HIGH) HIGH=VALUE
            IF (VALUE.LT.LOW) LOW=VALUE
         ELSE
            UNUPXR=UNUPXR+1.0
         END IF
 112  CONTINUE

*   Check that the range of pixel values is too big for the
*   current software version.
      IF (REAL(HIGH)-REAL(LOW).GT.REAL(VAL__MAXI)-2.) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Pixel range too large to handle.',STATUS)
         GOTO 9999
      END IF

*   Check that there are not too many bad pixels.
      IF (UNUPXR.GE.REAL(VAL__MAXI)-2.) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too many data points are bad.',STATUS)
         GOTO 9999
      END IF

      UNUPIX=NINT(UNUPXR)
      NUMBER=ELEMS-UNUPIX

*   Check that enough data points are available.
      IF (NUMBER.LT.3) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few data points have been '//
     :                    'specified.',STATUS)
         GOTO 9999
      END IF

*   Calculate the average data array value.
      IF (NUMBER.GT.0) MEAN=SUM/REAL(NUMBER)

*   Check the number of unused data points present in the array and
*   set STATUS if less than 3 data points are present.
      IF (NUMBER.LT.3) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few points are present for an accurate'//
     :                    ' estimate of the mode to be made.',STATUS)
         GOTO 9999
      END IF

*   Check if the range of values found is less than 3 and
*   set STATUS if true.
      IF (HIGH-LOW+1.0.LT.3.0) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The range of values found is less than 3!'//
     :                    ' Consequently, accurate modes are not'//
     :                    ' available.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END



      SUBROUTINE LOB1_HILOA(ELEMS,ARRAY,
     :                      UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)
*+
*  Name:
*     LOB1_HILOA

*  Purpose:
*     Find the highest and lowest count values in an image array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_HILOA(ELEMS,ARRAY,
*                     UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)

*  Description:
*     Establish the highest and lowest count values found in the image
*     ARRAY. The mean value is found also as is the number of pixels
*     that are bad.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        An array containing the image pixel counts.
*     UNUPIX = INTEGER (Returned)
*        The number of unused pixels in the image. Units pixels.
*     HIGH = REAL (Returned)
*        The highest count value found in the image pixels. Units counts.
*     MEAN = DOUBLE PRECISION (Returned)
*        Mean of the values found in the image pixels. Units counts.
*     LOW = REAL (Returned)
*        Lowest count value in the image. Also used as an array
*        offset index for the arrays SMOBAR and BARRAY. Units counts.
*     NUMBER = INTEGER (Returned)
*        The number of pixels non-bad. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
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
      INTEGER ELEMS                   ! Number of pixels in the image
      REAL ARRAY( ELEMS )             ! Image pixel counts

*  Arguments Returned:
      INTEGER NUMBER                  ! The number of pixels non-bad
      INTEGER UNUPIX                  ! Number of unused pixels in the data
      REAL HIGH                       ! Highest value in the array
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION MEAN           ! Average value in the array

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL VALUE                      ! Value of current array element
      REAL UNUPXR                     ! Number of bad pixels (Real)
      DOUBLE PRECISION SUM            ! Sum of all non-bad pixels in
                                      ! the data array

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look through the array in search of highest, lowest values and
*   also take the sum (which allows the mean to be calculated) and count
*   the number of unused pixels in the image.
      MEAN=0.0
      HIGH=-VAL__MAXR
      LOW=VAL__MAXR
      UNUPIX=0
      UNUPXR=0.0
      SUM= 0.0

      DO 112 I=1,ELEMS
         VALUE=ARRAY(I)
         IF (VALUE.NE.VAL__BADR) THEN
            SUM=SUM+VALUE
            IF (VALUE.GT.HIGH) HIGH=VALUE
            IF (VALUE.LT.LOW) LOW=VALUE
         ELSE
            UNUPXR=UNUPXR+1.0
         END IF
 112  CONTINUE

*   Check that the range of pixel values is too big for the
*   current software version.
      IF (REAL(HIGH)-REAL(LOW).GT.REAL(VAL__MAXI)-2.) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Pixel range too large to handle.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

*   Check that there are not too many bad pixels.
      IF (UNUPXR.GE.REAL(VAL__MAXI)-2.) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Too many data points are bad.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

      UNUPIX=NINT(UNUPXR)
      NUMBER=ELEMS-UNUPIX

*   Check that enough data points are available.
      IF (NUMBER.LT.3) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Too few data points have been '//
     :                    'specified.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

*   Calculate the average data array value.
      IF (NUMBER.GT.0) MEAN=SUM/REAL(NUMBER)

*   Check the number of unused data points present in the array and
*   set STATUS if less than 3 data points are present.
      IF (NUMBER.LT.3) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','Too few points are present for an accurate'//
     :                    ' estimate of the mode to be made.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

*   Check if the range of values found is less than 3 and
*   set STATUS if true.
      IF (HIGH-LOW+1.0.LT.3.0) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','The range of values found is less than 3!'//
     :                    ' Consequently, accurate modes are not'//
     :                    ' available.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

 9999 CONTINUE

      END
