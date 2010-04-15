

      SUBROUTINE HIS1_MOMDE(ELEMS,NUMBER,ARRAY,
     :                      MEAN,ADEV,VARI,SDEV,SKEW,KURT,STATUS)
*+
*  Name:
*     HIS1_MOMDE

*  Purpose:
*     Finds the deviations, skewness and kurtosis of pixels in an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_MOMDE(ELEMS,NUMBER,ARRAY,
*                     MEAN,ADEV,VARI,SDEV,SKEW,KURT,STATUS)

*  Description:
*     Finds values for the absolute deviation, standard deviation,
*     variance, skewness and kurtosis of pixels in an image.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     NUMBER = INTEGER (Given)
*        The number of non-bad image pixels. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        Array containing the image data.
*     MEAN = DOUBLE PRECISION (Given)
*        Mean of the values found in the image pixels. Units counts.
*     ADEV = DOUBLE PRECISION (Returned)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     VARI = DOUBLE PRECISION (Returned)
*        Variance of the image pixel count distribution.
*     SDEV(2) = DOUBLE PRECISION (Returned)
*        Standard deviation of the image pixel count distribution
*        and the background count std dev. Units counts.
*     SKEW = DOUBLE PRECISION (Returned)
*        Skewness of the image pixel count distribution.
*     KURT = DOUBLE PRECISION (Returned)
*        Kurtosis of the image pixel count distribution.
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
      INTEGER ELEMS                   ! Number of pixels in the data
      INTEGER NUMBER                  ! The number of pixels to be used
      REAL ARRAY(ELEMS)               ! Array containing image data
      DOUBLE PRECISION MEAN           ! Average value in the image
                                      ! array

*  Arguments Returned:
      DOUBLE PRECISION ADEV           ! Absolute deviation of array
      DOUBLE PRECISION KURT           ! Kurtosis of array values
      DOUBLE PRECISION SDEV(2)        ! Background standard deviation
      DOUBLE PRECISION SKEW           ! Skewness of the array values
      DOUBLE PRECISION VARI           ! Variance of array values

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      DOUBLE PRECISION VALUE          ! Temporary storage variable
      DOUBLE PRECISION P2             ! Temporary storage variable
      DOUBLE PRECISION P3             ! Temporary storage variable
      DOUBLE PRECISION P4             ! Temporary storage variable
      REAL TEMP                       ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look through the array to gather information for calculating the
*   standard deviation, absolute deviation, variance, skewness and
*   kurtosis of the distribution.
      DO 212 I=1,ELEMS

*      Get the pixel value and check that it isnt bad.
         TEMP=ARRAY(I)
         IF (TEMP.NE.VAL__BADR) THEN

*         Convert to double precision and then calculate the
*         absolute deviation (first moment of deviation).
            VALUE=DBLE(TEMP)-MEAN
            ADEV=ADEV+ABS(VALUE)
            P2=VALUE*VALUE
            P3=P2*VALUE
            P4=P3*VALUE

*         Variance.
            VARI=VARI+P2

*         Skewness.
            SKEW=SKEW+P3

*         Kurtosis.
            KURT=KURT+P4

         END IF
 212  CONTINUE

*   Derive values from the previous summations for absolute deviation,
*   variance, standard deviation, skewness and kurtosis.
      IF (NUMBER.GE.2) THEN
         ADEV=ADEV/DBLE(NUMBER)
         VARI=VARI/DBLE(NUMBER-1)
         SDEV(1)=SQRT(VARI)
         SKEW=SKEW/(DBLE(NUMBER)*SDEV(1)**3)
         KURT=KURT/(DBLE(NUMBER)*(VARI**2))-3
      ELSE
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few points for an accurate estimate'//
     :                    ' of the standard. deviation etc.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE HSB1_MOMDE(ELEMS,NUMBER,ARRAY,
     :                      MEAN,STATUS,ADEV,VARI,SDEV,SKEW,KURT)
*+
*  Name:
*     HSB1_MOMDE

*  Purpose:
*     Finds the deviations, skewness and kurtosis of pixels in an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_MOMDE(ELEMS,NUMBER,ARRAY,
*                     MEAN,STATUS,ADEV,VARI,SDEV,SKEW,KURT)

*  Description:
*     Finds values for the absolute deviation, standard deviation,
*     variance, skewness and kurtosis of pixels in an image.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     NUMBER = INTEGER (Given)
*        The number of image pixels to be used. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        Array containing the image data.
*     MEAN = DOUBLE PRECISION (Given)
*        Mean of the values found in the image pixels. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     ADEV = DOUBLE PRECISION (Returned)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     VARI = DOUBLE PRECISION (Returned)
*        Variance of the image pixel count distribution.
*     SDEV(2) = DOUBLE PRECISION (Returned)
*        Standard deviation of the image pixel count distribution
*        and the background standard deviation. Units counts.
*     SKEW = DOUBLE PRECISION (Returned)
*        Skewness of the image pixel count distribution.
*     KURT = DOUBLE PRECISION (Returned)
*        Kurtosis of the image pixel count distribution.

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
      INTEGER ELEMS                   ! Number of pixels in the data
      INTEGER NUMBER                  ! The number of pixels to be used
      REAL ARRAY(ELEMS)               ! Array containing image data
      DOUBLE PRECISION MEAN           ! Average value in the image
                                      ! array

*  Arguments Returned:
      DOUBLE PRECISION ADEV           ! Absolute deviation of array
      DOUBLE PRECISION KURT           ! Kurtosis of array values
      DOUBLE PRECISION SDEV(2)        ! Standard deviation
      DOUBLE PRECISION SKEW           ! Skewness of the array values
      DOUBLE PRECISION VARI           ! Variance of array values

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL VALUE                      ! Temporary storage variable
      DOUBLE PRECISION P2             ! Temporary storage variable
      DOUBLE PRECISION P3             ! Temporary storage variable
      DOUBLE PRECISION P4             ! Temporary storage variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look through the array to gather information for calculating the
*   standard deviation, absolute deviation, variance, skewness and
*   kurtosis of the distribution.
      DO 212 I=1,ELEMS
         VALUE=ARRAY(I)
         IF (VALUE.NE.VAL__BADR) THEN
*         Absolute deviation (first moment of deviation).
            VALUE=VALUE-MEAN
            ADEV=ADEV+ABS(VALUE)
            P2=VALUE*VALUE
            P3=P2*VALUE
            P4=P3*VALUE
*         Variance.
            VARI=VARI+P2
*         Skewness.
            SKEW=SKEW+P3
*         Kurtosis.
            KURT=KURT+P4
         END IF
 212  CONTINUE

*   Derive values from the previous summations for absolute deviation,
*   variance, standard deviation, skewness and kurtosis.
      IF (NUMBER.GE.2) THEN
         ADEV=ADEV/REAL(NUMBER)
         VARI=VARI/REAL(NUMBER-1)
         SDEV(1)=SQRT(VARI)
         SKEW=SKEW/(REAL(NUMBER)*SDEV(1)**3)
         KURT=KURT/(REAL(NUMBER)*(VARI**2))-3
      ELSE
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Too few points for an accurate estimate'//
     :                    ' of the standard. deviation etc.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE LOB1_MOMDE(ELEMS,NUMBER,ARRAY,
     :                      MEAN,ADEV,VARI,SDEV,STATUS)
*+
*  Name:
*     LOB1_MOMDE

*  Purpose:
*     Finds the absolute and standard deviations of pixels in an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_MOMDE(ELEMS,NUMBER,ARRAY,
*                     MEAN,ADEV,VARI,SDEV,STATUS)

*  Description:
*     Finds values for the absolute deviation, standard deviation and
*     variance of pixels in an image.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image. Units pixels.
*     NUMBER = INTEGER (Given)
*        The number of image pixels to be used. Units pixels.
*     ARRAY(ELEMS) = INTEGER (Given)
*        Array containing the image data.
*     MEAN = DOUBLE PRECISION (Given)
*        Mean of the values found in the image pixels. Units counts.
*     ADEV = DOUBLE PRECISION (Returned)
*        Absolute deviation of the image pixels count distribution.
*        Units counts.
*     VARI = DOUBLE PRECISION (Returned)
*        Variance of the image pixel count distribution.
*     SDEV(2) = DOUBLE PRECISION (Returned)
*        Standard deviation of the image pixel count distribution
*        and the standard deviation of the background. Units counts.
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
      INCLUDE 'LOB_PAR'               ! LOBACK system variables

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the data
      INTEGER NUMBER                  ! The number of pixels to be used
      REAL ARRAY(ELEMS)               ! Array containing image data
      DOUBLE PRECISION MEAN           ! Average value in the image
                                      ! array

*  Arguments Returned:
      DOUBLE PRECISION ADEV           ! Absolute deviation of array
      DOUBLE PRECISION SDEV(2)        ! Standard deviation and the
                                      ! standard deviation of background
      DOUBLE PRECISION VARI           ! Variance of array values

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL VALUE                      ! Temporary storage variable
      DOUBLE PRECISION P2             ! Temporary storage variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look through the array to gather information for calculating the
*   standard deviation, absolute deviation and  variance
*   of the distribution.
      DO 212 I=1,ELEMS
         VALUE=ARRAY(I)
         IF (VALUE.NE.VAL__BADR) THEN
*         Absolute deviation (first moment of deviation).
            VALUE=VALUE-MEAN
            ADEV=ADEV+ABS(VALUE)
            P2=VALUE*VALUE
*         Variance.
            VARI=VARI+P2
         END IF
 212  CONTINUE

*   Derive values from the previous summations for absolute deviation,
*   variance and standard deviation.
      IF (NUMBER.GE.2) THEN
         ADEV=ADEV/REAL(NUMBER)
         VARI=VARI/REAL(NUMBER-1)
         SDEV(1)=SQRT(VARI)
      ELSE
         CALL MSG_OUT(' ','WARNING',STATUS)
         CALL MSG_OUT(' ','Too few points for an accurate estimate'//
     :                    ' of the standard. deviation etc.',STATUS)
         STATUS=SAI__ERROR
         GOTO 9999
      END IF

 9999 CONTINUE

      END
