*+  DIARAR - divides an array by an array (pixel by pixel)

      SUBROUTINE KFH_DIARAR(ARRA,INVA,ARRB,INVB,NVALS,ARRC,INVC,
     :                      MIN,MAX,STATUS)
*    Description :
*	It divides two 1-D arrays which are the same size, excluding
*	certain values, and calculating the min and max of the
*	resulting array at the same time.
*
*	Division by zero results in an invalid pixel being set
*
*    Invocation :
*	CALL KFH_DIARAR(ARRA,INVA,ARRB,INVB,NVALS,ARRC,INVC,MIN,MAX,
*    :   STATUS)
*    Parameters :
*	ARRA(NVALS)	_REAL
*		The first input array
*	INVA		_REAL
*		The invalid pixel flag for array ARRA
*	ARRB(NVALS)	_REAL
*		The second input array
*	INVB		_REAL
*		The invalid pixel flag for array ARRB
*	NVALS		_INTEGER
*		The size of all three arrays
*	ARRC(NVALS)	_REAL
*		The output array
*	ARRC		_REAL
*		The invalid pixel flag for the output array
*	MIN		_REAL
*		The minimum value of array ARRC
*	MAX		_REAL
*		The maximum value of array ARRC
*       STATUS          _INTEGER
*               Status value
*    Method :
*	If either of the input pixels is invalidor division
*	by zero is about to be attempted an invalid
*	value is stored in the output. Otherwise the ratio of the
*	two input pixels is stored. The minimum and maximum values
*	of the resulting array are found on the same pass, taking
*	a valid estimate to start with.
*    Authors :
*	K.F.Hartley   (RGO)
*    History :
*	23-Aug-83 First version (KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NVALS			! Dimensions of all arrays
      REAL ARRA(NVALS)			! First input array
      REAL ARRB(NVALS)			! Second input array
      REAL ARRC(NVALS)			! Output array
      REAL INVA				! Invalid pixel value for ARRA
      REAL INVB				! Invalid pixel value for ARRB
      REAL INVC				! Invalid pixel value for ARRC
      REAL MIN				! The minimum value of ARRC
      REAL MAX				! The maximum value of ARRC
      INTEGER I				! The loop index
      LOGICAL SET			! Logical flag saying whether
*					! min and max values are set
*-

*
*    If the status value is bad, then return to the main routine.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*      Set the invalid pixel value for the output equal to that for
*      the first input array.
*
         INVC=INVA
*
*      Give the falg an initial value
*      (This is needed to ensure that a valid value is found for min
*      and max)
*
         SET=.FALSE.
*
*      Loop through all the pixels
*
         DO I=1,NVALS
            IF (ARRA(I).NE.INVA.AND.ARRB(I).NE.INVB.AND.ARRB(I).NE.0.0)
     :                                                             THEN
*
*            If two valid pixels and a non-zero divisor do the division
*
               ARRC(I) = ARRA(I) / ARRB(I)
               IF (.NOT.SET) THEN
*
*               and if there is no current min and max then
*               use this value.
*               The flag can then be set to TRUE
*
                  MIN=ARRC(I)
                  MAX=ARRC(I)
                  SET=.TRUE.
               ELSE
*
*                Otherwise update estimate of min and max
*
                  IF (ARRC(I).LT.MIN) MIN=ARRC(I)
                  IF (ARRC(I).GT.MAX) MAX=ARRC(I)
               END IF
            ELSE
*
*            If either of the input pixels is invalid, then so is
*            the output pixel
*
               ARRC(I)=INVC
            END IF
         END DO
*
*      If ALL the input pixels are invalid then set min and max
*      to the invalid value
*
         IF (.NOT.SET) THEN
            MIN=INVC
            MAX=INVC
         END IF

      ENDIF
      END
