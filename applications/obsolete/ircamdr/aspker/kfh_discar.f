*+   DISCAR - Divides an scalar by an array
      SUBROUTINE KFH_DISCAR(ARRA,INVA,NVALS,SCALAR,ARRC,INVC,MIN,MAX,
     :                      STATUS)
*    Description :
*      Every valid pixel of an input array is divided into a scalar
*      and the minimum and maximum of the output array are found
*      on the same pass through the data.
*
*      Division by zero results in an invalid pixel being set.
*
*    Invocation :
*     CALL KFH_DISCAR(ARRA,INVA,NVALS,SCALAR,ARRC,INVC,MIN,MAX,STATUS)
*    Parameters :
*	ARRA(NVALS)		_REAL
*		The input array
*	INVA			_REAL
*		The invalid pixel value for array ARRA
*	NVALS			_INTEGER
*		The size of input and output arrays
*	SCALAR			_REAL
*		The scalar to be added to ARRA
*	ARRC(NVALS)		_REAL
*		The output array
*	INVC			_REAL
*		The invalid pixel value for ARRC
*	MIN			_REAL
*		The minimum value found in ARRC
*	MAX			_REAL
*		The maximum value found for ARRC
*       STATUS                  _INTEGER
*
*    Method :
*       It simply loops around multiplying all valid pixels in the
*       input array by the scalar. In the same pass through the
*	array min and max values are of the output array are found
*
*    Authors :
*	K.F.Hartley	(RGO)
*    History :
*	23-Aug-83	(KFH) First version
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NVALS			! The size of the arrays
      REAL ARRA(NVALS)			! The input array
      REAL ARRC(NVALS)			! The output array
      REAL INVA				! The invalid pixel value for ARRA
      REAL INVC				! The invalid pixel value for ARRC
      REAL MIN				! The minimum value of ARRC
      REAL MAX				! The maximum value of ARRC
      REAL SCALAR			! The multiplicative scalar
      INTEGER I				! The loop index
      LOGICAL SET			! A logical flag for whether
*					! min and max values exist
*    Local data  :
*
*   Set invalid pixel value for output to same as for input
*
      INVC=INVA
*-

*
*   If there is a bad status on entry, then return to the calling
*   program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*      At the start no estimates have been made of the minimum and
*      maximum values of the output array.
*
         SET=.FALSE.
*
*      Loop through the pixels
*
         DO I=1,NVALS
            IF (ARRA(I).NE.INVA.AND.ARRA(I).NE.0.0) THEN
*
*            If the input is valid do the division
*
               ARRC(I) = SCALAR / ARRA(I)
               IF (.NOT.SET) THEN
*
*               If we do not have an initial estimate for min and max
*               use this value, and set the flag.
*
                  MIN=ARRC(I)
                  MAX=ARRC(I)
                  SET=.TRUE.
               ELSE
*
*               If we have then improve the estimate
*
                  IF (ARRC(I).LT.MIN) MIN=ARRC(I)
                  IF (ARRC(I).GT.MAX) MAX=ARRC(I)
               END IF
            ELSE
*
*            If the input is invalid, then so is the output
*
               ARRC(I)=INVC
            END IF
         END DO
*
*      If ALL the pixels are invalid, then set the min and max
*      to the invalid value!
*
         IF (.NOT.SET) THEN
            MIN=INVC
            MAX=INVC
         END IF

      ENDIF
      END
