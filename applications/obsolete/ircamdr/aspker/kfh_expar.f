*+   EXPAR - takes the Exponential of an array
      SUBROUTINE KFH_EXPAR(ARRA,INVA,NVALS,BASE,ARRC,INVC,MIN,MAX,
     :                     STATUS)
*    Description :
*      The exponential of every pixel in an array is taken
*      and the minimum and maximum of the output array are found
*      on the same pass through the data.
*
*    Invocation :
*     CALL KFH_EXPAR(ARRA,INVA,NVALS,BASE,ARRC,INVC,MIN,MAX,STATUS)
*    Parameters :
*	ARRA(NVALS)		_REAL
*		The input array
*	INVA			_REAL
*		The invalid pixel value for array ARRA
*	NVALS			_INTEGER
*		The size of input and output arrays
*       BASE                    _REAL
*               The base of the exponential
*	ARRC(NVALS)		_REAL
*		The output array
*	INVC			_REAL
*		The invalid pixel value for ARRC
*	MIN			_REAL
*		The minimum value found in ARRC
*	MAX			_REAL
*		The maximum value found for ARRC
*       STATUS                  _INTEGER
*               Status value
*
*    Method :
*       It simply loops around taking all valid pixels in the
*       input array, and calculating the new value of the pixel
*       as BASE to the power PIXEL. In the same pass through the
*	array min and max values are of the output array are found.
*       The program checks for input pixel values that will give
*       unacceptably high results for the exponential operation
*       and replaces these pixels with a DATA_BLANK value.
*
*    Authors :
*	K.F.Hartley	(RGO)
*       M.J.McCaughrean (UOE)
*    History :
*	23-Aug-83	(RGVAD::KFH) First version
*       30-May-85       (REVA::MJM)  Generalised to any base
*       27-May-1994     (SKL@JACH) Changed VAXMAX to PRIMDAT's NUM__MAXR
*                       for machine independence
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NVALS			! The size of the arrays
      REAL ARRA(NVALS)			! The input array
      REAL ARRC(NVALS)			! The output array
      REAL BASE                         ! The base of the exponential
      REAL INMAX                        ! Maximum allowable pixel value
      REAL VAXMAX                       ! Largest number allowed by VAX
      REAL INVA				! The invalid pixel value for ARRA
      REAL INVC				! The invalid pixel value for ARRC
      REAL MIN				! The minimum value of ARRC
      REAL MAX				! The maximum value of ARRC
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
*   If the status is bad, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*      Calculate the largest allowable input pixel value that will
*      not lead to an error when taking it as the power of the input
*      base.

         VAXMAX = NUM__MAXR

         INMAX = LOG( VAXMAX ) / LOG( BASE )

*
*      At the start no estimates have been made of the minimum and
*      maximum values of the output array.
*
         SET=.FALSE.
*
*      Loop through the pixels
*
         DO I=1,NVALS
            IF ( ARRA(I).NE.INVA.AND.ARRA(I).LT.INMAX ) THEN
*
*            If the input is valid and not too large
*            calculate the exponential
*
               ARRC(I) = BASE ** (ARRA(I))
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
*      If ALL the pixels are invalid, then set the min and max to the
*      invalid value!
*
         IF (.NOT.SET) THEN
            MIN=INVC
            MAX=INVC
         END IF

      ENDIF
      END
