	SUBROUTINE ACCESS_PIXEL( CURSOR_IMAGE, X_PIXEL, Y_PIXEL, CURSOR_VALUE)

* Description : Takes input X,Y pixel value and array and gets the value of
*               the pixel, putting it into the output variable

	IMPLICIT NONE

	INCLUDE 'PLT2DCOM'

	INTEGER X_PIXEL
	INTEGER Y_PIXEL

	REAL CURSOR_IMAGE( NX, NY)
	REAL CURSOR_VALUE

*     Get the pixel value and put into output variable

	CURSOR_VALUE = CURSOR_IMAGE( X_PIXEL, Y_PIXEL)

	END
