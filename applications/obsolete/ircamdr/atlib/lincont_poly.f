	SUBROUTINE LINCONT_POLY( NUMCOEFFS, COEFFS, VALMBIAS, VALLIN)

* Description : Applies linearization polynomial correction to a pixel value

	IMPLICIT NONE

	INTEGER
     :	  NUMCOEFFS             ! number of coefficients

	REAL
     :	  COEFFS( NUMCOEFFS),   ! array of coefficients
     :	  VALMBIAS,             ! value minus bias before correction
     :	  VALLIN,               ! value minus bias after correction
     :	  FACTOR                ! correction factor

*      caluclate correction factor for this pixel

	FACTOR = 1.0 +
     :	         COEFFS( 1)*VALMBIAS +
     :	         COEFFS( 2)*VALMBIAS**2 +
     :           COEFFS( 3)*VALMBIAS**3 +
     :	         COEFFS( 4)*VALMBIAS**4 +
     :           COEFFS( 5)*VALMBIAS**5

*      correct pixel to be input pixel times correction factor

	VALLIN =  VALMBIAS*FACTOR

	END
