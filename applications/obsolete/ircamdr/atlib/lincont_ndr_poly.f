	SUBROUTINE LINCONT_NDR_POLY( NUMCOEFFS, COEFFS, RAT, VALMPMK,
     :	                             VALLIN)

* Description : Applies linearization polynomial correction to a pixel value

	IMPLICIT NONE

	INTEGER
     :	  NUMCOEFFS             ! number of coefficients

	REAL
     :	  COEFFS( NUMCOEFFS),   ! array of coefficients
     :	  VALMPMK,              ! value minus ktc before correction
     :	  VALLIN,               ! value minus ktc after correction
     :	  RAT,                  ! ratio of rr to exposure time
     :	  FACTOR                ! correction factor

*      caluclate correction factor for this pixel

	FACTOR = 1.0 +
     :	         COEFFS( 1)*(( 1+RAT)**2 - RAT**2)*VALMPMK +
     :	         COEFFS( 2)*(( 1+RAT)**3 - RAT**3)*VALMPMK**2 +
     :           COEFFS( 3)*(( 1+RAT)**4 - RAT**4)*VALMPMK**3

*      correct pixel to be input pixel times correction factor

	IF( ABS( FACTOR) .GT. 1.0E-20) THEN

	  VALLIN =  VALMPMK/FACTOR

	ELSE

	  VALLIN = 0.0

	END IF

	END
