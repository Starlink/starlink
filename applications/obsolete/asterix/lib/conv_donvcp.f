*+  CONV_DONVCP - Conversion 3-vector cross-product
	SUBROUTINE CONV_DONVCP(VA,VB,VC)
*    Description :
*    History :
*          Author: Gavin Eadie, 1972;
*          Fortran-77 version by Clive Page, 1984 November.
*          16-Nov-1988   Asterix88 version (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
	DOUBLE PRECISION VA(3)         ! Input 3-vector
	DOUBLE PRECISION VB(3)         ! Input 3-vector
*    Import-Export :
*    Export :
	DOUBLE PRECISION VC(3)         ! Output 3-vector   VC = VA x VB
*    Local constants :
*    Local variables :
	DOUBLE PRECISION VNORM
        INTEGER I
*-
	VC(1) = VA(2)*VB(3) - VA(3)*VB(2)
	VC(2) = VA(3)*VB(1) - VA(1)*VB(3)
	VC(3) = VA(1)*VB(2) - VA(2)*VB(1)
*
	VNORM = SQRT(VC(1)**2 + VC(2)**2 + VC(3)**2)
*
	DO I = 1,3
	  VC(I) = VC(I) / VNORM
	END DO
*
	END
