*+  CONV_DONGEN - Generates coordinate Conversion matrix from two 3-vectors
	SUBROUTINE CONV_DONGEN(VZ,VX,CTOS)
*    Description :
*    History :
*     Author: Gavin Eadie, 1972;
*     Fortran-77 version by Clive Page, 1984 November.
*     16-Nov-1988    Asterix88 version  (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      DOUBLE PRECISION VZ(3)     ! Vector of pole of new coords in old frame
      DOUBLE PRECISION VX(3)     ! Vector of reference point on equator i.o.f.
*    Import-Export :
*    Export :
      DOUBLE PRECISION CTOS(3,3) ! Unit matrix for conversion to new coordinates
*    Local constants :
*    Local variables :
      INTEGER I
*-
      DO I = 1,3
	  CTOS(I,3) = VZ(I)
      END DO
*
      CALL CONV_DONVCP ( VZ,VX,CTOS(1,2) )
      CALL CONV_DONVCP ( CTOS(1,2),VZ,CTOS(1,1) )
*
      END
