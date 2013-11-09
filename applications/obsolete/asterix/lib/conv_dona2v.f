*+  CONV_DONA2V - Converts spherical-polar coordinate to 3-vector - Double prec.
	SUBROUTINE CONV_DONA2V(AZ,EL,V)
*    Description :
*     <description of what the subroutine does>
*    History :
*          Author: Gavin Eadie, 1972;
*          Fortran-77 version by Clive Page, 1984 November.
*          Asterix88 version by Richard Saxton May 1988
*    Type Definitions :
      IMPLICIT NONE
*    Import :
	DOUBLE PRECISION AZ            !R.A. (or longitude or azimuth), radians.
        DOUBLE PRECISION EL            !Dec (or latitude or elevation), radians.
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
	DOUBLE PRECISION V(3)          !Returns 3-vector of unit length.
*    Local constants :
*    Local variables :
        DOUBLE PRECISION CEL           !Cos of the elevation
*-
	CEL = COS(EL)
	V(1) = COS(AZ) * CEL
	V(2) = SIN(AZ) * CEL
	V(3) = SIN(EL)
*
	END
