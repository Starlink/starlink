*+  ARR_SUMDIM : Find total number of elements in a multi-dimensional array
      SUBROUTINE ARR_SUMDIM( NACTDIM, IDIM, NELM )
*    Description :
*     Finds product of sizes of dimensions of a given array
*    Author:
*     David Allan ( dja @ uk.ac.bham.sr.star )
*    History :
*     25 Jul 88 : Original
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER			NACTDIM		! Number of dimensions
      INTEGER			IDIM(*)		! Sizes of dimensions
*    Export :
      INTEGER			NELM		! Total number of elements
*    Local variables :
      INTEGER			I		! Loop counter
*-

      NELM = 1
      IF ( NACTDIM .GT. 0 ) THEN
        DO I = 1 , NACTDIM
	  NELM = NELM * IDIM(I)
        END DO
      END IF
      END
