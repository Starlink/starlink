*+  ARR_INIT1<T> - Initialise a <TYPE> array to a given value
      SUBROUTINE ARR_INIT1<T>(VALUE,N,ARRAY,STATUS)
*    Description :
*     Initialises the REAL array ARRAY of length N to the value
*     in VALUE.
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     27 Nov 85:  original (BHVAD::JCMP)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      <TYPE>			VALUE			! initialisation value
      INTEGER 			N			! size of array
*    Export :
      <TYPE> 			ARRAY(*)		! array to be initialised
*    Status :
        INTEGER STATUS
*    Local variables :
	INTEGER I	! loop index
*-
      IF (STATUS.EQ.SAI__OK) THEN
	DO I = 1, N
	   ARRAY(I) = VALUE
	ENDDO
      ENDIF
      END
