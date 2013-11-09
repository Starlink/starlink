*+  ARR_RANG1RLQ - Obtain 1D array data range as REAL numbers using logic quality
      SUBROUTINE ARR_RANG1RLQ(NVAL,ARRAY,LQUAL,RMIN,RMAX,STATUS)
*
*    Description :
*
*     The REAL array values in ARRAY of size NVAL are tested
*     and a REAL minimum and maximum returned in RMIN and RMAX, resp.
*
*    Authors :
*    History :
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      INTEGER NVAL		! size of ARRAY
      REAL ARRAY(*)       	! array values
      LOGICAL LQUAL(*)       	! quality values
*    Export :
      REAL RMIN         	! min. array value
      REAL RMAX         	! max. array value
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER I
*-
      IF (STATUS.EQ.SAI__OK) THEN

        RMIN=VAL__MAXR
        RMAX=VAL__MINR
        DO I=1,NVAL
           IF ( LQUAL(I) ) THEN
             IF(ARRAY(I).LT.RMIN) RMIN=ARRAY(I)
             IF(ARRAY(I).GT.RMAX) RMAX=ARRAY(I)
           END IF
        ENDDO

      ENDIF
      END
