*+  ARR_RANG1RQ - Obtain 1D array data range as REAL numbers using byte quality
      SUBROUTINE ARR_RANG1RQ(NVAL,ARRAY,QUAL,MASK,RMIN,RMAX,STATUS)
*
*    Description :
*
*     The REAL array values in ARRAY of size NVAL are tested
*     and a REAL minimum and maximum returned in RMIN and RMAX, resp.
*
*    Authors :
*    History :
*
*      1 Mar 94 : Updated to used BIT_ routine for ANDing (DJA)
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
      BYTE QUAL(*)       	! array values
      BYTE MASK                 ! quality mask
*    Export :
      REAL RMIN         	! min. array value
      REAL RMAX         	! max. array value
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local variables :
      INTEGER I
*-
      IF (STATUS.EQ.SAI__OK) THEN

        RMIN=VAL__MAXR
        RMAX=VAL__MINR
        DO I=1,NVAL
           IF ( BIT_ANDUB(QUAL(I),MASK).EQ.QUAL__GOOD ) THEN
             IF(ARRAY(I).LT.RMIN) RMIN=ARRAY(I)
             IF(ARRAY(I).GT.RMAX) RMAX=ARRAY(I)
           END IF
        ENDDO

      ENDIF
      END
