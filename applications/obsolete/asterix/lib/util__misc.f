*+  UTIL_QUALSPLIT - Splits data array into good and bad pixels
      SUBROUTINE UTIL_QUALSPLIT(LP2,LP3,LP4,DIM1,DIM2,DIM3,DIM4,
     &             QUAL,BADBITS,DATA,NGOOD,GOODX,GOODY,NBAD,BADX)
* Description :
*     Takes an array of data with its corresponding quality array, and
*    indices to define a slice of this array.
*    It then creates an array of "good" values with a corresponding "good"
*    counts array and an array of "bad" values.
*    N.B. The input data and quality arrays must be ordered so that the
*        axis of interest is the first dimension.
* History :
*    13 May 1988    original           (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Global constants
      INCLUDE 'QUAL_PAR'              !Definitions of quality values
* Import :
      INTEGER LP2,LP3,LP4             !These define the slice of the 4-d array.
      INTEGER DIM1,DIM2,DIM3,DIM4     !Dimensions of data and quality arrays.
*
      BYTE QUAL(DIM1,DIM2,DIM3,DIM4)  !Quality array
      BYTE BADBITS                    !Quality mask
      REAL DATA(DIM1,DIM2,DIM3,DIM4)  !Data array
* Export :
      INTEGER NGOOD                   ! Number of good points in slice
      INTEGER GOODX(DIM1)             !"Good" points in 1st dimension
      REAL GOODY(DIM1)                ! Corresponding data values
      INTEGER NBAD                    ! Number of bad points in slice
      INTEGER BADX(DIM1)              !"Bad" points in 1st dimension
* Local constants :
      BYTE BIT_ANDUB
* Local variables :
      INTEGER LP1
      LOGICAL LBAD                            !Bad pixel ?
*-
      NGOOD=0
      NBAD=0
*
* Test each point in this slice of data
      DO LP1=1,DIM1
*
* Test quality of point
        LBAD= BIT_ANDUB(QUAL(LP1,LP2,LP3,LP4) .AND. BADBITS)
*
        IF (LBAD .NE. QUAL_GOOD) THEN
            NBAD=NBAD+1
            BADX(NBAD)= LP1
        ELSE
            NGOOD=NGOOD+1
            GOODX(NGOOD)= LP1
            GOODY(NGOOD)= DATA(LP1,LP2,LP3,LP4)
        ENDIF
*
      ENDDO
*
      END
