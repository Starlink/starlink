*+UTIL_BINSEARCH	Binary search of sorted real array.
	SUBROUTINE UTIL_BINSEARCH(NELS, ARRAY, VALUE, MID)
* Description :
* History :
*     Author	Clive Page	1987 May 5
*     May 11 1988   Asterix88 version    (LTVAD::RDS)
* Type Definitions :
        IMPLICIT NONE
* Import :
	INTEGER NELS		!input: number of elements of data array.
	REAL ARRAY(NELS)	!input: data array in ascending order.
	REAL VALUE		!input: trial value.
* Import-Export :
*     <declarations and descriptions for imported/exported arguments>
* Export :
	INTEGER MID		!output: element of array nearest VALUE.
				!	rounding downwards.
* Local constants :
*     <local constants defined by PARAMETER>
* Local variables :
        INTEGER K,L
*-
	IF(VALUE .LT. ARRAY(1)) THEN
		MID = 1
		GO TO 100
	ELSE IF(VALUE .GT. ARRAY(NELS)) THEN
		MID = NELS
		GO TO 100
	END IF
	K = 1
	L = NELS
	MID = NELS/2
*
10	CONTINUE
	IF(VALUE .GE. ARRAY(MID)) THEN
		K = MID
	ELSE
		L = MID
	END IF
	MID = (K+L)/2
	IF(K .LT. MID .AND. MID .LT. L) GO TO 10
*
100	CONTINUE
*
	END
*+UTIL_QUALSPLIT    Splits data array into good and bad pixels
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
        LBAD= QUAL(LP1,LP2,LP3,LP4) .AND. BADBITS
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
