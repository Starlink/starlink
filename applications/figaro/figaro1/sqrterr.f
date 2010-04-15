C+
      SUBROUTINE SQRTERR
C
C     S Q R T E R R
C
C     Sets an error array for which the value of each element is
C     given by the square root of the corresponding data array
C     element.  Optionally, the data array element value may be divided
C     by a specified constant before the square root is taken.  The
C     case where the constant is 1 is suitable for data such as
C     photon-counting data is being used.  For CCDs and similar devices,
C     the constant should be equal to the ADU/photon value.
C
C     Command parameters -
C
C     IMAGE  (Character) The name of the structure containing the image.
C
C     FACTOR (Numeric) The value of the constant by which the data
C            values are divided before the square root is taken.
C
C     OUTPUT (Character) The name of the result of the operation.  This
C            can be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the error array a direct
C            copy of the input.
C
C                                      KS / AAO 8th Jan 1987
C
C     Modified:
C
C     27th Aug 1987  DJA / AAO. Revised DSA_ routines - some specs
C                    changed. Dynamic memory handling now done by
C                    DYN_ package.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     30th Sep 1992  HME / UoE, Starlink.  TABs removed, INCLUDE
C                    changed.
C     29th Jul 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Numeric parameter limits - close to VAX real limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Local variables
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      EPTR          ! Dynamic-memory pointer to output
                                 ! error array
      INTEGER      ESLOT         ! Slot number for output error array
      REAL         FACTOR        ! See above
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      OPTR          ! Dynamic-memory pointer to output
                                 ! data array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      STATUS        ! Running status for DSA_ routines
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of constant factor
C
      CALL PAR_RDVAL('FACTOR',FMIN,FMAX,1.,' ',FACTOR)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map error array
C
      CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',EPTR,ESLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Generate the error values from the data and from FACTOR.
C
      CALL FIG_SQRERR(%VAL(CNF_PVAL(OPTR)),NELM,FACTOR,
     :                %VAL(CNF_PVAL(EPTR)))
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_SQRERR (DATA,NELM,FACTOR,ERRORS)
C
C     F I G _ S Q R E R R
C
C     Calculates an error array given a data array and a constant
C     value, as Error=SQRT(Data/Const).
C
C     Parameters -  (">" input, "<" output)
C
C     (>) DATA    (Real array DATA(NELM)) The data array.
C     (>) NELM    (Integer) The number of elements in the array.
C     (>) FACTOR  (Real) The constant factor to be applied.
C     (<) ERRORS  (Real array ERRORS(NELM)) The resulting error array.
C
C     Common variables used -  None
C
C     Subroutines / functions used - None
C
C                                           KS / AAO 8th Jan 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL DATA(NELM),FACTOR,ERRORS(NELM)
C
C     Local variables
C
      INTEGER I
C
C     Calculate error array
C
      IF (FACTOR.EQ.0.0) THEN
         DO I=1,NELM
            ERRORS(I)=DATA(I)
         END DO
      ELSE
         DO I=1,NELM
            ERRORS(I)=SQRT(ABS(DATA(I)/FACTOR))
         END DO
      END IF
C
      END
