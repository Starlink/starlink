C+
      SUBROUTINE ERRCON
C
C     E R R C O N
C
C     Converts a Figaro file that has an error array containing
C     percentage errors into one that has absolute values in the
C     error array.  This is needed because of the ill-thought-out
C     use of percentage errors at one stage in Figaro.
C
C     Command parameters -
C
C     SPECTRUM  (Character) The name of the file to be converted.
C               This will usually be a spectrum, but data of any
C               dimension will be accepted.
C
C     OUTPUT    (Character) The name of the resulting file. This
C               can be the same as for SPECTRUM. If not, a new
C               structure is created, with everything but the error
C               array a direct copy of the input.
C
C                                      KS / AAO. 21st July 1986
C     Modified:
C
C     30th Jul 1987  DJA / AAO. New DSA_ routines. Also now uses the
C                    DYN_ set of dynamic memory routines
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     5th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      LOGICAL      ERREXIST     ! TRUE if error array exists
      INTEGER      IGNORE       ! Used to pass ignorable staus code
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OESLOT       ! Map slot number for output error array
      INTEGER      OEPTR        ! Dynamic-memory pointer to output error array
      INTEGER      OSLOT        ! Map slot number outputdata array
      INTEGER      STATUS       ! Running status for DSA_ routines
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('SPECT',10,NDIM,DIMS,NELM,STATUS)
C
C     Check the error array dimensions
C
      CALL DSA_SEEK_ERRORS('SPECT',ERREXIST,STATUS)
      IF (.NOT.ERREXIST) THEN
         CALL PAR_WRUSER('There is no error array!',IGNORE)
         GOTO 500
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map data and errors
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      CALL DSA_MAP_ERRORS('OUTPUT','UPDATE','FLOAT',OEPTR,OESLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check on the error data array
C
      CALL FIG_ECHECK(%VAL(CNF_PVAL(OEPTR)),NELM)
C
C     Now perform the conversion
C
      CALL FIG_ECON(%VAL(CNF_PVAL(OPTR)),NELM,%VAL(CNF_PVAL(OEPTR)))
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
      SUBROUTINE FIG_ECHECK(ERRORS,NERR)
C
C     F I G _ E C H E C K
C
C     Checks that an error array at least looks as though it contains
C     percentage errors - ie values between 0 and 100.  A warning is
C     issued if this is not the case.  A warning is also issued if all
C     the values are small - less than one.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ERRORS    (Real array ERRORS(NERR)) The error array.
C     (>) NERR      (Integer) The number of elements in ERRORS.
C
C                                          KS / AAO 21st July 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NERR
      REAL ERRORS(NERR)
C
C     Local variables
C
      LOGICAL LTONE
      INTEGER I,STATUS
C
      LTONE=.TRUE.
      DO I=1,NERR
         IF ((ERRORS(I).LT.0.0).OR.(ERRORS(I).GT.100.0)) THEN
            CALL PAR_WRUSER('Warning - Error array contains values '//
     :                             'outside the range 0..100',STATUS)
            CALL PAR_WRUSER(
     :             'These do not look like percentage errors',STATUS)
            GO TO 500
         END IF
         IF (ERRORS(I).GT.1.0) LTONE=.FALSE.
      END DO
      IF (LTONE) THEN
         CALL PAR_WRUSER('Warning - every element of the error array '//
     :         'is less than 1.0',STATUS)
         CALL PAR_WRUSER('Either your data is exceptionally precise, '//
     :         'or these are not percentage errors.',STATUS)
      END IF
  500 CONTINUE
C
      END
C+
      SUBROUTINE FIG_ECON(DATA,NELM,ERRORS)
C
C     F I G _ E C O N
C
C     Converts a percentage error array into an absoulte value one.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) DATA      (Real array DATA(NELM)) The data array.
C     (>) NELM      (Integer) The number of elements in the arrays.
C     (>) ERRORS    (Real array ERRORS(NELM)) The error array.
C
C                                          KS / AAO 21st July 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL DATA(NELM),ERRORS(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ERRORS(I)=ABS(ERRORS(I)*DATA(I)/100.0)
      END DO
C
      END
