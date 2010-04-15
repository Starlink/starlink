C+
      SUBROUTINE FIG_CMPCHK (STRUCT,MAXDIM,NDIM,DIMS,FAULT)
C
C     F I G _ C M P C H K
C
C     Checks that a given structure fulfils the Figaro requirements
C     for a complex structure, namely that it should have both
C     real and imaginary arrays, which should match in shape and size,
C     and that their dimensions should factorise in a way acceptable to
C     the FFT routines used.  The dimensions are returned by this routine.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) STRUCT   (Character) The DSA reference name of the structure,
C                  case unimportant, may be padded at the end with
C                  blanks.
C     (>) MAXDIM   (Integer) The maximum number of dimensions.
C                  Usually set to 10.
C     (<) NDIM     (Integer) The actual number of dimensions in the
C                  data.
C     (<) DIMS     (Integer array DIMS(MAXDIM)) The dimensions of the
C                  data.
C     (<) FAULT    (Logical) Returned true if the structure is invalid,
C                  false if OK.  Error messages are output by this
C                  routine using PAR_WRUSER, if a fault is found.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     DTA_SZVAR   (DTA_ package) Get size of a data object.
C     FIG_C06CHK  (FIG_    "   ) Check dimensions for FFT factorisation.
C     PAR-WRUSER  (PAR_    "   ) Output message to user.
C     ICH_LEN     (ICH_    "   ) Position of last non-blank char.
C
C                                              KS / AAO 10th Sept 1986
C     Modified
C
C     30th Mar 1991.  KS/AAO. Recoded to use DSA routines, and only to
C                     warn if no imaginary array is present.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FAULT
      INTEGER MAXDIM, NDIM, DIMS(MAXDIM)
      CHARACTER*(*) STRUCT
C
C     Local variables
C
      LOGICAL EXIST
      INTEGER I, NELM, NEXT, PREV, STATUS
C
      FAULT=.FALSE.
      STATUS=0
      CALL DSA_DATA_SIZE (STRUCT,MAXDIM,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) THEN
         FAULT=.TRUE.
         GO TO 500
      END IF
      DO I=1,NDIM
         CALL FIG_C06CHK(DIMS(I),NEXT,PREV)
         IF (DIMS(I).NE.NEXT) THEN
            CALL PAR_WRUSER(
     :           'Existing real array has invalid dimensions',STATUS)
            CALL PAR_WRUSER('It may be that R2CMPLX needs to be used.',
     :                                                          STATUS)

            FAULT=.TRUE.
            GO TO 500
         END IF
      END DO
      CALL DSA_SEEK_IMAGINARY(STRUCT,EXIST,STATUS)
      IF (STATUS.NE.0) THEN
         FAULT=.TRUE.
         GO TO 500
      END IF
      IF (.NOT.EXIST) THEN
         CALL PAR_WRUSER('"Complex" structure has no imaginary array.',
     :                                                        STATUS)
         CALL PAR_WRUSER('It may be that R2CMPLX needs to be used.',
     :                                                          STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
  500 CONTINUE
C
      END
