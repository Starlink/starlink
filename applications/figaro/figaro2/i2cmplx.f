C+
      SUBROUTINE I2CMPLX
C
C     I 2 C M P L X
C
C     Sets the imaginary part of an existing complex data structure.
C     The complex data structure will probably have been created in the
C     first instance by R2CMPLX, which leaves the imaginary part zero.
C     I2CMPLX takes the data array from a specified file (usually not a
C     complex one - note that if it is complex, it is the modulus that
C     is used) and uses it to form the imaginary part of the complex
C     data.
C
C     Command parameters -
C
C     IDATA       (Character) The data structure containing the array
C                 that is to form the imaginary part of the complex
C                 data.  That is, the data array of IDATA becomes the
C                 imaginary data array of the output complex structure.
C     CDATA       (Character) The resulting complex data structure.
C                 Note that this is an already existing structure.
C
C                                             KS / AAO 8th Sept 1986
C     Modified:
C
C     20th Feb 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     20th Feb 1991  JMS / AAO. Added STATUS checks to support user
C                    requested aborts. Now performs a simple copy
C                    of input array into imaginary array if no format
C                    change required.
C     30th Mar 1991  KS / AAO. Removed source of FIG_CMPCHK - this
C                    common routine is now in the FIG library.
C     13th Dec 1991  HME / UoE, Starlink. Re-appended source of
C                    FIG_CMPCHK, because its object module had not
C                    made it into the library and shareable image.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Remove the
C                    source of FIG_CMPCHK again.
C     25th Jul 1995  HME / UoE, Starlink.  Map input with type Float,
C                    so that FIG_CHCOPY works properly. Must then use
C                    VEC_RTOD instead of GEN_MOVE when there is no
C                    padding to do.
C     13th Mar 1996  HME / UoE, Starlink.  Adapt to the FDA library.
C                    Input read-only. Use DSA_INPUT_UPDATE for CDATA
C                    Map complex in single call.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL DSA_SAME_DATA
      CHARACTER GEN_NTH*2        ! Returns 'st','th','rd' etc .
                                 ! appropriate to N
      CHARACTER ICH_CI*12        ! Return an integer as a character
                                 ! string
      INTEGER  ICH_LEN           ! Position of last non-blank char in
                                 ! string
C
C     Local variables
C
      INTEGER   DIMS(10)         ! Image dimensions
      INTEGER   IPTR             ! Dynamic-memory pointer for imag. data
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   RPTR             ! Dynamic-memory pointer for real data
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
C
C     Local variables
C
      LOGICAL CHANGE
      LOGICAL FAULT, SPACED
      INTEGER DIMS0(10),I
      INTEGER IIPTR, NDIM0, IGNORE, IERR, NBAD
      INTEGER  NELM0, NPTR
      CHARACTER STRING*80
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      FAULT=.FALSE.
      STATUS=0
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get name of imaginary data and open the file.
C
      CALL DSA_INPUT ('IDATA','IDATA',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IDATA',10,NDIM0,DIMS0,NELM,STATUS)
C
C     Get name of resulting complex structure.
C
      CALL DSA_INPUT_UPDATE ('CDATA','CDATA',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check that we have a valid complex structure and get its dimensions.
C
      IF(DSA_SAME_DATA('CDATA','IDATA',STATUS))THEN
         CALL FIG_CMPCHK ('IDATA',10,NDIM,DIMS,FAULT)
      ELSE
         CALL FIG_CMPCHK ('CDATA',10,NDIM,DIMS,FAULT)
      END IF
      IF (FAULT.OR.STATUS.NE.0) GO TO 500
C
C     We are going to force each dimension of the input data to match
C     that of the complex data.
C
      NELM=1
      NELM0=1
      CHANGE=.FALSE.
      IF (NDIM0.NE.NDIM) THEN
         CALL PAR_WRUSER('Input data and complex data have a different'
     :                               //' number of dimensions',STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
      SPACED=.FALSE.
      DO I=1,NDIM
         NELM0=NELM0*DIMS0(I)
         NELM=NELM*DIMS(I)
         STRING=ICH_CI(I)
         NPTR=ICH_LEN(STRING(:2))+1
         STRING(NPTR:)=GEN_NTH(I)//' dimension of input data ('//
     :                                          ICH_CI(DIMS0(I))
         NPTR=ICH_LEN(STRING)+1
         IF (DIMS0(I).NE.DIMS(I)) THEN
            IF (.NOT.SPACED) THEN
               CALL PAR_WRUSER(' ',STATUS)
               SPACED=.TRUE.
            END IF
            STRING(NPTR:)=') differs from the corresponding'
            CALL PAR_WRUSER(STRING(:NPTR+33),STATUS)
            STRING='dimension ('//ICH_CI(DIMS(I))
            NPTR=ICH_LEN(STRING)+1
            IF (DIMS0(I).GT.DIMS(I)) THEN
               STRING(NPTR:)=
     :            ') of the complex data, and will be truncated.'
            ELSE
               STRING(NPTR:)=
     :            ') of the complex data, and will be zero extended.'
            END IF
            CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),STATUS)
            CHANGE=.TRUE.
         END IF
      END DO
      IF (CHANGE) THEN
         CALL PAR_WRUSER(' ',STATUS)
         CALL PAR_WRUSER(
     :     'This may well cause spurious results in an FFT, especially',
     :                                                           STATUS)
         CALL PAR_WRUSER(
     :     'if the data does not go smoothly to zero at the ends',
     :                                                           STATUS)
      END IF
C
C     Map the input array.
C
      CALL DSA_MAP_DATA('IDATA','READ','FLOAT',IIPTR,SLOT,STATUS)
C
C     Map the output real and imaginary arrays.
C
C     CALL DSA_MAP_IMAGINARY('CDATA','UPDATE','DOUBLE',IPTR,SLOT,
C    :                        STATUS)
      CALL DSA_MAP_COMPLEX('CDATA','UPDATE','DOUBLE',RPTR,IPTR,SLOT,
     :                     STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Copy the data from the input array into the imaginary array.
C     If no format change is involved, then this is a simple copy
C     Otherwise, the data has to be re-dimensioned as well.
C
      IF (CHANGE) THEN
         CALL FIG_CHCOPY(NDIM,DIMS0,DIMS,NELM0,NELM,
     :                   %VAL(CNF_PVAL(IIPTR)),%VAL(CNF_PVAL(IPTR)))
      ELSE
         IGNORE=0
         CALL VEC_RTOD(.FALSE.,NELM,%VAL(CNF_PVAL(IIPTR)),
     :                  %VAL(CNF_PVAL(IPTR)),IERR,NBAD,IGNORE)
         IF (IGNORE.NE.0) CALL ERR_ANNUL(IGNORE)
      END IF

  500 CONTINUE
C
C     Close down everything.
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END
