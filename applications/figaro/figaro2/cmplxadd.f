C+
      SUBROUTINE CMPLXADD
C
C     C M P L X A D D   /   C M P L X S U B
C
C     C M P L X D I V   /   C M P L X M U L T
C
C     This routine services the Figaro complex structure arithmetic
C     routines CMPLXADD, CMPLXSUB, CMPLXDIV, and CMPLXMULT.  These
C     commands operate on two complex structures, performing element
C     by element arithmetic to produce a new structure.  The complex
C     structures may be of any dimensions, so long as they match.
C
C     Command parameters -
C
C     CDATA    (Character) The name of the first complex structure.
C     CDATA1   (Character) The name of the second complex structure.
C     OUTPUT   (Character) The name of the resulting structure.
C
C     Command keywords - None
C
C                                         KS / AAO  10th Sept 1986.
C     Modified:
C
C      7th Feb 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     18th Feb 1991  JMS / AAO. Added STATUS checks to support user
C                    requested aborts.
C     30th Mar 1991  KS / AAO. Removed source of FIG_CMPCHK - this
C                    common routine is now in the FIG library.
C     13th Dec 1991  HME / UoE, Starlink. Re-appended source of
C                    FIG_CMPCHK, because its object module had not
C                    made it into the library and shareable image.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Remove the
C                    source of FIG_CMPCHK again. Call PAR_WRUSER
C                    rather than DSA_WRUSER.
C     13th Mar 1996  HME / UoE, Starlink.  Adapt to the FDA library.
C                    Map complex in single call.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL DSA_SAME_DATA
C
C     Local variables
C
      CHARACTER COMMAND*9        ! Figaro command name
      INTEGER   DIMS(10)         ! Image dimensions
      LOGICAL   FAULT            ! Either input is not valid complex
                                 ! structure?
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image
      INTEGER   IPTR             ! Dynamic-memory pointer for imaginary
                                 ! data in CDATA
      INTEGER   I1PTR            ! Dynamic-memory pointer for imaginary
                                 ! data in CDATA1
      INTEGER   RPTR             ! Dynamic-memory pointer for real data
                                 ! in CDATA
      INTEGER   R1PTR            ! Dynamic-memory pointer for real data
                                 ! in CDATA1
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      FAULT=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Get value of CDATA and open file
C
      CALL DSA_INPUT ('CDATA0','CDATA',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check that this is a valid complex structure and get its
C     dimensions.
C
      CALL FIG_CMPCHK('CDATA0',10,NDIM,DIMS,FAULT)
      IF (FAULT) THEN
         CALL PAR_WRUSER('Error is in first complex structure.',STATUS)
         GO TO 500
      END IF
      CALL DSA_DATA_SIZE ('CDATA0',10,NDIM,DIMS,NELM,STATUS)
C
C     Similarly for CDATA1, allowing for the possibility that the
C     two are the same.
C
      CALL DSA_INPUT ('CDATA1','CDATA1',STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (.NOT.DSA_SAME_DATA('CDATA0','CDATA1',STATUS)) THEN

         CALL FIG_CMPCHK('CDATA1',10,NDIM,DIMS,FAULT)
         IF (FAULT) THEN
            CALL PAR_WRUSER(
     :         'Error is in second complex structure.',STATUS)
            GO TO 500
         END IF
C
C        Check that the dimensions match
C
         CALL DSA_MATCH_SIZES('CDATA0','CDATA1',STATUS)
         IF(STATUS.NE.0)GOTO 500

      END IF
C
C     Get name of OUTPUT structure, and if necessary create it by
C     copying the first input structure.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','CDATA0',0,NEW_FILE,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the first structure input data arrays.  Note that these will
C     have been copied to 'OUTPUT' as a new output file was created by
C     copying the first input file, so these arrays can be used instead.
C
C     CALL DSA_MAP_DATA('OUTPUT','UPDATE','DOUBLE',RPTR,SLOT,STATUS)
C     CALL DSA_MAP_IMAGINARY('OUTPUT','UPDATE','DOUBLE',IPTR,SLOT,
C    :                       STATUS)
C     IPTR=DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_COMPLEX('OUTPUT','UPDATE','DOUBLE',
     :                     RPTR,IPTR,SLOT,STATUS)
C
C     Now map the second structure arrays
C
      IF (.NOT.DSA_SAME_DATA('CDATA0','CDATA1',STATUS)) THEN
C        CALL DSA_MAP_DATA('CDATA1','READ','DOUBLE',R1PTR,SLOT,STATUS)
C        R1PTR=DYN_ELEMENT(ADDRESS)
C        CALL DSA_MAP_IMAGINARY('CDATA1','READ','DOUBLE',I1PTR,SLOT,
C    :                          STATUS)
C        I1PTR=DYN_ELEMENT(ADDRESS)
         CALL DSA_MAP_COMPLEX('CDATA1','READ','DOUBLE',
     :                        R1PTR,I1PTR,SLOT,STATUS)
      ELSE
         R1PTR=RPTR
         I1PTR=IPTR
      END IF
      IF(STATUS.NE.0) GOTO 500
C
C     Work out which command we're servicing
C
      CALL PAR_COMMAND(COMMAND)
C
C     Perform the required arithmetic
C
      IF (COMMAND.EQ.'CMPLXADD') THEN
         CALL GEN_ADDAD(NELM,%VAL(CNF_PVAL(RPTR)),%VAL(CNF_PVAL(R1PTR)),
     :                  %VAL(CNF_PVAL(RPTR)))
         CALL GEN_ADDAD(NELM,%VAL(CNF_PVAL(IPTR)),%VAL(CNF_PVAL(I1PTR)),
     :                  %VAL(CNF_PVAL(IPTR)))
      ELSE IF (COMMAND.EQ.'CMPLXSUB') THEN
         CALL GEN_SUBAD(NELM,%VAL(CNF_PVAL(RPTR)),%VAL(CNF_PVAL(R1PTR)),
     :                  %VAL(CNF_PVAL(RPTR)))
         CALL GEN_SUBAD(NELM,%VAL(CNF_PVAL(IPTR)),%VAL(CNF_PVAL(I1PTR)),
     :                  %VAL(CNF_PVAL(IPTR)))
      ELSE IF (COMMAND.EQ.'CMPLXMULT') THEN
         CALL FIG_CMPMUL(NELM,%VAL(CNF_PVAL(RPTR)),%VAL(CNF_PVAL(IPTR)),
     :                   %VAL(CNF_PVAL(R1PTR)),%VAL(CNF_PVAL(I1PTR)),
     :                   %VAL(CNF_PVAL(RPTR)),%VAL(CNF_PVAL(IPTR)))
      ELSE IF (COMMAND.EQ.'CMPLXDIV') THEN
         CALL FIG_CMPDIV(NELM,%VAL(CNF_PVAL(RPTR)),%VAL(CNF_PVAL(IPTR)),
     :                   %VAL(CNF_PVAL(R1PTR)),%VAL(CNF_PVAL(I1PTR)),
     :                   %VAL(CNF_PVAL(RPTR)),%VAL(CNF_PVAL(IPTR)))
      END IF

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
      IF(FAULT)CALL FIG_SETERR

      END
C+
      SUBROUTINE FIG_CMPMUL(NELM,RDATA,IDATA,RDATA1,IDATA1,ROUT,IOUT)
C
C     F I G _ C M P M U L
C
C     Multiplies together two complex arrays (each passed as a separate
C     real and imaginary array).
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NELM     (Integer) Number of elements in each array.
C     (>) RDATA    (Double precision array RDATA(NELM)) Real part of
C                  the first complex array.
C     (>) IDATA    (Double precision array IDATA(NELM)) Imaginary part
C                  the first complex array.
C     (>) RDATA1   (Double precision array RDATA1(NELM)) Real part of
C                  the second complex array.
C     (>) IDATA1   (Double precision array IDATA1(NELM)) Imaginary part
C                  of the second complex array.
C     (>) ROUT     (Double precision array ROUT(NELM)) Real part of
C                  the resulting complex array.
C     (>) IOUT     (Double precision array IOUT(NELM)) Imaginary part
C                  of the resulting complex array.
C
C     Common variables used - None
C
C                                            KS / AAO 11th Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      DOUBLE PRECISION RDATA(NELM), IDATA(NELM), RDATA1(NELM)
      DOUBLE PRECISION IDATA1(NELM), ROUT(NELM), IOUT(NELM)
C
C     Local variables
C
      INTEGER I
      DOUBLE COMPLEX C
C
      DO I=1,NELM
         C=DCMPLX(RDATA(I),IDATA(I))*DCMPLX(RDATA1(I),IDATA1(I))
         ROUT(I)=DREAL(C)
         IOUT(I)=DIMAG(C)
      END DO
C
      END
C+
      SUBROUTINE FIG_CMPDIV(NELM,RDATA,IDATA,RDATA1,IDATA1,ROUT,IOUT)
C
C     F I G _ C M P D I V
C
C     Divides two complex arrays (each passed as a separate
C     real and imaginary array).
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NELM     (Integer) Number of elements in each array.
C     (>) RDATA    (Double precision array RDATA(NELM)) Real part of
C                  the first complex array.
C     (>) IDATA    (Double precision array IDATA(NELM)) Imaginary part
C                  the first complex array.
C     (>) RDATA1   (Double precision array RDATA1(NELM)) Real part of
C                  the second complex array.
C     (>) IDATA1   (Double precision array IDATA1(NELM)) Imaginary part
C                  of the second complex array.
C     (>) ROUT     (Double precision array ROUT(NELM)) Real part of
C                  the resulting complex array.
C     (>) IOUT     (Double precision array IOUT(NELM)) Imaginary part
C                  of the resulting complex array.
C
C     (RDATA,IDATA) is divided by (RDATA1,IDATA1) to give (ROUT,IOUT).
C
C     Common variables used - None
C
C                                            KS / AAO 11th Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      DOUBLE PRECISION RDATA(NELM), IDATA(NELM), RDATA1(NELM)
      DOUBLE PRECISION IDATA1(NELM), ROUT(NELM), IOUT(NELM)
C
C     Local variables
C
      INTEGER I
      DOUBLE COMPLEX C
C
      DO I=1,NELM
         IF (ABS(RDATA1(I))+ABS(IDATA1(I)).GT.0.0D0) THEN
           C=DCMPLX(RDATA(I),IDATA(I))/DCMPLX(RDATA1(I),IDATA1(I))
         ELSE
           C=(0.0,0.0)
         END IF
         ROUT(I)=DREAL(C)
         IOUT(I)=DIMAG(C)
      END DO
C
      END
