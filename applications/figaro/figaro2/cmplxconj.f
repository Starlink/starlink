C+
      SUBROUTINE CMPLXCONJ
C
C     C M P L X C O N J
C
C     This routine produces the complex conjugate of a complex data
C     structure.
C
C     Command parameters -
C
C     CDATA    (Character) The name of the first complex structure.
C     OUTPUT   (Character) The name of the resulting structure.  This
C              may be the same as CDATA, in which case the operation
C              is performed in situ.  Otherwise, a new file is created.
C
C     Command keywords - None
C
C                                         KS / AAO  23rd Sept 1986.
C     Modified:
C
C     17th Feb 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     30th Mar 1991  KS / AAO. Removed source of FIG_CMPCHK - this
C                    common routine is now in the FIG library.
C     13th Dec 1991  HME / UoE, Starlink. Re-appended source of
C                    FIG_CMPCHK, because its object module had not
C                    made it into the library and shareable image.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Remove the
C                    source of FIG_CMPCHK again.
C     13th Mar 1996  HME / UoE, Starlink.  Adapt to the FDA library.
C                    Map complex in single call.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      INTEGER   DIMS(10)         ! Image dimensions
      LOGICAL   FAULT            ! Input is not valid complex data?
      INTEGER   IPTR             ! Dynamic-memory pointer for imaginary
                                 ! data
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image
      INTEGER   RPTR             ! Dynamic-memory pointer for real data
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
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
      CALL DSA_INPUT ('CDATA','CDATA',STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Check that this is a valid complex structure and get its
C     dimensions.
C
      CALL FIG_CMPCHK('CDATA',10,NDIM,DIMS,FAULT)
      IF (FAULT) GO TO 500
C
C     Get size of data in CDATA
C
      CALL DSA_DATA_SIZE ('CDATA',10,NDIM,DIMS,NELM,STATUS)
C
C     Open OUTPUT structure based on CDATA
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','CDATA',0,0,STATUS)
C
C     Map imaginary array in OUTPUT structure
C
C     CALL DSA_MAP_IMAGINARY('OUTPUT','UPDATE','DOUBLE',IPTR,SLOT,
C    :                       STATUS)
      CALL DSA_MAP_COMPLEX('OUTPUT','UPDATE','DOUBLE',
     :                     RPTR,IPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Negate the imaginary array, thereby producing the complex
C     conjugate of the data.
C
      CALL GEN_MULCAD(%VAL(CNF_PVAL(IPTR)),NELM,-1.0D0,
     :                %VAL(CNF_PVAL(IPTR)))

  500 CONTINUE

      IF (FAULT) CALL FIG_SETERR
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
