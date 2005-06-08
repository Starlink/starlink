C+
      SUBROUTINE IDIFF
C
C     I D I F F
C
C     Given an image, creates a new image in which each pixel
C     is the average absolute difference between the corresponding
C     pixel in the input image and its immediate neighbours.  This
C     highlights regions in the image where the data is changing
C     rapidly.
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the image.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     Command keywords  - None
C
C     User variables used - None
C                                      KS / CIT 22nd April 1984
C
C     Modified:
C
C     27th Jul 1987  DJA / AAO. Revised DSA_ routines - some specs
C                    changed. Modified dynamic memory handling - now
C                    uses DYN_ package.
C     25th Sep 1992  HME / UoE, Starlink. INCLUDE changed. TABs removed.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      INTEGER      BYTES        ! Number of bytes of workspace required
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      DPTR         ! Dynamic-memory pointer to data array
      INTEGER      DSLOT        ! Map slot number of input data array
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      NY           ! Size of 2nd dimension (if present)
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number for output data array
      INTEGER      STATUS       ! Running status for DSA_ routines
      INTEGER      WPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT        ! Map slot number of workspace
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
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
      IF (STATUS.NE.0) GOTO 500
C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map data.  Note that GEN_IDIFF cannot operate on data in situ,
C     so in the single operand case workspace must be obtained and
C     the data copied back from it later.
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      IF (DPTR.EQ.OPTR) THEN
         BYTES=DSA_TYPESIZE('FLOAT',STATUS)*NELM
         CALL DSA_GET_WORKSPACE (BYTES,WPTR,WSLOT,STATUS)
         CALL GEN_MOVE (BYTES,%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(WPTR)))
         DPTR=WPTR
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Operate on the data.
C
      CALL GEN_IDIFF(%VAL(CNF_PVAL(DPTR)),NX,NY,%VAL(CNF_PVAL(OPTR)))
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
