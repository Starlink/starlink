C+
      SUBROUTINE CMPLX2R
C
C     C M P L X 2 R    /     C M P L X 2 I   /   C M P L X 2 M
C
C     Creates a real data structure (ie one with just a real data array)
C     as opposed to a complex data structure, (in the Figaro sense of a
C     structure with both  real and imaginary data arrays)
C     from a complex data structure.  
C     In the case of CMPLX2R it is the real part of the complex data 
C     that forms the data array in the resulting structure.  
C     For CMPLX2I, it is the imaginary part, and for CMPLX2M
C     it is the modulus of the complex data.
C
C     Command parameters -
C
C     CDATA    (Character) The name of the input complex structure.
C     OUTPUT   (Character) The name of the resulting structure.  This
C              may be the same as CDATA. In either case a new file 
C              is created.
C
C     Command keywords - None
C
C                                         KS / AAO  24th Sept 1986.
C     Modified:
C
C     20th Feb 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines.
C     13th Feb 1991  JMS / AAO. Added STATUS checks to support user
C                    requested aborts.
C     30th Mar 1991  KS / AAO. Removed source of FIG_CMPCHK - this
C                    common routine is now in the FIG library.
C     13th Dec 1991  HME / UoE, Starlink. Re-appended source of
C                    FIG_CMPCHK, because its object module had not
C                    made it into the library and shareable image.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Remove the
C                    source of FIG_CMPCHK again. Call VEC_DTOR rather
C                    than the discontinued CNV_FMTCNV.
C     13th Mar 1996  HME / UoE, Starlink.  Adapt to the FDA library.
C                    No concurrent mapping. Delete output imaginary
C                    before mapping output data.
C                    Map complex in single call.
C+
      IMPLICIT NONE
C
C     Functions
C
      INTEGER DYN_ELEMENT
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      INTEGER   ADDRESS     ! Virtual address for data array
      INTEGER   ADDRESS2    ! Virtual address for data array
      CHARACTER COMMAND*8   ! Figaro command name 
      INTEGER   DIMS(10)    ! Image dimensions
      LOGICAL   FAULT       ! True if input is not a valid complex structure
      INTEGER   IERR        ! Returned by VEC_
      INTEGER   IGNORE      ! Status for VEC_
      INTEGER   IPTR        ! Dynamic memory element for imaginary data
      INTEGER   NBAD        ! Number of bad format conversions
      INTEGER   NDIM        ! Number of image dimensions
      INTEGER   NELM        ! Number of elements in image
      INTEGER   OPTR        ! Dynamic memory element for output data
      INTEGER   RPTR        ! Dynamic memory element for real data
      INTEGER   SLOT        ! Slot number for mapped data - ignored
      INTEGER   STATUS      ! Running status for DSA routines
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Dynamic memory common - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C     
C     Initial values
C
      STATUS=0
      FAULT=.FALSE.
C
C     Get command name
C
      CALL PAR_COMMAND(COMMAND)
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get value of CDATA and open file
C
      CALL DSA_INPUT ('CDATA','CDATA',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check that this is a valid complex structure and get its
C     dimensions.
C
      CALL FIG_CMPCHK('CDATA',10,NDIM,DIMS,FAULT)
      IF (FAULT) GO TO 500
      CALL DSA_DATA_SIZE ('CDATA',10,NDIM,DIMS,NELM,STATUS)
C
C     Open a new structure based on CDATA.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','CDATA',0,NEW_FILE,STATUS)
      CALL DSA_DELETE_IMAGINARY('OUTPUT',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Force the existence of a data array of type 'FLOAT' and
C     map this for 'WRITE'.
C
      CALL DSA_COERCE_DATA_ARRAY('OUTPUT','FLOAT',NDIM,DIMS,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ADDRESS,SLOT,STATUS)
      OPTR=DYN_ELEMENT(ADDRESS)
C
C     Write the appropriate data into the output data array
C
      IF (COMMAND.EQ.'CMPLX2R') THEN
C
C       Output data is a copy of input data array
C 
         CALL DSA_MAP_DATA('CDATA','READ','DOUBLE',ADDRESS,SLOT,
     :                      STATUS)
         RPTR=DYN_ELEMENT(ADDRESS)
         IF(STATUS.NE.0)GOTO 500
         IGNORE=0
         CALL VEC_DTOR(.FALSE.,NELM,DYNAMIC_MEM(RPTR),
     :      DYNAMIC_MEM(OPTR),IERR,NBAD,IGNORE)
         IF (IGNORE.NE.0) CALL ERR_ANNUL(IGNORE)
C        CALL CNV_FMTCNV('DOUBLE','FLOAT',DYNAMIC_MEM(RPTR),
C    :                    DYNAMIC_MEM(OPTR),NELM,NBAD)
         
      ELSEIF (COMMAND.EQ.'CMPLX2I') THEN
C
C       Output data is copied from input imaginary array
C
C        CALL DSA_MAP_IMAGINARY('CDATA','READ','DOUBLE',ADDRESS,SLOT,
C    :                           STATUS)
         CALL DSA_MAP_COMPLEX('CDATA','READ','DOUBLE',
     :      ADDRESS2,ADDRESS,SLOT,STATUS)
         IPTR=DYN_ELEMENT(ADDRESS)
         IF(STATUS.NE.0)GOTO 500
         IGNORE=0
         CALL VEC_DTOR(.FALSE.,NELM,DYNAMIC_MEM(IPTR),
     :      DYNAMIC_MEM(OPTR),IERR,NBAD,IGNORE)
         IF (IGNORE.NE.0) CALL ERR_ANNUL(IGNORE)
C        CALL CNV_FMTCNV('DOUBLE','FLOAT',DYNAMIC_MEM(IPTR),
C    :                    DYNAMIC_MEM(OPTR),NELM,NBAD)
          
      ELSEIF (COMMAND.EQ.'CMPLX2M') THEN
C
C         Input real and imaginary data arrays are mapped and used to 
C         calculate the modulus array which is written to the output
C         data array.
C
C        CALL DSA_MAP_IMAGINARY('CDATA','READ','DOUBLE',ADDRESS,SLOT,
C    :                            STATUS)
C        IPTR=DYN_ELEMENT(ADDRESS)
C        CALL DSA_MAP_DATA('CDATA','READ','DOUBLE',ADDRESS,SLOT,
C    :                       STATUS)
C        RPTR=DYN_ELEMENT(ADDRESS)
         CALL DSA_MAP_COMPLEX('CDATA','READ','DOUBLE',
     :      ADDRESS,ADDRESS2,SLOT,STATUS)
         RPTR=DYN_ELEMENT(ADDRESS)
         IPTR=DYN_ELEMENT(ADDRESS2)
         IF(STATUS.NE.0)GOTO 500
         CALL FIG_CMODUL(NELM,DYNAMIC_MEM(RPTR),DYNAMIC_MEM(IPTR),
     :                    DYNAMIC_MEM(OPTR))
      ENDIF


  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END
