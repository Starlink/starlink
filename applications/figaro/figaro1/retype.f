C+
C                              R E T Y P E
C  Name:
C     RETYPE
C
C  Function:
C     Change the type of the main data array in a Figaro structure.
C
C  Description:
C     RETYPE converts the data in the main array of a Figaro structure
C     from its current type to a specified type.  For example, it could
C     convert a USHORT data array created by a data acquisition system
C     into a FLOAT array (which is easier to work with, especially if
C     you contemplate dividing your data by things such as flat fields).
C
C  Parameters:
C     INPUT     (Character) The name of the input data structure.
C     TYPE      (Character) The type to which the data array is to be
C               converted.  This can be any of the types recognised
C               by Figaro.
C     OUTPUT    (Character) The name of the output data structure.
C
C  Keywords: None
C
C  Note:
C     Depending on the structure of the file, RETYPE may end up creating
C     files that are larger than they need to be.  In this case,
C     TRIMFILE can be used to cut out the deadwood.
C
C  Support: K.Shortridge, AAO.
C-
C  History:
C     23 Apr 1990  KS/AAO. Original version.
C     27 Apr 1990  KS/AAO. Modified to use DSA_PREFERRED_TYPE to
C                  allow for the possibility of structured types.
C     30 Sep 1992  HME / UoE, Starlink.  Avoid discontinued CNV_
C                  routines. Since we have to use DSA_FMTCON instead
C                  of CNV_FMTCNV, we need to work with FMTCON type
C                  codes from dsa_types.inc. And we have to pass
C                  pointers to DSA_FMTCON rather than arrays. As a
C                  consequence %VAL must replace DYNAMIC_MEM.
C     07 Oct 1994  KS / AAO. DSA_FMTCON modified to use arrays rather
C                  than pointers. DYNAMIC_MEM re-instated (thereby
C                  fixing, rather belatedly) a bug reported by many
C                  users!
C     21 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Bad pixel handling.
C     24 Jul 1996  MJC / Starlink, UCL.  Corrected type of SAME.
C     2005 June 10 MJC (Starlink)  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      SUBROUTINE RETYPE
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL DSA_SAME_DATA
      INTEGER DSA_TYPESIZE, ICH_LEN
      CHARACTER*16 ICH_CI
C
C     Local variables
C
      INTEGER   BYTES            ! Number of bytes in input array
      INTEGER   DIMS(10)         ! Dimensions of data array
      LOGICAL   FLAGGED          ! True if data has flagged values
      INTEGER   I                ! Loop index
      INTEGER   IGNORE           ! Status we don't care about
      INTEGER   IPTR             ! Dynamic-mem pointer for input data
                                 ! (or copy)
      INTEGER   LENGTH           ! Number of characters in STRING
      INTEGER   NBAD             ! Number of bad conversions
      INTEGER   NDIM             ! Number of data array dimensions
      INTEGER   NELM             ! Number of elements in data array
      CHARACTER OLDTYPE*32       ! Original type for data
      INTEGER   OLD_PRIM_CODE    ! Original primitive type code
      CHARACTER OLD_PRIM_TYPE*16 ! Primitive type corresponding to
                                 ! OLDTYPE
      INTEGER   OPTR             ! Dynamic-mem pointer for output data
      INTEGER   PRIM_CODE        ! Requested primitive type code
      CHARACTER PRIM_TYPE*16     ! Primitive type to use to map data
      LOGICAL   SAME             ! Input and output are the same array?
      INTEGER   SLOT             ! Slot used for data mapping
      INTEGER   STATUS           ! Inherited status used by DSA routines
      CHARACTER STRING*64        ! Used to format conversion error
                                 ! message
      LOGICAL   STRUCT           ! Input array is a structured type?
      CHARACTER TYPE*32          ! Requested data type
      INTEGER   WORK_SLOT        ! Slot for work array - ignored
      INTEGER   WPTR             ! Dynamic-mem pointer for work array
C
C     Include DSA type codes.
C
      INCLUDE 'DSA_TYPES'
C
C     Initialise DSA system
C
      STATUS=0
      CALL DSA_OPEN (STATUS)
C
C     Open input data file, and get size and type of main data array.
C     If it's a structured type, play safe and treat it as DOUBLE.
C
      CALL DSA_INPUT ('INPUT','INPUT',STATUS)
      CALL DSA_DATA_TYPE ('INPUT',OLDTYPE,STRUCT,STATUS)
      CALL DSA_DATA_SIZE ('INPUT',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500   ! Error exit
      IF (STRUCT) THEN
         CALL PAR_WRUSER('Input data type is '//
     :            OLDTYPE(:ICH_LEN(OLDTYPE))//' (structured).',IGNORE)
         OLDTYPE='DOUBLE'
      ELSE
         CALL PAR_WRUSER('Input data type is '//
     :                              OLDTYPE(:ICH_LEN(OLDTYPE)),IGNORE)
      END IF
C
C     Get new type.
C
      CALL PAR_RDCHAR('TYPE','FLOAT',TYPE)
C
C     Open output data file.
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','INPUT',0,0,STATUS)
C
C     Eventually we are going to coerce the type of the output data
C     array.  This will destroy the data in it, which we will then have
C     to replace, converting from a copy of the original data array.
C     If this is being done in situ, then we need to save a copy of
C     the existing data array.  Otherwise, we can use the input data
C     array.  In both cases we need to start by mapping the input
C     array (allowing for the possibility that it may contain flagged
C     data values).
C
      CALL DSA_PREFERRED_TYPE (OLDTYPE,OLD_PRIM_TYPE,STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('INPUT',STATUS)
      CALL DSA_MAP_DATA ('INPUT','READ',OLD_PRIM_TYPE,IPTR,SLOT,STATUS)
      CALL DSA_SEEK_FLAGGED_VALUES ('INPUT',FLAGGED,STATUS)
      IF (FLAGGED) CALL DSA_USE_FLAGGED_VALUES ('OUTPUT',STATUS)
      SAME=DSA_SAME_DATA('INPUT','OUTPUT',STATUS)
      IF (SAME) THEN
C
C        For the in situ case, get a work array the size of the
C        original data array, copy the data into the work array and
C        close the input array.
C
         CALL DSA_GET_WORK_ARRAY (NELM,OLD_PRIM_TYPE,WPTR,
     :                            WORK_SLOT,STATUS)
         BYTES=NELM*DSA_TYPESIZE(OLD_PRIM_TYPE,STATUS)
         IF (STATUS.NE.0) GO TO 500    ! Error exit
         CALL GEN_MOVE (BYTES,%VAL(CNF_PVAL(IPTR)),%VAL(CNF_PVAL(WPTR)))
         CALL DSA_UNMAP (SLOT,STATUS)
         IPTR=WPTR
      END IF
C
C     Now coerce the output array type and map the new array.
C
      CALL DSA_COERCE_DATA_ARRAY ('OUTPUT',TYPE,NDIM,DIMS,STATUS)
      CALL DSA_PREFERRED_TYPE (TYPE,PRIM_TYPE,STATUS)
      CALL DSA_MAP_DATA ('OUTPUT','WRITE',PRIM_TYPE,OPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500    ! Error exit
C
C     And then convert the input data (or our copy of it) into the
C     output array.
C
      DO I=1,MAX_TYPES
         IF (OLD_PRIM_TYPE.EQ.TYPE_NAMES(I))
     :      OLD_PRIM_CODE=FMTCON_CODE(I)
         IF (PRIM_TYPE.EQ.TYPE_NAMES(I)) PRIM_CODE=FMTCON_CODE(I)
      END DO
      CALL DSA_FMTCON(FLAGGED,OLD_PRIM_CODE,PRIM_CODE,
     :                %VAL(CNF_PVAL(IPTR)),%VAL(CNF_PVAL(OPTR)),
     :                NELM,NBAD)
      IF (NBAD.GT.0) THEN
         CALL PAR_WRUSER(' ',IGNORE)
         STRING='Warning - '//ICH_CI(NBAD)
         LENGTH=ICH_LEN(STRING)
         STRING(LENGTH+1:)=' conversion errors occurred'
         CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),IGNORE)
         CALL PAR_WRUSER(' ',IGNORE)
      END IF
C
C     Close down
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
