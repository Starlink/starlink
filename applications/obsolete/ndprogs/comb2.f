      SUBROUTINE COMB2
C+
C
C   ---------
C   C O M B 2
C   ---------
C
C   Description
C   -----------
C   This routine services the following commands:
C
C     ARITH2 - Arithmetic operations between two images. The operations
C              supported are: +, -, *, /, **.
C
C     LOGIC2 - Bitwise logical operations between two images. The operations
C              supported are: AND, OR, XOR, NAND, NOR, NXOR.
C
C     MASK2  - Masking operations between two images. The operations
C              supported are: MAX, MIN, replace where IMAGE is non-magic,
C              replace where IMAGE1 is non-magic, replace where IMAGE and
C              IMAGE1 are both non-magic, replace where IMAGE is magic.
C              (Read bad quality for magic if quality data is present)
C
C   If the input images have different dimensionalities, the one with the
C   larger number of dimensions must be given first, as IMAGE. In such a
C   case the combination may be performed repeatedly, starting and ending at
C   given coordinates in the higher dimensions of IMAGE. For example a 2-D
C   IMAGE1 may be combined with several consecutive planes of a 3-D IMAGE.
C
C   The axes of IMAGE1 are aligned with the corresponding axes of IMAGE,
C   i.e. a 1-D IMAGE1 is always combined with the X rows of IMAGE. This is
C   the most efficient way to operate on arrays, producing the lowest number
C   of page faults. If it is required to combine a 1-D IMAGE1 with the Z
C   rows of a 3-D IMAGE, the XYZ to ZXY transposition of IMAGE may be
C   achieved by prior use of TRANSPOSE.
C
C   The output structure will be a copy of the FIRST input structure.
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C     (bitwise logical operations accept SHORT array type only).
C   - Subsetting supported.
C   - Magic values supported.
C   - Quality and error arrays supported.
C   - Batch execution supported.
C
C
C   Environment
C   -----------
C   FIGARO
C
C
C   Parameters (read or written)
C   ----------------------------
C   IMAGE    Name of the structure containing the first image. (character)
C            (prompted for).
C
C   IMAGE1   Name of the structure containing the second image. (character)
C            (prompted for).
C
C   START    Coordinate in each dimension of IMAGE at which the operation is
C            to start. (real, array)(prompted for).
C
C   END      Coordinate in each dimension of IMAGE at which the operation is
C            to end. (real, array)(prompted for).
C
C   OPER     Operation to be performed. (character)(prompted for).
C
C   OUTPUT   Name of the structure containing the output image. May be the
C            same as IMAGE. (character)(prompted for).
C
C
C   Keywords
C   --------
C   WHOLE    Instructs the program to operate on the whole image. Otherwise,
C            a subset of the image may be selected.
C
C
C   Propagation of data structure
C   -----------------------------
C   - Principal structure is IMAGE.
C   - All standard objects are copied from IMAGE.
C   - Data array is modified.
C
C
C   Method
C   ------
C   - The IMAGE and IMAGE1 structures are tested for the bad data flag. If
C     either is found and non-zero, magic values are assumed to be present
C     and are left in the data.
C   - A subset of IMAGE is optionally selected by specifying the start and
C     end coordinates in each dimension. If IMAGE has more dimensions than
C     IMAGE1, the required start and end coordinates in each of the higher
C     dimensions are also prompted for. For example, if it is required to
C     combine a 2-D IMAGE1 with a 3-D IMAGE, the start and end pixels in the
C     third dimension of IMAGE control which XY planes of IMAGE are combined
C     with IMAGE1.
C   - The structure IMAGE is copied to OUTPUT. If the data array of either
C     IMAGE or IMAGE1 is of type FLOAT, then both IMAGE and IMAGE1 are
C     mapped as FLOAT.
C   - Error and quality arrays are mapped as appropriate.
C   - A subroutine appropriate to the required operation, the data type, and
C     the presence or absence of magic values is called to operate on the
C     IMAGE1 and OUTPUT data arrays.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C      DSA_AXIS_RANGE
C      DSA_CLOSE
C      DSA_DATA_SIZE
C      DSA_INPUT
C      DSA_MAP_DATA
C      DSA_MAP_ERRORS
C      DSA_MAP_QUALITY
C      DSA_OPEN
C      DSA_OUTPUT
C      DSA_USE_FLAGGED_VALUES
C      DSA_WRUSER
C
C   Library DYN:
C      DYN_ELEMENT
C
C   Library ICH:
C      ICH_ENCODE
C      ICH_FOLD
C      ICH_LEN
C
C   Library NDP:
C      NDP_ERROR_ARITH_R
C      NDP_ERROR_ARITH_W
C      NDP_GET_IMAGE_INFO
C      NDP_PAR_RDARY
C      NDP_SET_BAD_PIXEL
C
C   Library PAR:
C      PAR_COMMAND
C      PAR_RDCHAR
C      PAR_RDVAL
C
C
C   Internal subroutines called
C   ---------------------------
C   COMB2_AC_W
C   COMB2_AC_WQ
C   COMB2_AC_R
C   COMB2_AC_RQ
C   COMB2_B_W
C   COMB2_B_WQ
C   REPLACE_ERRORS_R2
C   REPLACE_ERRORS_W2
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C   INCLUDE 'DCV_FUN'
C
C
C   Extensions to FORTRAN 77
C   ------------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C   - Allow a fractional offset between images before combination, with
C     linear interpolation of pixel values.
C   - Purging of the error array when void values are present. At present,
C     this would require use of DSA common block data and routines in the
C     NON-shareable DSA library. Hint, hint, Keith?
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   29-AUG-1990   - Bug which sometimes caused the output file to be mapped
C                   as the wrong type was fixed.  A few typos were also
C                   corrected.  (JRL)
C   10-AUG-1991   - Quality and error handling implemented (GOLDJIL)
C   16-SEP-1991   - Bug which led to erroneous output if IMAGE
C                   contained SHORT data and IMAGE1 FLOAT data fixed.
C                   (GOLDJIL)
C   30-NOV-1992   - Unix version (GOLDJIL).
C   06-OCT-1994   - Removed a Trojan Horse message. Also ensured that an
C                   infinite loop does not occur when an improper operation is
C                   requested. Removed unused variables. (GJP)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Functions used.
C
      INTEGER  DYN_ELEMENT,ICH_ENCODE,ICH_LEN
      LOGICAL  PAR_ABORT
C
C   Local variables.
C
      CHARACTER ACTION*8            ! What do do with a duff error array
      INTEGER   ADDRESS             ! Address of dynamic memory element
      INTEGER   ADDRESS1            ! Another of the above :-)
      LOGICAL   BADERR              ! Flags presence of void errors in array
      LOGICAL   BADPIX              ! Value of bad pixel flag in IMAGE
      LOGICAL   BADPIX1             ! Value of bad pixel flag in IMAGE1
      CHARACTER COMMAND*16          ! Name of command being serviced
      INTEGER   DIMS(10)            ! Dimensions of IMAGE
      INTEGER   DIMS1(10)           ! Dimensions of IMAGE1
      INTEGER   DUMINT              ! INTEGER dummy variable
      REAL      DUMREAL             ! Of course it's REAL, dummy!
      REAL      END(6)              ! End coordinates of IMAGE subset
      INTEGER   ENDPIX(6)           ! End pixel in IMAGE
      LOGICAL   ERR                 ! Flags presence of any error data
      CHARACTER ERR_MAP_MODE*8      ! Read or write errors?
      INTEGER   I                   ! Loop counter
      INTEGER   IE1PTR              ! Dynamic pointer to IMAGE1 errors
      LOGICAL   IERR,IERR1          ! Flags for presence of error data in I/P
      INTEGER   IE1SLOT             ! Slot numbers for error data
      INTEGER   IM1PTR              ! Dynamic pointer to IMAGE1 data array
      INTEGER   IQ1PTR              ! Dynamic pointer to IMAGE1 quality array
      LOGICAL   IQUAL,IQUAL1        ! Flags for presence of quality data in i/p
      INTEGER   IQ1SLOT      ! Slots for i/p quality arrays
      INTEGER   I1SLOT              ! Map slot number for IMAGE1 data
      CHARACTER MAP_TYPE*8          ! Data type of MAPPED information
      INTEGER   NDIM                ! Number of dimensions in IMAGE
      INTEGER   NDIM1               ! Number of dimensions in IMAGE1
      INTEGER   NELM                ! Number of elements in IMAGE
      INTEGER   NELM1               ! Number of elements in IMAGE1
      INTEGER   NEXT                ! Pointer returned by ICH_ENCODE
      INTEGER   OEPTR               ! Dynamic pointer to o/p error array
      INTEGER   OESLOT              ! Slot number for o/p error array
      INTEGER   OQPTR               ! Dynamic pointer to o/p quality array
      INTEGER   OQSLOT              ! Slot number for o/p quality array
      CHARACTER OPER*8              ! Operation name
      INTEGER   OSLOT               ! Map slot number for OUTPUT data
      INTEGER   OUTPTR              ! Dynamic pointer to OUTPUT data array
      LOGICAL   PURGE_ERR           ! Get rid of error array, please
      LOGICAL   PURGE_QUAL          ! Oh, and quality data while you're at it
      LOGICAL   QUAL                ! Flags presence of any quality data
      CHARACTER QUAL_MAP_MODE*8     ! Read or write errors?
      INTEGER   STAPIX(6)           ! Start pixel in IMAGE
      REAL      START(6)            ! Start coordinates of IMAGE subset
      INTEGER   STATUS              ! Status code
      CHARACTER STRING*80           ! Message string
      CHARACTER TYPE*8              ! IMAGE data array type
      CHARACTER TYPE1*8             ! IMAGE1 data array type
      REAL      VMAX(6)             ! Maximum values for NDP_PAR_RDARY
      REAL      VMIN(6)             ! Minimum values for NDP_PAR_RDARY
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=0)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
      INCLUDE 'DCV_FUN'
C
C   Initialize.
C
      BADERR=.FALSE.
      STATUS=0
      DO I=1,6
        STAPIX(I)=1
        VMIN(I)=MIN_FLOAT
      END DO
C
C   Open DSA system.
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get command name.
C
      CALL PAR_COMMAND(COMMAND)
C
C   Open file for IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Display information on IMAGE.
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.TRUE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   If command is LOGIC2, check that IMAGE is INTEGER*2.
C
      IF(COMMAND.EQ.'LOGIC2')THEN
        IF(TYPE.NE.'SHORT')THEN
          CALL DSA_WRUSER
     &      ('IMAGE data type must be SHORT for bitwise operations.\\N')
          GO TO 500
        END IF
      END IF
C
C   Get dimensions of IMAGE array.
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
      DO I=1,NDIM
        ENDPIX(I)=REAL(DIMS(I))
        VMAX(I)=REAL(DIMS(I))
      END DO
C
C   Open file for IMAGE1.
C
      CALL DSA_INPUT('IMAGE1','IMAGE1',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Display information on IMAGE1.
C
      CALL NDP_GET_IMAGE_INFO
     &  ('IMAGE1',.TRUE.,.TRUE.,TYPE1,BADPIX1,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   If command is LOGIC2, check that IMAGE1 is INTEGER*2.
C
      IF(COMMAND.EQ.'LOGIC2')THEN
        IF(TYPE1.NE.'SHORT')THEN
          CALL DSA_WRUSER
     &  ('IMAGE1 data type must be SHORT for bitwise operations.\\N')
          GO TO 500
        END IF
      END IF
C
C   Get dimensions of IMAGE1 array.
C
      CALL DSA_DATA_SIZE('IMAGE1',6,NDIM1,DIMS1,NELM1,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Set single BADPIX flag.
C
      BADPIX=BADPIX.OR.BADPIX1
C
C   Check that IMAGE has the higher dimensionality.
C
      IF(NDIM1.GT.NDIM)THEN
        CALL DSA_WRUSER('IMAGE must have the higher dimensionality.\\N')
        GO TO 500
      END IF
C
C   Check that any error information is compatible in IMAGE & IMAGE1
C
      CALL DSA_SEEK_ERRORS('IMAGE',IERR,STATUS)
      CALL DSA_SEEK_ERRORS('IMAGE1',IERR1,STATUS)
      CALL DSA_SEEK_QUALITY('IMAGE',IQUAL,STATUS)
      CALL DSA_SEEK_QUALITY('IMAGE1',IQUAL1,STATUS)
C
      PURGE_ERR=.FALSE.
      PURGE_QUAL=.FALSE.
      ERR = IERR.AND.IERR1
      QUAL = IQUAL.AND.IQUAL1
      IF (IERR.XOR.IERR1) THEN          ! Only one has error data
        CALL DSA_WRUSER('\\n** Error array present in one')
        CALL DSA_WRUSER(' structure only - this will be')
        CALL DSA_WRUSER(' ignored\\n\\n')
C
C   We need only purge errors if IMAGE has error data, since this
C   is copied to OUTPUT.
C
        IF (IERR) PURGE_ERR = .TRUE.
      END IF
      IF (IQUAL.XOR.IQUAL1) THEN        ! Only one has quality data
        IF (COMMAND.NE.'MASK2') THEN    ! Some MASK2 options only need 1 q.a.
          CALL DSA_WRUSER('\\n** Quality array present in one')
          CALL DSA_WRUSER(' structure only - this will be')
          CALL DSA_WRUSER(' ignored\\n\\n')
          IF (IQUAL) PURGE_QUAL = .TRUE.  ! See above comment
        END IF
      END IF
C
C   Get IMAGE axis range.
C
      CALL NDP_AXIS_RANGE
     &  ('IMAGE',DIMS,NDIM,START,END,STAPIX,ENDPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Make sure the range does not exceed the dimensions of IMAGE1.
C
      DO I=1,NDIM1
        IF(ENDPIX(I).GT.DIMS1(I))THEN
          STRING='Warning: the requested range for IMAGE axis '
          DUMINT=ICH_ENCODE(STRING,REAL(I),45,0,NEXT)
          CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
          CALL DSA_WRUSER(' exceeds the corresponding dimension of ')
          CALL DSA_WRUSER('IMAGE1. The operation will stop at the end ')
          CALL DSA_WRUSER('of this dimension of IMAGE1.\\N')
          ENDPIX(I)=DIMS1(I)
        END IF
      END DO
C
C   Get operation name.
C
   10 CONTINUE
      CALL PAR_CNPAR('OPER')
      CALL PAR_RDCHAR('OPER',' ',STRING)
      IF (PAR_ABORT()) GO TO 500
      OPER=STRING(:ICH_LEN(STRING))
      IF(COMMAND.EQ.'ARITH2')THEN
        IF(OPER.NE.'+' .AND.
     &     OPER.NE.'-' .AND.
     &     OPER.NE.'*' .AND.
     &     OPER.NE.'/' .AND.
     &     OPER.NE.'**')THEN
          CALL DSA_WRUSER('Unrecognized option.\\N')
          GO TO 10
        END IF
      ELSE IF(COMMAND.EQ.'LOGIC2')THEN
C
C   It doesn't make sense to have an error array when using bit operations
C
        IF (ERR) PURGE_ERR=.TRUE.
        IF(OPER.NE.'AND' .AND.
     &     OPER.NE.'OR' .AND.
     &     OPER.NE.'XOR' .AND.
     &     OPER.NE.'NAND' .AND.
     &     OPER.NE.'NOR' .AND.
     &     OPER.NE.'NXOR')THEN
          CALL DSA_WRUSER('Unrecognized option.\\N')
          GO TO 10
        END IF
      ELSE IF(COMMAND.EQ.'MASK2')THEN
        IF(OPER.NE.'MAX' .AND.
     &     OPER.NE.'MIN' .AND.
     &     OPER.NE.'REP1' .AND.
     &     OPER.NE.'REP2' .AND.
     &     OPER.NE.'REP3' .AND.
     &     OPER.NE.'REP4')THEN
          CALL DSA_WRUSER('Unrecognized option.\\N')
          GO TO 10
        END IF
      END IF
C
C   Open file for OUTPUT.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Magic values are not to be removed from data arrays (providing
C   quality is not present)...
C
      IF ((.NOT.IQUAL).AND.(.NOT.IQUAL1)) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        CALL DSA_USE_FLAGGED_VALUES('IMAGE1',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Map OUTPUT data array. If IMAGE1 is FLOAT, OUTPUT must be mapped as FLOAT.
C   Otherwise, map OUTPUT as SHORT or FLOAT depending on IMAGE's type. (As
C   OUTPUT is simply a copy of IMAGE). (Ditto error array)
C
      IF (PURGE_ERR) THEN
        ERR_MAP_MODE='READ'
      ELSE
        ERR_MAP_MODE='UPDATE'
      END IF
      IF (PURGE_QUAL) THEN
        QUAL_MAP_MODE='READ'
      ELSE
        QUAL_MAP_MODE='UPDATE'
      END IF
      IF(TYPE1.EQ.'FLOAT')THEN
        CALL DSA_MAP_DATA
     &    ('OUTPUT','UPDATE','FLOAT',ADDRESS,OSLOT,STATUS)
        MAP_TYPE='FLOAT'
        IF (ERR) CALL DSA_MAP_ERRORS
     &      ('OUTPUT',ERR_MAP_MODE,'FLOAT',ADDRESS1,OESLOT,STATUS)
      ELSE
        IF(TYPE.EQ.'SHORT')THEN
          CALL DSA_MAP_DATA
     &      ('OUTPUT','UPDATE','SHORT',ADDRESS,OSLOT,STATUS)
          MAP_TYPE='SHORT'
          IF (ERR) CALL DSA_MAP_ERRORS
     &        ('OUTPUT',ERR_MAP_MODE,'SHORT',ADDRESS1,OESLOT,STATUS)

        ELSE
          CALL DSA_MAP_DATA
     &      ('OUTPUT','UPDATE','FLOAT',ADDRESS,OSLOT,STATUS)
          MAP_TYPE='FLOAT'
          IF (ERR) CALL DSA_MAP_ERRORS
     &        ('OUTPUT',ERR_MAP_MODE,'FLOAT',ADDRESS1,OESLOT,STATUS)

        END IF
      END IF
      IF(STATUS.NE.0)GO TO 500
      OUTPTR=DYN_ELEMENT(ADDRESS)
      OEPTR=DYN_ELEMENT(ADDRESS1)
C
C   Map IMAGE1 data array. If IMAGE is FLOAT, IMAGE1 must be mapped as FLOAT.
C   Otherwise, map IMAGE1 as SHORT or FLOAT depending on its own type. Ditto
C   the error array (if any).
C
      IF(TYPE.EQ.'FLOAT')THEN
        CALL DSA_MAP_DATA
     &    ('IMAGE1','READ','FLOAT',ADDRESS,I1SLOT,STATUS)
        IF (ERR) CALL DSA_MAP_ERRORS
     &    ('IMAGE1','READ','FLOAT',ADDRESS1,IE1SLOT,STATUS)
      ELSE
        IF(TYPE1.EQ.'SHORT')THEN
          CALL DSA_MAP_DATA
     &      ('IMAGE1','READ','SHORT',ADDRESS,I1SLOT,STATUS)
          IF (ERR) CALL DSA_MAP_ERRORS
     &      ('IMAGE1','READ','SHORT',ADDRESS1,IE1SLOT,STATUS)
        ELSE
          CALL DSA_MAP_DATA
     &      ('IMAGE1','READ','FLOAT',ADDRESS,I1SLOT,STATUS)
          IF (ERR) CALL DSA_MAP_ERRORS
     &      ('IMAGE1','READ','FLOAT',ADDRESS1,IE1SLOT,STATUS)

        END IF
      END IF
      IF(STATUS.NE.0)GO TO 500
      IM1PTR=DYN_ELEMENT(ADDRESS)
      IE1PTR=DYN_ELEMENT(ADDRESS1)
C
C   Map quality arrays
C
      IF (IQUAL1) THEN
        CALL DSA_MAP_QUALITY('IMAGE1','READ','BYTE',
     &                        ADDRESS,IQ1SLOT,STATUS)
        IQ1PTR=DYN_ELEMENT(ADDRESS)
      END IF
      IF (IQUAL) THEN
        CALL DSA_MAP_QUALITY('OUTPUT',QUAL_MAP_MODE,'BYTE',
     &                        ADDRESS,OQSLOT,STATUS)
        OQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C   Perform the required combination.
C
      CALL DSA_WRUSER('Performing combination...\\N')
C
C   Bitwise logical operations. Note that error arrays are ignored by
C   these sets of operations, and that quality arrays are not compatible
C   with the '_*Q' routines using magic values.
C
      IF(COMMAND.EQ.'LOGIC2')THEN
        IF(.NOT.BADPIX)THEN
          CALL COMB2_B_W(DYNAMIC_MEM(OUTPTR),DYNAMIC_MEM(IM1PTR),
     &                   DYNAMIC_MEM(OQPTR),DYNAMIC_MEM(IQ1PTR),
     &                   QUAL,DIMS,NDIM,NELM,DIMS1,NDIM1,NELM1,
     &                   STAPIX,ENDPIX,OPER,DUMINT)
        ELSE
          CALL COMB2_B_WQ(DYNAMIC_MEM(OUTPTR),DYNAMIC_MEM(IM1PTR),
     &                    DIMS,NDIM,NELM,DIMS1,NDIM1,NELM1,
     &                    STAPIX,ENDPIX,OPER,MAGIC_SHORT)
        END IF
C
C   Arithmetical and comparison operations.
C   - for INTEGER*2 data.
C
      ELSE
        IF(MAP_TYPE.EQ.'SHORT')THEN
C
C     - with magic values absent (the magic value is supplied in case a
C       forbidden operation is attempted).
C
          IF(.NOT.BADPIX)THEN
            CALL COMB2_AC_W(DYNAMIC_MEM(OUTPTR),DYNAMIC_MEM(IM1PTR),
     &                      DYNAMIC_MEM(OEPTR),DYNAMIC_MEM(IE1PTR),
     &                      DYNAMIC_MEM(OQPTR),DYNAMIC_MEM(IQ1PTR),
     &                      ERR,QUAL,IQUAL,IQUAL1,
     &                      DIMS,NDIM,NELM,DIMS1,NDIM1,NELM1,
     &                      STAPIX,ENDPIX,OPER,MAGIC_SHORT,BADERR)
          ELSE
            CALL COMB2_AC_WQ(DYNAMIC_MEM(OUTPTR),DYNAMIC_MEM(IM1PTR),
     &                       DYNAMIC_MEM(OEPTR),DYNAMIC_MEM(IE1PTR),
     &                       ERR,
     &                       DIMS,NDIM,NELM,DIMS1,NDIM1,NELM1,
     &                       STAPIX,ENDPIX,OPER,MAGIC_SHORT,BADERR)
          END IF
C
C   - for REAL data.
C
        ELSE
C
C     - with magic values absent (the magic value is supplied in case a
C       forbidden operation is attempted).
C
          IF(.NOT.BADPIX)THEN
            CALL COMB2_AC_R(DYNAMIC_MEM(OUTPTR),DYNAMIC_MEM(IM1PTR),
     &                      DYNAMIC_MEM(OEPTR),DYNAMIC_MEM(IE1PTR),
     &                      DYNAMIC_MEM(OQPTR),DYNAMIC_MEM(IQ1PTR),
     &                      ERR,QUAL,IQUAL,IQUAL1,
     &                      DIMS,NDIM,NELM,DIMS1,NDIM1,NELM1,
     &                      STAPIX,ENDPIX,OPER,MAGIC_FLOAT,BADERR)
          ELSE
            CALL COMB2_AC_RQ(DYNAMIC_MEM(OUTPTR),DYNAMIC_MEM(IM1PTR),
     &                       DYNAMIC_MEM(OEPTR),DYNAMIC_MEM(IE1PTR),
     &                       ERR,
     &                       DIMS,NDIM,NELM,DIMS1,NDIM1,NELM1,
     &                       STAPIX,ENDPIX,OPER,MAGIC_FLOAT,BADERR)
          END IF
        END IF
      END IF
C
C     Set bad pixel flag if appropriate.
C
      IF (.NOT.QUAL) THEN
        CALL NDP_SET_BAD_PIXEL('OUTPUT',.FALSE.,BADPIX,STATUS)
      END IF
C
C   Now remove the error and/or quality array from OUTPUT
C   if necessary...
C
      IF (ERR.AND.BADERR) THEN
        CALL PAR_RDCHAR('ERR_ACT','PURGE',ACTION)
        CALL ICH_FOLD(ACTION)
        IF (ACTION(1:1).EQ.'R') THEN     ! Replace duff values
          CALL PAR_RDVAL('ERR_VAL',MIN_FLOAT,MAX_FLOAT,0.0,
     &                    ' ',DUMREAL)
          IF (TYPE.EQ.'SHORT') THEN
            CALL REPLACE_ERRORS_W2(DYNAMIC_MEM(OEPTR),NELM,
     &                            DCV_RTOW(DUMREAL))
          ELSE
            CALL REPLACE_ERRORS_R2(DYNAMIC_MEM(OEPTR),NELM,
     &                            DUMREAL)
          END IF
        ELSE
          CALL DSA_WRUSER('\\n ** The error array may now')
          CALL DSA_WRUSER(' contain void values.\\n')
        END IF
      END IF
C
C   Tidy up and exit.
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END





      SUBROUTINE COMB2_B_W
     &  (OARRAY,ARRAY1,
     &   OQARRAY,QARRAY1,QUAL,
     &   DIMS,NDIM,NELM,DIMS1,NDIM1,NELM1,
     &   STAPIX,ENDPIX,OPER,MAGICVAL)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) OPER
      INTEGER       DIMS(10),NDIM,NELM,DIMS1(10),NDIM1,NELM1,
     &              STAPIX(6),ENDPIX(6)
      INTEGER*2     OARRAY(NELM),ARRAY1(NELM1),MAGICVAL
      BYTE          OQARRAY(NELM),QARRAY1(NELM1)
      LOGICAL       QUAL
C
C     Local variables
C
      INTEGER   I            ! Loop counter
      INTEGER   II           ! Loop counter
      INTEGER   INC(6)       ! 1-D increments for dimensions of OARRAY
      INTEGER   INC1(6)      ! 1-D increments for dimensions of ARRAY1
      INTEGER   IND1         ! OARRAY axis 1 index
      INTEGER   IND2         ! OARRAY axis 2 index
      INTEGER   IND3         ! OARRAY axis 3 index
      INTEGER   IND4         ! OARRAY axis 4 index
      INTEGER   IND5         ! OARRAY axis 5 index
      INTEGER   IND6         ! OARRAY axis 6 index
      INTEGER   I1OFF        ! Total 1-D offset in ARRAY1
      INTEGER   I1OFF2       ! 1-D offset for axis 2
      INTEGER   I1OFF3       ! 1-D offset for axis 3
      INTEGER   I1OFF4       ! 1-D offset for axis 4
      INTEGER   I1OFF5       ! 1-D offset for axis 5
      INTEGER   I1OFF6       ! 1-D offset for axis 6
      INTEGER   OOFF         ! Total 1-D offset in OARRAY
      INTEGER   OOFF1        ! 1-D offset for axis 1
      INTEGER   OOFF2        ! 1-D offset for axis 2
      INTEGER   OOFF3        ! 1-D offset for axis 3
      INTEGER   OOFF4        ! 1-D offset for axis 4
      INTEGER   OOFF5        ! 1-D offset for axis 5
      INTEGER   OOFF6        ! 1-D offset for axis 6
C
C     Compute offset in OARRAY needed to increment each dimension
C
      DO I=1,NDIM
        INC(I)=1
        DO II=1,I-1
          INC(I)=INC(I)*DIMS(II)
        END DO
      END DO
C
C     Compute offset in ARRAY1 needed to increment each dimension
C
      DO I=1,NDIM1
        INC1(I)=1
        DO II=1,I-1
          INC1(I)=INC1(I)*DIMS1(II)
        END DO
      END DO
C
C     Bitwise AND
C
      IF(OPER.EQ.'AND')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                      IF (QUAL) THEN
                        IF ((OQARRAY(OOFF).EQ.0).AND.
     &                      (QARRAY1(I1OFF).EQ.0)) THEN
                          OARRAY(OOFF)=OARRAY(OOFF).AND.ARRAY1(I1OFF)
                        ELSE
                          IF (OQARRAY(OOFF).EQ.0) OQARRAY(OOFF) = 1
                        END IF
                      ELSE
                        OARRAY(OOFF)=OARRAY(OOFF).AND.ARRAY1(I1OFF)
                      END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise OR
C
      ELSE IF(OPER.EQ.'OR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                      IF (QUAL) THEN
                        IF ((OQARRAY(OOFF).EQ.0).AND.
     &                      (QARRAY1(I1OFF).EQ.0)) THEN
                          OARRAY(OOFF)=OARRAY(OOFF).OR.ARRAY1(I1OFF)
                        ELSE
                          IF (OQARRAY(OOFF).EQ.0) OQARRAY(OOFF) = 1
                        END IF
                      ELSE
                        OARRAY(OOFF)=OARRAY(OOFF).OR.ARRAY1(I1OFF)
                      END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise XOR
C
      ELSE IF(OPER.EQ.'XOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                      IF (QUAL) THEN
                        IF ((OQARRAY(OOFF).EQ.0).AND.
     &                      (QARRAY1(I1OFF).EQ.0)) THEN
                          OARRAY(OOFF)=OARRAY(OOFF).XOR.ARRAY1(I1OFF)
                        ELSE
                          IF (OQARRAY(OOFF).EQ.0) OQARRAY(OOFF) = 1
                        END IF
                      ELSE
                        OARRAY(OOFF)=OARRAY(OOFF).XOR.ARRAY1(I1OFF)
                      END IF

                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise NAND
C
      ELSE IF(OPER.EQ.'NAND')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                      IF (QUAL) THEN
                        IF ((OQARRAY(OOFF).EQ.0).AND.
     &                      (QARRAY1(I1OFF).EQ.0)) THEN
                          OARRAY(OOFF)=(.NOT.OARRAY(OOFF))
     &                                                .AND.ARRAY1(I1OFF)
                        ELSE
                          IF (OQARRAY(OOFF).EQ.0) OQARRAY(OOFF) = 1
                        END IF
                      ELSE
                        OARRAY(OOFF)=(.NOT.OARRAY(OOFF))
     &                                                .AND.ARRAY1(I1OFF)
                      END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise NOR
C
      ELSE IF(OPER.EQ.'NOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                      IF (QUAL) THEN
                        IF ((OQARRAY(OOFF).EQ.0).AND.
     &                      (QARRAY1(I1OFF).EQ.0)) THEN
                          OARRAY(OOFF)=(.NOT.OARRAY(OOFF))
     &                                            .OR.ARRAY1(I1OFF)
                        ELSE
                          IF (OQARRAY(OOFF).EQ.0) OQARRAY(OOFF) = 1
                        END IF
                      ELSE
                        OARRAY(OOFF)=(.NOT.OARRAY(OOFF))
     &                                             .OR.ARRAY1(I1OFF)
                      END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise NXOR
C
      ELSE IF(OPER.EQ.'NXOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                      OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).XOR.ARRAY1(I1OFF)
C
                      IF (QUAL) THEN
                        IF ((OQARRAY(OOFF).EQ.0).AND.
     &                      (QARRAY1(I1OFF).EQ.0)) THEN
                          OARRAY(OOFF)=(.NOT.OARRAY(OOFF))
     &                                             .XOR.ARRAY1(I1OFF)
                        ELSE
                          IF (OQARRAY(OOFF).EQ.0) OQARRAY(OOFF) = 1
                        END IF
                      ELSE
                        OARRAY(OOFF)=(.NOT.OARRAY(OOFF))
     &                                             .XOR.ARRAY1(I1OFF)
                      END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
      END IF
C
      END





      SUBROUTINE COMB2_B_WQ
     &  (OARRAY,ARRAY1,
     &   DIMS,NDIM,NELM,DIMS1,NDIM1,NELM1,
     &   STAPIX,ENDPIX,OPER,MAGICVAL)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) OPER
      INTEGER       DIMS(10),NDIM,NELM,DIMS1(10),NDIM1,NELM1,
     &              STAPIX(6),ENDPIX(6)
      INTEGER*2     OARRAY(NELM),ARRAY1(NELM1),MAGICVAL
C
C     Local variables
C
      INTEGER   I            ! Loop counter
      INTEGER   II           ! Loop counter
      INTEGER   INC(6)       ! 1-D increments for dimensions of OARRAY
      INTEGER   INC1(6)      ! 1-D increments for dimensions of ARRAY1
      INTEGER   IND1         ! OARRAY axis 1 index
      INTEGER   IND2         ! OARRAY axis 2 index
      INTEGER   IND3         ! OARRAY axis 3 index
      INTEGER   IND4         ! OARRAY axis 4 index
      INTEGER   IND5         ! OARRAY axis 5 index
      INTEGER   IND6         ! OARRAY axis 6 index
      INTEGER   I1OFF        ! Total 1-D offset in ARRAY1
      INTEGER   I1OFF2       ! 1-D offset for axis 2
      INTEGER   I1OFF3       ! 1-D offset for axis 3
      INTEGER   I1OFF4       ! 1-D offset for axis 4
      INTEGER   I1OFF5       ! 1-D offset for axis 5
      INTEGER   I1OFF6       ! 1-D offset for axis 6
      INTEGER   OOFF         ! Total 1-D offset in OARRAY
      INTEGER   OOFF1        ! 1-D offset for axis 1
      INTEGER   OOFF2        ! 1-D offset for axis 2
      INTEGER   OOFF3        ! 1-D offset for axis 3
      INTEGER   OOFF4        ! 1-D offset for axis 4
      INTEGER   OOFF5        ! 1-D offset for axis 5
      INTEGER   OOFF6        ! 1-D offset for axis 6
C
C     Compute offset in OARRAY needed to increment each dimension
C
      DO I=1,NDIM
        INC(I)=1
        DO II=1,I-1
          INC(I)=INC(I)*DIMS(II)
        END DO
      END DO
C
C     Compute offset in ARRAY1 needed to increment each dimension
C
      DO I=1,NDIM1
        INC1(I)=1
        DO II=1,I-1
          INC1(I)=INC1(I)*DIMS1(II)
        END DO
      END DO
C
C     Bitwise AND
C
      IF(OPER.EQ.'AND')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                    IF(OARRAY(OOFF).GT.MAGICVAL .AND.
     &                 ARRAY1(I1OFF).GT.MAGICVAL)THEN
                      OARRAY(OOFF)=OARRAY(OOFF).AND.ARRAY1(I1OFF)
                    ELSE
                      OARRAY(OOFF)=MAGICVAL
                    END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise OR
C
      ELSE IF(OPER.EQ.'OR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                    IF(OARRAY(OOFF).GT.MAGICVAL .AND.
     &                 ARRAY1(I1OFF).GT.MAGICVAL)THEN
                      OARRAY(OOFF)=OARRAY(OOFF).OR.ARRAY1(I1OFF)
                    ELSE
                      OARRAY(OOFF)=MAGICVAL
                    END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise XOR
C
      ELSE IF(OPER.EQ.'XOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                    IF(OARRAY(OOFF).GT.MAGICVAL .AND.
     &                 ARRAY1(I1OFF).GT.MAGICVAL)THEN
                      OARRAY(OOFF)=OARRAY(OOFF).XOR.ARRAY1(I1OFF)
                    ELSE
                      OARRAY(OOFF)=MAGICVAL
                    END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise NAND
C
      ELSE IF(OPER.EQ.'NAND')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                    IF(OARRAY(OOFF).GT.MAGICVAL .AND.
     &                 ARRAY1(I1OFF).GT.MAGICVAL)THEN
                      OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).AND.ARRAY1(I1OFF)
                    ELSE
                      OARRAY(OOFF)=MAGICVAL
                    END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise NOR
C
      ELSE IF(OPER.EQ.'NOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                    IF(OARRAY(OOFF).GT.MAGICVAL .AND.
     &                 ARRAY1(I1OFF).GT.MAGICVAL)THEN
                      OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).OR.ARRAY1(I1OFF)
                    ELSE
                      OARRAY(OOFF)=MAGICVAL
                    END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
C
C     Bitwise NXOR
C
      ELSE IF(OPER.EQ.'NXOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          IF(NDIM1.EQ.5)THEN
            I1OFF6=0
          ELSE
            I1OFF6=(IND6-1)*INC1(6)
          END IF
C
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            IF(NDIM1.EQ.4)THEN
              I1OFF5=0
            ELSE
              I1OFF5=(IND5-1)*INC1(5)
            END IF
C
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              IF(NDIM1.EQ.3)THEN
                I1OFF4=0
              ELSE
                I1OFF4=(IND4-1)*INC1(4)
              END IF
C
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                IF(NDIM1.EQ.2)THEN
                  I1OFF3=0
                ELSE
                  I1OFF3=(IND3-1)*INC1(3)
                END IF
C
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  IF(NDIM1.EQ.1)THEN
                    I1OFF2=0
                  ELSE
                    I1OFF2=(IND2-1)*INC1(2)
                  END IF
C
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    I1OFF=1+OOFF1+I1OFF2+I1OFF3+I1OFF4+I1OFF5+I1OFF6
C
                    IF(OARRAY(OOFF).GT.MAGICVAL .AND.
     &                 ARRAY1(I1OFF).GT.MAGICVAL)THEN
                      OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).XOR.ARRAY1(I1OFF)
                    ELSE
                      OARRAY(OOFF)=MAGICVAL
                    END IF
C
                  END DO
                END DO
                IF(NDIM.EQ.3)CALL NDP_DISPLAY_PROGRESS(3,IND3)
              END DO
              IF(NDIM.EQ.4)CALL NDP_DISPLAY_PROGRESS(4,IND4)
            END DO
            IF(NDIM.EQ.5)CALL NDP_DISPLAY_PROGRESS(5,IND5)
          END DO
          IF(NDIM.EQ.6)CALL NDP_DISPLAY_PROGRESS(6,IND6)
        END DO
      END IF
C
      END


