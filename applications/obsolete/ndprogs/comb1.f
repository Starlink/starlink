      SUBROUTINE COMB1
C+
C
C   ---------
C   C O M B 1
C   ---------
C
C   Description
C   -----------
C   This routine services the following commands:
C
C     ARITH1 - Arithmetic operations between an image and a scalar. The
C              operations supported are: +, -, *, /, **.
C
C     LOGIC1 - Bitwise logical operations between an image and a scalar. The
C              operations supported are: AND, OR, XOR, NAND, NOR, NXOR.
C
C     MASK1  - Masking operations between an image and a scalar. The
C              operations supported are: MAX, MIN, replace where IMAGE is
C              non-magic (or good quality), replace where IMAGE is magic
C              (or bad quality).
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
C   IMAGE    Name of the structure containing the input image. (character)
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
C   VALUE    Scalar to be combined with IMAGE. (real)(prompted for).
C
C   OUTPUT   Name of the structure containing the output image. May be the
C            same as IMAGE. (character)(prompted for).
C
C   ERR_ACT  Action to be taken with a void error array (character,prompted)
C
C   ERR_VAL  Value to replace void errors with (real, prompted)
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
C   - The IMAGE structure is tested for the bad data flag. If it is found
C     and non-zero, magic values are assumed to be present and are left in
C     the data.
C   - The structure IMAGE is copied to OUTPUT.
C   - A subroutine appropriate to the required operation, the data type,
C     and the presence or absence of magic values is called to operate on
C     the OUTPUT data array.
C
C   Note
C   ----
C
C     Lines commented with 'C*' indicate the current non-compatibility
C     of quality arrays and magic values, and have been included for
C     forward compatibilty.
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
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
C      ICH_FOLD
C      ICH_LEN
C
C   Library NDP:
C      NDP_AXIS_RANGE
C      NDP_GET_IMAGE_INFO
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
C   COMB1_AC_W
C   COMB1_AC_WQ
C   COMB1_AC_R
C   COMB1_AC_RQ
C   COMB1_B_W
C   COMB1_B_WQ
C   REPLACE_ERRORS_R1
C   REPLACE_ERRORS_W1
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY.INC'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C   INCLUDE 'QUALITY_MASK'
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
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Chris Benn  RGO  (RGVAD::CRB or CRB@UK.AC.RGO.STAR)
C   Julian Gold RGO (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1989  - Original program
C   12-JLY-1991  - Quality and error arrays implemented, with new options:
C                  REP1 replace data when quality is good
C                  REP4 replace data when quality is bad
C                  (only applicable when quality arrays exist). (GOLDJIL)
C   20-AUG-1991  - Why oh why did the author compare COMMAND with 'ACOMB1'
C                  etc when the command names are 'LOGIC1' etc ? Fixed now.
C                  Sigh...
C   30-NOV-1992  - Unix version. Changed REP1 to REPG, REP4 to REPB which
C                  are a tad less cryptic. (GOLDJIL)
C   06-OCT-1994  - Removed a Trojan Horse message. Also ensured that an
C                  infinite loop does not occur when an improper operation is
C                  requested. Removed unused variables. (GJP)
C   13-OCT-1994  - Modified code to avoid an eternal loop
C                  when attempting to divide by zero in ARITH1. (GJP)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Functions used
C
      INTEGER  DYN_ELEMENT,ICH_LEN
C
C   Local variables
C
      CHARACTER ACTION*16           ! String dictating fortune of error array
      INTEGER   ADDRESS             ! Address of dynamic memory element
      LOGICAL   BADERR              ! Flags a void error array
      LOGICAL   BADPIX              ! Value of bad pixel flag in IMAGE
      CHARACTER COMMAND*16          ! Name of command being serviced
      INTEGER   DIMS(10)            ! Dimensions of IMAGE
      INTEGER   DUMINT              ! INTEGER dummy variable
      REAL      DUMREAL             ! REAL dummy variable
      REAL      END(6)              ! End coordinates of IMAGE subset
      INTEGER   ENDPIX(6)           ! End pixel in IMAGE
      LOGICAL   ERR                 ! Flags existence of error array
      INTEGER   IEPTR               ! Dynamic pointer to i/p error array
      INTEGER   IESLOT              ! Map slot number for i/p error array
      INTEGER   IMPTR               ! Dynamic pointer to IMAGE data array
      INTEGER   IQPTR               ! Dynamic pointer to i/p quality array
      INTEGER   IQSLOT              ! Map slot number for i/p quality array
      INTEGER   ISLOT               ! Map slot number for IMAGE data
      INTEGER   NDIM                ! Number of dimensions in IMAGE
      INTEGER   NELM                ! Number of elements in IMAGE
      INTEGER   OEPTR               ! Dynamic pointer to o/p error array
      INTEGER   OESLOT              ! Map slot number for o/p error array
      CHARACTER OPER*4              ! Operation name
      INTEGER   OQPTR               ! Dynamic pointer to o/p quality array
      INTEGER   OQSLOT              ! Map slot number for o/p quality array
      INTEGER   OSLOT               ! Map slot number for OUTPUT data
      INTEGER   OUTPTR              ! Dynamic pointer to OUTPUT data array
      REAL      PMAX                ! Maximum parameter value for PAR_RDVAL
      REAL      PMIN                ! Minimum parameter value for PAR_RDVAL
      LOGICAL   QUAL                ! Flags existence of quality array
      INTEGER   STAPIX(6)           ! Start pixel in IMAGE
      REAL      START(6)            ! Start coordinates of IMAGE subset
      INTEGER   STATUS              ! Status code
      CHARACTER STRING*80           ! Message string
      CHARACTER TYPE*8              ! IMAGE data array type
      INTEGER*2 VAL_SHORT           ! INTEGER*2 constant value
      REAL      VAL_FLOAT           ! REAL constant value
      INTEGER   I                   ! Loop variable
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=0)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
      INCLUDE 'DCV_FUN'
C
C   Initialize
C
      STATUS=0
      DO I=1,6
         STAPIX(I)=1
      END DO
C
C   Open DSA system
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get command name
C
      CALL PAR_COMMAND(COMMAND)
C
C   Open file for IMAGE
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Display information on IMAGE
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.TRUE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   If command is LOGIC1, check that IMAGE is INTEGER*2
C
      IF(COMMAND.EQ.'LOGIC1')THEN
        IF(TYPE.NE.'SHORT')THEN
          CALL DSA_WRUSER
     &      ('IMAGE data type must be SHORT for bitwise operations.\\N')
          GO TO 500
        END IF
      END IF
C
C   Get dimensions of IMAGE data array
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get IMAGE axis range
C
      CALL NDP_AXIS_RANGE
     &  ('IMAGE',DIMS,NDIM,START,END,STAPIX,ENDPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get operation name
C
   20 CONTINUE
      CALL PAR_CNPAR('OPER')
      CALL PAR_RDCHAR('OPER',' ',STRING)
      OPER=STRING(:ICH_LEN(STRING))
      IF(COMMAND.EQ.'ARITH1')THEN
        IF(OPER.NE.'+' .AND.
     &     OPER.NE.'-' .AND.
     &     OPER.NE.'*' .AND.
     &     OPER.NE.'/' .AND.
     &     OPER.NE.'**')THEN
          CALL DSA_WRUSER('Unrecognized option.\\N')
          GO TO 20
        END IF
      ELSE IF(COMMAND.EQ.'LOGIC1')THEN
        IF(OPER.NE.'AND' .AND.
     &     OPER.NE.'OR' .AND.
     &     OPER.NE.'XOR' .AND.
     &     OPER.NE.'NAND' .AND.
     &     OPER.NE.'NOR' .AND.
     &     OPER.NE.'NXOR')THEN
          CALL DSA_WRUSER('Unrecognized option.\\N')
          GO TO 20
        END IF
      ELSE IF(COMMAND.EQ.'MASK1')THEN
        IF(OPER.NE.'MAX' .AND.
     &     OPER.NE.'MIN' .AND.
     &     OPER.NE.'REPG' .AND.
     &     OPER.NE.'REPB')THEN
          CALL DSA_WRUSER('Unrecognized option.\\N')
          GO TO 20
        END IF
      END IF
C
C   Get value of scalar if applicable
C
      IF(OPER.NE.'LO' .AND. OPER.NE.'EX')THEN
   30   CONTINUE
        IF(COMMAND.EQ.'LOGIC1')THEN
          PMIN=REAL(MIN_SHORT)
          PMAX=REAL(MAX_SHORT)
        ELSE
          PMIN=MIN_FLOAT
          PMAX=MAX_FLOAT
        END IF
        CALL PAR_CNPAR('VALUE')
        CALL PAR_RDVAL('VALUE',PMIN,PMAX,0.0,' ',DUMREAL)
        IF(OPER.EQ.'/' .AND. DUMREAL.EQ.0.0)THEN
          CALL DSA_WRUSER('What?\\N')
          GO TO 30
        END IF
        IF(COMMAND.EQ.'LOGIC1')THEN
          VAL_SHORT=INT(DUMREAL)
        ELSE
          VAL_FLOAT=DUMREAL
        END IF
      END IF
C
C   Open file for OUTPUT
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Magic values are not to be removed from data arrays
C
      CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
      CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Map IMAGE data array
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA
     &    ('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA
     &    ('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      IMPTR=DYN_ELEMENT(ADDRESS)
C
C   Map OUTPUT data array in same type as IMAGE
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('OUTPUT','WRITE','SHORT',
     &                         ADDRESS,OSLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',
     &                         ADDRESS,OSLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      OUTPTR=DYN_ELEMENT(ADDRESS)

C
C   Find quality/error information (if any)
C
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',
     &                        ADDRESS,IQSLOT,STATUS)
        IQPTR = DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',
     &                          ADDRESS,OQSLOT,STATUS)
        OQPTR = DYN_ELEMENT(ADDRESS)
      END IF
      IF (STATUS.NE.0) GO TO 500
      IF (ERR) THEN
        IF (TYPE.EQ.'SHORT') THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','SHORT',
     &                           ADDRESS,IESLOT,STATUS)
          IEPTR = DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','SHORT',
     &                            ADDRESS,OESLOT,STATUS)
          OEPTR = DYN_ELEMENT(ADDRESS)
        ELSE
          CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',
     &                           ADDRESS,IESLOT,STATUS)
          IEPTR = DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',
     &                            ADDRESS,OESLOT,STATUS)
          OEPTR = DYN_ELEMENT(ADDRESS)
        END IF
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C   Perform the required combination
C
      CALL DSA_WRUSER('Performing combination...\\N')
C
C   Bitwise logical operations
C
      IF(COMMAND.EQ.'LOGIC1')THEN
        IF(.NOT.BADPIX)THEN
          CALL COMB1_B_W(DYNAMIC_MEM(OUTPTR),DIMS,NDIM,NELM,
     &                   STAPIX,ENDPIX,OPER,VAL_SHORT,DUMINT,
     &                   DYNAMIC_MEM(OQPTR),QUAL)
        ELSE
          CALL COMB1_B_WQ(DYNAMIC_MEM(OUTPTR),DIMS,NDIM,NELM,
     &                    STAPIX,ENDPIX,OPER,VAL_SHORT,MAGIC_SHORT,
     &                    DYNAMIC_MEM(OQPTR),QUAL)
        END IF
C
C   Arithmetic and comparison operations
C
      ELSE
        IF(TYPE.EQ.'SHORT')THEN
          IF(.NOT.BADPIX)THEN
            CALL COMB1_AC_W(DYNAMIC_MEM(OUTPTR),DIMS,NDIM,NELM,
     &                      STAPIX,ENDPIX,OPER,VAL_FLOAT,DUMINT,
     &                      DYNAMIC_MEM(OQPTR),QUAL,
     &                      DYNAMIC_MEM(OEPTR),ERR,BADERR)
          ELSE
            CALL COMB1_AC_WQ(DYNAMIC_MEM(OUTPTR),DIMS,NDIM,NELM,
     &                       STAPIX,ENDPIX,OPER,VAL_FLOAT,MAGIC_SHORT,
C*   &                       DYNAMIC_MEM(OQPTR),QUAL,
     &                       DYNAMIC_MEM(OEPTR),ERR,BADERR)
          END IF
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL COMB1_AC_R(DYNAMIC_MEM(OUTPTR),DIMS,NDIM,NELM,
     &                      STAPIX,ENDPIX,OPER,VAL_FLOAT,DUMREAL,
     &                      DYNAMIC_MEM(OQPTR),QUAL,
     &                      DYNAMIC_MEM(OEPTR),ERR,BADERR)
          ELSE
            CALL COMB1_AC_RQ(DYNAMIC_MEM(OUTPTR),DIMS,NDIM,NELM,
     &                       STAPIX,ENDPIX,OPER,VAL_FLOAT,MAGIC_FLOAT,
C*   &                       DYNAMIC_MEM(OQPTR),QUAL,
     &                       DYNAMIC_MEM(OEPTR),ERR,BADERR)
          END IF
        END IF
      END IF
C
C   Update bad pixel flag
C
      IF (.NOT.QUAL) THEN
        CALL NDP_SET_BAD_PIXEL('OUTPUT',.FALSE.,BADPIX,STATUS)
      END IF
C
C   Now we make sure that the user is happy with the error array
C
      IF (ERR.AND.BADERR) THEN
        CALL PAR_RDCHAR('ERR_ACT','REPLACE',ACTION)
        CALL ICH_FOLD(ACTION)
        IF (ACTION(1:1).EQ.'R') THEN ! Replace duff values
          CALL PAR_RDVAL('ERR_VAL',MIN_FLOAT,MAX_FLOAT,0.0,
     &                                          ' ',DUMREAL)
          IF (TYPE.EQ.'SHORT') THEN
            CALL REPLACE_ERRORS_W1(DYNAMIC_MEM(OEPTR),NELM,
     &                                  DCV_RTOW(DUMREAL))
          ELSE
            CALL REPLACE_ERRORS_R1(DYNAMIC_MEM(OEPTR),NELM,DUMREAL)
          END IF ! TYPE
        ELSE
          CALL DSA_WRUSER('\\n*** The error array may ')
          CALL DSA_WRUSER('now contain void values ***\\n')
        END IF ! ACTION
      END IF ! ERR
C
C   Tidy up and exit
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END



      SUBROUTINE COMB1_B_W
     &  (OARRAY,DIMS,NDIM,NELM,STAPIX,ENDPIX,OPER,VALUE,MAGICVAL,
     &   QARRAY,QUAL)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) OPER
      INTEGER       DIMS(10),NDIM,NELM,STAPIX(6),ENDPIX(6)
      INTEGER*2     OARRAY(NELM),VALUE,MAGICVAL
      LOGICAL       QUAL
      BYTE          QARRAY(NELM)
C
C     Local variables
C
      INTEGER   I            ! Loop counter
      INTEGER   II           ! Loop counter
      INTEGER   INC(6)       ! 1-D increments for dimensions of OARRAY
      INTEGER   IND1         ! OARRAY axis 1 index
      INTEGER   IND2         ! OARRAY axis 2 index
      INTEGER   IND3         ! OARRAY axis 3 index
      INTEGER   IND4         ! OARRAY axis 4 index
      INTEGER   IND5         ! OARRAY axis 5 index
      INTEGER   IND6         ! OARRAY axis 6 index
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
C     Bitwise AND
C
      IF(OPER.EQ.'AND')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (QUAL) THEN
                      IF (QARRAY(OOFF).EQ.0)
     &                  OARRAY(OOFF)=OARRAY(OOFF).AND.VALUE
                    ELSE
                      OARRAY(OOFF) = OARRAY(OOFF).AND.VALUE
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
C     Bitwise OR
C
      ELSE IF(OPER.EQ.'OR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (QUAL) THEN
                      IF (QARRAY(OOFF).EQ.0)
     &                  OARRAY(OOFF)=OARRAY(OOFF).OR.VALUE
                    ELSE
                      OARRAY(OOFF) = OARRAY(OOFF).OR.VALUE
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
C     Bitwise XOR
C
      ELSE IF(OPER.EQ.'XOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (QUAL) THEN
                      IF (QARRAY(OOFF).EQ.0)
     &                  OARRAY(OOFF)=OARRAY(OOFF).XOR.VALUE
                    ELSE
                      OARRAY(OOFF) = OARRAY(OOFF).XOR.VALUE
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
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (QUAL) THEN
                      IF (QARRAY(OOFF).EQ.0)
     &                    OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).AND.VALUE
                    ELSE
                      OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).AND.VALUE
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
C     Bitwise NOR
C
      ELSE IF(OPER.EQ.'NOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (QUAL) THEN
                      IF (QARRAY(OOFF).EQ.0)
     &                  OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).OR.VALUE
                      ELSE
                        OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).OR.VALUE
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
C     Bitwise NXOR
C
      ELSE IF(OPER.EQ.'NXOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (QUAL) THEN
                      IF (QARRAY(OOFF).EQ.0)
     &                  OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).XOR.VALUE
                    ELSE
                      OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).XOR.VALUE
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
      END IF
C
      END





      SUBROUTINE COMB1_B_WQ
     &  (OARRAY,DIMS,NDIM,NELM,STAPIX,ENDPIX,OPER,VALUE,MAGICVAL
C*   &   ,QARRAY,QUAL
     &  )
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) OPER
      INTEGER       DIMS(10),NDIM,NELM,STAPIX(6),ENDPIX(6)
      INTEGER*2     OARRAY(NELM),VALUE,MAGICVAL
C*    LOGICAL       QUAL
C*    BYTE          QARRAY(NELM)

C
C     Local variables
C
      INTEGER   I            ! Loop counter
      INTEGER   II           ! Loop counter
      INTEGER   INC(6)       ! 1-D increments for dimensions of OARRAY
      INTEGER   IND1         ! OARRAY axis 1 index
      INTEGER   IND2         ! OARRAY axis 2 index
      INTEGER   IND3         ! OARRAY axis 3 index
      INTEGER   IND4         ! OARRAY axis 4 index
      INTEGER   IND5         ! OARRAY axis 5 index
      INTEGER   IND6         ! OARRAY axis 6 index
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
C     Bitwise AND
C
      IF(OPER.EQ.'AND')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (OARRAY(OOFF).GT.MAGICVAL) THEN
C*                    IF (QUAL) THEN
C*                      IF (QARRAY(OOFF).EQ.0) THEN
C*
                          OARRAY(OOFF)=OARRAY(OOFF).AND.VALUE
C*
C*                      END IF
C*                    ELSE
C*                      OARRAY(OOFF) = OARRAY(OOFF).AND.VALUE
C*                    END IF
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
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (OARRAY(OOFF).GT.MAGICVAL) THEN
C*                    IF (QUAL) THEN
C*                      IF (QARRAY(OOFF).EQ.0) THEN
C*
                          OARRAY(OOFF)=OARRAY(OOFF).OR.VALUE
C*
C*                      END IF
C*                    ELSE
C*                      OARRAY(OOFF) = OARRAY(OOFF).OR.VALUE
C*                    END IF
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
C     Bitwise XOR
C
      ELSE IF(OPER.EQ.'XOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (OARRAY(OOFF).GT.MAGICVAL) THEN
C*                    IF (QUAL) THEN
C*                      IF (QARRAY(OOFF).EQ.0) THEN
C*
                          OARRAY(OOFF)=OARRAY(OOFF).XOR.VALUE
C*
C*                      END IF
C*                    ELSE
C*                      OARRAY(OOFF) = OARRAY(OOFF).XOR.VALUE
C*                    END IF
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
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (OARRAY(OOFF).GT.MAGICVAL) THEN
C*                    IF (QUAL) THEN
C*                      IF (QARRAY(OOFF).EQ.0) THEN
C*
                          OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).AND.VALUE
C*
C*                      END IF
C*                    ELSE
C*                      OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).AND.VALUE
C*                    END IF
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
C     Bitwise NOR
C
      ELSE IF(OPER.EQ.'NOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (OARRAY(OOFF).GT.MAGICVAL) THEN
C*                    IF (QUAL) THEN
C*                      IF (QARRAY(OOFF).EQ.0) THEN
C*
                          OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).OR.VALUE
C*
C*                      END IF
C*                    ELSE
C*                      OARRAY(OOFF) = (.NOT.OARRAY(OOFF)).OR.VALUE
C*                    END IF
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
C     Bitwise NXOR
C
      ELSE IF(OPER.EQ.'NXOR')THEN
        DO IND6=STAPIX(6),MAX(1,ENDPIX(6))
          OOFF6=(IND6-1)*INC(6)
          DO IND5=STAPIX(5),MAX(1,ENDPIX(5))
            OOFF5=(IND5-1)*INC(5)
            DO IND4=STAPIX(4),MAX(1,ENDPIX(4))
              OOFF4=(IND4-1)*INC(4)
              DO IND3=STAPIX(3),MAX(1,ENDPIX(3))
                OOFF3=(IND3-1)*INC(3)
                DO IND2=STAPIX(2),MAX(1,ENDPIX(2))
                  OOFF2=(IND2-1)*INC(2)
                  DO IND1=STAPIX(1),MAX(1,ENDPIX(1))
                    OOFF1=IND1-1
                    OOFF=1+OOFF1+OOFF2+OOFF3+OOFF4+OOFF5+OOFF6
                    IF (OARRAY(OOFF).GT.MAGICVAL) THEN
C*                    IF (QUAL) THEN
C*                      IF (QARRAY(OOFF).EQ.0) THEN
C*
                          OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).XOR.VALUE
C*
C*                      END IF
C*                    ELSE
C*                      OARRAY(OOFF)=(.NOT.OARRAY(OOFF)).XOR.VALUE
C*                    END IF
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
