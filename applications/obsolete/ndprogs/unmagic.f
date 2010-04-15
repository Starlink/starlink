      SUBROUTINE UNMAGIC
C+
C
C   -------------
C   U N M A G I C
C   -------------
C
C
C   Description
C   -----------
C   Sets each bad pixel in an image to a value derived from its
C   neighbours. Several methods for computing the new value are available.
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported.
C   - Magic values and quality arrays supported.
C   - Variance arrays not supported.
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
C   IMAGE   Name of the structure containing the input image. (character)
C           (prompted for).
C
C   START   The coordinate in each dimension of IMAGE at which the edit
C           operation is to start. (real, array)(prompted for).
C
C   END     The coordinate in each dimension of IMAGE at which the edit
C           operation is to end. (real, array)(prompted for).
C
C   INTERP  Method used to compute the value of each output pixel (integer)
C           (prompted for). These method numbers are also used in TRANSFORM.
C           Method 1 (nearest neighbour) is offered in TRANSFORM but is not
C           appropriate to UNMAGIC.
C           2 = average of neighbours
C           3 = linear interpolation
C           4 = higher order (not yet available)
C           5 = replace with a constant value
C
C   MINADJ  For method 2, the minimum number of adjacent pixels which must
C           be non-magic for an average value to be computable. (integer)
C           (prompted for).
C
C   VALUE   For method 5, the value to be assigned to all magic value
C           pixels. (real)(prompted for).
C
C   OUTPUT  Name of the structure containing the output image. May be the
C           same as IMAGE. (character)(prompted for).
C
C
C   Keywords
C   --------
C   WHOLE   Instructs the program to edit the whole image. Otherwise, a
C           subset of the image may be selected.
C
C
C   Propagation of data structure
C   -----------------------------
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
C   - A subroutine appropriate to the data type and the required interpolation
C     method values is called to interpolate using the values available in the
C     IMAGE data array, and store the results in the OUTPUT data array.
C   - The OUTPUT bad data flag is unset only if the whole of IMAGE was used
C     and no magic values remain.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C      DSA_CLOSE
C      DSA_DATA_SIZE
C      DSA_INPUT
C      DSA_MAP_DATA
C      DSA_MAP_QUALITY
C      DSA_OPEN
C      DSA_OUTPUT
C      DSA_SEEK_QUALITY
C      DSA_USE_FLAGGED_VALUES
C      DSA_WRUSER
C
C   Library DYN:
C      DYN_ELEMENT
C
C   Library ICH:
C      ICH_ENCODE
C      ICH_LEN
C
C   Library NDP:
C      NDP_ADJACENT
C      NDP_AXIS_RANGE
C      NDP_CORNERS
C      NDP_DISPLAY_PROGRESS
C      NDP_GET_IMAGE_INFO
C      NDP_LINEAR
C      NDP_SET_BAD_PIXEL
C
C   Library PAR:
C      PAR_RDVAL
C
C
C   Internal subroutines called
C   ---------------------------
C   UNMAGIC_AVERAGE_W
C   UNMAGIC_AVERAGE_R
C   UNMAGIC_CONSTANT_W
C   UNMAGIC_CONSTANT_R
C   UNMAGIC_LINEAR_W
C   UNMAGIC_LINEAR_R
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
C   Possible future upgrades:
C   ------------------------
C   - Provide a higher order interpolation method.
C   - Repeat the chosen interpolation operation until no magic value pixels
C     remain. This would mean using work arrays so that the output of one
C     iteration can become the input for the next.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989  - Original program
C   09-OCT-1991  - Quality array support added. Code written in GENERIC
C                  format. (GOLDJIL)
C   10-DEC-1992  - Unix version (GOLDJIL).
C   06-OCT-1994  - Removed lots of unused variables. (GJP)
C
C
C+------------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Functions used.
C
      INTEGER  DYN_ELEMENT,ICH_ENCODE,ICH_LEN
C
C   Local variables.
C
      INTEGER   ADDRESS             ! Address of dynamic memory element
      LOGICAL   BADPIX              ! Value of bad pixel flag
      INTEGER   DIMS(10)            ! Dimensions of IMAGE
      INTEGER   DUMINT              ! INTEGER dummy variable
      REAL      DUMREAL             ! REAL dummy variable
      REAL      END(6)              ! End coordinate
      INTEGER   ENDPIX(6)           ! End pixel
      INTEGER   I                   ! Loop counter
      INTEGER   IMPTR               ! Dynamic pointer to IMAGE data array
      INTEGER   INTERP              ! Interpolation method number
      INTEGER   IQPTR               ! Dynanmic pointer to IMAGE quality
      INTEGER   IQSLOT              ! Map slot number for IMAGE quality
      INTEGER   ISLOT               ! Map slot number for IMAGE data
      REAL      MAGICPIX            ! Number of magic value pixels remaining
      INTEGER   MINADJ              ! Number of non-magic adjacent pixels
      INTEGER   NDIM                ! Number of dimensions in IMAGE
      INTEGER   NELM                ! Number of elements in IMAGE
      INTEGER   NEXT                ! Pointer returned by ICH_ENCODE
      INTEGER   OQPTR               ! Dynamic pointer to OUTPUT quality
      INTEGER   OQSLOT              ! Map slot number for OUTPUT quality
      INTEGER   OSLOT               ! Map slot number for OUTPUT data
      INTEGER   OUTPTR              ! Dynamic pointer to OUTPUT data array
      REAL      PMAX                ! Maximum parameter value for PAR_RDVAL
      REAL      PMIN                ! Minimum parameter value for PAR_RDVAL
      LOGICAL   QUAL                ! Data quality flag
      INTEGER   STAPIX(6)           ! Start pixel
      REAL      START(6)            ! Start coordinate
      INTEGER   STATUS              ! Status code
      CHARACTER STRING*80           ! Message string
      CHARACTER TYPE*8              ! Data array type
      INTEGER*2 VAL_SHORT           ! INTEGER*2 constant value
      REAL      VAL_FLOAT           ! REAL constant value
      LOGICAL   WHOLE               ! Whole image processed flag
      REAL      ZAPPIX              ! Number of magic value pixels replaced
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=0)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C   Initialize.
C
      STATUS=0
      ZAPPIX=0.0
      MAGICPIX=0.0
C
C   Open DSA system
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
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
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF(.NOT.(BADPIX.OR.QUAL))THEN
        CALL DSA_WRUSER
     &          ('The bad data flag of this image is not set, ')
        CALL DSA_WRUSER
     &          ('\\nand I can''t find a quality array either.\\n')
        CALL DSA_WRUSER
     &          ('However we will continue, in case it really ')
        CALL DSA_WRUSER
     &          ('is wrong...\\N')
      END IF
C
C   Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get IMAGE axis range.
C
      CALL NDP_AXIS_RANGE
     &  ('IMAGE',DIMS,NDIM,START,END,STAPIX,ENDPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get interpolation method.
C
      CALL PAR_RDVAL('INTERP',2.0,5.0,2.0,' ',DUMREAL)
      INTERP=INT(DUMREAL)
C
C   Get number of adjacent non-magic pixels.
C
      IF(INTERP.EQ.2)THEN
        CALL PAR_RDVAL('MINADJ',1.0,REAL(3**NDIM-1),1.0,' ',DUMREAL)
        MINADJ=INT(DUMREAL)
      ELSE IF(INTERP.EQ.3)THEN
        MINADJ=2**NDIM
      END IF
C
C   Get constant value if required.
C
      IF(INTERP.EQ.5)THEN
        IF(TYPE.EQ.'SHORT')THEN
          PMIN=REAL(MIN_SHORT)
          PMAX=REAL(MAX_SHORT)
        ELSE
          PMIN=MIN_FLOAT
          PMAX=MAX_FLOAT
        END IF
        CALL PAR_RDVAL('VALUE',PMIN,PMAX,0.0,' ',DUMREAL)
        IF(TYPE.EQ.'SHORT')THEN
          VAL_SHORT=INT(DUMREAL)
        ELSE
          VAL_FLOAT=DUMREAL
        END IF
      END IF
C
C   Open file for OUTPUT.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Magic values are not to be removed from the data arrays unless a
C   quality array is present.
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Map IMAGE data array.
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      IMPTR=DYN_ELEMENT(ADDRESS)
C
C   Map OUTPUT data array.
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('OUTPUT','WRITE','SHORT',ADDRESS,OSLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ADDRESS,OSLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      OUTPTR=DYN_ELEMENT(ADDRESS)
C
C   Map quality arrays if necessary
C
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',
     &                        ADDRESS,IQSLOT,STATUS)
        IQPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_QUALITY('OUTPUT','UPDATE','BYTE',
     &                        ADDRESS,OQSLOT,STATUS)
        OQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C   Perform the editing operation.
C
      CALL DSA_WRUSER('Editing data array...\\N')
C
      IF(TYPE.EQ.'SHORT')THEN
        IF(INTERP.EQ.2)THEN
          CALL UNMAGIC_AVERAGE_W(DYNAMIC_MEM(IMPTR),
     &                           DYNAMIC_MEM(OUTPTR),
     &                           DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &                           MINADJ,ZAPPIX,MAGICPIX,MAGIC_SHORT,
     &                           QUAL,DYNAMIC_MEM(IQPTR),
     &                           DYNAMIC_MEM(OQPTR))
        ELSE IF(INTERP.EQ.3)THEN
          CALL UNMAGIC_LINEAR_W(DYNAMIC_MEM(IMPTR),
     &                          DYNAMIC_MEM(OUTPTR),
     &                          DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &                          MINADJ,ZAPPIX,MAGICPIX,MAGIC_SHORT,
     &                          QUAL,DYNAMIC_MEM(IQPTR),
     &                          DYNAMIC_MEM(OQPTR))
        ELSE IF(INTERP.EQ.5)THEN
          CALL UNMAGIC_CONSTANT_W(DYNAMIC_MEM(OUTPTR),
     &                            DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &                            VAL_SHORT,ZAPPIX,MAGIC_SHORT,
     &                            QUAL,DYNAMIC_MEM(IQPTR),
     &                            DYNAMIC_MEM(OQPTR))
        END IF
      ELSE
        IF(INTERP.EQ.2)THEN
          CALL UNMAGIC_AVERAGE_R(DYNAMIC_MEM(IMPTR),
     &                           DYNAMIC_MEM(OUTPTR),
     &                           DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &                           MINADJ,ZAPPIX,MAGICPIX,MAGIC_FLOAT,
     &                           QUAL,DYNAMIC_MEM(IQPTR),
     &                           DYNAMIC_MEM(OQPTR))
        ELSE IF(INTERP.EQ.3)THEN
          CALL UNMAGIC_LINEAR_R(DYNAMIC_MEM(IMPTR),
     &                          DYNAMIC_MEM(OUTPTR),
     &                          DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &                          MINADJ,ZAPPIX,MAGICPIX,MAGIC_FLOAT,
     &                          QUAL,DYNAMIC_MEM(IQPTR),
     &                          DYNAMIC_MEM(OQPTR))
        ELSE IF(INTERP.EQ.5)THEN
          CALL UNMAGIC_CONSTANT_R(DYNAMIC_MEM(OUTPTR),
     &                            DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &                            VAL_FLOAT,ZAPPIX,MAGIC_FLOAT,
     &                            QUAL,DYNAMIC_MEM(IQPTR),
     &                            DYNAMIC_MEM(OQPTR))
        END IF
      END IF
C
      STRING='No. of bad pixels edited = '
      DUMINT=ICH_ENCODE(STRING,ZAPPIX,36,0,NEXT)
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
      STRING='No. which could not be edited = '
      DUMINT=ICH_ENCODE(STRING,MAGICPIX,33,0,NEXT)
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
C   Set bad pixel flag as appropriate if the whole image was processed. If a
C   subset was processed this poses a problem since the program has no way of
C   knowing whether magic values still remain outside the subset. A run of the
C   STATS program using the CHECK option should follow immediately to remove
C   the doubt.
C
      IF (.NOT.QUAL) THEN
        WHOLE=.TRUE.
        DO I=1,NDIM
          IF((STAPIX(I).NE.1) .OR. (ENDPIX(I).NE.DIMS(I)))WHOLE=.FALSE.
        END DO
        IF(WHOLE)THEN
          IF(INT(MAGICPIX).EQ.0)THEN
            CALL NDP_SET_BAD_PIXEL('OUTPUT',.TRUE.,.FALSE.,STATUS)
          ELSE
            CALL NDP_SET_BAD_PIXEL('OUTPUT',.FALSE.,.TRUE.,STATUS)
          END IF
        ELSE
          CALL DSA_WRUSER
     &            ('** Warning**  Since a subset was processed, ')
          CALL DSA_WRUSER
     &            ('the bad pixel flag cannot be unset with ')
          CALL DSA_WRUSER
     &            ('certainty. Its value has not been changed. ')
          CALL DSA_WRUSER
     &            ('A run of STATS with the CHECK option is ')
          CALL DSA_WRUSER
     &            ('recommended to remove all doubt.\\N')
        END IF
      END IF ! (.NOT.QUAL)
C
C   Tidy up and exit.
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END

