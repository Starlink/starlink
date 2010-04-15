      SUBROUTINE MAGIC
C+
C
C   ---------
C   M A G I C
C   ---------
C
C   Description
C   -----------
C   Replaces all pixels in an image or an image subset, whose values lie
C   outside a specified range, with the magic value or if a quality
C   array is present, flags bad quality.
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported.
C   - Magic values supported.
C   - Variance arrays not supported.
C   - Quality arrays supported
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
C   IMAGE   Name of the structure containing the image to be inspected
C           (character)(prompted for).
C
C   START   Coordinate in each dimension of IMAGE at which the edit
C           operation is to start (real, array)(prompted for).
C
C   END     Coordinate in each dimension of IMAGE at which the edit
C           operation is to end (real, array)(prompted for).
C
C   LOW     Lowest value of range outside which pixel values will be
C           replaced with the magic value (real)(prompted for).
C
C   HIGH    Highest value of range outside which pixel values will be
C           replaced with the magic value (real)(prompted for).
C
C   OUTPUT  Name of the structure containing the output image. May be the
C           same as IMAGE (character)(prompted for).
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
C   - The IMAGE structure is copied to OUTPUT.
C   - A subroutine appropriate to the data type and the presence or absence
C     of magic values is called to edit the OUTPUT data array.
C   - The OUTPUT bad data flag is set if any pixels were changed to the
C     magic value.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_DATA_SIZE
C     DSA_GET_RANGE
C     DSA_INPUT
C     DSA_MAP_DATA
C     DSA_MAP_QUALITY
C     DSA_OPEN
C     DSA_OUTPUT
C     DSA_SEEK_QUALITY
C     DSA_SEEK_RANGE
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library ICH:
C     ICH_ENCODE
C     ICH_LEN
C
C   Library NDP:
C     NDP_AXIS_RANGE
C     NDP_DISPLAY_PROGRESS
C     NDP_GET_IMAGE_INFO
C     NDP_SET_BAD_PIXEL
C
C
C   Internal subroutines called
C   ---------------------------
C   MAGIC_DATA_W
C   MAGIC_DATA_WQ
C   MAGIC_DATA_R
C   MAGIC_DATA_RQ
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1988   - Original program
C   03-AUG-1990   - Fixed typo which kept the real/magic_value routine
C                   from ever being called.  (JRL)
C   12-DEC-1991   - Quality array processing added.
C                   Code re-written in GENERIC format. (GOLDJIL)
C   02-DEC-1992   - Unix version (GOLDJIL)
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Functions used
C
      INTEGER  DYN_ELEMENT,ICH_ENCODE,ICH_LEN
C
C   Local variables
C
      INTEGER   ADDRESS            ! Address of dynamic memory element
      LOGICAL   BADPIX             ! Value of bad pixel flag
      INTEGER   DIMS(10)           ! Dimensions of IMAGE
      INTEGER   DUMINT             ! INTEGER dummy variable
      REAL      DUMREAL            ! REAL dummy variable
      REAL      END(6)             ! End coordinates of subset
      INTEGER   ENDPIX(6)          ! End pixel of subset
      INTEGER*2 HIGHI              ! INTEGER*2 maximum value of data range
      REAL      HIGHR              ! REAL maximum value of data range
      INTEGER   IMPTR              ! Dynamic pointer to IMAGE data array
      INTEGER   ISLOT              ! Map slot number for IMAGE data
      INTEGER*2 LOWI               ! INTEGER*2 minimum value of data range
      REAL      LOWR               ! REAL minimum value of data range
      REAL      MAGICPIX           ! Number of pixels replaced
      INTEGER   NDIM               ! Number of dimensions in IMAGE
      INTEGER   NELM               ! Number of elements in IMAGE
      INTEGER   OSLOT              ! Map slot number for output data
      LOGICAL   QUAL               ! Flags presence of quality array
      INTEGER   IQPTR              ! Dynamic pointer to i/p quality array
      INTEGER   OQPTR              ! Dynamic pointer to o/p quality array
      INTEGER   IQSLOT             ! Map slot number for i/p quality array
      INTEGER   OQSLOT             ! Map slot number for o/p quality array
      INTEGER   OUTPTR             ! Dynamic pointer to output data
      INTEGER   STAPIX(6)          ! Start pixel of subset
      REAL      START(6)           ! Start coordinate of subset
      INTEGER   STATUS             ! Status code
      CHARACTER STRING*80          ! Message string
      CHARACTER TYPE*8             ! IMAGE data array type
      REAL      VMAX               ! Maximum of IMAGE data array
      REAL      VMIN               ! Minimum of IMAGE data array
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=0)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C   Initialize
C
      STATUS=0
C
C   Open DSA system

C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Open file for IMAGE
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get information about IMAGE
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
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
C   Get IMAGE data range
C
c     CALL DSA_SEEK_RANGE('IMAGE',VALID,STATUS)
c     IF(STATUS.NE.0)GO TO 500
c     IF(VALID)THEN
c       CALL DSA_GET_RANGE('IMAGE',VMIN,VMAX,STATUS)
c       IF(STATUS.NE.0)GO TO 500
c     ELSE
        IF(TYPE.EQ.'SHORT')THEN
          VMIN=REAL(MIN_SHORT)
          VMAX=REAL(MAX_SHORT)
        ELSE
          VMIN=MIN_FLOAT
          VMAX=MAX_FLOAT
        END IF
c     END IF
C
C   Get minimum value of data range to be kept
C
      CALL PAR_RDVAL('LOW',VMIN,VMAX,0.0,' ',DUMREAL)
      IF(TYPE.EQ.'SHORT')THEN
        LOWI=INT(DUMREAL)
      ELSE
        LOWR=DUMREAL
      END IF
C
C   Get maximum value of data range to be kept
C
      CALL PAR_RDVAL('HIGH',VMIN,VMAX,0.0,' ',DUMREAL)
      IF(TYPE.EQ.'SHORT')THEN
        HIGHI=INT(DUMREAL)
      ELSE
        HIGHR=DUMREAL
      END IF
C
C   Open file for OUTPUT
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Test for quality data
C
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C   Magic values are not to be removed from the data arrays if there's
C   no quality data
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Map IMAGE data array
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      IMPTR=DYN_ELEMENT(ADDRESS)
C
C   Map OUTPUT data array
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('OUTPUT','WRITE','SHORT',ADDRESS,OSLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ADDRESS,OSLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      OUTPTR=DYN_ELEMENT(ADDRESS)
C
C   Map quality information (if present)
C
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',
     &                        ADDRESS,IQSLOT,STATUS)
        IQPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',
     &                        ADDRESS,OQSLOT,STATUS)
        OQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C   Perform edit
C
      CALL DSA_WRUSER('Editing data array...\\N')
      IF(TYPE.EQ.'SHORT')THEN
        IF(.NOT.BADPIX)THEN
          CALL MAGIC_DATA_W
     &      (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &       DIMS,NDIM,NELM,STAPIX,ENDPIX,LOWI,HIGHI,MAGICPIX,
     &       MAGIC_SHORT,QUAL,DYNAMIC_MEM(OQPTR))
        ELSE
          CALL MAGIC_DATA_WQ
     &      (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &       DIMS,NDIM,NELM,STAPIX,ENDPIX,LOWI,HIGHI,MAGICPIX,
     &       MAGIC_SHORT)
        END IF
      ELSE
        IF(.NOT.BADPIX)THEN
          CALL MAGIC_DATA_R
     &      (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &       DIMS,NDIM,NELM,STAPIX,ENDPIX,LOWR,HIGHR,MAGICPIX,
     &       MAGIC_FLOAT,QUAL,DYNAMIC_MEM(OQPTR))
        ELSE
          CALL MAGIC_DATA_RQ
     &      (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &       DIMS,NDIM,NELM,STAPIX,ENDPIX,LOWR,HIGHR,MAGICPIX,
     &       MAGIC_FLOAT)
        END IF
      END IF
C
      IF (.NOT.QUAL) THEN
        STRING='No. of pixels set to magic value = '
      ELSE
        STRING='No. of pixels set to bad quality = '
      END IF
      DUMINT=ICH_ENCODE(STRING,MAGICPIX,36,0,DUMINT)
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
C   Set bad pixel flag if appropriate
C
      IF (.NOT.QUAL) THEN
        IF(.NOT.BADPIX)THEN
          IF(INT(MAGICPIX).GT.0)THEN
            CALL NDP_SET_BAD_PIXEL('OUTPUT',.TRUE.,.TRUE.,STATUS)
          END IF
        END IF
      END IF
C
C   Tidy up and exit
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END

