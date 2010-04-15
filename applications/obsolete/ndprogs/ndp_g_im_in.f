      SUBROUTINE NDP_GET_IMAGE_INFO
     &  (REF_NAME,DISPLAY,WARNING,TYPE_UC,BADPIX,STATUS)
C+
C
C   -----------------------------------
C   N D P _ G E T _ I M A G E _ I N F O
C   -----------------------------------
C
C   Description
C   -----------
C   Returns the data array type of an image and its bad data flag value.
C
C   Also, if instructed, displays the following information about the image:
C     full file name,
C     object name (i.e. image label),
C     data range,
C     data array type,
C     presence or absence of magic values,
C     presence of quality or error arrays
C     and for each axis:
C       dimension,
C       range of calibrations,
C       label,
C       units.
C
C   Also, if instructed, displays a warning message if the data array type
C   is SHORT or INT, to advise the user of possible integer truncation and
C   overflow errors. This would only be appropriate when the calling program
C   performs floating point computations.
C
C
C   Parameters
C   ----------
C   REF_NAME  (> character). Reference name associated with the structure.
C   DISPLAY   (> logical). Instruction to display information. Warning and
C             error messages are always displayed.
C   WARNING   (> logical). Instruction to display warning if data type is
C             SHORT or INT.
C   TYPE_UC   (< character). Data array type in upper case.
C   BADPIX    (< logical). Bad data flag.
C   STATUS    (! integer). Status code.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_AXIS_RANGE
C     DSA_AXIS_SIZE
C     DSA_DATA_SIZE
C     DSA_DATA_TYPE
C     DSA_GET_ACTUAL_NAME
C     DSA_GET_AXIS_INFO
C     DSA_GET_DATA_INFO
C     DSA_GET_RANGE
C     DSA_MAP_AXIS_DATA
C     DSA_SEEK_AXIS
C     DSA_SEEK_FLAGGED_VALUES
C     DSA_SEEK_ERRORS
C     DSA_SEEK_QUALITY
C     DSA_SEEK_RANGE
C     DSA_UNMAP
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library GEN:
C     GEN_ELEMF
C
C   Library ICH:
C     ICH_CI
C     ICH_ENCODE
C     ICH_FOLD
C     ICH_LEN
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   09-MAY-1990   - Changed so that it unmaps axis data arrays when
C                   it's finished with them.  This had previously caused
C                   problems if one wished to close the structure in the
C                   main routine.  (JRL)
C   19-AUG-1991   - Now prints quality/error array information. (GOLDJIL)
C   13-NOV-1992   - Unix version (GOLDJIL).
C   31-OCT-1994   - Ensure that the TYPE_UC output starting with SIMPLE
C                   is modfied. (GJP)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used.
C
      CHARACTER     ICH_CI*1
      INTEGER       DYN_ELEMENT,ICH_ENCODE,ICH_FOLD,ICH_LEN
      REAL          GEN_ELEMF
C
C     Parameters.
C
      CHARACTER*(*) REF_NAME            ! Reference name of the image file
      LOGICAL       DISPLAY             ! Instruction to display image info
      LOGICAL       WARNING             ! Instruction to issue overflow warning
      CHARACTER*8   TYPE_UC             ! Type of data array in upper case
      LOGICAL       BADPIX              ! Bad data flag
      INTEGER       STATUS              ! Status code
C
C     Local variables.
C
      CHARACTER     ACT_NAME*128        ! Actual full structure name
      INTEGER       ADDRESS             ! Address of dynamic memory element
      INTEGER       AXDIMS(10)          ! Dimensionality of axis array
      CHARACTER     AXINFO(2)*64        ! Axis label and units
      INTEGER       AXNDIM              ! Number of dimensions in axis array
      INTEGER       AXNELM              ! Number of elements in axis array
      INTEGER       AXPTR               ! Dynamic pointer to axis data
      INTEGER       AXSLOT              ! Map slot number for axis data
      CHARACTER     DATINFO(2)*64       ! Data array label and units
      INTEGER       DIMS(10)            ! Dimensionality of data array
      INTEGER       DUMINT              ! INTEGER dummy variable
      REAL          DUMREAL             ! REAL dummy variable
      REAL          END                 ! End value of axis
      LOGICAL       ERRS                ! Flags presence of error array
      LOGICAL       EXIST               ! Existence flag
      REAL          HIGH                ! High value of data range
      INTEGER       I                   ! Loop counter
      REAL          LOW                 ! Low value of data range
      INTEGER       NDIM                ! Number of dimensions
      INTEGER       NELM                ! Number of elements
      INTEGER       NEXT                ! Pointer returned by ICH_ENCODE
      LOGICAL       QUAL                ! Flags presence of quality array
      CHARACTER     TYPE*8              ! Type of data array
      CHARACTER     TYPE88*1            ! Single character from TYPE_UC
      REAL          START               ! Start value of axis
      CHARACTER     STRING*80           ! Message string
      LOGICAL       STRUCT              ! Set if data object is a structure
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C  Return immediately if bad status passed.
C
      IF(STATUS.NE.0)RETURN
C
C  Get actual full structure name corresponding to reference name.
C
      CALL DSA_GET_ACTUAL_NAME(REF_NAME,ACT_NAME,STATUS)
C
C  Get data array information and display if requested -
C
      IF(DISPLAY)THEN
        CALL DSA_WRUSER(' \\N')
        CALL DSA_WRUSER('  '//ACT_NAME(:ICH_LEN(ACT_NAME))//'\\N')
C
C  - get data range.
C
        CALL DSA_SEEK_RANGE(REF_NAME,EXIST,STATUS)
        IF(EXIST)THEN
          CALL DSA_GET_RANGE(REF_NAME,LOW,HIGH,STATUS)
          CALL DSA_WRUSER('  Range = ')
          DUMINT=ICH_ENCODE(STRING,LOW,1,2,NEXT)
          CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
          CALL DSA_WRUSER(' to ')
          DUMINT=ICH_ENCODE(STRING,HIGH,1,2,NEXT)
          CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
        END IF
C
C  - get data array label and units.
C
        CALL DSA_GET_DATA_INFO(REF_NAME,2,DATINFO,0,DUMREAL,STATUS)
        IF(ICH_LEN(DATINFO(2)).GT.0)THEN
          CALL DSA_WRUSER
     &      ('  Label = '//DATINFO(2)(:ICH_LEN(DATINFO(2)))//'\\N')
        END IF
        IF(ICH_LEN(DATINFO(1)).GT.0)THEN
          CALL DSA_WRUSER
     &      ('  Units = '//DATINFO(1)(:ICH_LEN(DATINFO(1)))//'\\N')
        END IF
      END IF
C
C  Get type of data array -
C
      CALL DSA_DATA_TYPE(REF_NAME,TYPE,STRUCT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      DUMINT=ICH_FOLD(TYPE)
      TYPE_UC=TYPE

C  Remove the word SIMPLE from the front of TYPE_UC so that ADDND etc
C  can cope with the input they receive.

C  Check that SIMPLE is at the front.
      IF (TYPE_UC(1:2).EQ.'SI') THEN
         TYPE88=TYPE_UC(8:8)
         IF (TYPE88.EQ.'F') TYPE_UC='FLOAT'
         IF (TYPE88.EQ.'D') TYPE_UC='DOUBLE'
         IF (TYPE88.EQ.'I') TYPE_UC='INT'
         IF (TYPE88.EQ.'S') TYPE_UC='SHORT'
         IF (TYPE88.EQ.'B') TYPE_UC='BYTE'
         IF (TYPE88.EQ.'U') TYPE_UC='USHORT'
      END IF
C
C  - display type.
C
      IF(DISPLAY)THEN
        CALL DSA_WRUSER('  ')
        CALL DSA_WRUSER('Data array type is '//TYPE_UC)
        CALL DSA_WRUSER('\\N')
      END IF
C
C  - inform if the data array is in a CSTRUCT.
C
      IF(TYPE_UC.EQ.'CSTRUCT')THEN
        CALL DSA_WRUSER('Will be converted to FLOAT for processing\\N')
      END IF
C
C  Get bad pixel flag.
C
      CALL DSA_SEEK_FLAGGED_VALUES(REF_NAME,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C  - display value.
C
      IF(DISPLAY)THEN
        IF(BADPIX)THEN
          CALL DSA_WRUSER('  Magic values are present\\N')
        ELSE
          CALL DSA_WRUSER('  Magic values are absent\\N')
        END IF
      END IF
C
C  Now find out quality and error info
C
      IF (DISPLAY) THEN
        CALL DSA_SEEK_QUALITY(REF_NAME,QUAL,STATUS)
        CALL DSA_SEEK_ERRORS(REF_NAME,ERRS,STATUS)
        IF (QUAL)
     &      CALL DSA_WRUSER('  Quality data present\\n')
        IF (ERRS)
     &      CALL DSA_WRUSER('  Error data present\\n')
      END IF
C
C  Get data array size for comparison with axis sizes.
C
      CALL DSA_DATA_SIZE(REF_NAME,6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C  Get axis information and display if requested -
C
      IF(DISPLAY)THEN
        CALL DSA_WRUSER('  ')
        CALL DSA_WRUSER('Axis information:')
        CALL DSA_WRUSER('\\N')
      END IF
C
      DO I=1,NDIM
C
C  - get axis size, check for multi-dimensionality, and check that it matches
C    the data array.
C
        CALL DSA_AXIS_SIZE(REF_NAME,I,6,AXNDIM,AXDIMS,AXNELM,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IF(AXNDIM.GT.1)THEN
          CALL DSA_WRUSER('Axis '//ICH_CI(I))
          CALL DSA_WRUSER(' is multi-dimensional. This application ')
          CALL DSA_WRUSER('does not support multi-dimensional axes.\\N')
          STATUS=1
          GO TO 500
        END IF
        IF(AXDIMS(1).NE.DIMS(I))THEN
          CALL DSA_WRUSER('The size of axis '//ICH_CI(I)//', which is ')
          DUMINT=ICH_ENCODE(STRING,REAL(AXDIMS(1)),1,0,NEXT)
          CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
          CALL DSA_WRUSER(', does not match dimension '//ICH_CI(I))
          CALL DSA_WRUSER(' of the data array, which is ')
          DUMINT=ICH_ENCODE(STRING,REAL(DIMS(I)),1,0,NEXT)
          CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'.\\N')
          STATUS=1
          GO TO 500
        END IF
        IF(DISPLAY)THEN
          CALL DSA_WRUSER('    Axis '//ICH_CI(I)//' dimension = ')
          DUMINT=ICH_ENCODE(STRING,REAL(AXDIMS(1)),1,0,NEXT)
          CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
        END IF
C
C  - if the axis exists, map it and get its range. Cannot use DSA_AXIS_RANGE
C    for this because it always sets START to the minimum and END to the
C    maximum, which does not allow for decreasing axis values.
C
        CALL DSA_SEEK_AXIS(REF_NAME,I,EXIST,STATUS)
        IF(EXIST)THEN
          CALL DSA_MAP_AXIS_DATA
     &      (REF_NAME,I,'READ','FLOAT',ADDRESS,AXSLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          AXPTR=DYN_ELEMENT(ADDRESS)
          START=GEN_ELEMF(DYNAMIC_MEM(AXPTR),1)
          END=GEN_ELEMF(DYNAMIC_MEM(AXPTR),AXNELM)
          CALL DSA_UNMAP(AXSLOT,STATUS)
        ELSE
          START=1.0
          END=REAL(AXNELM)
        END IF
        IF(DISPLAY)THEN
          CALL DSA_WRUSER('           range     = ')
          DUMINT=ICH_ENCODE(STRING,START,1,2,NEXT)
          CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
          CALL DSA_WRUSER(' to ')
          DUMINT=ICH_ENCODE(STRING,END,1,2,NEXT)
          CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
C  - get label and units.
C
          CALL DSA_GET_AXIS_INFO(REF_NAME,I,2,AXINFO,0,DUMREAL,STATUS)
          IF(ICH_LEN(AXINFO(2)).GT.0)THEN
            CALL DSA_WRUSER
     &        ('           label     = '//AXINFO(2)(:ICH_LEN(AXINFO(2)))
     &        //'\\N')
          END IF
          IF(ICH_LEN(AXINFO(1)).GT.0)THEN
            CALL DSA_WRUSER
     &        ('           units     = '//AXINFO(1)(:ICH_LEN(AXINFO(1)))
     &        //'\\N')
          END IF
        END IF
      END DO
C
C  Issue overflow/rounding warning for integer data if requested.
C
      IF((TYPE_UC.EQ.'SHORT' .OR. TYPE_UC.EQ.'INT') .AND. WARNING)THEN
        CALL DSA_WRUSER(' \\N')
        CALL DSA_WRUSER('** Warning **  Overflow errors may occur ')
        CALL DSA_WRUSER('when this program processes an integer data ')
        CALL DSA_WRUSER('array. Also, any fractional results will be ')
        CALL DSA_WRUSER('truncated to integers. Have you considered ')
        CALL DSA_WRUSER('converting the data array to FLOAT?\\N')
        IF(.NOT.DISPLAY)CALL DSA_WRUSER(' \\N')
      END IF
C
      IF(DISPLAY)CALL DSA_WRUSER(' \\N')
C
  500 CONTINUE
      END
