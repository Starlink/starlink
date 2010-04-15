
      SUBROUTINE COLLAPSE
C+
C
C   ---------------
C   C O L L A P S E
C   ---------------
C
C   Description
C   -----------
C   Collapses an n-D image in one or more dimensions to form an (n-c)-D
C   image, where c is the number of collapsed dimensions. The pixel values
C   are summed along the specified axes. If a magic value pixel is found
C   while summing it is ignored, but the sum is still valid.
C
C
C   Scope of program
C   ----------------
C   - Images of two to six dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
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
C   IMAGE  Name of the structure containing the input image. (character)
C          (prompted for).
C
C   AXKEY  Keys for dimensions along which image is to be collapsed.
C          (integer, array)(prompted for).
C
C   START  Coordinate in each dimension of IMAGE at which the summation is
C          to start. (real, array)(prompted for).
C
C   END    Coordinate in each dimension of IMAGE at which the summation is
C          to end. (real, array)(prompted for).
C
C   OUTPUT Name of the structure containing the output image. May be the
C          same as IMAGE. (character)(prompted for).
C
C
C   Keywords
C   --------
C   WHOLE  Instructs the program to sum along the whole ranges of the
C          selected axes, using the whole range of all other dimensions.
C          Otherwise, a subset of each dimension may be selected.
C
C   FLOAT  Instructs the program to create an output structure containing a
C          data array of type FLOAT. Otherwise, the output data array will
C          be of the same type as the input.
C
C
C   Propagation of data structure
C   -----------------------------
C   - All standard objects except the data array and axes are copied from
C     IMAGE.
C   - Data array and axes are modified and reshaped.
C
C
C   Method
C   ------
C   - The IMAGE structure is tested for the bad data flag. If is it found
C     and non-zero, magic values are assumed to be present and are left in
C     the data.
C   - If the data array in IMAGE is of type SHORT, the option of creating an
C     OUTPUT array of type FLOAT is given, to prevent overflow errors. If
C     this option is selected, the OUTPUT structure is created using the
C     appropriate structure definition file.
C   - If the OUTPUT array is to be of the same type as the IMAGE array, the
C     structure IMAGE is copied to OUTPUT, with the exception of the data
C     array and axes.  DSA_RESHAPE_ routines are called to modify the
C     dimensions of the OUTPUT data and axis arrays.
C   - The calibrations, labels, and units of the uncollapsed IMAGE axes are
C     copied to the appropriate OUTPUT axes.
C   - A subroutine appropriate to the data type, the presence or absence of
C     magic values, and whether or not axis 1 is collapsed is called to
C     collapse the OUTPUT data array. If axis 1 is not involved, it is more
C     efficient to use a routine which does not test for the fact.
C   - If a magic value pixel is found while summing, it is ignored, but the
C     sum is still valid. This method differs from the usual treatment of
C     magic values, since the occurrence of an input magic value pixel
C     generally renders the corresponding output pixel invalid.
C     In the case of a quality array, the output pixel is flagged as bad
C     in the quality array if any of the summed pixels is bad.
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_CREATE_STRUCTURE
C     DSA_DATA_SIZE
C     DSA_GET_AXIS_INFO
C     DSA_INPUT
C     DSA_MAP_AXIS_DATA
C     DSA_MAP_DATA
C     DSA_MAP_ERRORS
C     DSA_MAP_QUALITY
C     DSA_OPEN
C     DSA_OUTPUT
C     DSA_RESHAPE_AXIS
C     DSA_RESHAPE_DATA
C     DSA_SEEK_ERRORS
C     DSA_SEEK_QUALITY
C     DSA_SET_AXIS_INFO
C     DSA_SET_STRUCT_VAR
C     DSA_SIMPLE_OUTPUT
C     DSA_TYPESIZE
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library ICH:
C     ICH_ENCODE
C
C   Library NDP:
C     NDP_AXIS_RANGE
C     NDP_DISPLAY_PROGRESS
C     NDP_ERROR_ARITH_W
C     NDP_ERROR_ARITH_R
C     NDP_GET_IMAGE_INFO
C     NDP_PAR_RDARY
C
C   Library PAR:
C     PAR_RDVAL
C
C   Library GEN:
C     GEN_FILL
C
C   Internal subroutines called
C   ---------------------------
C   COLLAPSE_NEWAXIS
C   COLLAPSE_AX1COL_W
C   COLLAPSE_AX1COL_WQ
C   COLLAPSE_AX1COL_R
C   COLLAPSE_AX1COL_RQ
C   COLLAPSE_AX1NOTCOL_W
C   COLLAPSE_AX1NOTCOL_WQ
C   COLLAPSE_AX1NOTCOL_R
C   COLLAPSE_AX1NOTCOL_RQ
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
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   16-OCT-1991   - Quality and error array handling added. Subroutines
C                   written in GENERIC form.
C                   Dependence on .DEF file removed. (GOLDJIL)
C   07-FEB-1992   - DSA_CFILLx calls replaced with GEN_FILL, since the
C                   former is not in the shareable library (GOLDJIL)
C   27-NOV-1992   - Unix version (GOLDJIL).
C   06-OCT-1994   - Removed unused variables. (GJP)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Functions used.
C
      CHARACTER ICH_CI*8
      INTEGER   DYN_ELEMENT
      INTEGER   DSA_TYPESIZE
C
C   Local variables.
C
      INTEGER   ADDRESS             ! Address of dynamic memory element
      INTEGER   AXKEY(6)            ! Keys to dimensions to be collapsed
      CHARACTER AXINFO(2)*32        ! Axis label and units
      INTEGER   AXPTR(6)            ! Dynamic memory pointers to input axes
      INTEGER   AXSLOT(6)           ! Map slot numbers for input axes
      LOGICAL   BADPIX              ! Value of bad pixel flag
      INTEGER   DIMS(10)            ! Dimensions of image
      REAL      DUMARR(6)           ! REAL array dummy variable
      INTEGER   DUMINT              ! INTEGER dummy variable
      REAL      END(6)              ! End coordinates of subset
      INTEGER   ENDPIX(6)           ! End pixel of subset
      LOGICAL   ERR                 ! Error array flag
      LOGICAL   FLOAT               ! Instruction to create FLOAT output array
      INTEGER   I                   ! Loop counter
      INTEGER   IEPTR               ! Dynamic pointer to i/p error array
      INTEGER   IESLOT              ! Map slot number for i/p error array
      INTEGER   IMPTR               ! Dynamic pointer to IMAGE data array
      CHARACTER INFO*32             ! DSA_SIMPLE_OUTPUT format string
      INTEGER   IQPTR               ! Dynamic pointer to i/p quality array
      INTEGER   IQSLOT              ! Map slot number for i/p quality array
      INTEGER   ISLOT               ! Map slot number for IMAGE data
      INTEGER   J                   ! Loop counter
      INTEGER   NDIM                ! Number of dimensions in IMAGE
      INTEGER   NELM                ! Number of elements in IMAGE
      INTEGER   OAXPTR(6)           ! Dynamic memory pointers to output axes
      INTEGER   OAXSLOT(6)          ! Map slot numbers for output axes
      INTEGER   ODIMS(10)           ! Dimensions of OUTPUT
      INTEGER   OEPTR               ! Dynamic pointer to o/p error array
      INTEGER   OESLOT              ! Map slot number for o/p error array
      INTEGER   ONDIM               ! Number of dimensions in OUTPUT
      INTEGER   ONELM               ! Number of elements in OUTPUT
      INTEGER   OQPTR               ! Dynamic pointer to o/p quality array
      INTEGER   OQSLOT              ! Map slot number for o/p quality array
      INTEGER   OSLOT               ! Map slot number for output data
      INTEGER   OUTPTR              ! Dynamic pointer to output data
      LOGICAL   QUAL                ! Quality array flag
      INTEGER   STAPIX(6)           ! Start pixel of subset
      REAL      START(6)            ! Start coordinate of subset
      INTEGER   STATUS              ! Status code
      CHARACTER TYPE*8              ! IMAGE data array type
      REAL      VMAX(6)             ! Maximum value of parameter
      REAL      VMIN(6)             ! Minimum value of parameter
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=1)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C   Initialize.
C
      STATUS=0
C
C   Open DSA system.
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Open file for IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
C
C   Get information about IMAGE.
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
      DO I=1,NDIM
        ENDPIX(I)=DIMS(I)
      END DO
      IF(NDIM.LT.2)THEN
        CALL DSA_WRUSER('Image must have at least two dimensions.\\N')
        GO TO 500
      END IF
C
C   Get keys to axes along which image is to be collapsed.
C
      DO I=1,NDIM
        VMIN(I)=0.0
        VMAX(I)=1.0
      END DO
      CALL NDP_PAR_RDARY('AXKEY',VMIN,VMAX,'N',' ',NDIM,6,DUMARR)
      DO I=1,NDIM
        AXKEY(I)=INT(DUMARR(I))
      END DO
C
C   Get IMAGE axis range.
C
      CALL NDP_AXIS_RANGE
     &  ('IMAGE',DIMS,NDIM,START,END,STAPIX,ENDPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Find out about quality and error arrays in IMAGE
C
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C   Compute dimensions and number of elements in output image.
C
      J=0
      ONDIM=0
      ONELM=1
      DO I=1,NDIM
        IF(AXKEY(I).EQ.0 .AND. STAPIX(I).NE.ENDPIX(I))THEN
          J=J+1
          ONDIM=ONDIM+1
          ODIMS(J)=ENDPIX(I)-STAPIX(I)+1
          ONELM=ONELM*ODIMS(J)
        END IF
      END DO
C
C   Quit if the resulting dimensionality is zero.
C
      IF(ONDIM.EQ.0)THEN
        CALL DSA_WRUSER('The output image will have no dimensions!\\N')
        GO TO 500
      END IF
C
C   If all is OK, create an empty output file
C
      CALL DSA_OUTPUT
     &    ('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   If the IMAGE data array is of type SHORT, ask whether the OUTPUT array
C   should be FLOAT. It is normal for the two arrays to be of the same type,
C   but this option prevents the overflow errors which often occur when SHORT
C   data is summed over a large area.
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL PAR_RDKEY('FLOAT',.TRUE.,FLOAT)
      END IF
C
C   If so, create OUTPUT structure with a data array of type FLOAT.
C
      IF(FLOAT)THEN
C
C   - change type indicator to FLOAT, so that the data arrays will be mapped as
C     FLOAT, and the computations will be done in floating point.
C
        TYPE='FLOAT'
C
C   - Create a new structure...
C
        INFO='D'
        IF (QUAL) INFO=INFO//',Q(BYTE)'
        IF (ERR)  INFO=INFO//',E(FLOAT)'
        DO I=1,ONDIM
          INFO=INFO//',A'//ICH_CI(I)
        END DO
        CALL DSA_SIMPLE_OUTPUT('OUTPUT',INFO,TYPE,
     &                         ONDIM,ODIMS,STATUS)
        IF (STATUS.NE.0) GO TO 500
      END IF
C
C   Reshape OUTPUT data array.
C
      IF(.NOT.FLOAT)THEN
        CALL DSA_WRUSER('Reshaping data array...\\N')
        CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',ONDIM,ODIMS,STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Rearrange the axes.
C   - map IMAGE axes.
C
      DO I=1,NDIM
        CALL DSA_MAP_AXIS_DATA
     &    ('IMAGE',I,'READ','FLOAT',ADDRESS,AXSLOT(I),STATUS)
        IF(STATUS.NE.0)GO TO 500
        AXPTR(I)=DYN_ELEMENT(ADDRESS)
      END DO
C
      J=0
      DO I=1,NDIM
        IF(AXKEY(I).EQ.0 .AND. STAPIX(I).NE.ENDPIX(I))THEN
          J=J+1
C
C   - reshape OUTPUT axis.
C
          IF(.NOT.FLOAT)THEN
            CALL DSA_RESHAPE_AXIS
     &        ('OUTPUT',J,'IMAGE',I,1,ODIMS(J),STATUS)
            IF(STATUS.NE.0)GO TO 500
          END IF
C
C   - map OUTPUT axis.
C
          CALL DSA_MAP_AXIS_DATA
     &      ('OUTPUT',J,'WRITE','FLOAT',ADDRESS,OAXSLOT(J),STATUS)
          IF(STATUS.NE.0)GO TO 500
          OAXPTR(J)=DYN_ELEMENT(ADDRESS)
C
C   - copy values from appropriate IMAGE axis.
C
          CALL COLLAPSE_NEWAXIS
     &      (DYNAMIC_MEM(AXPTR(I)),DYNAMIC_MEM(OAXPTR(J)),
     &       DIMS(I),ODIMS(J),STAPIX(I))
C
C   - transfer axis label and units.
C
          CALL DSA_GET_AXIS_INFO('IMAGE',I,2,AXINFO,0,DUMINT,STATUS)
          CALL DSA_SET_AXIS_INFO('OUTPUT',J,2,AXINFO,0,DUMINT,STATUS)
        END IF
      END DO
C
C   Magic values are not to be removed from data arrays (unless quality
C   data is present)
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Map IMAGE data array (error/quality array?)
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IMPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','SHORT',
     &                        ADDRESS,IESLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IMPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',
     &                        ADDRESS,IESLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      END IF
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',
     &                        ADDRESS,IQSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        IQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C   Map OUTPUT data array.
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('OUTPUT','WRITE','SHORT',
     &                    ADDRESS,OSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','SHORT',
     &                        ADDRESS,OESLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      ELSE
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',
     &                    ADDRESS,OSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',
     &                        ADDRESS,OESLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      END IF
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',
     &                        ADDRESS,OQSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        OQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C   Initialize OUTPUT data array (quality/error array?) with zeros.
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL GEN_FILL(ONELM*DSA_TYPESIZE('SHORT',STATUS),
     &                0,DYNAMIC_MEM(OUTPTR))
        IF (ERR) CALL GEN_FILL(ONELM*DSA_TYPESIZE('SHORT',STATUS),
     &                         0,DYNAMIC_MEM(OEPTR))
      ELSE
        CALL GEN_FILL(ONELM*DSA_TYPESIZE('FLOAT',STATUS),
     &                0,DYNAMIC_MEM(OUTPTR))
        IF (ERR) CALL GEN_FILL(ONELM*DSA_TYPESIZE('FLOAT',STATUS),
     &                0,DYNAMIC_MEM(OEPTR))
      END IF
      IF (QUAL) CALL GEN_FILL(ONELM*DSA_TYPESIZE('BYTE',STATUS),
     &                        0,DYNAMIC_MEM(OQPTR))
C
C   Perform the collapse.
C
      CALL DSA_WRUSER('Collapsing...\\N')
C
C   - if axis 1 is collapsed.
C
      IF(AXKEY(1).EQ.1)THEN
        IF(TYPE.EQ.'SHORT')THEN
          IF(.NOT.BADPIX)THEN
            CALL COLLAPSE_AX1COL_W
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DIMS,NDIM,NELM,ONELM,STAPIX,ENDPIX,AXKEY,
     &         QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          ELSE
            CALL COLLAPSE_AX1COL_WQ
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DIMS,NDIM,NELM,ONELM,STAPIX,ENDPIX,AXKEY,MAGIC_SHORT,
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          END IF
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL COLLAPSE_AX1COL_R
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DIMS,NDIM,NELM,ONELM,STAPIX,ENDPIX,AXKEY,
     &         QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          ELSE
            CALL COLLAPSE_AX1COL_RQ
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DIMS,NDIM,NELM,ONELM,STAPIX,ENDPIX,AXKEY,MAGIC_FLOAT,
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          END IF
        END IF
C
C   - if axis 1 is not collapsed.
C
      ELSE
        IF(TYPE.EQ.'SHORT')THEN
          IF(.NOT.BADPIX)THEN
            CALL COLLAPSE_AX1NOTCOL_W
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DIMS,NDIM,NELM,ONELM,STAPIX,ENDPIX,AXKEY,
     &         QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          ELSE
            CALL COLLAPSE_AX1NOTCOL_WQ
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DIMS,NDIM,NELM,ONELM,STAPIX,ENDPIX,AXKEY,MAGIC_SHORT,
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
        END IF
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL COLLAPSE_AX1NOTCOL_R
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DIMS,NDIM,NELM,ONELM,STAPIX,ENDPIX,AXKEY,
     &         QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          ELSE
            CALL COLLAPSE_AX1NOTCOL_RQ
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DIMS,NDIM,NELM,ONELM,STAPIX,ENDPIX,AXKEY,MAGIC_FLOAT,
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          END IF
        END IF
      END IF
C
C   Output warning message about treatment of magic values etc.
C
      IF(BADPIX)THEN
        CALL DSA_WRUSER('** Warning **  Magic values were ')
        CALL DSA_WRUSER('encountered during the summation but were\\N')
        CALL DSA_WRUSER('ignored. In such a case an output pixel is ')
        CALL DSA_WRUSER
     &           ('still valid, being the sum of the non-magic\\N')
        CALL DSA_WRUSER('input pixels. However its value will be ')
        CALL DSA_WRUSER('inconsistent with those of neighbouring\\N')
        CALL DSA_WRUSER
     &           ('pixels which were not affected in this way.\\N')
      END IF
      IF (QUAL) THEN
        CALL DSA_WRUSER('** Warning **  One bad quality pixel ')
        CALL DSA_WRUSER('encountered during summation will result\\n')
        CALL DSA_WRUSER('in a corresponding bad quality pixel ')
        CALL DSA_WRUSER('in the output.\\n')
      END IF
      IF ((BADPIX.OR.QUAL).AND.ERR) THEN
        CALL DSA_WRUSER('Further, the error values ')
        CALL DSA_WRUSER('for that pixel will no longer be valid.\\n')
      ELSE
        CALL DSA_WRUSER('\\n')
      END IF
C
C   Tidy up and exit
C
500   CONTINUE
      CALL DSA_CLOSE (STATUS)
C
      END





      SUBROUTINE COLLAPSE_NEWAXIS(ARRAY,OARRAY,NELM,ONELM,STAPIX)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER   NELM,ONELM,STAPIX
      REAL      ARRAY(NELM),OARRAY(ONELM)
C
C     Local variables
C
      INTEGER   I,J
C
      J=1
C
      DO I=STAPIX,STAPIX+ONELM-1
        OARRAY(J)=ARRAY(I)
        J=J+1
      END DO
C
      END
