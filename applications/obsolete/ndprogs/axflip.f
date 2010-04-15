      SUBROUTINE AXFLIP
C+
C
C   -----------
C   A X F L I P
C   -----------
C
C   Description
C   -----------
C   Reverses an image along a specified axis.
C
C
C   Scope of program
C   ----------------
C   - Currently handles 3-D images only.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting not supported.
C   - Magic values not supported since not relevant.
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
C   IMAGE   Name of the structure containing the input image. (character)
C           (prompted for).
C
C   AXIS    Number of the axis along which the image is to be reversed.
C           (integer)(prompted for).
C
C   OUTPUT  Name of the structure containing the output image. May be the
C           same as IMAGE. (character)(prompted for).
C
C   Keywords
C   --------
C   None.
C
C
C   Propagation of data structure
C   -----------------------------
C   - All standard objects are copied from IMAGE.
C   - Data array and one axis are modified.
C
C
C   Method
C   ------
C   - The structure IMAGE is copied to OUTPUT.
C   - The required axis array is reversed in OUTPUT.
C   - A subroutine appropriate to the data type is called to reverse the
C     OUTPUT data array.
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_DATA_SIZE
C     DSA_FREE_WORKSPACE
C     DSA_GET_WORKSPACE
C     DSA_GET_WORK_ARRAY
C     DSA_INPUT
C     DSA_MAP_AXIS_DATA
C     DSA_MAP_DATA
C     DSA_MAP_ERRORS
C     DSA_MAP_QUALITY
C     DSA_OPEN
C     DSA_OUTPUT
C     DSA_SEEK_ERRORS
C     DSA_SEEK_QUALITY
C     DSA_TYPESIZE
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
C     NDP_DISPLAY_PROGRESS
C     NDP_GET_IMAGE_INFO
C
C   Library PAR:
C     PAR_RDVAL
C
C
C   Internal subroutines called
C   ---------------------------
C   AXFLIP_NEWAXIS
C   AXFLIP_3D_W
C   AXFLIP_3D_R
C   AXFLIP_3D_B
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
C   DO WHILE / END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C   - Only 3-D images are handled at present. Subroutines to be written:
C       AXFLIP_2D_<T>
C       AXFLIP_4D_<T>
C       AXFLIP_5D_<T>
C       AXFLIP_6D_<T>
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   03-AUG-1990   - Fixed so that adjustable array sizes are not passed
C                   as elements of another array.  This practice has been
C                   banned by the v5.2 compiler.  Fixed bug where workspace
C                   had not been allocated before the call to AXFLIP_NEWAXIS.
C                   Also fixed a few loose ends in the prolog.  (JRL)
C   16-OCT-1991   - Quality and error arrays implemented. GENERIC subroutines
C                   written. (GOLDJIL)
C   27-NOV-1992   - Unix version (GOLDJIL)
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used.
C
      INTEGER  DSA_TYPESIZE,DYN_ELEMENT
C
C
C     Local variables.
C
      INTEGER   ADDRESS       ! Address of dynamic memory element
      INTEGER   AXIS          ! Axis to be reversed
      LOGICAL   BADPIX        ! Value of bad pixel flag
      INTEGER   DIMS(10)      ! Dimensions of image
      REAL      DUMREAL       ! REAL dummy variable
      INTEGER   ELEM          ! Element size in bytes
      LOGICAL   ERR           ! Errrr...error flag
      INTEGER   IEPTR         ! Dynamic pointer to i/p error array
      INTEGER   IESLOT        ! Map slot number for i/p error array
      INTEGER   IMPTR         ! Dynamic pointer to image data
      INTEGER   IQPTR         ! Dynamic pointer to i/p quality array
      INTEGER   IQSLOT        ! Map slot number for i/p quality array
      INTEGER   ISLOT         ! Map slot number for input data
      INTEGER   NDIM          ! Dimensionality of image
      INTEGER   NELM          ! Number of elements in image
      INTEGER   OAXPTR        ! Dynamic pointer to OUTPUT axis
      INTEGER   OAXSLOT       ! Map slot number for OUTPUT axis
      INTEGER   OEPTR         ! Dynamic pointer to o/p error array
      INTEGER   OESLOT        ! Map slot number for o/p error array
      INTEGER   OQPTR         ! Dynamic pointer to o/p quality array
      INTEGER   OQSLOT        ! Map slot number for o/p quality array
      INTEGER   OSLOT         ! Map slot number for output data
      INTEGER   OUTPTR        ! Dynamic pointer to output image data
      LOGICAL   QUAL          ! Quality array flag
      INTEGER   STATUS        ! Status code
      CHARACTER TYPE*8        ! Data array type
      INTEGER   WKPTR         ! Dynamic pointer to workspace
      INTEGER   WKSLOT        ! Map slot number for workspace
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=0)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C     Initialize.
C
      STATUS=0
C
C     Open DSA system.
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Open file for IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get information about IMAGE.
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get element size.
C
      ELEM=DSA_TYPESIZE(TYPE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',3,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
      IF(NDIM.LT.3)THEN
        CALL DSA_WRUSER('This program handles 3-D images only.\\N')
        GO TO 500
      END IF
C
C     Find any error or quality arrays lurking in the bowels of the structure
C
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get axis to be reversed.
C
      CALL PAR_RDVAL('AXIS',1.0,REAL(NDIM),1.0,' ',DUMREAL)
      AXIS=INT(DUMREAL)
C
C     Open file for OUTPUT.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Map required OUTPUT axis.
C
      CALL DSA_MAP_AXIS_DATA
     &  ('OUTPUT',AXIS,'UPDATE','FLOAT',ADDRESS,OAXSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      OAXPTR=DYN_ELEMENT(ADDRESS)
C
C     Get some workspace
C
      CALL DSA_GET_WORK_ARRAY(DIMS(AXIS),'FLOAT',ADDRESS,WKSLOT,STATUS)
      IF (STATUS .NE. 0) GO TO 500
      WKPTR = DYN_ELEMENT(ADDRESS)
C
C     Reverse axis array.
C
      CALL AXFLIP_NEWAXIS
     &  (DYNAMIC_MEM(OAXPTR),DYNAMIC_MEM(WKPTR),DIMS(AXIS))
C
C     Free the workspace
C
      CALL DSA_FREE_WORKSPACE(WKSLOT,STATUS)
      IF (STATUS .NE .0) GO TO 500
C
C     Magic values are not to be removed from data arrays (unless a quality
C     array is at home...)
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C     Map IMAGE data array (and error / quality array?)
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
     &                       ADDRESS,IQSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        IQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C     Map OUTPUT data array etc.
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
     &                       ADDRESS,OQSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        OQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C  Unix has no equivalent of VMS working set size, so process data all at once.
C
      IF(NDIM.EQ.3)THEN
        CALL DSA_WRUSER('Reversing data array...\\N')
        IF(TYPE.EQ.'SHORT')THEN
          CALL AXFLIP_3D_W
     &       (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),DIMS(1),
     &        DIMS(2),DIMS(3),DIMS,AXIS)
        ELSE
          CALL AXFLIP_3D_R
     &       (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),DIMS(1),
     &        DIMS(2),DIMS(3),DIMS,AXIS)
        END IF
        IF (ERR) THEN
          CALL DSA_WRUSER('Reversing error array...\\N')
          IF(TYPE.EQ.'SHORT')THEN
            CALL AXFLIP_3D_W
     &         (DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),DIMS(1),
     &          DIMS(2),DIMS(3),DIMS,AXIS)
          ELSE
            CALL AXFLIP_3D_R
     &         (DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),DIMS(1),
     &          DIMS(2),DIMS(3),DIMS,AXIS)
          END IF
        END IF ! (ERR)
        IF (QUAL) THEN
          CALL DSA_WRUSER('Reversing quality array...\\N')
          CALL AXFLIP_3D_B
     &       (DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),DIMS(1),
     &        DIMS(2),DIMS(3),DIMS,AXIS)
        END IF ! (QUAL)
      END IF  ! (NDIM...)
C
C     Tidy up and exit.
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END





      SUBROUTINE AXFLIP_NEWAXIS(ARRAY,WORK,DAXIS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER DAXIS              ! Size of axis to be reversed
      REAL    ARRAY(DAXIS)       ! Axis data array
      REAL    WORK(DAXIS)        ! Workspace array
C
C     Local variables
C
      INTEGER   I                ! Array index
      INTEGER   J                ! Array index
C
      J=1
C
      DO I=1,DAXIS
        WORK(I)=ARRAY(I)
      END DO
C
      DO I=DAXIS,1,-1
        ARRAY(I)=WORK(J)
        J=J+1
      END DO
C
      END




