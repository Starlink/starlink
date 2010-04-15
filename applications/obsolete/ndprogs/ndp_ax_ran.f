      SUBROUTINE NDP_AXIS_RANGE
     &  (REF_NAME,DIMS,NDIM,START,END,STAPIX,ENDPIX,STATUS)
C+
C
C   ---------------------------
C   N D P _ A X I S _ R A N G E
C   ---------------------------
C
C   Description
C   -----------
C   Prompts for a range of axis values and pixel numbers for each axis in
C   an image structure. The values obtained can then be used to set the
C   limits of an array operation.
C
C
C   Parameters
C   ----------
C   REF_NAME  (> character). Reference name associated with the structure.
C   DIMS      (> integer array). Image dimensions.
C   NDIM      (> integer). Number of dimensions, maximum 6.
C   START     (< real array). Start axis values.
C   END       (< real array). End axis values.
C   STAPIX    (< integer array). Start pixel numbers.
C   ENDPIX    (< integer array). End pixel numbers.
C   STATUS    (! integer). Status code.
C
C
C   Prompts
C   -------
C   WHOLE  If specified, the whole range of each axis is taken, otherwise
C          a range of values may be selected. (keyword).
C   START  Start coordinate on each axis. (real, array).
C   END    End coordinate on each axis. (real, array).
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_MAP_AXIS_DATA
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library GEN:
C     GEN_BSEARCH
C     GEN_ELEMF
C
C   Library NDP:
C     NDP_PAR_RDARY
C
C   Library PAR:
C     PAR_RDKEY
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
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   13-NOV-1992   - Unix version (GOLDJIL)
C   16-DEC-1992   - Patched feature that assumed monotonic increasing axis
C                   values (GOLDJIL)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used
C
      INTEGER       DYN_ELEMENT,GEN_BSEARCH
      REAL          GEN_ELEMF
C
C     Parameters
C
      CHARACTER*(*) REF_NAME
      INTEGER       DIMS(10),NDIM,STAPIX(6),ENDPIX(6),STATUS
      REAL          START(6),END(6)
C
C     Local variables
C
      INTEGER       ADDRESS           ! Address of dynamic memory element
      INTEGER       AXELM(6)          ! Dynamic memory pointers to axes
      INTEGER       AXSLOT(6)         ! Map slot numbers for axes
      REAL          EMIN(6)           ! Minimum end coordinates
      REAL          EMAX(6)           ! Maximum end coordinates
      INTEGER       I                 ! Loop counter
      REAL          SMIN(6)           ! Minimum start coordinates
      REAL          SMAX(6)           ! Maximum start coordinates
      LOGICAL       WHOLE             ! Instruction to use whole axes
      REAL          MAX_TEMP          ! Temporary max and min
      REAL          MIN_TEMP
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Return immediately if bad status passed.
C
      IF(STATUS.NE.0)RETURN
C
C     Set pixel ranges for whole axes.
C
      DO I=1,NDIM
        STAPIX(I)=1
        ENDPIX(I)=DIMS(I)
      END DO
      DO I=NDIM+1,6
        STAPIX(I)=1
        ENDPIX(I)=1
      END DO
C
C     Map axes and get coordinate ranges.
C
      DO I=1,NDIM
        CALL DSA_MAP_AXIS_DATA
     &    (REF_NAME,I,'READ','FLOAT',ADDRESS,AXSLOT(I),STATUS)
        IF(STATUS.NE.0)GO TO 500
        AXELM(I)=DYN_ELEMENT(ADDRESS)
        MIN_TEMP=GEN_ELEMF(DYNAMIC_MEM(AXELM(I)),1)
        MAX_TEMP=GEN_ELEMF(DYNAMIC_MEM(AXELM(I)),DIMS(I))
	IF (MAX_TEMP .LT. MIN_TEMP) THEN
	  SMAX(I)=MIN_TEMP
	  SMIN(I)=MAX_TEMP
        ELSE
	  SMAX(I)=MAX_TEMP
	  SMIN(I)=MIN_TEMP
        END IF
        EMIN(I)=SMIN(I)
        EMAX(I)=SMAX(I)
      END DO
C
C     Set coordinate ranges for whole axes.
C
      DO I=1,NDIM
        START(I)=SMIN(I)
        END(I)=SMAX(I)
      END DO
C
C     See if whole axes are to be used -
C
      CALL PAR_RDKEY('WHOLE',.FALSE.,WHOLE)
C
C     - if not, get coordinates of subset.
C
      IF(.NOT.WHOLE)THEN
        CALL NDP_PAR_RDARY('START',SMIN,SMAX,'N',' ',NDIM,6,START)
        CALL NDP_PAR_RDARY('END',EMIN,EMAX,'N',' ',NDIM,6,END)
C
C     - find nearest pixels.
C
        DO I=1,NDIM
          STAPIX(I)=GEN_BSEARCH(DYNAMIC_MEM(AXELM(I)),DIMS(I),START(I))
          ENDPIX(I)=GEN_BSEARCH(DYNAMIC_MEM(AXELM(I)),DIMS(I),END(I))
        END DO
      END IF
C
  500 CONTINUE
      END

