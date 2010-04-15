      SUBROUTINE NDP_SET_AXES(REF_NAME,DIMS,NDIM,STATUS)
C+
C
C   -----------------------
C   N D P _ S E T _ A X E S
C   -----------------------
C
C   Description
C   -----------
C   Assigns data values to one or more of the axis arrays in an image
C   structure. An array parameter consisting of a flag for each axis is
C   prompted for, where 1 = modify axis and 0 = leave axis alone. Start
C   and end values for the axes to be modified are then prompted for. Flags
C   for the type of scale are obtained, where 0 = linear and 1 = logarithmic.
C   The required scale is then computed over the whole range of each axis.
C   The logarithmic flag in the axis structure is set or unset as appropriate.
C
C
C   Parameters
C   ----------
C   REF_NAME  (> character). Reference name associated with the structure.
C   DIMS      (> integer). Dimensions of the image data array.
C   NDIM      (> integer). Number of dimensions in the image.
C   STATUS    (! integer). Status code.
C
C
C   Prompts
C   -------
C   AXKEY  Keys for axes to be modified. 1 = modify, 0 = leave alone.
C          (integer array).
C   START  Start values. If an axis is to be left alone, its corresponding
C          start value is ignored. (real array).
C   END    Endvalues. If an axis is to be left alone, its corresponding end
C          value is ignored. (real array).
C   AXLOG  Keys for axes to be calibrated logarithmically. 1 = logarithmic,
C          0 = linear. (integer array).
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_FIND_REF
C     DSA_MAP_AXIS_DATA
C     DSA_SET_AXIS_INFO
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library FIG:
C     FIG_WFILL
C
C   Library ICH:
C     ICH_LEN
C
C   Library NDP:
C     NDP_PAR_RDARY:
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYN_SOURCE:DYNAMIC_MEMORY.INC'
C   INCLUDE 'NDP_SOURCE:NDP_NUMERIC_RANGES.INC'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
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
C   12-MAR-1992   - Replaced the DTA stuff with DSA_SET_AXIS_INFO call
C   06-OCT-1994   - Removed unused variables. (GJP)
CC
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C  Parameters.
C
      CHARACTER*(*) REF_NAME
      INTEGER       DIMS(10)
      INTEGER       NDIM
      INTEGER       STATUS
C
C  Functions.
C
      INTEGER DYN_ELEMENT
C
C  Local variables.
C
      INTEGER       ADDRESS
      CHARACTER     AXCHARS(2)*32
      REAL          AXEND(6)
      INTEGER       AXKEY(6)
      INTEGER       AXLOG(6)
      CHARACTER     AXNAMES(6)*1
      INTEGER       AXPTR(6)
      INTEGER       AXSLOT(6)
      REAL          AXSTART(6)
      REAL*8        DLOG(1)
      REAL          DUMARR(6)
      INTEGER       I
      LOGICAL       LOG
      REAL          VMAX(6)
      REAL          VMIN(6)
C
      DATA          AXNAMES/'X','Y','T','U','V','W'/
C
      INCLUDE  'DYNAMIC_MEMORY'
      INCLUDE  'NUMERIC_RANGES'
C
C   Return immediately if bad status passed.
C
      IF(STATUS.NE.0)RETURN
C
C   Get keys to axes to be calibrated.
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
C   Get start and end values.
C
      DO I=1,NDIM
        VMIN(I)=MIN_FLOAT
        VMAX(I)=MAX_FLOAT
      END DO
      CALL NDP_PAR_RDARY('AXSTART',VMIN,VMAX,'N',' ',NDIM,6,AXSTART)
      CALL NDP_PAR_RDARY('AXEND',VMIN,VMAX,'N',' ',NDIM,6,AXEND)
C
C  Get keys for logarithmic calibration.
C
      DO I=1,NDIM
        VMIN(I)=0.0
        VMAX(I)=1.0
      END DO
      CALL NDP_PAR_RDARY('AXLOG',VMIN,VMAX,'N',' ',NDIM,6,DUMARR)
      DO I=1,NDIM
        AXLOG(I)=INT(DUMARR(I))
      END DO
C
C  Calibrate axes.
C
      AXCHARS(1) = ' '
      AXCHARS(2) = ' '
      DO I=1,NDIM
        LOG=.FALSE.
C
C  - map axis array.
C
        CALL DSA_MAP_AXIS_DATA
     &    (REF_NAME,I,'WRITE','FLOAT',ADDRESS,AXSLOT(I),STATUS)
        IF(STATUS.NE.0)GO TO 500
        AXPTR(I)=DYN_ELEMENT(ADDRESS)
C
C  - fill axis array with a linear or logarithmic scale.
C
        IF(AXKEY(I).EQ.1)THEN
          IF(AXLOG(I).EQ.1) THEN
            LOG=.TRUE.
            DLOG(1) = 1.0D0
          ELSE
            DLOG(1) = 0.0D0
          END IF
          CALL FIG_WFILL
     &      (AXSTART(I),AXEND(I),LOG,DIMS(I),DYNAMIC_MEM(AXPTR(I)))
C
C  - create and/or set .LOG flag as required.
C
          CALL DSA_SET_AXIS_INFO(REF_NAME,I,0,AXCHARS,1,DLOG,STATUS)
          IF (STATUS .NE. 0) GO TO 500
        END IF
      END DO
C
  500 CONTINUE
      END
