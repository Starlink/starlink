      SUBROUTINE SETAXES
C+
C
C   -------------
C   S E T A X E S
C   -------------
C
C   Description:
C   ------------
C   Modifies one or more of the axis structures in an image structure.
C
C
C   Scope of program:
C   -----------------
C   - Images of up to six dimensions accepted.
C   - Data array types not relevant.
C   - Subsetting not relevant.
C   - Magic values not relevant.
C   - Quality, variance, and error arrays not relevant.
C   - Batch execution supported.
C
C
C   Environment:
C   ------------
C   FIGARO
C
C
C   Parameters (read or written):
C   -----------------------------
C   IMAGE   Name of the structure containing the axes to be modified.
C           (character)(prompted for).
C
C   OUTPUT  Name of the structure containing the output image. May be the
C           same as IMAGE. (character)(prompted for).
C
C   AXKEY   Key to axes to be modified. (integer, array)(prompted for).
C
C   AXSTART Start calibration value for each axis. (real, array)
C           (prompted for).
C
C   AXEND   End calibration value for each axis. (real, array)
C           (prompted for).
C
C   AXLOG   Logarithmic flag for each axis. (integer, array)(prompted for).
C
C
C   Keywords:
C   ---------
C   None.
C
C
C   Propagation of data structure:
C   ------------------------------
C   - All standard objects are copied from IMAGE.
C   - One of the axis structures is modified.
C
C
C   Method:
C   -------
C   - If OUTPUT is the same as IMAGE, the IMAGE structure is modified in
C     situ, i.e. a new version of the container file is not created.
C   - NDP_SET_AXES prompts for the axes to be modified, and fills each with
C     a linear or logarithmic scale between the required limits. A flag is
C     set in the axis structure if the scale is logarithmic.
C
C
C   External functions & subroutines called:
C   ----------------------------------------
C   Library DSA
C      DSA_CLOSE
C      DSA_INPUT
C      DSA_DATA_SIZE
C      DSA_MAP_AXIS_DATA
C      DSA_OPEN
C      DSA_OUTPUT
C
C   Library NDP
C      NDP_SET_AXES
C
C
C   Internal subroutines called:
C   ----------------------------
C   None.
C
C
C   INCLUDE statements:
C   -------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C
C
C   Extensions to FORTRAN 77:
C   -------------------------
C   IMPLICIT NONE / Names > 6 characters
C
C
C   Possible future upgrades:
C   -------------------------
C
C
C   Author/s:
C   ---------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History:
C   --------
C   01-FEB-1988   - Original program
C   04-DEC-1992   - Unix version (GOLDJIL)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions.
C
      INTEGER   DYN_ELEMENT
C
C     Local variables.
C
      INTEGER   ADDRESS
      INTEGER   AXSLOT
      INTEGER   AXPTR
      LOGICAL   BADPIX
      INTEGER   DIMS(10)
      INTEGER   NDIM
      INTEGER   NELM
      INTEGER   STATUS
      CHARACTER TYPE*8
C
C     Note NEW_FILE=0, i.e. a new data structure is NOT created.
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=0,NO_DATA=0)
C
      INCLUDE 'DYNAMIC_MEMORY'
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
C     Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Open file for OUTPUT.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Map first axis. Unnecessary, but without this the program will not link!
C
      CALL DSA_MAP_AXIS_DATA
     &  ('OUTPUT',1,'READ','FLOAT',ADDRESS,AXSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      AXPTR=DYN_ELEMENT(ADDRESS)
C
C     Calibrate axis array(s).
C
      CALL NDP_SET_AXES('OUTPUT',DIMS,NDIM,STATUS)
C
C     Tidy up and exit.
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
      END
