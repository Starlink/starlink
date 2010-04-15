      SUBROUTINE map2ts

*+
*  Name:
*     MAP2TS

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAP2TS

*  Description:
*     Could use DTA_CYVAR to copy axis structures with one call but this
*     would require knowledge of the actual name of the structures that
*     hold the information. At the time of writing this is all about to
*     change, hence the more long-winded approach that is used.
*
*     In fact the present long-winded approach can be replaced by
*     using DSA_SAVE_AXIS and DSA_RESTORE_AXIS when Figaro 3.0 is released.
*
*     Captain's Log (supplemental) DSA_SAVE_AXIS doesn't seem to work
*     with NDF structures, so persevere with hands on approach for the
*     time being.
*
*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1989 (JBVAD::PAH):
*        Original version.
*     21-MAY-1991 (REVAD::JFL):
*        Completely re-written to make it look nicer, the original
*        worked better in some ways.
*     22-MAR-1994 (REVAD::HME):
*        Change include statements. No longer include GSD include files.
*     13-JUN-1994 (REVAD::HME):
*        Disuse DSA_WRUSER.
*     12-OCT-1994 (hme@roe):
*        Use JCMT_QDISORT as translator to GEN_QFISORT.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

*  Dynamic memory include file - defines DYNAMIC_MEM

      INCLUDE 'DYNAMIC_MEMORY'

*  GSD variables

      INCLUDE 'SAE_PAR'

*  general astronomical parameters
      INCLUDE 'ASTRO_PAR'

*  Local Constants:
      INTEGER MAXDIM
      PARAMETER (MAXDIM=10)

*  Functions
      LOGICAL DSA_SAME_DATA
      INTEGER DYN_ELEMENT, ICH_LEN, DSA_TYPESIZE

*  Local variables
      LOGICAL FAULT              ! T to flag a Figaro fault
      LOGICAL ERRORS             ! T if input file has error array
      LOGICAL STRUC              ! T if DTA item is a structure
      INTEGER STATUS             ! main status
      INTEGER DSTAT              ! DTA status
      INTEGER ADDRESS            ! DSA address
      INTEGER SLOT               ! DSA slot
      INTEGER NDIM               ! number of dimensions to input data array
      INTEGER DIMS(MAXDIM)       ! dimensions of input data array
      INTEGER NELM               ! size of data array
      INTEGER DATPTR             ! pointer to input data
      INTEGER ERRPTR             !    "        "    errors
      INTEGER IGNORE
      INTEGER XAXPTR             !    "        "    x-axis
      INTEGER YAXPTR             !    "        "    y-axis
      INTEGER LSTPTR             !    "        "    LST
      INTEGER OUTDPTR            ! pointer to output data
      INTEGER OUTEPTR            !    "        "    errors
      INTEGER OUTAPTR            !    "        "    axis
      INTEGER IND_PTR            !    "        "    index array
      INTEGER TS_XAXPTR          ! pointer to x-axis array in TSDAT structure
      INTEGER TS_YAXPTR          !    "       y-axis        "         "
      INTEGER FW_PTR             ! pointer to floating point work array
      INTEGER DW_PTR             !    "       double precision   "
      INTEGER FSIZE              ! no. bytes in floating point variable
      INTEGER DSIZE              !    "        " double precision
      REAL FBAD                  ! bad pixel value for real numbers
      CHARACTER*32 X_INFO (2)    ! input x-axis info
      CHARACTER*32 Y_INFO (2)    !    "  y-axis   "
      CHARACTER*128 DTA_NAME     ! DTA item name
      CHARACTER*128 TSDAT_DTA_NAME ! DTA name for TSDAT structure in JCMT
                                  ! structure
      CHARACTER*128 TS_XUNITS    ! DTA name for TSDAT copy of x-axis units
      CHARACTER*128 TS_XLABEL    ! DTA name for TSDAT copy of x-axis label
      CHARACTER*128 TS_YUNITS    ! DTA name for TSDAT copy of y-axis units
      CHARACTER*128 TS_YLABEL    ! DTA name for TSDAT copy of y-axis label
      CHARACTER*80 ERROR         ! DTA error translation
      CHARACTER*80 OUTFILE       ! output filename

*.

      FAULT = .FALSE.

*  Initialise DSA system

      STATUS = 0
      CALL DSA_OPEN (STATUS)

*  open the input file and map the data array

      CALL DSA_INPUT ('INPUT', 'INPUT', STATUS)

*  search for a TSDAT structure in the JCMT-specific structure of the
*  input file which would signify that the file is already in time
*  sequence format

      CALL DTA_CRNAM ('INPUT', 'MORE.JCMT.TSDAT', 0, 0, TSDAT_DTA_NAME,
     :   DSTAT)
      CALL DTA_STRUC (TSDAT_DTA_NAME, STRUC, DSTAT)
      IF (DSTAT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('File is already in time-sequence format',
     :         IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF
      ELSE
         DSTAT = 0
      ENDIF

*  tell DSA using magic values, get the flag value for real data

      CALL DSA_USE_FLAGGED_VALUES ('INPUT', STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT', FBAD, STATUS)

*  map input data array, and error array if it exists

      CALL DSA_DATA_SIZE ('INPUT', 2, NDIM, DIMS, NELM, STATUS)
      IF (NDIM .EQ. 1) THEN
         DIMS(2) = 1
      END IF
      CALL DSA_MAP_DATA ('INPUT', 'READ', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      DATPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_SEEK_ERRORS ('INPUT', ERRORS, STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('INPUT', 'READ', 'FLOAT', ADDRESS,
     :      SLOT, STATUS)
         ERRPTR = DYN_ELEMENT(ADDRESS)
      ENDIF

*  map axis arrays

      CALL DSA_MAP_AXIS_DATA ('INPUT', 1, 'READ', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      XAXPTR = DYN_ELEMENT(ADDRESS)
      IF (NDIM .GT. 1) THEN
         CALL DSA_MAP_AXIS_DATA ('INPUT', 2, 'READ', 'FLOAT', ADDRESS,
     :      SLOT, STATUS)
         YAXPTR = DYN_ELEMENT(ADDRESS)
      END IF

*  read the axis information

      CALL DSA_GET_AXIS_INFO ('INPUT', 1, 2, X_INFO, 0, 0, STATUS)
      IF (NDIM .GT. 1) THEN
         CALL DSA_GET_AXIS_INFO ('INPUT', 2, 2, Y_INFO, 0, 0, STATUS)
      END IF

*  check the LST values are present and map them

      CALL DTA_CRNAM ('INPUT', 'MORE.JCMT.LST.DATA_ARRAY', 0, 0,
     :   DTA_NAME, DSTAT)
      NELM = DIMS(1)*DIMS(2)
      CALL DTA_MRVARD (DTA_NAME, NELM, ADDRESS, DSTAT)
      LSTPTR = DYN_ELEMENT(ADDRESS)
      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('JCMT structure does not contain LST '//
     :         'information.',IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF
      ENDIF

*  OK, input file seems to have all we need, open the output file,
*  insist on creation of new version even if file by that name exists
*  already but don't copy data or axis arrays

      CALL DSA_OUTPUT ('OUTPUT', 'OUTPUT', 'INPUT', 1, 1, STATUS)

*  create a TSDAT structure in the output JCMT structure to hold the original
*  axis arrays and an array that will hold the index of the time
*  sorted data in the original map.

      CALL DTA_CRNAM ('OUTPUT', 'MORE.JCMT.TSDAT', 0, 0, TSDAT_DTA_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (TSDAT_DTA_NAME, 'struc', DSTAT)

*  create and map arrays in TSDAT to hold map axes, and time-spectrum -> map
*  index use any old structure names as they will never be accessed by general
*  applications (using old Figaro format names actually)

*  x-axis

      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'struc', DSTAT)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.DATA', 1, DIMS(1), DTA_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.DATA', 0, 0, DTA_NAME, DSTAT)
      NELM = DIMS(1)
      CALL DTA_MUVARF (DTA_NAME, NELM, ADDRESS, DSTAT)
      TS_XAXPTR = DYN_ELEMENT (ADDRESS)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.UNITS', 1, 32, DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.UNITS', 0, 0, TS_XUNITS, DSTAT)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.LABEL', 1, 32, DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.LABEL', 0, 0, TS_XLABEL, DSTAT)

*  write x-axis units and label

      CALL DTA_WRVARC (TS_XUNITS, ICH_LEN(X_INFO(1)), X_INFO(1), DSTAT)
      CALL DTA_WRVARC (TS_XLABEL, ICH_LEN(X_INFO(2)), X_INFO(2), DSTAT)

      IF (NDIM .GT. 1) THEN

*  y-axis

         CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y', 0, 0, DTA_NAME, DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'struc', DSTAT)
         CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.DATA', 1, DIMS(2), DTA_NAME,
     :      DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'FLOAT', DSTAT)
         CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.DATA', 0, 0, DTA_NAME,
     :      DSTAT)
         NELM = DIMS(2)
         CALL DTA_MUVARF (DTA_NAME, NELM, ADDRESS, DSTAT)
         TS_YAXPTR = DYN_ELEMENT (ADDRESS)
         CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.UNITS', 1, 32, DTA_NAME,
     :      DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.UNITS', 0, 0, TS_YUNITS,
     :      DSTAT)
         CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.LABEL', 1, 32, DTA_NAME,
     :      DSTAT)
         CALL DTA_CRVAR (DTA_NAME, 'CHAR', DSTAT)
         CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.LABEL', 0, 0, TS_YLABEL,
     :      DSTAT)

*  write y-axis units and label

         CALL DTA_WRVARC (TS_YUNITS, ICH_LEN(Y_INFO(1)), Y_INFO(1),
     :      DSTAT)
         CALL DTA_WRVARC (TS_YLABEL, ICH_LEN(Y_INFO(2)), Y_INFO(2),
     :      DSTAT)

      END IF

*  index array

      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Z', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'struc', DSTAT)
      NELM = DIMS(1) * DIMS(2)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Z.DATA', 1, NELM, DTA_NAME,
     :   DSTAT)
      CALL DTA_CRVAR (DTA_NAME, 'INT', DSTAT)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Z.DATA', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_MUVARI (DTA_NAME, NELM, ADDRESS, DSTAT)
      IND_PTR = DYN_ELEMENT(ADDRESS)

*  check DSTAT

      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('Failed to create elements in output '//
     :         'TSDAT structure.',IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF

*  use flagged values in output structure

      CALL DSA_USE_FLAGGED_VALUES ('OUTPUT', STATUS)

*  create and map output data, error and axis arrays

      NELM = DIMS(1) * DIMS(2)
      CALL DSA_RESHAPE_DATA ('OUTPUT', 'INPUT', 1, NELM, STATUS)
      CALL DSA_RESHAPE_AXIS ('OUTPUT', 1, 'INPUT', 1, 1, NELM, STATUS)
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      OUTDPTR = DYN_ELEMENT(ADDRESS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :      SLOT, STATUS)
         OUTEPTR = DYN_ELEMENT(ADDRESS)
      ENDIF
      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'DOUBLE',
     :   ADDRESS, SLOT, STATUS)
      OUTAPTR = DYN_ELEMENT(ADDRESS)

*  set flagged value flag after creation of data arrays

      CALL DSA_SET_FLAGGED_VALUES ('OUTPUT', .TRUE., STATUS)

*  re-title output x-axis

      X_INFO(1) = 'radians'
      X_INFO(2) = 'Local Sidereal Time'
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 1, 2, X_INFO, 0, 0, STATUS)

*  get size in bytes of various data types used

      FSIZE = DSA_TYPESIZE ('FLOAT', STATUS)
      DSIZE = DSA_TYPESIZE ('DOUBLE', STATUS)

*  get some workspace for the GEN_?VSORT arrays

      NELM = DIMS(1)*DIMS(2)
      CALL DSA_GET_WORKSPACE (FSIZE*NELM, ADDRESS, SLOT, STATUS)
      FW_PTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORKSPACE (DSIZE*NELM, ADDRESS, SLOT, STATUS)
      DW_PTR = DYN_ELEMENT(ADDRESS)

      IF (STATUS .EQ. SAI__OK) THEN

*  create a sort vector using the LST array

         CALL JCMT_QDISORT (DYNAMIC_MEM(LSTPTR), NELM,
     :      DYNAMIC_MEM(IND_PTR))

*  copy the input data into the output data and sort in LST order

         CALL GEN_MOVE (FSIZE*NELM, DYNAMIC_MEM(DATPTR),
     :      DYNAMIC_MEM(OUTDPTR))
         CALL GEN_FVSORT (DYNAMIC_MEM(IND_PTR), NELM, 1,
     :      DYNAMIC_MEM(FW_PTR), DYNAMIC_MEM(OUTDPTR))

*  same for errors, and LST->axis

         IF (ERRORS) THEN
            CALL GEN_MOVE (FSIZE*NELM, DYNAMIC_MEM(ERRPTR),
     :         DYNAMIC_MEM(OUTEPTR))
            CALL GEN_FVSORT (DYNAMIC_MEM(IND_PTR), NELM, 1,
     :         DYNAMIC_MEM(FW_PTR), DYNAMIC_MEM(OUTEPTR))
         ENDIF
         CALL GEN_MOVE (DSIZE*NELM, DYNAMIC_MEM(LSTPTR),
     :      DYNAMIC_MEM(OUTAPTR))
         CALL GEN_DVSORT (DYNAMIC_MEM(IND_PTR), NELM, 1,
     :      DYNAMIC_MEM(DW_PTR), DYNAMIC_MEM(OUTAPTR))

*  copy original axis data into TSDAT

         CALL GEN_MOVE (FSIZE*DIMS(1), DYNAMIC_MEM(XAXPTR),
     :      DYNAMIC_MEM(TS_XAXPTR))
         IF (NDIM .GT. 1) THEN
            CALL GEN_MOVE (FSIZE*DIMS(2), DYNAMIC_MEM(YAXPTR),
     :         DYNAMIC_MEM(TS_YAXPTR))
         END IF

      END IF

*  tidy up

 500  IF (DSTAT .NE. 0) THEN
         CALL DTA_ERROR (DSTAT, ERROR)
         CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)),IGNORE)
         FAULT = .TRUE.
      ENDIF
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END
