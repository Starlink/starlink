      SUBROUTINE ts2map
*+
*  Name:
*     TS2MAP

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TS2MAP

*  Description:
*     Could use DTA_CYVAR to copy axis structures with one call but this
*     would require knowledge of the actual name of the structures that
*     hold the information. At the time of writing this is all about to
*     change, hence the more long-winded approach that is used.
*
*     The axis fiddling should all be replaced by DSA_SAVE_AXIS and
*     DSA_RESTORE_AXIS when these routines are fixed to work with NDFs
*     as well as Figaro format files.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1989 (JBVAD::PAH):
*        Original version.
*     22-MAR-1994 (REVAD::HME):
*        Change include statements.
*     13-JUN-1994 (REVAD::HME):
*        Disuse DSA_WRUSER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

*  Dynamic memory include file - defines DYNAMIC_MEM

      INCLUDE 'DYNAMIC_MEMORY'

*  SAE flags
      INCLUDE 'SAE_PAR'

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
      INTEGER NDIM               ! number of dimensions to original map data
      INTEGER DIMS(MAXDIM)       ! dimensions of original map
      INTEGER NELM               ! size of data array
      INTEGER DATPTR             ! pointer to input data
      INTEGER ERRPTR             !    "        "    errors
      INTEGER IGNORE
      INTEGER IND_PTR            !    "        "    index array
      INTEGER TS_XAXPTR          ! pointer to x-axis array in TSDAT structure
      INTEGER TS_YAXPTR          !    "       y-axis        "         "
      INTEGER OUTDPTR            ! pointer to output data
      INTEGER OUTEPTR            !    "        "    errors
      INTEGER XAXPTR             !    "        "    x-axis
      INTEGER YAXPTR             !    "        "    y-axis
      INTEGER FW_PTR             ! pointer to floating point work array
      INTEGER FSIZE              ! no. bytes in floating point variable
      CHARACTER*32 X_INFO (2)    ! output x-axis info
      CHARACTER*32 Y_INFO (2)    !    "   y-axis   "
      CHARACTER*128 DTA_NAME     ! DTA item name
      CHARACTER*128 TSDAT_DTA_NAME ! DTA name for TSDAT structure in JCMT
                                 ! structure
      CHARACTER*80 ERROR         ! DTA error translation
      CHARACTER*80 OUTFILE       ! output file name

*.

      FAULT = .FALSE.

*  Initialise DSA system

      STATUS = 0
      CALL DSA_OPEN (STATUS)

*  open the input file

      CALL DSA_INPUT ('INPUT', 'INPUT', STATUS)

*  search for a TSDAT structure in the JCMT-specific structure of the
*  input file which signifies that the file is in time sequence format

      DSTAT = 0
      CALL DTA_CRNAM ('INPUT', 'MORE.JCMT.TSDAT', 0, 0, TSDAT_DTA_NAME,
     :   DSTAT)
      CALL DTA_STRUC (TSDAT_DTA_NAME, STRUC, DSTAT)
      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('File is not in time-sequence format',
     :         IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF

*  tell DSA using flagged values on input file

      CALL DSA_USE_FLAGGED_VALUES ('INPUT', STATUS)

*  map input data array, and error array if it exists

      CALL DSA_DATA_SIZE ('INPUT', 1, NDIM, DIMS, NELM, STATUS)
      CALL DSA_MAP_DATA ('INPUT', 'READ', 'FLOAT', ADDRESS, SLOT,
     :   STATUS)
      DATPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_SEEK_ERRORS ('INPUT', ERRORS, STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('INPUT', 'READ', 'FLOAT', ADDRESS,
     :      SLOT, STATUS)
         ERRPTR = DYN_ELEMENT(ADDRESS)
      ENDIF

*  map arrays in TSDAT that hold original map axes, and time-spectrum -> map
*  index, read in original axis labels and units. Axes and index are stored
*  in old Figaro format as no one will ever want to access them with
*  applications other than this one.

*  original x-axis

      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.DATA', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_SZVAR (DTA_NAME, 1, NDIM, DIMS(1), DSTAT)
      NELM = DIMS(1)
      CALL DTA_MUVARF (DTA_NAME, NELM, ADDRESS, DSTAT)
      TS_XAXPTR = DYN_ELEMENT (ADDRESS)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.UNITS', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_RDVARC (DTA_NAME, 32, X_INFO(1), DSTAT)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'X.LABEL', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_RDVARC (DTA_NAME, 32, X_INFO(2), DSTAT)

*  original y-axis

      IF ((DSTAT .EQ. 0) .AND. (STATUS .EQ. SAI__OK)) THEN

         CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.DATA', 0, 0, DTA_NAME,
     :      DSTAT)
         CALL DTA_SZVAR (DTA_NAME, 1, NDIM, DIMS(2), DSTAT)
         IF (DSTAT .NE. 0) THEN

*  the original data was 1 dimensional

            NDIM = 1
            DIMS(2) = 1
            DSTAT = 0
         ELSE
            NDIM = 2
            NELM = DIMS(2)
            CALL DTA_MUVARF (DTA_NAME, NELM, ADDRESS, DSTAT)
            TS_YAXPTR = DYN_ELEMENT (ADDRESS)
            CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.UNITS', 0, 0, DTA_NAME,
     :         DSTAT)
            CALL DTA_RDVARC (DTA_NAME, 32, Y_INFO(1), DSTAT)
            CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Y.LABEL', 0, 0, DTA_NAME,
     :         DSTAT)
            CALL DTA_RDVARC (DTA_NAME, 32, Y_INFO(2), DSTAT)
         END IF

      END IF

*  index array

      NELM = DIMS(1) * DIMS(2)
      CALL DTA_CRNAM (TSDAT_DTA_NAME, 'Z.DATA', 0, 0, DTA_NAME, DSTAT)
      CALL DTA_MRVARI (DTA_NAME, NELM, ADDRESS, DSTAT)
      IND_PTR = DYN_ELEMENT(ADDRESS)

*  check DSTAT

      IF (DSTAT .NE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER ('Failed to read required elements from '//
     :         'input TSDAT structure.',IGNORE)
            FAULT = .TRUE.
            GOTO 500
         END IF
      END IF


*  OK, input file seems to have all we need, open the output file
*  but don't copy data or axis arrays

      CALL DSA_OUTPUT ('OUTPUT', 'OUTPUT', 'INPUT', 1, 1, STATUS)

*  use flagged values in output structure

      CALL DSA_USE_FLAGGED_VALUES ('OUTPUT', STATUS)

*  create and map output data, error and axis arrays

      CALL DSA_RESHAPE_DATA ('OUTPUT', 'INPUT', NDIM, DIMS, STATUS)
      CALL DSA_COERCE_AXIS_DATA ('OUTPUT', 1, 'FLOAT', 1, DIMS(1),
     :   STATUS)
      CALL DSA_COERCE_AXIS_DATA ('OUTPUT', 2, 'FLOAT', 1, DIMS(2),
     :   STATUS)
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      OUTDPTR = DYN_ELEMENT(ADDRESS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :      SLOT, STATUS)
         OUTEPTR = DYN_ELEMENT(ADDRESS)
      ENDIF
      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      XAXPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 2, 'WRITE', 'FLOAT', ADDRESS,
     :   SLOT, STATUS)
      YAXPTR = DYN_ELEMENT(ADDRESS)

*  can only set flagged values flag after creating output arrays

      CALL DSA_SET_FLAGGED_VALUES ('OUTPUT', .TRUE., STATUS)

*  re-title output axes

      CALL DSA_SET_AXIS_INFO ('OUTPUT', 1, 2, X_INFO, 0, 0, STATUS)
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 2, 2, Y_INFO, 0, 0, STATUS)

*  get size in bytes of various data types used

      FSIZE = DSA_TYPESIZE ('FLOAT', STATUS)

*  get some workspace for the unsorting

      NELM = DIMS(1)*DIMS(2)
      CALL DSA_GET_WORKSPACE (FSIZE*NELM, ADDRESS, SLOT, STATUS)
      FW_PTR = DYN_ELEMENT(ADDRESS)

      IF (STATUS .EQ. SAI__OK) THEN

*  copy the input data into the workspace, then unsort into the output
*  data.

         CALL GEN_MOVE (FSIZE*NELM, DYNAMIC_MEM(DATPTR),
     :      DYNAMIC_MEM(FW_PTR))
         CALL JCMT_FUNSORT (DYNAMIC_MEM(IND_PTR), NELM,
     :      DYNAMIC_MEM(FW_PTR), DYNAMIC_MEM(OUTDPTR))

*  same for errors

         IF (ERRORS) THEN
            CALL GEN_MOVE (FSIZE*NELM, DYNAMIC_MEM(ERRPTR),
     :         DYNAMIC_MEM(FW_PTR))
            CALL JCMT_FUNSORT (DYNAMIC_MEM(IND_PTR), NELM,
     :         DYNAMIC_MEM(FW_PTR), DYNAMIC_MEM(OUTEPTR))
         ENDIF

*  copy original axis data from TSDAT

         CALL GEN_MOVE (FSIZE*DIMS(1), DYNAMIC_MEM(TS_XAXPTR),
     :      DYNAMIC_MEM(XAXPTR))
         IF (NDIM .GT. 1) THEN
            CALL GEN_MOVE (FSIZE*DIMS(2), DYNAMIC_MEM(TS_YAXPTR),
     :         DYNAMIC_MEM(YAXPTR))
         END IF

      END IF

*  delete the TSDAT structure in the output JCMT structure

      CALL DTA_CRNAM ('OUTPUT', 'MORE.JCMT.TSDAT', 0, 0, TSDAT_DTA_NAME,
     :   DSTAT)
      CALL DTA_DLVAR (TSDAT_DTA_NAME, DSTAT)


*  tidy up

 500  IF (DSTAT .NE. 0) THEN
         CALL DTA_ERROR (DSTAT, ERROR)
         CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)),IGNORE)
         FAULT = .TRUE.
      ENDIF
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END
