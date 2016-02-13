      SUBROUTINE ECH_DEFINE_MODULES
*+
*  Name:
*     ECHOMOP - ECH_DEFINE_MODULES

*  Purpose:
*     Define module/parameter/object dependencies.

*  Description:
*     This routine declares the static data for a set of arrays, these arrays
*     control the details of all parameter and data object access in ECHOMOP.

*  Invocation:
*     CALL ECH_DEFINE_MODULES

*  Bugs:
*     None known.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     24-APR-1996 (MJC):
*       Update of prologue.
*     22-AUG-1996 (MJC):
*       Done away with the database file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_MODLUT.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_ECHOMOP.INC'
      INCLUDE 'ECH_FEATURE.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Local Variables:
      INTEGER I
      INTEGER J
      INTEGER M
      INTEGER CI
      INTEGER NI
      INTEGER OI

*  Functions called:
      INTEGER ECH_OBJ_IND
*.


*  Boolean values.
      BOOLEAN_DEFAULTS( 0 ) = .FALSE.
      BOOLEAN_DEFAULTS( 1 ) = .TRUE.

*  Zero-out arrays.
      DO I = 1, MAX_REQUIRED_OBJECTS
         REQUIRED_OBJECTS( I ) = ' '
         REQUIRED_TYPE( I ) = ' '
         OBJECT_SWITCH( I ) = 0
         IN_RDCTN_FILE( I ) = .FALSE.
         EXPECT_IN_DATAFILE( I ) = .FALSE.
         IS_WORKSPACE( I ) = .FALSE.
         FORCE_UPPERCASE( I ) = .FALSE.
         WS_DIMENSIONS( I ) = ' '
         WS_DIMEN_MULT( I ) = 0
         DEFAULTS_INDICES( I ) = 0
         DO J = 1, MAX_MODULES
            MODULE_PROPERTIES( I, J ) = 0
         END DO
      END DO

      DO I = 0, MAX_NUM_DEFAULTS
         VALUE_MINS( I ) = -1.0E20
         VALUE_DEFAULTS( I ) = 0.0
         VALUE_MAXS( I ) = 1.0E20
         UNITS_STRINGS( I ) = 'Undefined'
      END DO

      DO I = 0, MAX_MODULES
         ONCE_PER_ORDER( I ) = .FALSE.
      END DO

      DO I = 0, MAX_CHAR_DEFAULTS
         STRING_DEFAULTS( I ) = ' '
      END DO

*  Initialise default value counters to zero.
      NI = 0
      CI = 0

*  Define all required objects by name and type (data access routines are
*  assumed to translate into the requested type automatically )
      OI = 1

* 'A'
      REQ_OBJ_LUT( 1 ) = OI
      REQUIRED_OBJECTS( OI ) = 'ARC'
      REQUIRED_TYPE( OI ) = 'IMAGE'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ARC_TYPE'
      REQUIRED_TYPE( OI ) = 'CHAR'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ASCII_FILE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'echomop_output.txt'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'AUTO_ID'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'AVG_SPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

* 'B'
      OI = OI + 1
      REQ_OBJ_LUT( 2 ) = OI
      REQUIRED_OBJECTS( OI ) = 'BAD_COLS'
      REQUIRED_TYPE( OI ) = 'INT'
      EXPECT_IN_DATAFILE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = 'NXBAD'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'BAD_ORDER'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0
      VALUE_MAXS( NI ) = 1000
      VALUE_DEFAULTS( NI ) = 0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'BAD_ROWS'
      REQUIRED_TYPE( OI ) = 'INT'
      EXPECT_IN_DATAFILE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = 'NYBAD'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'BIN_SIZE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 10000000.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Wavelength Units'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'BLAZE_MEDIANS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'BLAZE_SPECT'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'BLZFIT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      FORCE_UPPERCASE( CI ) = .TRUE.
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'POLY'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'BLZ_INTERACT'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'BLZ_NPOLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 7.
      UNITS_STRINGS( NI ) = 'Coefficients'

* 'C'
      OI = OI + 1
      REQ_OBJ_LUT( 3 ) = OI
      REQUIRED_OBJECTS( OI ) = 'CALOBJ_MASK'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CALSKY_MASK'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CENTRAL_ONUM'
      REQUIRED_TYPE( OI ) = 'INT'
      EXPECT_IN_DATAFILE( OI ) = .TRUE.
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0
      VALUE_MAXS( NI ) = MAX_ALLOWED_ORDNUM
      VALUE_DEFAULTS( NI ) = 0
      UNITS_STRINGS( NI ) = 'Order number'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CENTRAL_WAVE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      EXPECT_IN_DATAFILE( OI ) = .TRUE.
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = ABS_MAX_WAVELENGTH
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = WAVELENGTH_UNITS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CONTIN_PROFILE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CONVOLVED'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NY]'
      WS_DIMEN_MULT( OI ) = 0

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CONVOLVED2'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NY]'
      WS_DIMEN_MULT( OI ) = 0

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CORREL_COUNT'
      REQUIRED_TYPE( OI ) = 'INT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NY]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CORRELATE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NY]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CRX_MEDIAN'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX,NY]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'CRY_MEDIAN'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX,NY]'

* 'D'
      OI = OI + 1
      REQ_OBJ_LUT( 4 ) = OI
      REQUIRED_OBJECTS( OI ) = 'DECOS_DATA'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DECOS_GAUSS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DECOS_INDEX_X'
      REQUIRED_TYPE( OI ) = 'SHORT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DECOS_INDEX_Y'
      REQUIRED_TYPE( OI ) = 'SHORT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DECOS_SDATA'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DECOS_XAXIS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DECOS_YAXIS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DEK_ABOVE'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DEK_BELOW'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DIAGONAL'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DIAGONAL2'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DISPLAY'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DSAMPLE_XCOORD'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'DSAMPLE_YCOORD'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

* 'E'
      OI = OI + 1
      REQ_OBJ_LUT( 5 ) = OI
      REQUIRED_OBJECTS( OI ) = 'ECH_ECHAR'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'ECH_ECHAR'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECH_FTRDB'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'ARCDIRS:THAR'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECH_RDCTN'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'ECH_RDCTN'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECH_RDUCD'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'ECH_RDUCD'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECHARC_ORDNUM'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECHARC_LABELX'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECHARC_LABELY'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECHARC_LABELZ'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECHARC_REFFTR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ECHARC_REFWAV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_DATABASE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_IND_SIZ'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_INTENSITY'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_LABELX'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_LABELZ'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_LEFT'
      REQUIRED_TYPE( OI ) = 'BYTE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_QIND_SIZ'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_QUICK_INDEX'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_QUICK_VALUE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_RIGHT'
      REQUIRED_TYPE( OI ) = 'BYTE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_SOURCE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_WAVE_INDEX'
      REQUIRED_TYPE( OI ) = 'SHORT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_WAVELENGTH'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_WSAMPLES'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EFTRDB_WSTATUS'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ERR_SPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ERR_STDSPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EXTR_ARC_VAR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EXTR_OBJ_VAR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EXTR_STAR_VAR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EXTRACT_MODE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'O'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EXTRACTED_ARC'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EXTRACTED_OBJ'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'EXTRACTED_STAR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

* 'F'
      OI = OI + 1
      REQ_OBJ_LUT( 6 ) = OI
      REQUIRED_OBJECTS( OI ) = 'F2D_DEVIATION'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'F2D_DX_COORD'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'F2D_DY_COORD'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'F2D_F_OF_XY'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'F2D_FITTED_F'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'F2D_LINECENT'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'F2D_WEIGHTS'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'F2D_WORK_NAG'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS * 10

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'F2D_XV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXRFLN]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FLAT_ERRORS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FFIELD'
      REQUIRED_TYPE( OI ) = 'IMAGE'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WAVES'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = 4

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WAVES_WORK'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = 4

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WAVES2'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = 4

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WORK_3XDOUBLE'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'
      WS_DIMEN_MULT( OI ) = 3

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WORK_XDOUBLE'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WORK_XDOUBLE2'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WORK_XINT'
      REQUIRED_TYPE( OI ) = 'INT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WORK_XREAL'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WORK_XREAL2'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WORK_XREAL3'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FIT_WORK_XREAL4'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FITTED_FLAT'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FITTED_SSKY'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FITTED_PFL'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FITTED_SKY'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FITTED_WAVES'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FLTFIT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'MEAN'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FSKY_ERRORS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'FSSKY_ERRORS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

* 'G'
      REQ_OBJ_LUT( 7 ) = OI
* 'H'
      OI = OI + 1
      REQ_OBJ_LUT( 8 ) = OI
      REQUIRED_OBJECTS( OI ) = 'HARD'
      REQUIRED_TYPE( OI ) = 'CHAR'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'HARDCOPY'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'HI_WAVE'
      REQUIRED_TYPE( OI ) = 'FLOAT'

* 'I'
      OI = OI + 1
      REQ_OBJ_LUT( 9 ) = OI
      REQUIRED_OBJECTS( OI ) = 'ID_COUNT'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ID_LINES'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ID_STATUS'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ID_WAVES'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ID_WIDTHS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'IDX_NREF_FRAME'
      REQUIRED_TYPE( OI ) = 'INT'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'IDX_NUM_ORDERS'
      REQUIRED_TYPE( OI ) = 'INT'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'INPTIM'
      REQUIRED_TYPE( OI ) = 'IMAGE'

* 'J'
      REQ_OBJ_LUT( 10 ) = OI
* 'K'
      REQ_OBJ_LUT( 11 ) = OI
* 'L'
      OI = OI + 1
      REQ_OBJ_LUT( 12 ) = OI
      REQUIRED_OBJECTS( OI ) = 'LOC_BOX_AVG'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'LOC_BOX_SIZ'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 100.
      VALUE_DEFAULTS( NI ) = 10.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'LOW_WAVE'
      REQUIRED_TYPE( OI ) = 'FLOAT'

* 'M'
      OI = OI + 1
      REQ_OBJ_LUT( 13 ) = OI
      REQUIRED_OBJECTS( OI ) = 'MAX_DISPERSION'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.00001
      VALUE_MAXS( NI ) = 1000.
      VALUE_DEFAULTS( NI ) = 1.0
      UNITS_STRINGS( NI ) = 'Wavelength Units per pixel'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'MFILTER1'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NO_OF_BINS]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'MFILTER2'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NO_OF_BINS]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'MIN_DISPERSION'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.00001
      VALUE_MAXS( NI ) = 1000.
      VALUE_DEFAULTS( NI ) = .01
      UNITS_STRINGS( NI ) = 'Wavelength Units per pixel'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'MODEL_PROFILE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

* 'N'
      OI = OI + 1
      REQ_OBJ_LUT( 14 ) = OI
      REQUIRED_OBJECTS( OI ) = 'NO_OF_BINS'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NO_OF_ORDERS'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NREF_FRAME'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NUM_ORDERS'
      REQUIRED_TYPE( OI ) = 'INT'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NX'
      REQUIRED_TYPE( OI ) = 'INT'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NX_PIXELS'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NX_REBIN'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NXBAD'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 99999.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Columns'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NY'
      REQUIRED_TYPE( OI ) = 'INT'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NY_PIXELS'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'NYBAD'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 99999.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Rows'

* 'O'
      OI = OI + 1
      REQ_OBJ_LUT( 15 ) = OI
      REQUIRED_OBJECTS( OI ) = 'OBJ_ABOVE'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OBJ_BELOW'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OBJ_MASK'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OBJ_SKY_GAP'
      REQUIRED_TYPE( OI ) = 'INT'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OBJECT'
      REQUIRED_TYPE( OI ) = 'IMAGE'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OBJECT_PROFILE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OBJFIT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      FORCE_UPPERCASE( CI ) = .TRUE.
      STRING_DEFAULTS( CI ) = 'POLY'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OBS_INTEN'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OBS_LINES'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OPTEXT_MODE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'O'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ORDER_IDNUM'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ORDER_NUMBER'
      REQUIRED_TYPE( OI ) = 'INT'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ORDER_SLOPE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'ORDER_YPOS'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OUTFLAT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'echflat'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'OUTPUT_IMAGE'
      REQUIRED_TYPE( OI ) = 'OUTIMAGE'

* 'P'
      OI = OI + 1
      REQ_OBJ_LUT( 16 ) = OI
      REQUIRED_OBJECTS( OI ) = 'PFL_INTERACT'
      REQUIRED_TYPE( OI ) = 'LOGICAL'
      DEFAULTS_INDICES( OI ) = 1

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'PFL_MODE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'A'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'PHOTON_TO_ADU'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      EXPECT_IN_DATAFILE( OI ) = .TRUE.
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.000001
      VALUE_MAXS( NI ) = 1.0e6
      VALUE_DEFAULTS( NI ) = 1.0
      UNITS_STRINGS( NI ) = 'Electrons'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'PLY_ORDERINCS'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MAXPOLY,NUM_ORDERS]'
      WS_DIMEN_MULT( OI ) = MAX_SLICE_PIXELS

* 'Q'
      REQ_OBJ_LUT( 17 ) = OI

* 'R'
      OI = OI + 1
      REQ_OBJ_LUT( 18 ) = OI
      REQUIRED_OBJECTS( OI ) = 'RBNOBJ'
      REQUIRED_TYPE( OI ) = 'OUTIMAGE'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RDCTN_ARC'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RDCTN_FFIELD'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RDCTN_INPTIM'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RDCTN_OBJECT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RDCTN_SLITIM'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REBIN_ARC'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REBIN_EARC'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REBIN_EORDER'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REBIN_ORDER'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REBIN_QUALITY'
      REQUIRED_TYPE( OI ) = 'BYTE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REBIN_WORK'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NO_OF_BINS]'
      WS_DIMEN_MULT( OI ) = 2

*  Warning: there is an "unresolved problem" relating to the order
*  of objects in result files.  In a nutshell, the DATA_ARRAY component
*  of the output NDF must be created *before* anything else.  At the
*  moment the only way to get this to happen is to have the next few
*  items out-of-alphabetical-order before other result file members.
      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_ORDER'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_ORDERS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_SPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_SCRORDER'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_SCRORDERS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_DATAZ'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

*  RESULT_DATAZ is the last of the objects which are stored in the main
*  DATA_ARRAY of the output files.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_FITWAVE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_FITWAVES'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REF_CONTINUUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REF_LINES'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REF_SPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_DATAX'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_DATAY'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_ERROR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_ERRORS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_LABELX'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_LABELY'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_LABELZ'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_ONUM'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_ONUMS'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_SCRERR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_SCRERROR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_SCRWAVE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_SCRWAVES'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_SPECTERR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_WAVES'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RDCTN_STAR'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RDCTN_TRACIM'
      REQUIRED_TYPE( OI ) = 'CHAR'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'REF_LINE_FWHM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'READOUT_NOISE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      EXPECT_IN_DATAFILE( OI ) = .TRUE.
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 1.0e6
      VALUE_DEFAULTS( NI ) = 0.0
      UNITS_STRINGS( NI ) = 'Counts'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_FORMAT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'NDF'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_TYPE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'EXTOBJ'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'RESULT_WAVESCALE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

* 'S'
      OI = OI + 1
      REQ_OBJ_LUT( 19 ) = OI
      REQUIRED_OBJECTS( OI ) = 'SCRNCHD_ARC'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SCRNCHD_ARCV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SCRNCHD_OBJ'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SCRNCHD_OBJV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SCRNCHD_STAR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SCRNCHD_STRV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SCRNCHD_WAVES'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SCRUNCH_TYPE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'OBJ'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SET_WSCALE'
      REQUIRED_TYPE( OI ) = 'LOGICAL'
      DEFAULTS_INDICES( OI ) = 1

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SKY_MASK'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SKY_SPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SKY_VARIANCE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SKYFIT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'MEAN'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SLITIM'
      REQUIRED_TYPE( OI ) = 'IMAGE'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SMOOTH_DIAG'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SO_FAR'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SOFT'
      REQUIRED_TYPE( OI ) = 'CHAR'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SSKY_SPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'SSKY_VARIANCE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'STACK'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'ECHOMOP'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'STAR'
      REQUIRED_TYPE( OI ) = 'IMAGE'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'STAR_ABOVE'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'STAR_BELOW'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'STAR_PROFILE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'START_WAVE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 10000000.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Wavelength Units'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'STD_SPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.


* 'T'
      OI = OI + 1
      REQ_OBJ_LUT( 20 ) = OI
      REQUIRED_OBJECTS( OI ) = 'TRACE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRACE_MODE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'C'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRACE_PATH'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRACE_RE_FITS'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX,NUM_ORDERS]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRACE_WIDTH'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRACIM'
      REQUIRED_TYPE( OI ) = 'IMAGE'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRCFIT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      FORCE_UPPERCASE( CI ) = .TRUE.
      STRING_DEFAULTS( CI ) = 'POLY'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRC_CLIPPED'
      REQUIRED_TYPE( OI ) = 'INT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRC_IN_DEV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRC_INTERACT'
      REQUIRED_TYPE( OI ) = 'LOGICAL'
      DEFAULTS_INDICES( OI ) = 1

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRC_NPOLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 4.
      UNITS_STRINGS( NI ) = 'Coefficients'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRC_OUT_DEV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TRC_POLY'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_AAACODE'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 200.
      VALUE_DEFAULTS( NI ) = 1.
      UNITS_STRINGS( NI ) = 'Astr. Abstracts code'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_AIRTOVAC'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_ARCHIVE'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_AUTLOC'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_AUTOMATE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = ' '
      FORCE_UPPERCASE( CI ) = .FALSE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_BATCH'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_BLZRSET'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CLONE'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'NULL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CLPBY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 1000.
      VALUE_DEFAULTS( NI ) = 1.
      UNITS_STRINGS( NI ) = 'Fitted points'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CLPMXDEV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.0e-5
      VALUE_MAXS( NI ) = 10.
      VALUE_DEFAULTS( NI ) = 0.5
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CNSDEV'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.01
      VALUE_MAXS( NI ) = 10.0
      VALUE_DEFAULTS( NI ) = 0.5
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CRCLEAN'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CRINTER'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CRMAX'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0
      VALUE_MAXS( NI ) = 1000000
      VALUE_DEFAULTS( NI ) = 0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CRTRC'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CRXBOX'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1
      VALUE_MAXS( NI ) = 49
      VALUE_DEFAULTS( NI ) = 5
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_CRYBOX'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1
      VALUE_MAXS( NI ) = 49
      VALUE_DEFAULTS( NI ) = 5
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_DB_SCOPE'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 5
      VALUE_MAXS( NI ) = 30
      VALUE_DEFAULTS( NI ) = 10
      UNITS_STRINGS( NI ) = 'Neighbours'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_DIAGNOSE'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_DEKABV'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 100.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_DEKBLW'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 100.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_DEKTHR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.00001
      VALUE_MAXS( NI ) = 0.99999
      VALUE_DEFAULTS( NI ) = 0.8
      UNITS_STRINGS( NI ) = 'Fraction'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_DPRBTHR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.000001
      VALUE_MAXS( NI ) = 1.0
      VALUE_DEFAULTS( NI ) = 0.9
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_DSGMTHR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.01
      VALUE_MAXS( NI ) = 10.
      VALUE_DEFAULTS( NI ) = 2.5
      UNITS_STRINGS( NI ) = 'Sigma'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FCHECK'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFINTER'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFLMED'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFLSMP'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 3.
      VALUE_MAXS( NI ) = 1000.
      VALUE_DEFAULTS( NI ) = 10.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFNXREJ'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 100.0
      VALUE_DEFAULTS( NI ) = 0.0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFNXPLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 1.0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFNYREJ'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 10.0
      VALUE_DEFAULTS( NI ) = 0.0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFNYPLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 0.0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFSUBSMP'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FFTHRESH'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 100.0
      VALUE_DEFAULTS( NI ) = 10.0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FIBRES'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FINCPLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 3.
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_FLUX'
      REQUIRED_TYPE( OI ) = 'LOGICAL'
      DEFAULTS_INDICES( OI ) = 1

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_HELIO'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = -100.
      VALUE_MAXS( NI ) = 100.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Kilometres per sec'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_IDINMN'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 8
      VALUE_MAXS( NI ) = 100
      VALUE_DEFAULTS( NI ) = 8
      UNITS_STRINGS( NI ) = 'Features'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_IDINMX'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 8
      VALUE_MAXS( NI ) = 100
      VALUE_DEFAULTS( NI ) = 30
      UNITS_STRINGS( NI ) = 'Features'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_IDMDLT'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 3
      VALUE_MAXS( NI ) = 11
      VALUE_DEFAULTS( NI ) = 6
      UNITS_STRINGS( NI ) = 'Neighbours'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_IDMXDIF'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0000001
      VALUE_MAXS( NI ) = 1.0
      VALUE_DEFAULTS( NI ) = 0.03
      UNITS_STRINGS( NI ) = 'N-d origin distance'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_IDSDLT'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 2
      VALUE_MAXS( NI ) = 10
      VALUE_DEFAULTS( NI ) = 3
      UNITS_STRINGS( NI ) = 'Neighbours'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_IDSTRNG'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.000001
      VALUE_MAXS( NI ) = 1000000.
      VALUE_DEFAULTS( NI ) = 15.0
      UNITS_STRINGS( NI ) = 'Max/Obs ratio'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_INTR'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_IUE'
      REQUIRED_TYPE( OI ) = 'INT'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_LOG'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MAX2DPNTS'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 10.
      VALUE_MAXS( NI ) = 100000.
      VALUE_DEFAULTS( NI ) = 1000.
      UNITS_STRINGS( NI ) = 'Arc line coords'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MAX2DPLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 2.
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 30.
      UNITS_STRINGS( NI ) = 'Coefficients'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MAXLINES'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 10.
      VALUE_MAXS( NI ) = 1000.
      VALUE_DEFAULTS( NI ) = 100.
      UNITS_STRINGS( NI ) = 'Line slots'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MAXPOLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 16.
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 50.
      UNITS_STRINGS( NI ) = 'Coefficients'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MAXRFLN'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 50.
      VALUE_MAXS( NI ) = FLOAT( max_allowed_rf_feat )
      VALUE_DEFAULTS( NI ) = 200.
      UNITS_STRINGS( NI ) = 'Reference lines'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MAXWPLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 2.
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 10.
      UNITS_STRINGS( NI ) = 'Coefficients'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MERGE'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MINCR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 1.0e20
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Pixel intensity'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MRGMAXX'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 10000.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MRGMINX'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 10000.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MRGWGHT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'SN'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MXBADSMP'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 1000.
      VALUE_DEFAULTS( NI ) = 10.
      UNITS_STRINGS( NI ) = 'Max bad samples'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MXSKYPIX'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 3.0
      VALUE_MAXS( NI ) = FLOAT( max_slice_pixels )
      VALUE_DEFAULTS( NI ) = 21.0
      UNITS_STRINGS( NI ) = 'Sky pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_MXSMP'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 20.0
      VALUE_MAXS( NI ) = 1000.0
      VALUE_DEFAULTS( NI ) = 500.0
      UNITS_STRINGS( NI ) = 'X sampling boxes'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_NOARC'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_NOFLAT'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_OBJABV'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 100.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_OBJBLW'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 100.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_OBJPOLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 0.0
      UNITS_STRINGS( NI ) = 'Fit coeffs'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_OBJREJ'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 1000.0
      VALUE_DEFAULTS( NI ) = 5.0
      UNITS_STRINGS( NI ) = 'Reject cycles'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_OBJRTHR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.01
      VALUE_MAXS( NI ) = 100.0
      VALUE_DEFAULTS( NI ) = 5.0
      UNITS_STRINGS( NI ) = 'Significance'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_PAGE'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 255.
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Screen lines'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_PARTORD'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_PFLSSAMP'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 3.0
      VALUE_MAXS( NI ) = 5001.0
      VALUE_DEFAULTS( NI ) = 301.0
      UNITS_STRINGS( NI ) = 'Subsamples'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_PFSAMP'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 3.
      VALUE_MAXS( NI ) = 1001.
      VALUE_DEFAULTS( NI ) = 101.
      UNITS_STRINGS( NI ) = 'Subsamples'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_PREBAL'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_QUAD'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_QUICK'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_REPORT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = ' '
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_REVCHK'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_RFLNTHR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.0
      VALUE_MAXS( NI ) = 1000000.
      VALUE_DEFAULTS( NI ) = 1.25
      UNITS_STRINGS( NI ) = '1 + fraction'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SATRTN'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 1.0e20
      VALUE_DEFAULTS( NI ) = 1.0E20
      UNITS_STRINGS( NI ) = 'Pixel intensity'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SCFRACT'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = 1.
      VALUE_DEFAULTS( NI ) = 0.5
      UNITS_STRINGS( NI ) = 'Fractional Ratio'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SCRADD'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 1.0e6
      VALUE_DEFAULTS( NI ) = 1.0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SCRMODE'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 1.0
      VALUE_DEFAULTS( NI ) = 1.0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKEW'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 1.0e10
      VALUE_DEFAULTS( NI ) = 0.0
      UNITS_STRINGS( NI ) = ' '

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKVRCORR'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYHILIM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.00001
      VALUE_MAXS( NI ) = 0.99999
      VALUE_DEFAULTS( NI ) = 0.5
      UNITS_STRINGS( NI ) = 'Fraction'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYINTER'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYLINW'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 100.
      VALUE_DEFAULTS( NI ) = 5.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYLTHR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0001
      VALUE_MAXS( NI ) = 1000000.
      VALUE_DEFAULTS( NI ) = 3.
      UNITS_STRINGS( NI ) = 'Sigma'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYPOLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = -1.0
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 0.0
      UNITS_STRINGS( NI ) = 'Fit coeffs'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYREJ'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.0
      VALUE_MAXS( NI ) = 1000.0
      VALUE_DEFAULTS( NI ) = 5.0
      UNITS_STRINGS( NI ) = 'Reject cycles'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYRTHR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.01
      VALUE_MAXS( NI ) = 100.0
      VALUE_DEFAULTS( NI ) = 5.0
      UNITS_STRINGS( NI ) = 'Significance'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYSIM'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_SKYXPLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 0.
      UNITS_STRINGS( NI ) = 'Fit coeffs'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_TRCNS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.01
      VALUE_MAXS( NI ) = 0.99
      VALUE_DEFAULTS( NI ) = 0.1
      UNITS_STRINGS( NI ) = 'Fraction'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_TWTHR'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.01
      VALUE_MAXS( NI ) = 0.999
      VALUE_DEFAULTS( NI ) = 0.95
      UNITS_STRINGS( NI ) = 'Fraction'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_UHRF'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_USE_NXF'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 0.01
      VALUE_MAXS( NI ) = 2.0
      VALUE_DEFAULTS( NI ) = 0.2
      UNITS_STRINGS( NI ) = 'Fraction of X pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_USEAAA'
      REQUIRED_TYPE( OI ) = 'LOGICAL'
      DEFAULTS_INDICES( OI ) = 1

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_XBOX'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 10000.
      VALUE_DEFAULTS( NI ) = 20.
      UNITS_STRINGS( NI ) = 'Pixels'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_XZONE'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 8.
      VALUE_DEFAULTS( NI ) = 1.
      UNITS_STRINGS( NI ) = 'Horizontal subwindows'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_YBLAZE'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TUNE_YZONE'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 8.
      VALUE_DEFAULTS( NI ) = 1.
      UNITS_STRINGS( NI ) = 'Vertical subwindows'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'TWO_D_DLAMBDA'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      is_workspace( OI ) = .TRUE.
      ws_dimensions( OI ) = '[NX]'
      ws_dimen_mult( OI ) = max_slice_pixels

* 'U'
      OI = OI + 1
      REQ_OBJ_LUT( 21 ) = OI
      REQUIRED_OBJECTS( OI ) = 'USE_MEDIAN'
      REQUIRED_TYPE( OI ) = 'LOGICAL'

* 'V'
      OI = OI + 1
      REQ_OBJ_LUT( 22 ) = OI
      REQUIRED_OBJECTS( OI ) = 'VAR_MATRIX'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[TUNE_MXSKYPIX,TUNE_MXSKYPIX]'

* 'W'
      OI = OI + 1
      REQ_OBJ_LUT( 23 ) = OI
      REQUIRED_OBJECTS( OI ) = 'W_NPOLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      DEFAULTS_INDICES( OI ) = NI
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = FLOAT( MAX_FIT_COEFFS )
      VALUE_DEFAULTS( NI ) = 7.
      UNITS_STRINGS( NI ) = 'Coefficients'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'W_POLY'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'W_POLY_2D'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'W_POLY2'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'W2_NX_POLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 10.
      VALUE_DEFAULTS( NI ) = 3.
      UNITS_STRINGS( NI ) = 'Coefficients'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'W2_NY_POLY'
      REQUIRED_TYPE( OI ) = 'INT'
      NI = NI + 1
      VALUE_MINS( NI ) = 1.
      VALUE_MAXS( NI ) = 10.
      VALUE_DEFAULTS( NI ) = 3.
      UNITS_STRINGS( NI ) = 'Coefficients'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WAVELENGTH'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WAVFIT'
      REQUIRED_TYPE( OI ) = 'CHAR'
      CI = CI + 1
      DEFAULTS_INDICES( OI ) = CI
      STRING_DEFAULTS( CI ) = 'POLY'
      FORCE_UPPERCASE( CI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WFDB_DATABASE'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) =
     :      '[TUNE_DB_SCOPE,TUNE_DB_SCOPE,EFTRDB_IND_SIZ]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WFDB_LEFT'
      REQUIRED_TYPE( OI ) = 'BYTE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) =
     :      '[TUNE_DB_SCOPE,TUNE_DB_SCOPE,EFTRDB_IND_SIZ]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WFDB_RIGHT'
      REQUIRED_TYPE( OI ) = 'BYTE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) =
     :      '[TUNE_DB_SCOPE,TUNE_DB_SCOPE,EFTRDB_IND_SIZ]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WID_POLY'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WLEFT_OFFSET'
      REQUIRED_TYPE( OI ) = 'BYTE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[1]'
      WS_DIMEN_MULT( OI ) = MAX_META_INDEX * MAX_ID_FTRS * 2

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WNEXT_INDEX'
      REQUIRED_TYPE( OI ) = 'INT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[1]'
      WS_DIMEN_MULT( OI ) = MAX_META_INDEX * MAX_ID_FTRS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WORDER_NUMS'
      REQUIRED_TYPE( OI ) = 'INT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NUM_ORDERS]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WPFL_TCOUNT'
      REQUIRED_TYPE( OI ) = 'INT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX,NUM_ORDERS]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WPFL_TOTAL'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX,NUM_ORDERS]'

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WPREV_INDEX'
      REQUIRED_TYPE( OI ) = 'INT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[1]'
      WS_DIMEN_MULT( OI ) = MAX_META_INDEX * MAX_ID_FTRS

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WRATIOS'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[1]'
      WS_DIMEN_MULT( OI ) = MAX_META_INDEX * MAX_ID_FTRS * 4

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WRIGHT_OFFSET'
      REQUIRED_TYPE( OI ) = 'BYTE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[1]'
      WS_DIMEN_MULT( OI ) = MAX_META_INDEX * MAX_ID_FTRS * 2

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WSEAR_END'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WSEAR_START'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = 'WVSCALE_INDEX'
      REQUIRED_TYPE( OI ) = 'INT'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NUM_ORDERS]'
      WS_DIMEN_MULT( OI ) = 2

* 'X'
      OI = OI + 1
      REQ_OBJ_LUT( 24 ) = OI
      REQUIRED_OBJECTS( OI ) = 'X_TRACE_COORD'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

* 'Y'
      OI = OI + 1
      REQ_OBJ_LUT( 25 ) = OI
      REQUIRED_OBJECTS( OI ) = 'Y_TRACE_COORD'
      REQUIRED_TYPE( OI ) = 'DOUBLE'
      IS_WORKSPACE( OI ) = .TRUE.
      WS_DIMENSIONS( OI ) = '[NX]'

* 'Z'
      REQ_OBJ_LUT( 26 ) = OI

* '?'
      OI = OI + 1
      REQ_OBJ_LUT( 27 ) = OI
      REQUIRED_OBJECTS( OI ) = '1D_SPECTRUM'
      REQUIRED_TYPE( OI ) = 'FLOAT'
      IN_RDCTN_FILE( OI ) = .TRUE.

      OI = OI + 1
      REQUIRED_OBJECTS( OI ) = '2D_INTERACT'
      REQUIRED_TYPE( OI ) = 'LOGICAL'
      DEFAULTS_INDICES( OI ) = 1

*  Store index of last object defined.
      REQ_OBJ_LUT( 28 ) = OI

      M = 0

*  Set required objects for module ECH_COUNT_ORDERS
      M = M + 1
      MODULE_NAME( M ) = 'ECH_COUNT_ORDERS'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'USE_MEDIAN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE_WIDTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_XBOX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_AUTLOC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PARTORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ORDER_SLOPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CONVOLVED' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CONVOLVED2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CORRELATE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CORREL_COUNT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRACE_WIDTH' ), M )

*  Set required objects for module ECH_LOCATE_ORDERS
      M = M + 1
      MODULE_NAME( M ) = 'ECH_LOCATE_ORDERS'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE_WIDTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'USE_MEDIAN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_XBOX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_TWTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_AUTLOC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PARTORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ORDER_SLOPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ORDER_YPOS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CONVOLVED' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CONVOLVED2' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ORDER_YPOS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRACE_WIDTH' ), M )

*  Set required objects for module ECH_TRACE_ORDER
      M = M + 1
      MODULE_NAME( M ) = 'ECH_TRACE_ORDER'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ORDER_SLOPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ORDER_YPOS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE_WIDTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE_MODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'USE_MEDIAN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_XBOX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRCFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_NPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_IN_DEV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_IUE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXBADSMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE_PATH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRACE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRC_IN_DEV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRACE_PATH' ), M )

*  Set required objects for module ECH_FIT_ORDER_TRACE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_FIT_ORDER_TRACE'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRCFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_NPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_INTERACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_IN_DEV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_OUT_DEV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_CLIPPED' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CLPMXDEV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CLPBY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRC_IN_DEV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRC_OUT_DEV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRC_CLIPPED' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRC_POLY' ), M )

*  Set required objects for module ECH_GET_SLIT_EXTENT
      M = M + 1
      MODULE_NAME( M ) = 'ECH_GET_SLIT_EXTENT'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SLITIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'SLITIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PFL_INTERACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PFL_MODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_USE_NXF' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PFLSSAMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DEKTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DEKBLW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DEKABV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYHILIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CONTIN_PROFILE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'CONTIN_PROFILE' ), M )

*  Set required objects for module ECH_GET_OBJ_PROFILE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_GET_OBJ_PROFILE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJBLW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJABV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PFL_INTERACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PFL_MODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJBLW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJABV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_USE_NXF' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PFLSSAMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DEKTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYHILIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJECT_PROFILE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OBJECT_PROFILE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )

*  Set required objects for module ECH_GET_STAR_PROFILE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_GET_STAR_PROFILE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'STAR' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'STAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PFL_INTERACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PFL_MODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_USE_NXF' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PFLSSAMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DEKTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYHILIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'STAR_PROFILE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CALOBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CALSKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJBLW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJABV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'STAR_PROFILE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'CALOBJ_MASK' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'CALSKY_MASK' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'STAR_PROFILE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )

*  Set required objects for module ECH_SIMPLE_OPTEXT
      M = M + 1
      MODULE_NAME( M ) = 'ECH_SIMPLE_OPTEXT'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OPTEXT_MODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'STAR_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'STAR_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_STAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_STAR_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_STAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_STAR_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )

*  Set required objects for module ECH_PLOTTER
      M = M + 1
      MODULE_NAME( M ) = 'ECH_PLOTTER'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )

*  Set required objects for module ECH_GET_REF_FWHM
      M = M + 1
      MODULE_NAME( M ) = 'ECH_GET_REF_FWHM'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'AVG_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_LINE_FWHM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'REF_LINE_FWHM' ), M )

*  Set required objects for module ECH_LOCATE_REF_LINES
      M = M + 1
      MODULE_NAME( M ) = 'ECH_LOCATE_REF_LINES'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXRFLN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_RFLNTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_LINE_FWHM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBS_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBS_INTEN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'AVG_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_CONTINUUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OBS_LINES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OBS_INTEN' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )

*  Set required objects for module ECH_SCRUNCH_2D_ORDER
      M = M + 1
      MODULE_NAME( M ) = 'ECH_SCRUNCH_2D_ORDER'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W2_NX_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W2_NY_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY_2D' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TWO_D_DLAMBDA' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RBNOBJ' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'RBNOBJ' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'RBNOBJ' ), M )

*  Set required objects for module ECH_GEN_REBIN_SCALE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_GEN_REBIN_SCALE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BIN_SIZE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'START_WAVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SET_WSCALE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )

*  Set required objects for module ECH_SCRUNCH_ORDERS
      M = M + 1
      MODULE_NAME( M ) = 'ECH_SCRUNCH_ORDERS'
      CALL MNEEDS( ECH_OBJ_IND( 'SCRUNCH_TYPE' ), M )

*  Set required objects for module ECH_MODEL_SKY
      M = M + 1
      MODULE_NAME( M ) = 'ECH_MODEL_SKY'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOFLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYINTER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKYFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYRTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYSIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYXPLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYLINW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYLTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PHOTON_TO_ADU' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_SKY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FSKY_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_VARIANCE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SKY_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SKY_VARIANCE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_SKY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FSKY_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DSAMPLE_XCOORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DSAMPLE_YCOORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DIAGONAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DIAGONAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SMOOTH_DIAG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL3' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL4' ), M )

*  Set required objects for module ECH_FIT_2D_DISTORTION
      M = M + 1
      MODULE_NAME( M ) = 'ECH_FIT_2D_DISTORTION'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( '2D_INTERACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_LINE_FWHM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_COUNT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXRFLN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W2_NX_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W2_NY_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY_2D' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_DX_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_DY_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_F_OF_XY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_DEVIATION' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_FITTED_F' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_XV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_WEIGHTS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_LINECENT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'F2D_WORK_NAG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'W_POLY_2D' ), M )

*  Set required objects for module ECH_REBIN_ORDER
      M = M + 1
      MODULE_NAME( M ) = 'ECH_REBIN_ORDER'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W2_NX_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W2_NY_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY_2D' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TWO_D_DLAMBDA' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_ORDER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_QUALITY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_EORDER' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'REBIN_ORDER' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'REBIN_QUALITY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'REBIN_EORDER' ), M )

*  Set required objects for module ECH_CHECK_FRAME
      M = M + 1
      MODULE_NAME( M ) = 'ECH_CHECK_FRAME'
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BAD_ROWS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BAD_COLS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FCHECK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SATRTN' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )

*  Set required objects for module ECH_PLOT_TRACES
      M = M + 1
      MODULE_NAME( M ) = 'ECH_PLOT_TRACES'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )

*  Set required objects for module ECH_PLOT_ARC_WAVES
      M = M + 1
      MODULE_NAME( M ) = 'ECH_PLOT_ARC_WAVES'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_WAVES' ), M )

*  Set required objects for module ECH_TRACE_CONSISTENCY
      M = M + 1
      MODULE_NAME( M ) = 'ECH_TRACE_CONSISTENCY'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_NPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_INTERACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_TRCNS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CNSDEV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE_RE_FITS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XINT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_3XDOUBLE'), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRC_POLY' ), M )

*  Set required objects for module ECH_DETERMINE_SLOPE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_DETERMINE_SLOPE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'USE_MEDIAN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_XBOX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DSAMPLE_XCOORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DSAMPLE_YCOORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DIAGONAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DIAGONAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SMOOTH_DIAG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ORDER_SLOPE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ORDER_SLOPE' ), M )

*  Set required objects for module ECH_REBIN_ARCORDER
      M = M + 1
      MODULE_NAME( M ) = 'ECH_REBIN_ARCORDER'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W2_NX_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W2_NY_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY_2D' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TWO_D_DLAMBDA' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_EARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_QUALITY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'REBIN_ARC' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'REBIN_EARC' ), M )

*  Set required objects for module ECH_DUMMY
      M = M + 1
      MODULE_NAME( M ) = 'ECH_DUMMY'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CLONE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )

*  Set required objects for module ECH_MODEL_FLAT
      M = M + 1
      MODULE_NAME( M ) = 'ECH_MODEL_FLAT'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLTFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOFLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFINTER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFLMED' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFSUBSMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFLSMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFNXPLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFNYPLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFNXREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFNYREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FFTHRESH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PREBAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DSAMPLE_XCOORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DSAMPLE_YCOORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL3' ), M )

*  Set required objects for module ECH_WAVELENGTH_CALIB
      M = M + 1
      MODULE_NAME( M ) = 'ECH_WAVELENGTH_CALIB'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_NPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WAVFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'AUTO_ID' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'LOW_WAVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'HI_WAVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MIN_DISPERSION' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MAX_DISPERSION' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_IDMXDIF' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_IDMDLT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_IDSDLT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_IDSTRNG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_IDINMX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_IDINMN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_REVCHK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXRFLN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CENTRAL_WAVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CENTRAL_ONUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBS_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBS_INTEN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_COUNT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_STATUS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ORDER_IDNUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WSEAR_START' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WSEAR_END' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WAVES2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WAVES_WORK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WLEFT_OFFSET' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WRIGHT_OFFSET' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WPREV_INDEX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WNEXT_INDEX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WRATIOS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_FTRDB' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DB_SCOPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_IND_SIZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_QIND_SIZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_INTENSITY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_DATABASE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_LEFT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_RIGHT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_WAVE_INDEX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_QUICK_INDEX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_QUICK_VALUE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_LINE_FWHM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'REF_LINE_FWHM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OBS_LINES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OBS_INTEN' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_COUNT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_LINES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_STATUS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_WAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ORDER_IDNUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'WSEAR_START' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'WSEAR_END' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_WAVES' ), M )

*  Set required objects for module ECH_XCHNG_ECHARC
      M = M + 1
      MODULE_NAME( M ) = 'ECH_XCHNG_ECHARC'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_ECHAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECHARC_REFFTR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECHARC_REFWAV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECHARC_ORDNUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECHARC_LABELX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECHARC_LABELY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECHARC_LABELZ' ), M )

      CALL MUPDS( ECH_OBJ_IND( 'ECH_ECHAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECHARC_REFFTR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECHARC_REFWAV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECHARC_ORDNUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECHARC_LABELX' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECHARC_LABELY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECHARC_LABELZ' ), M )

*  Set required objects for module ECH_DECOSMIC_2
      M = M + 1
      MODULE_NAME( M ) = 'ECH_DECOSMIC_2'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MINCR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FINCPLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DSGMTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DPRBTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PLY_ORDERINCS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DECOS_DATA' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DECOS_SDATA' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DECOS_XAXIS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DECOS_YAXIS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DECOS_GAUSS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DECOS_INDEX_X' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DECOS_INDEX_Y' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL3' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL4' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XINT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'PLY_ORDERINCS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SKY_SPECTRUM' ), M )

*  Set required objects for module ECH_WAVE_CONSISTENCY
      M = M + 1
      MODULE_NAME( M ) = 'ECH_WAVE_CONSISTENCY'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_FTRDB' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_IND_SIZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_NPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'AUTO_ID' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXRFLN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBS_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBS_INTEN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_COUNT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_STATUS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ORDER_IDNUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WSEAR_START' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WSEAR_END' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACE_RE_FITS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_3XDOUBLE'), M )
      CALL MUPDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_COUNT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_LINES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_STATUS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_WAVES' ), M )

*  Set required objects for module ECH_IMAGE_TRACE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_IMAGE_TRACE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OUTPUT_IMAGE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OUTPUT_IMAGE' ), M )

*  Set required objects for module ECH_DECOSMIC_1
      M = M + 1
      MODULE_NAME( M ) = 'ECH_DECOSMIC_1'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CRXBOX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CRYBOX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MINCR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CRTRC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CRMAX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CRINTER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CRX_MEDIAN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'CRY_MEDIAN' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'TRACIM' ), M )

*  Set required objects for module ECH_EXT_OPT
      M = M + 1
      MODULE_NAME( M ) = 'ECH_EXT_OPT'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACT_MODE' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYRTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PFLSSAMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKVRCORR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CRCLEAN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PHOTON_TO_ADU' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_SKY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_PFL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FSKY_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'VAR_MATRIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MODEL_PROFILE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'MODEL_PROFILE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_SKY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FSKY_ERRORS' ), M )

*  Set required objects for module ECH_IMAGE_COSMIC
      M = M + 1
      MODULE_NAME( M ) = 'ECH_IMAGE_COSMIC'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OUTPUT_IMAGE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OUTPUT_IMAGE' ), M )

*  Set required objects for module ECH_SCRUNCH_OBJ
      M = M + 1
      MODULE_NAME( M ) = 'ECH_SCRUNCH_OBJ'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( '1D_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ERR_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRUNCH_TYPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCFRACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MERGE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKEW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_LOG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_YBLAZE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FLUX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_INTR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SET_WSCALE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_QUAD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCRMODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCRADD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_OBJV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_WORK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WVSCALE_INDEX' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_OBJ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_OBJV' ), M )
      CALL MUPDS( ECH_OBJ_IND( '1D_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ERR_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MFILTER1' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MFILTER2' ), M )

*  Set required objects for module ECH_SCRUNCH_ARC
      M = M + 1
      MODULE_NAME( M ) = 'ECH_SCRUNCH_ARC'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRUNCH_TYPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCFRACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKEW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MERGE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_LOG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_YBLAZE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FLUX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_INTR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SET_WSCALE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_QUAD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCRMODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCRADD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_WORK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_ARCV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WVSCALE_INDEX' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_ARC' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_ARCV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MFILTER1' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MFILTER2' ), M )

*  Set required objects for module ECH_SCRUNCH_STAR
      M = M + 1
      MODULE_NAME( M ) = 'ECH_SCRUNCH_STAR'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'STD_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ERR_STDSPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRUNCH_TYPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCFRACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MERGE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKEW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_LOG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_YBLAZE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FLUX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_INTR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SET_WSCALE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_QUAD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCRMODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCRADD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_STAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_STAR_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_STAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_STRV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_WORK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WVSCALE_INDEX' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_STAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_STRV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'STD_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ERR_STDSPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MFILTER1' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MFILTER2' ), M )

*  Set required objects for module ECH_FIT_ORDER_BLAZE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_FIT_ORDER_BLAZE'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOFLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLZFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLZ_NPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLZ_INTERACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_OUT_DEV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )

*  Set required objects for module ECH_FLATTEN_ORDERS
      M = M + 1
      ONCE_PER_ORDER( M ) = .TRUE.
      MODULE_NAME( M ) = 'ECH_FLATTEN_ORDERS'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_BLZRSET' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_YBLAZE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'BLAZE_SPECT' ), M )

*  Set required objects for module ECH_DISABLE_ORDER
      M = M + 1
      MODULE_NAME( M ) = 'ECH_DISABLE_ORDER'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BAD_ORDER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'TRC_POLY' ), M )

*  Set required objects for module DUMMY_EXTRES
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_EXTRES'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDUCD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WORDER_NUMS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_LABELY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_LABELZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_FITWAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_ONUMS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_ERRORS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_WAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_ORDERS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_ERRORS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_ONUMS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_FITWAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_LABELY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_LABELZ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECH_RDUCD' ), M )

*  Set required objects for module DUMMY_EXTOBJ
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_EXTOBJ'
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )

*  Set required objects for module DUMMY_EXTARC
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_EXTARC'
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )

*  Set required objects for module DUMMY_EXTCAL
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_EXTCAL'
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_STAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_STAR_VAR' ), M )

*  Set required objects for module DUMMY_SCRRES
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_EXTRES'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDUCD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WORDER_NUMS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_LABELZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_ONUMS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_SCRORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_SCRERROR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_SCRWAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_ONUMS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_LABELZ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_SCRORDERS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_SCRERROR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_SCRWAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECH_RDUCD' ), M )

*  Set required objects for module DUMMY_SCROBJ
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_SCROBJ'
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_OBJV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )

*  Set required objects for module DUMMY_SCRARC
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_SCRARC'
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_ARCV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )

*  Set required objects for module DUMMY_SCRCAL
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_SCRCAL'
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_STAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_STRV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )

*  Set required objects for module DUMMY_ESPECT
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_ESPECT'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDUCD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_LABELX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_LABELZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_SPECTERR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_WAVESCALE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECH_RDUCD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_LABELX' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_LABELZ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_SPECTERR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_WAVESCALE' ), M )

*  Set required objects for module DUMMY_OSPECT
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_OSPECT'
      CALL MNEEDS( ECH_OBJ_IND( '1D_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ERR_SPECTRUM' ), M )

*  Set required objects for module DUMMY_CSPECT
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_CSPECT'
      CALL MNEEDS( ECH_OBJ_IND( 'STD_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ERR_STDSPECTRUM' ), M )

*  Set required objects for module DUMMY_DIPSTK
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_DIPSTK'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'STACK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WORDER_NUMS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_WAVES' ), M )

*  Set required objects for module DUMMY_OBJSTK
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_STKOBJ'
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )

*  Set required objects for module DUMMY_ARCSTK
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_STKARC'
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )

*  Set required objects for module DUMMY_CALSTK
      M = M + 1
      MODULE_NAME( M ) = 'DUMMY_STKCAL'
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_STAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_STAR_VAR' ), M )

*  Set required objects for module ASCII
      M = M + 1
      MODULE_NAME( M ) = 'ASCII'
      CALL MNEEDS( ECH_OBJ_IND( 'ASCII_FILE' ), M )

*  Set required objects for module ECH_WRITE_RESULTS
      M = M + 1
      MODULE_NAME( M ) = 'ECH_WRITE_RESULTS'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_TYPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_FORMAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_ARCHIVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_AIRTOVAC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_USEAAA' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_AAACODE' ), M )

*  Set required objects for module ECH_2DEXT_OPT
      M = M + 1
      MODULE_NAME( M ) = 'ECH_2DEXT_OPT'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_ORDER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_QUALITY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_EARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_EORDER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACT_MODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYRTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PFLSSAMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKVRCORR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CRCLEAN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PHOTON_TO_ADU' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_SSKY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_PFL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FSSKY_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'VAR_MATRIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MODEL_PROFILE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SSKY_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SSKY_VARIANCE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_OBJV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_ARCV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'MODEL_PROFILE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_OBJ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_OBJV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_ARC' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_ARCV' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SSKY_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SSKY_VARIANCE' ), M )

*  Set required objects for module ECH_MODEL_SSKY
      M = M + 1
      MODULE_NAME( M ) = 'ECH_MODEL_SSKY'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_ORDER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_QUALITY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_EORDER' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYRTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYSIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKYFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYXPLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYLINW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYLTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PHOTON_TO_ADU' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_SSKY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FSSKY_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SSKY_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SSKY_VARIANCE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SSKY_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SSKY_VARIANCE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_SSKY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FSSKY_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DSAMPLE_XCOORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DSAMPLE_YCOORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DIAGONAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DIAGONAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SMOOTH_DIAG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL3' ), M )

*  Set required objects for module ECH_MODEL_PROFILE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_MODEL_PROFILE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PFLSSAMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_USE_NXF' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_SKY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WPFL_TOTAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WPFL_TCOUNT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MODEL_PROFILE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'MODEL_PROFILE' ), M )

*  Set required objects for module ECH_GENERATE_FDB
      M = M + 1
      MODULE_NAME( M ) = 'ECH_GENERATE_FDB'
      CALL MNEEDS( ECH_OBJ_IND( 'ARC_TYPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_FTRDB' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DB_SCOPE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_IND_SIZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_QIND_SIZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_SOURCE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_LABELX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_LABELZ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_INTENSITY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_WSAMPLES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_WSTATUS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_DATABASE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_LEFT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_RIGHT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_WAVE_INDEX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_QUICK_INDEX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EFTRDB_QUICK_VALUE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WFDB_DATABASE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WFDB_LEFT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WFDB_RIGHT' ), M )

      CALL MUPDS( ECH_OBJ_IND( 'ECH_FTRDB' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_QIND_SIZ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_IND_SIZ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_QUICK_INDEX' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_QUICK_VALUE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_SOURCE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_LABELX' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_LABELZ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_INTENSITY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_WAVELENGTH' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_WSAMPLES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_WSTATUS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_DATABASE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_LEFT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_RIGHT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EFTRDB_WAVE_INDEX' ), M )

*  Set required objects for module ECHOMOP
      M = M + 1
      MODULE_NAME( M ) = 'ECHOMOP'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PAGE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_BATCH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_QUICK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_REPORT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_CLONE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DISPLAY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SOFT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'HARD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SO_FAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SO_FAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RDCTN_TRACIM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RDCTN_TRACIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RDCTN_INPTIM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RDCTN_INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RDCTN_SLITIM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RDCTN_SLITIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RDCTN_FFIELD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RDCTN_FFIELD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RDCTN_ARC' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RDCTN_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RDCTN_STAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RDCTN_STAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RDCTN_OBJECT' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RDCTN_OBJECT' ), M )

*  Set required objects for module ECH_EXTR_QUICK
      M = M + 1
      MODULE_NAME( M ) = 'ECH_EXTR_QUICK'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PHOTON_TO_ADU' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )

*  Set required objects for module ECH_POLY_PROFILE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_POLY_PROFILE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PFL_INTERACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_PFLSSAMP' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_OBJRTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_USE_NXF' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_SKY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_PFL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJ_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OBJFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WPFL_TOTAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WPFL_TCOUNT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XDOUBLE' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_PFL' ), M )

*  Set required objects for module ECH_DUMMY2
      M = M + 1
      MODULE_NAME( M ) = 'ECH_DUMMY2'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )

*  Set required objects for module ECH_FIT_REF_FWHMS
      M = M + 1
      MODULE_NAME( M ) = 'ECH_FIT_REF_FWHMS'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WID_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXRFLN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_COUNT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_WIDTHS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_LINES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ID_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REF_LINE_FWHM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ID_WIDTHS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'WID_POLY' ), M )

*  Set required objects for module ECH_NORMAL_YBLAZE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_NORMAL_YBLAZE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLZ_NPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_YBLAZE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLAZE_MEDIANS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'BLAZE_MEDIANS' ), M )

*  Set required objects for module ECH_MODEL_BACK
      M = M + 1
      MODULE_NAME( M ) = 'ECH_MODEL_BACK'
      ONCE_PER_ORDER( M ) = .FALSE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOFLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKYFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYRTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYSIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PHOTON_TO_ADU' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKY_MASK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_SKY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FSKY_ERRORS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FITTED_SKY' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'FSKY_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL3' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL4' ), M )

*  Set required objects for module ECH_MODEL_SCATTER
      M = M + 1
      MODULE_NAME( M ) = 'ECH_MODEL_SCATTER'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKYFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_ARC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_ARC_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYRTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL3' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL4' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WPFL_TOTAL' ), M )

*  Set required objects for module ECH_REMOVE_BACK
      M = M + 1
      MODULE_NAME( M ) = 'ECH_REMOVE_BACK'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'OUTPUT_IMAGE' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'FFIELD' ), M )
      CALL MNEEDS_QUALITY( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS_ERRORS( ECH_OBJ_IND( 'INPTIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOFLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYREJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SKYFIT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYRTHR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYSIM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKYPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'READOUT_NOISE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'PHOTON_TO_ADU' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL2' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL3' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FIT_WORK_XREAL4' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'OUTPUT_IMAGE' ), M )

*  Set required objects for module ECH_FLATTEN_YBLAZE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_FLATTEN_YBLAZE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_YBLAZE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'BLAZE_MEDIANS' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )

*  Set required objects for module ECH_MULTI_MERGE
      M = M + 1
      MODULE_NAME( M ) = 'ECH_MULTI_MERGE'
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_REBIN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_BINS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'W_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( '1D_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ERR_SPECTRUM' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCFRACT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SKEW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_LOG' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_FLUX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_INTR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SET_WSCALE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_QUAD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCRMODE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_SCRADD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_AIRTOVAC' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MRGMINX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MRGMAXX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MRGWGHT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTRACTED_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'EXTR_OBJ_VAR' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_OBJ' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'SCRNCHD_OBJV' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'REBIN_WORK' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'WVSCALE_INDEX' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_WAVES' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_OBJ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'SCRNCHD_OBJV' ), M )
      CALL MUPDS( ECH_OBJ_IND( '1D_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ERR_SPECTRUM' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'WAVELENGTH' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MFILTER1' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'MFILTER2' ), M )

*  Set required objects for module ECH_GEN_FLAT
      M = M + 1
      MODULE_NAME( M ) = 'ECH_GEN_FLAT'
      ONCE_PER_ORDER( M ) = .TRUE.
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDCTN' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'ECH_RDUCD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NREF_FRAME' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_DIAGNOSE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NO_OF_ORDERS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NX_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'NY_PIXELS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TRC_POLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_BELOW' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'DEK_ABOVE' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MAXPOLY' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_NOFLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'TUNE_MXSKYPIX' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'X_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'Y_TRACE_COORD' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FITTED_FLAT' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'FLAT_ERRORS' ), M )
      CALL MNEEDS( ECH_OBJ_IND( 'RESULT_DATAZ' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'ECH_RDUCD' ), M )
      CALL MUPDS( ECH_OBJ_IND( 'RESULT_DATAZ' ), M )

*  There are currently 73/75 modules.

*  All switches are `off' switches, ie. default behavior in absence of
*  any switches is for all objects to be active.
*  Positive switch-off indices indicate a LOGICAL parameter specifying
*  whether to use the primary object, Negative switch-off indices
*  indicate an INTEGER (usually array dimension) parameter, for
*  which parameter=0 indiciates don't use.
*
*  Summary : LOGICAL switch-off parameter value of .TRUE.
*            INTEGER switch-off parameter value of 0
*
*  each indicate that the primary object is `switched off'.
      OBJECT_SWITCH( ECH_OBJ_IND( 'ARC' ) ) =
     :      ECH_OBJ_IND( 'TUNE_NOARC' )
      OBJECT_SWITCH( ECH_OBJ_IND( 'EXTRACTED_ARC' ) ) =
     :      ECH_OBJ_IND( 'TUNE_NOARC' )
      OBJECT_SWITCH( ECH_OBJ_IND( 'EXTR_ARC_VAR' ) ) =
     :      ECH_OBJ_IND( 'TUNE_NOARC' )
      OBJECT_SWITCH( ECH_OBJ_IND( 'FFIELD' ) ) =
     :      ECH_OBJ_IND( 'TUNE_NOFLAT' )
      OBJECT_SWITCH( ECH_OBJ_IND( 'FITTED_FLAT' ) ) =
     :      ECH_OBJ_IND( 'TUNE_NOFLAT' )
      OBJECT_SWITCH( ECH_OBJ_IND( 'FLAT_ERRORS' ) ) =
     :      ECH_OBJ_IND( 'TUNE_NOFLAT' )
      OBJECT_SWITCH( ECH_OBJ_IND( 'FITTED_PFL' ) ) =
     :      - ECH_OBJ_IND( 'TUNE_OBJPOLY' )
      OBJECT_SWITCH( ECH_OBJ_IND( 'TRACE_PATH' ) ) =
     :      - ECH_OBJ_IND( 'TUNE_IUE' )

      END
