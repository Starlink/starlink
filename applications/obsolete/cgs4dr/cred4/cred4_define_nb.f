*+  CRED4_DEFINE_NB - Define the contents of the CRED4_NB noticeboard
      SUBROUTINE CRED4_DEFINE_NB( STATUS )
*    Description :
*     This routine defines the contents of the CRED4_NB noticeboard.
*    Invocation :
*     CALL CRED4_DEFINE_NB( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      4-Jun-1990: Original version, which defined a
*                  noticeboard having the same structure as
*                  he old common block.                       (SMB)
*      5-Jun-1990: Modified to use new CGSNBS routines.       (SMB)
*     12-Jun-1990: Bug fix. ABORT_REDUCTION included.         (SMB)
*     18-Jun-1990: The "section already existed" status
*                  should NOT be treated as an error.         (SMB)
*     19-Jun-1990: Phase 2 of the major changes. Noticeboard
*                  structure changed to something more
*                  sensible.                                  (SMB)
*     21-Jun-1990: Modified to write noticeboard into
*                  CGS4_CONFIG directory.                     (SMB)
*     22-Jun-1990: NBFILE parameter included.                 (SMB)
*     17-Jul-1990: Parameters added to allow up to 4 reduced
*                  observation displays.                      (SMB)
*     18-Jul-1990: Typing mistakes fixed.                     (SMB)
*      8-Aug-1990: I have just discovered that ERR_REP resets
*                  STATUS back to ADAM__OK, which messes up
*                  the error handling. Mistake fixed.         (SMB)
*      1-Oct-1990: AFIT_NROWS parameter added, to allow more
*                  than one row to be averaged for line
*                  fitting.                                   (SMB)
*      2-Oct-1990: SUBTRACT_SKY parameter removed and replaced
*                  with ADD_OBS and DISPLAY_GRP. (DISPLAY_GRP
*                  commented out for the time being).         (SMB)
*     26-Oct-1990: GROUP display parameters added.            (SMB)
*      1-Nov-1990: DISP_FREQUENCY parameter added.            (SMB)
*      5-Nov-1990: VARIANCE_WT and SKY_WT parameters added.   (SMB)
*     19-Nov-1990: LAMBDA_METHOD parameter added, to control
*                  the wavelength calibration.                (SMB)
*      4-Jan-1991: Bad pixel mask and linearisation coefficients
*                  file name moved into noticeboard.          (SMB)
*      2-Feb-1991: ADD_IN_PAIRS and ERRORS parameters added,
*                  so that OBJECT and SKY observations can be
*                  added together in pairs. It has been discovered
*                  that the only way to sky-subtract reliably
*                  is to add pairs in this way.               (UKIRT::SMB)
*     26-Jun-1991: Add point_source_options                   (UKIRT::PND)
*     31-Jul-1991: BOXSIZE parameter added.                   (SMB)
*     14-Apr-1992: Add BIAS_MODE, DARK_MODE, FLAT_MODE,
*                  CALIB_MODE, STANDARD_MODE, SPECIFIED_BIAS,
*                  SPECIFIED_DARK, SPECIFIED_FLAT,
*                  SPECIFIED_CALIB and SPECIFIED_STD
*                  parameters, for DRIMP/5.1 and DRIMP/5.2.   (SMB)
*     17-Jul-1992: Add SUBTRACT_BIAS and ARCHIVE_OBS          (PND)
*     15-Jan-1993: End the definition by creating NBD         (PND)
*     18-Feb-1993: Conform to error strategy                  (PND)
*     22-Mar-1994: Add extract spc                            (PND,KLK)
*     12-May-1994: OK if noticeboard already exists           (PND)
*     24-May-1994: Add bright or faint source algorithm       (PND)
*     29-Jul-1994: Major change for Unix port (strip disp)    (PND)
*     09-Apr-1995: Replace _CHAR*80 with _CHAR for tcl/tk     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'NBS_ERR'          ! NBS error codes
*    Status :
      INTEGER STATUS             ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'     ! CRED4 common block
*    External references :
      INTEGER CHR_LEN            ! Finds length of string
*    Local constants :
      INTEGER NBS_TUNE_VAL
      PARAMETER ( NBS_TUNE_VAL = 8 * 32768 )
*    Local variables :
      INTEGER
     :  TOPSID,             ! Top level static ID
     :  LSID,               ! Local static ID (not used)
     :  DISPLAY_SID,        ! Static ID for DISPLAY structure
     :  REDUCTION_SID,      ! Static ID for REDUCTION structure
     :  SUBTRACT_BIAS_SID,  ! Static ID for SUBTRACT_BIAS structure
     :  SUBTRACT_DARK_SID,  ! Static ID for SUBTRACT_DARK structure
     :  ADD_INT_SID,        ! Static ID for ADD_INT structure
     :  ARCHIVE_OBS_SID,    ! Static ID for ARCHIVE_OBS structure
     :  FILE_OBS_SID,       ! Static ID for FILE_OBS structure
     :  NORMALISE_FF_SID    ! Static ID for NORMALISE_FF structure
      INTEGER
     :  DIVIDE_BY_FF_SID,   ! Static ID for DIVIDE_BY_FF structure
     :  ADD_OBS_SID,        ! Static ID for ADD_OBS structure
     :  TO_WAVELENGTH_SID,  ! Static ID for TO_WAVELENGTH structure
     :  DIVIDE_BY_STD_SID,  ! Static ID for DIVIDE_BY_STD structure
     :  EXTRACT_SPC_SID,    ! Static ID for EXTRACT_SPC structure
     :  AUTOFIT_SID,        ! Static ID for AUTOFIT structure
     :  MISCELLANEOUS_SID,  ! Static ID for MISCELLANEOUS structure
     :  FLAGS_SID           ! Static ID for FLAGS structure
      INTEGER IGNORE
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Begin the noticeboard definition and obtain the top level
*   static ID of the noticeboard.
      CALL NBS_TUNE( 'MAX_DEFN_SIZE', NBS_TUNE_VAL, IGNORE, STATUS )
      CALL NBS_BEGIN_DEFINITION( TOPSID, STATUS )
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Started definition of noticeboard', STATUS )

*   Define a top-level structures called REDUCTION, DISPLAY
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'REDUCTION', 'REDUCTION', REDUCTION_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'DISPLAY', 'DISPLAY', DISPLAY_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'MISCELLANEOUS', 'MISCELLANEOUS', MISCELLANEOUS_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'FLAGS', 'FLAGS', FLAGS_SID, STATUS )
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Defined top-level OK', STATUS )

*   Define a structure within REDUCTION for each of the reduction steps.
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'SUBTRACT_BIAS', 'REDUCTION_STEP', SUBTRACT_BIAS_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'SUBTRACT_DARK', 'REDUCTION_STEP', SUBTRACT_DARK_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'ADD_INT', 'REDUCTION_STEP', ADD_INT_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'ARCHIVE_OBS', 'REDUCTION_STEP', ARCHIVE_OBS_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'FILE_OBS', 'REDUCTION_STEP', FILE_OBS_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'NORMALISE_FF', 'REDUCTION_STEP', NORMALISE_FF_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'DIVIDE_BY_FF', 'REDUCTION_STEP', DIVIDE_BY_FF_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'ADD_OBS', 'REDUCTION_STEP', ADD_OBS_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'TO_WAVELENGTH', 'REDUCTION_STEP', TO_WAVELENGTH_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'DIVIDE_BY_STD', 'REDUCTION_STEP', DIVIDE_BY_STD_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'EXTRACT_SPC', 'REDUCTION_STEP', EXTRACT_SPC_SID, STATUS )
      CALL NBS_DEFINE_STRUCTURE( REDUCTION_SID, 'AUTOFIT', 'REDUCTION_STEP', AUTOFIT_SID, STATUS )
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Defined reduction structures', STATUS )

*   Define the items within these structures:-
*   All the structures will have an EXECUTE item.
      CALL NBS_DEFINE_PRIMITIVE( SUBTRACT_BIAS_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( SUBTRACT_DARK_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( ADD_INT_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( ARCHIVE_OBS_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( FILE_OBS_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( NORMALISE_FF_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DIVIDE_BY_FF_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( ADD_OBS_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( TO_WAVELENGTH_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DIVIDE_BY_STD_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( AUTOFIT_SID, 'EXECUTE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Defined execute structures', STATUS )

*   Define the flat field normalisation items within the flat
*   field normalisation structure.
      CALL NBS_DEFINE_PRIMITIVE( NORMALISE_FF_SID, 'METHOD', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( NORMALISE_FF_SID, 'ORDER', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( NORMALISE_FF_SID, 'BOXSIZE', '_INTEGER', 0, VAL__NBI, LSID, STATUS )

*   Define the wavelength calibration items within the wavelength
*   calibration structure.
      CALL NBS_DEFINE_PRIMITIVE( TO_WAVELENGTH_SID, 'METHOD', '_CHAR', 0, NBS_CLEN, LSID, STATUS )

*   Define the line fitting items within the autofit structure.
      CALL NBS_DEFINE_PRIMITIVE( AUTOFIT_SID, 'NROWS', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( AUTOFIT_SID, 'ROW1', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( AUTOFIT_SID, 'ROW2', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( AUTOFIT_SID, 'XSTART', '_REAL', 0, VAL__NBR, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( AUTOFIT_SID, 'XEND', '_REAL', 0, VAL__NBR, LSID, STATUS )
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Defined miscellaneous primitives', STATUS )

*   Define the extract_spc items
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'ROW1S', '_REAL', 0, VAL__NBR, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'ROW1E', '_REAL', 0, VAL__NBR, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'ROW2S', '_REAL', 0, VAL__NBR, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'ROW2E', '_REAL', 0, VAL__NBR, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'ROW3S', '_REAL', 0, VAL__NBR, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'ROW3E', '_REAL', 0, VAL__NBR, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'INVERT', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( EXTRACT_SPC_SID, 'ALGORITHM', '_CHAR', 0, NBS_CLEN, LSID, STATUS )

*   Include the display primitives
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P0', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P1', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P2', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P3', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P4', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P5', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P6', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P7', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'INT_P8', '_CHAR', 0, NBS_CLEN, LSID, STATUS )

      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P0', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P1', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P2', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P3', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P4', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P5', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P6', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P7', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'OBS_P8', '_CHAR', 0, NBS_CLEN, LSID, STATUS )

      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P0', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P1', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P2', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P3', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P4', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P5', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P6', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P7', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'GRP_P8', '_CHAR', 0, NBS_CLEN, LSID, STATUS )

      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P0', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P1', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P2', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P3', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P4', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P5', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P6', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P7', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( DISPLAY_SID, 'SPC_P8', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Defined display structures', STATUS )

*  Define the parameters within this structure.
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'BIAS_MODE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'DARK_MODE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'FLAT_MODE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'CALIB_MODE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'STANDARD_MODE', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'SPECIFIED_BIAS', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'SPECIFIED_DARK', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'SPECIFIED_FLAT', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'SPECIFIED_CALIB', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'SPECIFIED_STD', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'MASK', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'LINCOEFFS', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'VARIANCE_WT', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'ADD_IN_PAIRS', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'ERRORS', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'SKY_WT', '_REAL', 0, VAL__NBR, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_POLYFIT', '_CHAR', 0, NBS_CLEN, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_WEIGHT', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_DEGREE', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_NREJECT', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_SAYS1', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_SAYE1', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_SAYS2', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_SAYE2', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_SAYS3', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_SAYE3', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_SAYS4', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( MISCELLANEOUS_SID, 'PF_SAYE4', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Defined miscellaneous structures', STATUS )

*  Define the flags within this structure.
      CALL NBS_DEFINE_PRIMITIVE( FLAGS_SID, 'STOP_REDUCTION', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( FLAGS_SID, 'ABORT_REDUCTION', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( FLAGS_SID, 'PAUSE_REDUCTION', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( FLAGS_SID, 'REDUCING', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( FLAGS_SID, 'CRED4_BUSY', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( FLAGS_SID, 'RED4_BUSY', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      CALL NBS_DEFINE_PRIMITIVE( FLAGS_SID, 'P4_BUSY', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Defined flags structures', STATUS )

*   End the noticeboard definition and create the NBD on the fly.
      CALL NBS_END_DEFINITION( NOTICEBOARD(1:CHR_LEN(NOTICEBOARD)), 'CREATE_NOTICEBOARD', STATUS )

*   If NB already exists, warn user but treat as OK
      IF  ( STATUS .EQ. NBS__SECTIONEXISTED )  THEN

         CALL ERR_ANNUL( STATUS )
         CALL MSG_OUT( ' ', 'Re-using existing noticeboard', STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_DEFINE_NB: Failed to create noticeboard', STATUS )
      END IF
      END
