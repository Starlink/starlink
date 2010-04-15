*+  CRED4_FIND_NB - Find (map) the contents of the CRED4 noticeboard
      SUBROUTINE CRED4_FIND_NB( STATUS )
*    Description :
*     This routine finds (maps) each of the required items in the
*     noticeboard and stored the ID for each item in a common block.
*     (All the items are found at once in this routine because
*     NBS_FIND_ITEM is relatively slow, and it is more efficient to
*     find items only once at the beginning).
*    Invocation :
*     CALL CRED4_FIND_NB( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      5-Jun-1990: Original version.                           (SMB)
*     12-Jun-1990: Bug fix. ABORT_REDUCTION included.          (SMB)
*     18-Jun-1990: The "section already existed" status should
*                  NOT be treated as an error.                 (SMB)
*     19-Jun-1990: Phase 2 of the major changes: Modified for
*                  new noticeboard structure and common blocks.(SMB)
*     20-Jun-1990: SEQUENCE_SETUP parameter added.             (SMB)
*     21-Jun-1990: Typing mistakes fixed.                      (SMB)
*     21-Jun-1990: Restoring noticeboard moved elsewhere.      (SMB)
*     18-Jul-1990: Parameters added to allow a reduced
*                  observation to be displayed up to 4 times
*                  using any desired method.                   (SMB)
*      8-Aug-1990: I have just discovered that ERR_OUT resets
*                  STATUS back to SAI__OK, which messes up
*                  the error handling. Mistake fixed.          (SMB)
*      1-Oct-1990: AFIT_NROWS parameter added, to allow more
*                  than one row to be averaged for line
*                  fitting.                                    (SMB)
*      2-Oct-1990: SUBTRACT_SKY parameter removed and replaced
*                  with ADD_OBS and DISPLAY_GRP. (DISPLAY_GRP
*                  commented out for the time being).          (SMB)
*     25-Oct-1990: GROUP display parameters added.             (SMB)
*      1-Nov-1990: DISP_FREQUENCY parameter added.             (SMB)
*      5-Nov-1990: VARIANCE_WT and SKY_WT parameters added.    (SMB)
*     13-Nov-1990: ISTART, IEND, JSTART, JEND display parameters
*                  removed, as the P4 task has now been made
*                  more consistent.                            (SMB)
*     19-Nov-1990: LAMBDA_METHOD parameter added, to control
*                  the wavelength calibration.                 (SMB)
*      3-Jan-1991: Bad pixel mask and linearisation cofficients
*                  included in noticeboard and configuration.  (SMB)
*      2-Feb-1991: ADD_IN_PAIRS and ERRORS parameters added,
*                  so that OBJECT and SKY observations can be
*                  added together in pairs. It has been discovered
*                  that the only way to sky-subtract reliably
*                  is to add pairs in this way.                (UKIRT::SMB)
*     26-Jun-1991: Add point_source_options                    (UKIRT::PND)
*     31-Jul-1991: BOXSIZE parameter added.                    (SMB)
*     14-Apr-1992: Add BIAS_MODE, DARK_MODE, FLAT_MODE,
*                  CALIB_MODE, STANDARD_MODE, SPECIFIED_BIAS,
*                  SPECIFIED_DARK, SPECIFIED_FLAT,
*                  SPECIFIED_CALIB and SPECIFIED_STD
*                  parameters, for DRIMP/5.1 and DRIMP/5.2.    (SMB)
*     17-Jul-1992: Add SUBTRACT_BIAS and ARCHIVE_OBS           (PND)
*     18-Feb-1993: Conform to error strategy                   (PND)
*     23-Feb-1993: Replace ERR_OUT with ERR_REP                (PND)
*      3-Jan-1993: Add contouring etc                          (PND)
*     24-May-1994: Add bright or faint source algorithm        (PND)
*     29-Jul-1994: Major mods for Unix port (decouple P4)      (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'NBS_ERR'               ! NBS error codes.
*    Status :
      INTEGER STATUS                  ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'          ! CRED4 common block
*    External references :
      INTEGER CHR_LEN                 ! Finds used length of string
*    Local variables :
      INTEGER
     :  DISPLAY_TOPID,                ! ID of the DISPLAY structure.
     :  REDUCTION_TOPID,              ! ID of the REDUCTION structure.
     :  SUBTRACT_BIAS_TOPID,          ! ID of the SUBTRACT_BIAS structure
     :  SUBTRACT_DARK_TOPID,          ! ID of the SUBTRACT_DARK structure
     :  ADD_INT_TOPID,                ! ID of the ADD_INT structure
     :  ARCHIVE_OBS_TOPID,            ! ID of the ARCHIVE_OBS structure
     :  FILE_OBS_TOPID,               ! ID of the FILE_OBS structure
     :  NORMALISE_FF_TOPID            ! ID of the NORMALISE_FF structure
      INTEGER
     :  DIVIDE_BY_FF_TOPID,           ! ID of the DIVIDE_BY_FF structure
     :  ADD_OBS_TOPID,                ! ID of the ADD_OBS structure
     :  TO_WAVELENGTH_TOPID,          ! ID of the TO_WAVELENGTH structure
     :  DIVIDE_BY_STD_TOPID,          ! ID of the DIVIDE_BY_STD structure
     :  EXTRACT_SPC_TOPID,            ! ID of the EXTRACT_SPC structure
     :  AUTOFIT_TOPID,                ! ID of the AUTOFIT structure
     :  MISCELLANEOUS_TOPID,          ! ID of the MISCELLANEOUS structure
     :  FLAGS_TOPID                   ! ID of the FLAGS structure
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Find (map) the top level structure of the noticeboard,
*   and remember its ID in the common block.
      IF ( VERBOSE ) CALL MSG_OUT(' ', 'Finding noticeboard', STATUS )
      CALL NBS_FIND_NOTICEBOARD( NOTICEBOARD(1:CHR_LEN(NOTICEBOARD)), NB_TOPID, STATUS )

*   Find the display structure
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT(' ', 'Finding display items', STATUS )
      CALL NBS_FIND_ITEM( NB_TOPID, 'DISPLAY', DISPLAY_TOPID, STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P0', DISPLAY_INT_ID(0), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P1', DISPLAY_INT_ID(1), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P2', DISPLAY_INT_ID(2), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P3', DISPLAY_INT_ID(3), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P4', DISPLAY_INT_ID(4), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P5', DISPLAY_INT_ID(5), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P6', DISPLAY_INT_ID(6), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P7', DISPLAY_INT_ID(7), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'INT_P8', DISPLAY_INT_ID(8), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P0', DISPLAY_OBS_ID(0), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P1', DISPLAY_OBS_ID(1), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P2', DISPLAY_OBS_ID(2), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P3', DISPLAY_OBS_ID(3), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P4', DISPLAY_OBS_ID(4), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P5', DISPLAY_OBS_ID(5), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P6', DISPLAY_OBS_ID(6), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P7', DISPLAY_OBS_ID(7), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'OBS_P8', DISPLAY_OBS_ID(8), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P0', DISPLAY_GRP_ID(0), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P1', DISPLAY_GRP_ID(1), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P2', DISPLAY_GRP_ID(2), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P3', DISPLAY_GRP_ID(3), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P4', DISPLAY_GRP_ID(4), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P5', DISPLAY_GRP_ID(5), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P6', DISPLAY_GRP_ID(6), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P7', DISPLAY_GRP_ID(7), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'GRP_P8', DISPLAY_GRP_ID(8), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P0', DISPLAY_SPC_ID(0), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P1', DISPLAY_SPC_ID(1), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P2', DISPLAY_SPC_ID(2), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P3', DISPLAY_SPC_ID(3), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P4', DISPLAY_SPC_ID(4), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P5', DISPLAY_SPC_ID(5), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P6', DISPLAY_SPC_ID(6), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P7', DISPLAY_SPC_ID(7), STATUS )
      CALL NBS_FIND_ITEM( DISPLAY_TOPID, 'SPC_P8', DISPLAY_SPC_ID(8), STATUS )

*   Find a top-level structure called REDUCTION holding
*   the data reduction sequence.
       IF ( STATUS .EQ. SAI__OK .AND. VERBOSE ) CALL MSG_OUT(' ', 'Finding reduction items', STATUS )
       CALL NBS_FIND_ITEM( NB_TOPID, 'REDUCTION', REDUCTION_TOPID, STATUS )

*   Find the structures within the REDUCTION structure holding
*   the parameters for each of the reduction steps.
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'SUBTRACT_BIAS', SUBTRACT_BIAS_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'SUBTRACT_DARK', SUBTRACT_DARK_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'ADD_INT', ADD_INT_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'ARCHIVE_OBS', ARCHIVE_OBS_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'FILE_OBS', FILE_OBS_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'NORMALISE_FF', NORMALISE_FF_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'DIVIDE_BY_FF', DIVIDE_BY_FF_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'ADD_OBS', ADD_OBS_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'TO_WAVELENGTH', TO_WAVELENGTH_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'DIVIDE_BY_STD', DIVIDE_BY_STD_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'EXTRACT_SPC', EXTRACT_SPC_TOPID, STATUS )
      CALL NBS_FIND_ITEM( REDUCTION_TOPID, 'AUTOFIT', AUTOFIT_TOPID, STATUS )

*   Find all the EXECUTE items within these structures and
*   remember the IDs for these in the common block. (Note that,
*   for a data reduction step, the ID called XXX_ID refers to
*   the EXECUTE item with the XXX structure rather than the XXX
*   structure itself. This may cause some confusion. The reason
*   for this naming convention is because the relevant CRED4
*   parameter is called XXX. See the comments in CRED4COM.INC
*   for more details).
      IF ( STATUS .EQ. SAI__OK .AND .VERBOSE ) CALL MSG_OUT(' ', 'Finding execute items', STATUS )
      CALL NBS_FIND_ITEM( SUBTRACT_BIAS_TOPID, 'EXECUTE', SUBTRACT_BIAS_ID, STATUS )
      CALL NBS_FIND_ITEM( SUBTRACT_DARK_TOPID, 'EXECUTE', SUBTRACT_DARK_ID, STATUS )
      CALL NBS_FIND_ITEM( ADD_INT_TOPID, 'EXECUTE', ADD_INT_ID, STATUS )
      CALL NBS_FIND_ITEM( ARCHIVE_OBS_TOPID, 'EXECUTE', ARCHIVE_OBS_ID, STATUS )
      CALL NBS_FIND_ITEM( FILE_OBS_TOPID, 'EXECUTE', FILE_OBS_ID, STATUS )
      CALL NBS_FIND_ITEM( NORMALISE_FF_TOPID, 'EXECUTE', NORMALISE_FF_ID, STATUS )
      CALL NBS_FIND_ITEM( DIVIDE_BY_FF_TOPID, 'EXECUTE', DIVIDE_BY_FF_ID, STATUS )
      CALL NBS_FIND_ITEM( ADD_OBS_TOPID, 'EXECUTE', ADD_OBS_ID, STATUS )
      CALL NBS_FIND_ITEM( TO_WAVELENGTH_TOPID, 'EXECUTE', TO_WAVELENGTH_ID, STATUS )
      CALL NBS_FIND_ITEM( DIVIDE_BY_STD_TOPID, 'EXECUTE', DIVIDE_BY_STD_ID, STATUS )
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'EXECUTE', EXTRACT_SPC_ID, STATUS )
      CALL NBS_FIND_ITEM( AUTOFIT_TOPID, 'EXECUTE', AUTOFIT_ID, STATUS )

*   Find the items within the flat field normalisation structure.
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE )  CALL MSG_OUT( ' ', 'Finding miscellaneous primitives', STATUS )
      CALL NBS_FIND_ITEM( NORMALISE_FF_TOPID, 'METHOD', NORM_METHOD_ID, STATUS )
      CALL NBS_FIND_ITEM( NORMALISE_FF_TOPID, 'ORDER', ORDER_ID, STATUS )
      CALL NBS_FIND_ITEM( NORMALISE_FF_TOPID, 'BOXSIZE',  BOXSIZE_ID, STATUS )

*   Find the items within the wavelength calibration structure.
      CALL NBS_FIND_ITEM( TO_WAVELENGTH_TOPID, 'METHOD', LAMBDA_METHOD_ID, STATUS )

*   Find the automated extract spc items
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'ROW1S', SPC_ROW1S_ID, STATUS )
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'ROW1E', SPC_ROW1E_ID, STATUS )
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'ROW2S', SPC_ROW2S_ID, STATUS )
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'ROW2E', SPC_ROW2E_ID, STATUS )
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'ROW3S', SPC_ROW3S_ID, STATUS )
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'ROW3E', SPC_ROW3E_ID, STATUS )
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'INVERT', SPC_INVERT_ID, STATUS )
      CALL NBS_FIND_ITEM( EXTRACT_SPC_TOPID, 'ALGORITHM', SPC_ALGORITHM_ID, STATUS )

*   Find the items within the autofit structure.
      CALL NBS_FIND_ITEM( AUTOFIT_TOPID, 'NROWS', AFIT_NROWS_ID, STATUS )
      CALL NBS_FIND_ITEM( AUTOFIT_TOPID, 'ROW1', AFIT_ROW1_ID, STATUS )
      CALL NBS_FIND_ITEM( AUTOFIT_TOPID, 'ROW2', AFIT_ROW2_ID, STATUS )
      CALL NBS_FIND_ITEM( AUTOFIT_TOPID, 'XSTART', AFIT_XSTART_ID, STATUS )
      CALL NBS_FIND_ITEM( AUTOFIT_TOPID, 'XEND', AFIT_XEND_ID, STATUS )

*   Find a top-level structure called MISCELLANEOUS, containing
*   miscellaneous items.
      CALL NBS_FIND_ITEM( NB_TOPID, 'MISCELLANEOUS', MISCELLANEOUS_TOPID, STATUS )

*   Find the parameters within this structure.
      IF ( STATUS .EQ. SAI__OK .AND. VERBOSE ) CALL MSG_OUT(' ', 'Finding miscellaneous items', STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'BIAS_MODE', BIAS_MODE_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'DARK_MODE', DARK_MODE_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'FLAT_MODE', FLAT_MODE_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'CALIB_MODE',  CALIB_MODE_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'STANDARD_MODE', STANDARD_MODE_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'SPECIFIED_BIAS', SPECIFIED_BIAS_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'SPECIFIED_DARK', SPECIFIED_DARK_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'SPECIFIED_FLAT', SPECIFIED_FLAT_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'SPECIFIED_CALIB', SPECIFIED_CALIB_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'SPECIFIED_STD', SPECIFIED_STD_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'MASK', MASK_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'LINCOEFFS', LINCOEFFS_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'VARIANCE_WT', VARIANCE_WT_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'ADD_IN_PAIRS', ADD_IN_PAIRS_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'ERRORS', ERRORS_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'SKY_WT', SKY_WT_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_POLYFIT', PF_POLYFIT_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_WEIGHT', PF_WEIGHT_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_DEGREE', PF_DEGREE_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_NREJECT', PF_NREJECT_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_SAYS1', PF_SAYS1_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_SAYE1', PF_SAYE1_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_SAYS2', PF_SAYS2_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_SAYE2', PF_SAYE2_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_SAYS3', PF_SAYS3_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_SAYE3', PF_SAYE3_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_SAYS4', PF_SAYS4_ID, STATUS )
      CALL NBS_FIND_ITEM( MISCELLANEOUS_TOPID, 'PF_SAYE4', PF_SAYE4_ID, STATUS )

*   Find a top-level structure called FLAGS holding the
*   flags which are used to control the execution of CRED4,
*   and which CRED4 can use to signal the data reduction status
*   to the outside world.
      CALL NBS_FIND_ITEM( NB_TOPID, 'FLAGS', FLAGS_TOPID, STATUS )

*   Find the flags within this structure.
       IF ( STATUS .EQ. SAI__OK .AND. VERBOSE ) CALL MSG_OUT(' ', 'Finding flag items', STATUS )
      CALL NBS_FIND_ITEM( FLAGS_TOPID, 'STOP_REDUCTION', STOP_REDUCTION_ID, STATUS )
      CALL NBS_FIND_ITEM( FLAGS_TOPID, 'ABORT_REDUCTION', ABORT_REDUCTION_ID, STATUS )
      CALL NBS_FIND_ITEM( FLAGS_TOPID, 'PAUSE_REDUCTION', PAUSE_REDUCTION_ID, STATUS )
      CALL NBS_FIND_ITEM( FLAGS_TOPID, 'REDUCING', REDUCING_ID, STATUS )
      CALL NBS_FIND_ITEM( FLAGS_TOPID, 'CRED4_BUSY', CRED4_BUSY_ID, STATUS )
      CALL NBS_FIND_ITEM( FLAGS_TOPID, 'RED4_BUSY', RED4_BUSY_ID, STATUS )
      CALL NBS_FIND_ITEM( FLAGS_TOPID, 'P4_BUSY', P4_BUSY_ID, STATUS )

*   If everything has worked, a data reduction sequence
*   should now be set up.
      IF ( STATUS .EQ. SAI__OK .AND. VERBOSE ) THEN

         SEQUENCE_SETUP = .TRUE.
         CALL MSG_OUT( ' ', 'Data reduction sequence setup OK', STATUS )
      ENDIF
      END
