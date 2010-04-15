*+  CRED4_READ_NB - Read the contents of the CRED4 noticeboard into parameters
      SUBROUTINE CRED4_READ_NB( STATUS )
*    Description :
*     This routine reads the information contained in the noticeboard
*     and transfers it to variables stored in the common block. Any
*     changes made to the noticeboard are therefore transferred to these
*     variables, which will cause the data reduction to implement them.
*    Invocation :
*     CALL CRED4_READ_NB( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jun-1990: Original version, as part of the phase 2
*                  major changes.                             (SMB)
*     21-Jun-1990: Typing mistakes fixed.                     (SMB)
*     22-Jun-1990: Bug fix. SLICE_START and SLICE_END
*                  parameters are real.                       (SMB)
*     18-Jul-1990: Parameters added to allow a reduced
*                  observation to be displayed up to 4 times
*                  using any desired method.                  (SMB)
*      1-Oct-1990: AFIT_NROWS parameter added, to allow more
*                  than one row to be averaged for line
*                  fitting.                                   (SMB)
*      2-Oct-1990: SUBTRACT_SKY parameter removed and replaced
*                  with ADD_OBS.                              (SMB)
*     25-Oct-1990: GROUP display parameters added.            (SMB)
*      1-Nov-1990: DISP_FREQUENCY parameter added.            (SMB)
*      5-Nov-1990: VARIANCE_WT and SKY_WT parameters added.   (SMB)
*     13-Nov-1990: ISTART, IEND, JSTART, JEND display parameters
*                  removed, as the P4 task has now been made
*                  more consistent.                           (SMB)
*     19-Nov-1990: LAMBDA_METHOD parameter added, to control
*                  the wavelength calibration.                (SMB)
*      3-Jan-1991: Bad pixel mask and linearisation cofficients
*                  included in noticeboard and configuration. (SMB)
*      2-Feb-1991: ADD_IN_PAIRS and ERRORS parameters added,
*                  so that OBJECT and SKY observations can be
*                  added together in pairs. It has been discovered
*                  that the only way to sky-subtract reliably
*                  is to add pairs in this way.               (UKIRT::SMB)
*      3-Feb-1991: Typing mistake fixed.                      (UKIRT::SMB)
*     27-Jun-1991: Add point_source_options                   (UKIRT::PND)
*     31-Jul-1991: BOXSIZE parameter added.                   (SMB)
*     14-Apr-1992: Add BIAS_MODE, DARK_MODE, FLAT_MODE,
*                  CALIB_MODE, STANDARD_MODE, SPECIFIED_BIAS,
*                  SPECIFIED_DARK, SPECIFIED_FLAT,
*                  SPECIFIED_CALIB and SPECIFIED_STD
*                  parameters, for DRIMP/5.1 and DRIMP/5.2.   (SMB)
*     17-Jul-1992: Add SUBTRACT_BIAS and ARCHIVE_OBS          (PND)
*     11-Feb-1993: Conform to error strategy                  (PND)
*     22-Mar-1994: Add extract nod spc                        (PND,KLK)
*     24-May-1994: Add bright or faint source algorithm       (PND)
*     29-Jul-1994: Major changes for Unix port                (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'     ! CRED4 common block
*    Local variables :
      INTEGER ACTBYTES
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain the parameters controlling the data reduction sequence.
      IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Reading character items', STATUS )
      CALL NBS_GET_CVALUE( SUBTRACT_BIAS_ID, 0, SUBTRACT_BIAS, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( SUBTRACT_DARK_ID, 0, SUBTRACT_DARK, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( ADD_INT_ID, 0, ADD_INT, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( ARCHIVE_OBS_ID, 0, ARCHIVE_OBS, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( FILE_OBS_ID, 0, FILE_OBS, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( NORMALISE_FF_ID, 0, NORMALISE_FF, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DIVIDE_BY_FF_ID, 0, DIVIDE_BY_FF, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( ADD_OBS_ID, 0, ADD_OBS, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( TO_WAVELENGTH_ID, 0, TO_WAVELENGTH, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DIVIDE_BY_STD_ID, 0, DIVIDE_BY_STD, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( EXTRACT_SPC_ID, 0, EXTRACT_SPC, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( AUTOFIT_ID, 0, AUTOFIT, ACTBYTES, STATUS )

*   Obtain the display parameters
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Reading display items', STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(0), 0, DISPLAY_INT(0), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(1), 0, DISPLAY_INT(1), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(2), 0, DISPLAY_INT(2), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(3), 0, DISPLAY_INT(3), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(4), 0, DISPLAY_INT(4), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(5), 0, DISPLAY_INT(5), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(6), 0, DISPLAY_INT(6), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(7), 0, DISPLAY_INT(7), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_INT_ID(8), 0, DISPLAY_INT(8), ACTBYTES, STATUS )

      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(0), 0, DISPLAY_OBS(0), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(1), 0, DISPLAY_OBS(1), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(2), 0, DISPLAY_OBS(2), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(3), 0, DISPLAY_OBS(3), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(4), 0, DISPLAY_OBS(4), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(5), 0, DISPLAY_OBS(5), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(6), 0, DISPLAY_OBS(6), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(7), 0, DISPLAY_OBS(7), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_OBS_ID(8), 0, DISPLAY_OBS(8), ACTBYTES, STATUS )

      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(0), 0, DISPLAY_GRP(0), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(1), 0, DISPLAY_GRP(1), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(2), 0, DISPLAY_GRP(2), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(3), 0, DISPLAY_GRP(3), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(4), 0, DISPLAY_GRP(4), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(5), 0, DISPLAY_GRP(5), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(6), 0, DISPLAY_GRP(6), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(7), 0, DISPLAY_GRP(7), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_GRP_ID(8), 0, DISPLAY_GRP(8), ACTBYTES, STATUS )

      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(0), 0, DISPLAY_SPC(0), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(1), 0, DISPLAY_SPC(1), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(2), 0, DISPLAY_SPC(2), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(3), 0, DISPLAY_SPC(3), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(4), 0, DISPLAY_SPC(4), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(5), 0, DISPLAY_SPC(5), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(6), 0, DISPLAY_SPC(6), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(7), 0, DISPLAY_SPC(7), ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DISPLAY_SPC_ID(8), 0, DISPLAY_SPC(8), ACTBYTES, STATUS )

*   Obtain the miscellaneous parameters.
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Reading miscellaneous items', STATUS )
      CALL NBS_GET_CVALUE( BIAS_MODE_ID, 0, BIAS_MODE, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( DARK_MODE_ID, 0, DARK_MODE, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( FLAT_MODE_ID, 0, FLAT_MODE, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( CALIB_MODE_ID, 0, CALIB_MODE, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( STANDARD_MODE_ID, 0, STANDARD_MODE, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( SPECIFIED_BIAS_ID, 0, SPECIFIED_BIAS, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( SPECIFIED_DARK_ID, 0, SPECIFIED_DARK, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( SPECIFIED_FLAT_ID, 0, SPECIFIED_FLAT, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( SPECIFIED_CALIB_ID, 0, SPECIFIED_CALIB, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( SPECIFIED_STD_ID, 0, SPECIFIED_STD, ACTBYTES, STATUS )

      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Reading second misc items', STATUS )
      CALL NBS_GET_CVALUE( MASK_ID, 0, MASK, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( LINCOEFFS_ID, 0, LINCOEFFS, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( VARIANCE_WT_ID, 0, VAL__NBI, VARIANCE_WT, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( ADD_IN_PAIRS_ID, 0, VAL__NBI, ADD_IN_PAIRS, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( ERRORS_ID, 0, ERRORS, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( SKY_WT_ID, 0, VAL__NBR, SKY_WT, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( PF_POLYFIT_ID, 0, PF_POLYFIT, ACTBYTES, STATUS )

      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Reading third misc items', STATUS )
      CALL NBS_GET_VALUE( PF_WEIGHT_ID, 0, VAL__NBI, PF_WEIGHT, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_DEGREE_ID, 0, VAL__NBI, PF_DEGREE, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_NREJECT_ID, 0, VAL__NBI, PF_NREJECT, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_SAYS1_ID, 0, VAL__NBI, PF_SAYS1, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_SAYE1_ID, 0, VAL__NBI, PF_SAYE1, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_SAYS2_ID, 0, VAL__NBI, PF_SAYS2, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_SAYE2_ID, 0, VAL__NBI, PF_SAYE2, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_SAYS3_ID, 0, VAL__NBI, PF_SAYS3, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_SAYE3_ID, 0, VAL__NBI, PF_SAYE3, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_SAYS4_ID, 0, VAL__NBI, PF_SAYS4, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( PF_SAYE4_ID, 0, VAL__NBI, PF_SAYE4, ACTBYTES, STATUS )

*   Obtain the parameters controlling the flat field normalisation.
      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Reading other misc items', STATUS )
      CALL NBS_GET_CVALUE( NORM_METHOD_ID, 0, NORM_METHOD, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( ORDER_ID, 0, VAL__NBI, ORDER, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( BOXSIZE_ID, 0, VAL__NBI, BOXSIZE, ACTBYTES, STATUS )

*   Obtain the parameters controlling the wavelength calibration.
      CALL NBS_GET_CVALUE( LAMBDA_METHOD_ID, 0, LAMBDA_METHOD, ACTBYTES, STATUS )

*   Obtain the extract spc parameters
      CALL NBS_GET_VALUE( SPC_ROW1S_ID, 0, VAL__NBR, SPC_ROW1S, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( SPC_ROW1E_ID, 0, VAL__NBR, SPC_ROW1E, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( SPC_ROW2S_ID, 0, VAL__NBR, SPC_ROW2S, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( SPC_ROW2E_ID, 0, VAL__NBR, SPC_ROW2E, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( SPC_ROW3S_ID, 0, VAL__NBR, SPC_ROW3S, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( SPC_ROW3E_ID, 0, VAL__NBR, SPC_ROW3E, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( SPC_INVERT_ID, 0, VAL__NBI, SPC_INVERT, ACTBYTES, STATUS )
      CALL NBS_GET_CVALUE( SPC_ALGORITHM_ID, 0, SPC_ALGORITHM, ACTBYTES, STATUS )

*   Obtain the parameters controlling the line fitting.
      CALL NBS_GET_VALUE( AFIT_NROWS_ID, 0, VAL__NBI, AFIT_NROWS, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( AFIT_ROW1_ID, 0, VAL__NBI, AFIT_ROW1, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( AFIT_ROW2_ID, 0, VAL__NBI, AFIT_ROW2, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( AFIT_XSTART_ID, 0, VAL__NBR, AFIT_XSTART, ACTBYTES, STATUS )
      CALL NBS_GET_VALUE( AFIT_XEND_ID, 0, VAL__NBR, AFIT_XEND, ACTBYTES, STATUS )

      IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Noticeboard read OK', STATUS )
      END
