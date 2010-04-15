*+  CRED4_WRITE_PARAMETERS - Write parameters from common block values.
      SUBROUTINE CRED4_WRITE_PARAMETERS( STATUS )
*    Description :
*     This routine writes the information stored in variables in the
*     common block to the CRED4 parameters.
*     It may be used to update the values of those parameters, but
*     should not be used while data reduction is in progress.
*    Invocation :
*     CALL CRED4_WRITE_PARAMETERS( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     27-Jun-1990: Original version.                            (SMB)
*     18-Jul-1990: Parameters added to allow a reduced
*                  observation to be displayed up to 4 times
*                  using any desired method.                    (SMB)
*      1-Oct-1990: AFIT_NROWS parameter added, to allow more
*                  than one row to be averaged for line
*                  fitting.                                     (SMB)
*      2-Oct-1990: SUBTRACT_SKY parameter removed and replaced
*                  with ADD_OBS.                                (SMB)
*     25-Oct-1990: GROUP display parameters added.              (SMB)
*      1-Nov-1990: DISP_FREQUENCY parameter added.              (SMB)
*      5-Nov-1990: VARIANCE_WT and SKY_WT parameters added.     (SMB)
*     13-Nov-1990: ISTART, IEND, JSTART, JEND display parameters
*                  removed, as the P4 task has now been made
*                  more consistent.                             (SMB)
*     19-Nov-1990: LAMBDA_METHOD parameter added, to control
*                  the wavelength calibration.                  (SMB)
*      3-Jan-1991: Bad pixel mask and linearisation cofficients
*                  included in noticeboard and configuration.   (SMB)
*      2-Feb-1991: ADD_IN_PAIRS and ERRORS parameters added,
*                  so that OBJECT and SKY observations can be
*                  added together in pairs. It has been discovered
*                  that the only way to sky-subtract reliably
*                  is to add pairs in this way.                 (UKIRT::SMB)
*     27-Jun-1991: Add point_source_options                     (UKIRT::PND)
*     31-Jul-1991: BOXSIZE parameter added.                     (SMB)
*     14-Apr-1992: Add BIAS_MODE, DARK_MODE, FLAT_MODE,
*                  CALIB_MODE, STANDARD_MODE, SPECIFIED_BIAS,
*                  SPECIFIED_DARK, SPECIFIED_FLAT,
*                  SPECIFIED_CALIB and SPECIFIED_STD
*                  parameters, for DRIMP/5.1 and DRIMP/5.2.     (SMB)
*     17-Jul-1992: Add SUBTRACT_BIAS and ARCHIVE_OBS            (PND)
*     11-Feb-1993: Conform to error strategy                    (PND)
*     22-Mar-1994: Add extract spc                              (PND,KLK)
*     24-May-1994: Add bright and faint source algorithm        (PND)
*     30-Aug-1994: Ported to Unix                               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'     ! CRED4 common block
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Write the parameters controlling the data reduction sequence.
      CALL PAR_PUT0C( 'SUBTRACT_BIAS', SUBTRACT_BIAS, STATUS )
      CALL PAR_PUT0C( 'SUBTRACT_DARK', SUBTRACT_DARK, STATUS )
      CALL PAR_PUT0C( 'ADD_INT', ADD_INT, STATUS )
      CALL PAR_PUT0C( 'ARCHIVE_OBS', ARCHIVE_OBS, STATUS )
      CALL PAR_PUT0C( 'FILE_OBS', FILE_OBS, STATUS )
      CALL PAR_PUT0C( 'NORMALISE_FF', NORMALISE_FF, STATUS )
      CALL PAR_PUT0C( 'DIVIDE_BY_FF', DIVIDE_BY_FF, STATUS )
      CALL PAR_PUT0C( 'ADD_OBS', ADD_OBS, STATUS )
      CALL PAR_PUT0C( 'TO_WAVELENGTH', TO_WAVELENGTH, STATUS )
      CALL PAR_PUT0C( 'DIVIDE_BY_STD', DIVIDE_BY_STD, STATUS )
      CALL PAR_PUT0C( 'EXTRACT_SPC', EXTRACT_SPC, STATUS )
      CALL PAR_PUT0C( 'AUTOFIT', AUTOFIT, STATUS )

*   Write the display parameters
      CALL PAR_PUT0C( 'INT_P0', DISPLAY_INT(0), STATUS )
      CALL PAR_PUT0C( 'INT_P1', DISPLAY_INT(1), STATUS )
      CALL PAR_PUT0C( 'INT_P2', DISPLAY_INT(2), STATUS )
      CALL PAR_PUT0C( 'INT_P3', DISPLAY_INT(3), STATUS )
      CALL PAR_PUT0C( 'INT_P4', DISPLAY_INT(4), STATUS )
      CALL PAR_PUT0C( 'INT_P5', DISPLAY_INT(5), STATUS )
      CALL PAR_GET0C( 'INT_P6', DISPLAY_INT(6), STATUS )
      CALL PAR_GET0C( 'INT_P6', DISPLAY_INT(6), STATUS )
      CALL PAR_GET0C( 'INT_P7', DISPLAY_INT(7), STATUS )
      CALL PAR_GET0C( 'INT_P8', DISPLAY_INT(8), STATUS )
      CALL PAR_PUT0C( 'OBS_P0', DISPLAY_OBS(0), STATUS )
      CALL PAR_PUT0C( 'OBS_P1', DISPLAY_OBS(1), STATUS )
      CALL PAR_PUT0C( 'OBS_P2', DISPLAY_OBS(2), STATUS )
      CALL PAR_PUT0C( 'OBS_P3', DISPLAY_OBS(3), STATUS )
      CALL PAR_PUT0C( 'OBS_P4', DISPLAY_OBS(4), STATUS )
      CALL PAR_PUT0C( 'OBS_P5', DISPLAY_OBS(5), STATUS )
      CALL PAR_PUT0C( 'OBS_P6', DISPLAY_OBS(6), STATUS )
      CALL PAR_GET0C( 'OBS_P7', DISPLAY_OBS(7), STATUS )
      CALL PAR_GET0C( 'OBS_P8', DISPLAY_OBS(8), STATUS )
      CALL PAR_PUT0C( 'GRP_P0', DISPLAY_GRP(0), STATUS )
      CALL PAR_PUT0C( 'GRP_P1', DISPLAY_GRP(1), STATUS )
      CALL PAR_PUT0C( 'GRP_P2', DISPLAY_GRP(2), STATUS )
      CALL PAR_PUT0C( 'GRP_P3', DISPLAY_GRP(3), STATUS )
      CALL PAR_PUT0C( 'GRP_P4', DISPLAY_GRP(4), STATUS )
      CALL PAR_PUT0C( 'GRP_P5', DISPLAY_GRP(5), STATUS )
      CALL PAR_PUT0C( 'GRP_P6', DISPLAY_GRP(6), STATUS )
      CALL PAR_GET0C( 'GRP_P7', DISPLAY_GRP(7), STATUS )
      CALL PAR_GET0C( 'GRP_P8', DISPLAY_GRP(8), STATUS )
      CALL PAR_PUT0C( 'SPC_P0', DISPLAY_SPC(0), STATUS )
      CALL PAR_PUT0C( 'SPC_P1', DISPLAY_SPC(1), STATUS )
      CALL PAR_PUT0C( 'SPC_P2', DISPLAY_SPC(2), STATUS )
      CALL PAR_PUT0C( 'SPC_P3', DISPLAY_SPC(3), STATUS )
      CALL PAR_PUT0C( 'SPC_P4', DISPLAY_SPC(4), STATUS )
      CALL PAR_PUT0C( 'SPC_P5', DISPLAY_SPC(5), STATUS )
      CALL PAR_PUT0C( 'SPC_P6', DISPLAY_SPC(6), STATUS )
      CALL PAR_GET0C( 'SPC_P7', DISPLAY_SPC(7), STATUS )
      CALL PAR_GET0C( 'SPC_P8', DISPLAY_SPC(8), STATUS )

*   Write the miscellaneous parameters.
      CALL PAR_PUT0C( 'BIAS_MODE', BIAS_MODE, STATUS )
      CALL PAR_PUT0C( 'DARK_MODE', DARK_MODE, STATUS )
      CALL PAR_PUT0C( 'FLAT_MODE', FLAT_MODE, STATUS )
      CALL PAR_PUT0C( 'CALIB_MODE', CALIB_MODE, STATUS )
      CALL PAR_PUT0C( 'STANDARD_MODE', STANDARD_MODE, STATUS )
      CALL PAR_PUT0C( 'SPECIFIED_BIAS', SPECIFIED_BIAS, STATUS )
      CALL PAR_PUT0C( 'SPECIFIED_DARK', SPECIFIED_DARK, STATUS )
      CALL PAR_PUT0C( 'SPECIFIED_FLAT', SPECIFIED_FLAT, STATUS )
      CALL PAR_PUT0C( 'SPECIFIED_CALIB', SPECIFIED_CALIB, STATUS )
      CALL PAR_PUT0C( 'SPECIFIED_STD', SPECIFIED_STD, STATUS )
      CALL PAR_PUT0C( 'MASK', MASK, STATUS )
      CALL PAR_PUT0C( 'LINCOEFFS', LINCOEFFS, STATUS )
      CALL PAR_PUT0L( 'VARIANCE_WT', VARIANCE_WT, STATUS )
      CALL PAR_PUT0L( 'ADD_IN_PAIRS', ADD_IN_PAIRS, STATUS )
      CALL PAR_PUT0C( 'ERRORS', ERRORS, STATUS )
      CALL PAR_PUT0R( 'SKY_WT', SKY_WT, STATUS )
      CALL PAR_PUT0C( 'PF_POLYFIT', PF_POLYFIT, STATUS )
      CALL PAR_PUT0L( 'PF_WEIGHT', PF_WEIGHT, STATUS )
      CALL PAR_PUT0I( 'PF_DEGREE', PF_DEGREE, STATUS )
      CALL PAR_PUT0I( 'PF_NREJECT', PF_NREJECT, STATUS )
      CALL PAR_PUT0I( 'PF_SAYS1', PF_SAYS1, STATUS )
      CALL PAR_PUT0I( 'PF_SAYE1', PF_SAYE1, STATUS )
      CALL PAR_PUT0I( 'PF_SAYS2', PF_SAYS2, STATUS )
      CALL PAR_PUT0I( 'PF_SAYE2', PF_SAYE2, STATUS )
      CALL PAR_PUT0I( 'PF_SAYS3', PF_SAYS3, STATUS )
      CALL PAR_PUT0I( 'PF_SAYE3', PF_SAYE3, STATUS )
      CALL PAR_PUT0I( 'PF_SAYS4', PF_SAYS4, STATUS )
      CALL PAR_PUT0I( 'PF_SAYE4', PF_SAYE4, STATUS )

*   Write the parameters controlling the flat field normalisation.
      CALL PAR_PUT0C( 'NORM_METHOD', NORM_METHOD, STATUS )
      CALL PAR_PUT0I( 'ORDER', ORDER, STATUS )
      CALL PAR_PUT0I( 'BOXSIZE', BOXSIZE, STATUS )

*   Write the parameters controlling the wavelength calibration.
      CALL PAR_PUT0C( 'LAMBDA_METHOD', LAMBDA_METHOD, STATUS )

*   Write the extract spc parameters
      CALL PAR_PUT0R( 'SPC_ROW1S', SPC_ROW1S, STATUS )
      CALL PAR_PUT0R( 'SPC_ROW1E', SPC_ROW1E, STATUS )
      CALL PAR_PUT0R( 'SPC_ROW2S', SPC_ROW2S, STATUS )
      CALL PAR_PUT0R( 'SPC_ROW2E', SPC_ROW2E, STATUS )
      CALL PAR_PUT0R( 'SPC_ROW3S', SPC_ROW3S, STATUS )
      CALL PAR_PUT0R( 'SPC_ROW3E', SPC_ROW3E, STATUS )
      CALL PAR_PUT0L( 'SPC_INVERT', SPC_INVERT, STATUS )
      CALL PAR_PUT0C( 'SPC_ALGORITHM', SPC_ALGORITHM, STATUS )

*   Write the parameters controlling the line fitting.
      CALL PAR_PUT0I( 'AFIT_NROWS', AFIT_NROWS, STATUS )
      CALL PAR_PUT0I( 'AFIT_ROW1', AFIT_ROW1, STATUS )
      CALL PAR_PUT0I( 'AFIT_ROW2', AFIT_ROW2, STATUS )
      CALL PAR_PUT0R( 'AFIT_XSTART', AFIT_XSTART, STATUS )
      CALL PAR_PUT0R( 'AFIT_XEND', AFIT_XEND, STATUS )

      END
