*+  CRED4COM.INC - INCLUDE file for CRED4 D-task
*    Description :
*     This is the INCLUDE file which contains the common blocks
*     for the CRED4 CD-task.
*    Programming note :
*     When parameters in the common blocks are altered, the following
*     files MIGHT also need to be edited :-
*      CRED4_DEFINE_NB.FOR                 ! 
*      CRED4_FIND_NB.FOR                   ! 
*      CRED4_LIST_PARAMETERS.FOR           ! 
*      CRED4_READ_CONFIG.FOR               ! 
*      CRED4_READ_NB.FOR                   ! 
*      CRED4_READ_PARAMETERS.FOR           ! 
*      CRED4_PUT_PARAMETERS.FOR            ! 
*      CRED4_WRITE_CONFIG.FOR              ! 
*      CRED4_WRITE_NB.FOR                  ! 
*      CRED4_WRITE_PARAMETERS.FOR          ! 
*      CRED4.IFL                           ! 
*    Authors :
*     J Lightfoot (ROE::JFL)
*     S M Beard  (ROE::SMB)
*     P N Daly (JAC::PND)
*    History :
*     September 1989: Original version.                              (JFL)
*     11-Oct-1989: Prologue added.                                   (SMB)
*     10-Jan-1990: Common block name changed from CRED4 to CRED4COM,
*                  to avoid confusion with name of main program.     (SMB)
*     10-Jan-1990: STRING_SIZE and MAX_REDUCTION_STEPS parameterized.(SMB)
*     26-Jan-1990: FILE_OBS and DIVIDE_BY_FF added.                  (SMB)
*     31-Jan-1990: Modification to cope with changes to P4 SLICE
*                  parameters.                                       (SMB)
*      8-Feb-1990: NBINS, FMIN and FMAX parameters added to DISPLAY
*                  structure, to cope with new HISTOGRAM P4 action.  (SMB)
*      9-Mar-1990: DSA__ERROR parameter added.                       (SMB)
*     28-Mar-1990: INTEGRATION_AVAILABLE and OBSERVATION_AVAILABLE
*                  flags added, to prevent CRED4 from trying to
*                  display files which do not exist.                 (SMB)
*      4-Apr-1990: NEWSPRAY renamed to NEWSURFACE.                   (SMB)
*      3-May-1990: FF_NORM and ORDER parameters added.               (SMB)
*     30-May-1990: Parameters for automatic line fitting added.      (SMB)
*      4-Jun-1990: Completely rewritten to store data reduction
*                  parameters in a noticeboard.                      (SMB)
*      5-Jun-1990: Rewritten again.                                  (SMB)
*      6-Jun-1990: EXECUTE replaced by ASK temporarily to ease
*                  transition to new version.                        (SMB)
*     13-Jun-1990: Length of character items in NBS reduced, after
*                  advice from JAB.                                  (SMB)
*     15-Jun-1990: Phase 1 of major changes completed, as described
*                  in CACT.                                          (SMB)
*     19-Jun-1990: Phase 2 of major changes, as described in CACT:
*                  VAX/VMS structures replaced and common blocks
*                  altered to store new information.                 (SMB)
*     20-Jun-1990: SEQUENCE_SETUP and PAUSE_REDUCTION flags included.(SMB)
*     22-Jun-1990: SLICE_START and SLICE_END parameters had the wrong
*                  type - bug fixed.                                 (SMB)
*     17-Jul-1990: More changes: The "displaying extracted spectrum"
*                  terminology was confusing. Parameters now added to
*                  allow a reduced observation to be displayed up to
*                  4 times using any desired method.                 (SMB)
*     18-Jul-1990: Typing mistakes fixed.                            (SMB)
*     10-Sep-1990: Version number identifier added. This will be
*                  changed each time the format of the noticeboard
*                  and/or configuration file is changed (V1.0).      (SMB)
*      1-Oct-1990: AFIT_NROWS parameter added (V1.1).                (SMB)
*      2-Oct-1990: SUBTRACT_SKY parameter removed and replaced with
*                  ADD_OBS and DISPLAY_GRP (V1.2).                   (SMB)
*     25-Oct-1990: Group display parameters added (V1.3).            (SMB)
*     31-Oct-1990: DISP_COUNTER added.                               (SMB)
*      1-Nov-1990: DISP_FREQUENCY added (V1.4).                      (SMB)
*      5-Nov-1990: SKY_WT and VARIANCE_WT parameters added (V1.5).   (SMB)
*     13-Nov-1990: ISTART, IEND, JSTART, JEND removed from graphics
*                  parameters (V1.6).                                (SMB)
*     19-Nov-1990: LAMBDA_METHOD parameter added, for wavelength
*                  calibration (V1.7).                               (SMB)
*     22-Nov-1990: VERBOSE flag added.                               (SMB)
*      3-Dec-1990: MAX_LIN_COEFFS removed.                           (SMB)
*      3-Jan-1991: Modified so that MASK and LINCOEFFS become part
*                  of the configuration (V1.8).                      (SMB)
*      2-Feb-1991: ADD_IN_PAIRS and ERRORS parameters added (V1.9).  (SMB)
*     26-Jun-1991: Add Point_source_options into CRED4_NB            (PND)
*     31-Jul-1991: BOXSIZE parameter added (V2.0).                   (SMB)
*     19-Feb-1992: Add POLYFITTED into common block                  (PND)
*     14-Apr-1992: Add BIAS_MODE, DARK_MODE, FLAT_MODE, CALIB_MODE,
*                  STANDARD_MODE, SPECIFIED_BIAS, SPECIFIED_DARK,
*                  SPECIFIED_FLAT, SPECIFIED_CALIB and SPECIFIED_STD
*                  parameters. SAVE the common blocks and reduce the
*                  number of continuation lines, to improve
*                  portability. MSG_VAL_LEN added, so that maximum
*                  use can be made of the VALUE string. (V2.1).      (SMB)
*     17-Jul-1992: Add SUBTRACT_BIAS and ARCHIVE_OBS                 (PND)
*     10-Dec-1993: Add task aliases                                  (PND)
*      3-Jan-1994: Add contouring                                    (PND)
*     22-Mar-1994: Add automated extract spc                         (PND,KLK)
*     28-Jul-1994: Rename QMAN_ALIAS, add QMAN_OK logical            (PND)
*     29-Jul-1994: Strip out all graphics stuff                      (PND)
*      2-Aug-1994: Reformatted for Unix Port                         (PND)
*    endhistory
*-

* Global constants :
      INTEGER MSG_VAL_LEN            ! Length of ADAM VALUE string.
      PARAMETER ( MSG_VAL_LEN = 444 ) 
      INTEGER STRING_SIZE            ! Size of string to contain dr command
      PARAMETER ( STRING_SIZE = 80 )  
      INTEGER DSA__ERROR             ! DSA error status code 
      PARAMETER ( DSA__ERROR = 5 )    
      INTEGER NBS_CLEN               ! Length of character items in NBS
      PARAMETER ( NBS_CLEN = 80 )
      INTEGER NUM_DISPLAYS           ! The number of displays available
      PARAMETER ( NUM_DISPLAYS = 8 )
      INTEGER RETVAL                 ! Re-schedule interval in milliseconds
      PARAMETER ( RETVAL = 2000 )

* Environmental variables
      CHARACTER*1 PREFIX             ! Prefix for EVs
      CHARACTER*1 SEPARATOR          ! File separator
      CHARACTER*20 SYSNAME           ! System name
      CHARACTER*( STRING_SIZE )
     :  CGS4_FORMAT,                 ! Home directory
     :  CGS4_HOME,                   ! Home directory
     :  CGS4_CT,                     ! Colour table directory
     :  CGS4_ENG,                    ! Engineering directory
     :  CGS4_CONFIG,                 ! Config directory
     :  CGS4_MASKS,                  ! Masks directory
     :  CGS4_INDEX,                  ! Index directory
     :  CGS4_DATA,                   ! Data directory
     :  CGS4_DATE,                   ! UT date
     :  RGDIR,                       ! Reduced group directory
     :  RODIR,                       ! Reduced observation directory
     :  RIDIR,                       ! Reduced integration directory
     :  IDIR,                        ! Raw integration directory
     :  ODIR                         ! Raw observation directory

      COMMON /CRED4SYS/
     :  PREFIX, SEPARATOR, SYSNAME, CGS4_FORMAT, CGS4_HOME, CGS4_CT, CGS4_ENG, CGS4_CONFIG,
     :  CGS4_MASKS, CGS4_INDEX, CGS4_DATA, CGS4_DATE, RGDIR, RODIR, RIDIR, IDIR, ODIR
      SAVE /CRED4SYS/

* Internal CRED4 parameters (character) :
      CHARACTER*15 RED4_ALIAS        ! Alias of RED4 task
      CHARACTER*15 P4_ALIAS          ! Alias of P4 task
      CHARACTER*15 FIGARO_ALIAS      ! Alias of FIGARO task
      CHARACTER*15 UKIRTFIG_ALIAS    ! Alias of FIGARO task
      CHARACTER*15 QMAN_ALIAS        ! Alias of QMAN task
      CHARACTER*40 QMAN_PWRD         ! Password for QMAN task
      CHARACTER*40 QMAN_LWRD         ! Lockword for QMAN task
      CHARACTER*40 NOTICEBOARD       ! Noticeboard name

      COMMON /CRED4CHAR/
     :  RED4_ALIAS, P4_ALIAS, FIGARO_ALIAS, UKIRTFIG_ALIAS,
     :  QMAN_ALIAS, QMAN_PWRD, QMAN_LWRD, NOTICEBOARD
      SAVE /CRED4CHAR/

* Internal CRED4 parameters (logical) :
      LOGICAL VERBOSE                ! T if verbose messages required.
      LOGICAL RED4_ACTIVE            ! T if a RED4 action is in progress
      LOGICAL P4_ACTIVE              ! T if a P4 action is in progress
      LOGICAL REDUCTION_OK           ! T if reduction OK up to that point
      LOGICAL INTEGRATION_AVAILABLE  ! T if an integration is available
      LOGICAL OBSERVATION_AVAILABLE  ! T if an observation is available
      LOGICAL GROUP_AVAILABLE        ! T if a group is available
      LOGICAL SPECTRUM_AVAILABLE     ! T if a spectrum is available
      LOGICAL NEWGROUP               ! T if a new group is to be started
      LOGICAL SEQUENCE_SETUP         ! T when the DR sequence set-up
      LOGICAL PAUSE_REDUCTION        ! T if the data reduction is paused
      LOGICAL POLYFITTED             ! T if enhanced polysky subtraction worked
      LOGICAL QMAN_OK                ! T if we can talk to QMAN

      COMMON /CRED4LOG/ 
     :  VERBOSE, RED4_ACTIVE, P4_ACTIVE, REDUCTION_OK, INTEGRATION_AVAILABLE, 
     :  OBSERVATION_AVAILABLE, GROUP_AVAILABLE, SPECTRUM_AVAILABLE, 
     :  NEWGROUP, SEQUENCE_SETUP, PAUSE_REDUCTION, POLYFITTED, QMAN_OK
      SAVE /CRED4LOG/

* Internal CRED4 parameters (integer) :
      INTEGER RED4_MESSID            ! ID of message to RED4 
      INTEGER P4_MESSID              ! ID of message to P4 
      INTEGER FIGARO_MESSID          ! ID of message to FIGARO 
      INTEGER UKIRTFIG_MESSID        ! ID of message to UKIRTFIG
      INTEGER QMAN_MESSID            ! ID of message to QMAN 
      INTEGER RED4_PATH              ! path to RED4 task
      INTEGER P4_PATH                ! path to P4 task
      INTEGER FIGARO_PATH            ! path to FIGARO task
      INTEGER UKIRTFIG_PATH          ! path to UKIRTFIG task
      INTEGER QMAN_PATH              ! path to QMAN task

      COMMON /CRED4INT/ 
     :  RED4_MESSID, P4_MESSID, FIGARO_MESSID, UKIRTFIG_MESSID, 
     :  QMAN_MESSID, RED4_PATH, P4_PATH,  FIGARO_PATH, UKIRTFIG_PATH, 
     :  QMAN_PATH
      SAVE /CRED4INT/

* Data reduction control parameters and notice board IDs :
      CHARACTER*( NBS_CLEN ) SUBTRACT_BIAS  ! Subtract BIAS frame.
      CHARACTER*( NBS_CLEN ) SUBTRACT_DARK  ! Subtract DARK frame.
      CHARACTER*( NBS_CLEN ) ADD_INT        ! Add integration to obs.
      CHARACTER*( NBS_CLEN ) FILE_OBS       ! File observation
      CHARACTER*( NBS_CLEN ) ARCHIVE_OBS    ! Archive the observation.
      CHARACTER*( NBS_CLEN ) NORMALISE_FF   ! Normalise a flat field.
      CHARACTER*( NBS_CLEN ) DIVIDE_BY_FF   ! Divide by flat field.
      CHARACTER*( NBS_CLEN ) ADD_OBS        ! Add OBJECT or SKY to group
      CHARACTER*( NBS_CLEN ) TO_WAVELENGTH  ! Calibrate X into wavelength.
      CHARACTER*( NBS_CLEN ) DIVIDE_BY_STD  ! Divide by flux standard.
      CHARACTER*( NBS_CLEN ) EXTRACT_SPC    ! Extract a 1-D spectrum.
      CHARACTER*( NBS_CLEN ) AUTOFIT        ! Autolocate emission lines.

      COMMON /CRED4RED1/ 
     :  SUBTRACT_BIAS,SUBTRACT_DARK, ADD_INT, FILE_OBS, ARCHIVE_OBS, 
     :  NORMALISE_FF, DIVIDE_BY_FF, ADD_OBS, TO_WAVELENGTH, 
     :  DIVIDE_BY_STD,  EXTRACT_SPC, AUTOFIT
      SAVE /CRED4RED1/ 

      INTEGER
     :  NB_TOPID, SUBTRACT_BIAS_ID, SUBTRACT_DARK_ID, 
     :  ADD_INT_ID, FILE_OBS_ID, ARCHIVE_OBS_ID, NORMALISE_FF_ID, 
     :  DIVIDE_BY_FF_ID, ADD_OBS_ID, TO_WAVELENGTH_ID, DIVIDE_BY_STD_ID, 
     :  EXTRACT_SPC_ID, AUTOFIT_ID

      COMMON /CRED4NBS1/
     :  NB_TOPID, SUBTRACT_BIAS_ID, SUBTRACT_DARK_ID, 
     :  ADD_INT_ID, FILE_OBS_ID, ARCHIVE_OBS_ID, NORMALISE_FF_ID, 
     :  DIVIDE_BY_FF_ID, ADD_OBS_ID, TO_WAVELENGTH_ID, DIVIDE_BY_STD_ID, 
     :  EXTRACT_SPC_ID, AUTOFIT_ID
      SAVE /CRED4NBS1/

* Data reduction control parameters and notice board IDs :
      CHARACTER*( NBS_CLEN ) DISPLAY_INT( 0:NUM_DISPLAYS ) 
      CHARACTER*( NBS_CLEN ) DISPLAY_OBS( 0:NUM_DISPLAYS ) 
      CHARACTER*( NBS_CLEN ) DISPLAY_GRP( 0:NUM_DISPLAYS ) 
      CHARACTER*( NBS_CLEN ) DISPLAY_SPC( 0:NUM_DISPLAYS )

      COMMON /CRED4RED2/ 
     :  DISPLAY_INT, DISPLAY_OBS, DISPLAY_GRP, DISPLAY_SPC
      SAVE /CRED4RED2/ 

      INTEGER
     : DISPLAY_INT_ID( 0:NUM_DISPLAYS ), DISPLAY_OBS_ID( 0:NUM_DISPLAYS ),
     : DISPLAY_GRP_ID( 0:NUM_DISPLAYS ), DISPLAY_SPC_ID( 0:NUM_DISPLAYS )

      COMMON /CRED4NBS2/
     : DISPLAY_INT_ID, DISPLAY_OBS_ID, DISPLAY_GRP_ID, DISPLAY_SPC_ID
      SAVE /CRED4NBS2/

* Data reduction control parameters and notice board IDs :
      CHARACTER*( NBS_CLEN ) BIAS_MODE        ! Mode for a BIAS 
      CHARACTER*( NBS_CLEN ) DARK_MODE        ! Mode for a DARK 
      CHARACTER*( NBS_CLEN ) FLAT_MODE        ! Mode for a FLAT
      CHARACTER*( NBS_CLEN ) CALIB_MODE       ! Mode for a CALIB
      CHARACTER*( NBS_CLEN ) STANDARD_MODE    ! Mode for a STANDARD
      CHARACTER*( NBS_CLEN ) SPECIFIED_BIAS   ! SpecBIAS observation
      CHARACTER*( NBS_CLEN ) SPECIFIED_DARK   ! Spec DARK observation
      CHARACTER*( NBS_CLEN ) SPECIFIED_FLAT   ! Spec flat-field observation
      CHARACTER*( NBS_CLEN ) SPECIFIED_CALIB  ! Spec wavelength calibration 
      CHARACTER*( NBS_CLEN ) SPECIFIED_STD    ! Specified STANDARD group
      CHARACTER*( NBS_CLEN ) MASK             ! Name of bad pixel mask
      CHARACTER*( NBS_CLEN ) LINCOEFFS        ! Name of linear coeffs file

      COMMON /CRED4RED3/
     :  BIAS_MODE, DARK_MODE, FLAT_MODE, CALIB_MODE, STANDARD_MODE, 
     :  SPECIFIED_BIAS, SPECIFIED_DARK, SPECIFIED_FLAT, SPECIFIED_CALIB, 
     :  SPECIFIED_STD, MASK, LINCOEFFS
      SAVE /CRED4RED3/

      INTEGER
     :  BIAS_MODE_ID, DARK_MODE_ID, FLAT_MODE_ID, CALIB_MODE_ID, 
     :  STANDARD_MODE_ID, SPECIFIED_BIAS_ID, SPECIFIED_DARK_ID, 
     :  SPECIFIED_FLAT_ID, SPECIFIED_CALIB_ID, SPECIFIED_STD_ID, 
     :  MASK_ID, LINCOEFFS_ID

      COMMON /CRED4NBS3/
     :  BIAS_MODE_ID, DARK_MODE_ID, FLAT_MODE_ID, CALIB_MODE_ID, 
     :  STANDARD_MODE_ID, SPECIFIED_BIAS_ID, SPECIFIED_DARK_ID, 
     :  SPECIFIED_FLAT_ID, SPECIFIED_CALIB_ID, SPECIFIED_STD_ID, 
     :  MASK_ID, LINCOEFFS_ID
      SAVE /CRED4NBS3/

* Data reduction control parameters and notice board IDs :
      CHARACTER*( NBS_CLEN ) ERRORS         ! The error propagation method
      CHARACTER*( NBS_CLEN ) PF_POLYFIT     ! Polyfit method 
      CHARACTER*( NBS_CLEN ) NORM_METHOD    ! Normalisation method 
      CHARACTER*( NBS_CLEN ) LAMBDA_METHOD  ! Wavelength calibration method
      CHARACTER*( NBS_CLEN ) SPC_ALGORITHM  ! Use bright/faint algorithm

      COMMON /CRED4RED4/
     :  ERRORS, PF_POLYFIT, NORM_METHOD, LAMBDA_METHOD, SPC_ALGORITHM
      SAVE /CRED4RED4/

      INTEGER
     :  ERRORS_ID, PF_POLYFIT_ID, NORM_METHOD_ID, LAMBDA_METHOD_ID, 
     :  SPC_ALGORITHM_ID

      COMMON /CRED4NBS4/
     :  ERRORS_ID, PF_POLYFIT_ID, NORM_METHOD_ID, LAMBDA_METHOD_ID, 
     :  SPC_ALGORITHM_ID
      SAVE /CRED4NBS4/

* Data reduction control parameters and notice board IDs :
      INTEGER PF_DEGREE     ! Degree of polynomial to be fitted
      INTEGER PF_NREJECT    ! Number of points to reject
      INTEGER PF_SAYS1      ! Start of first  sky area
      INTEGER PF_SAYE1      ! End   of first  sky area
      INTEGER PF_SAYS2      ! Start of second sky area
      INTEGER PF_SAYE2      ! End   of second sky area
      INTEGER PF_SAYS3      ! Start of third  sky area
      INTEGER PF_SAYE3      ! End   of third  sky area
      INTEGER PF_SAYS4      ! Start of fourth sky area
      INTEGER PF_SAYE4      ! End   of fourth sky area
      INTEGER ORDER         ! The required polynomial order for normalisation
      INTEGER BOXSIZE       ! The required smooth box size for normalisation
      INTEGER AFIT_NROWS    ! Number of rows to be averaged together
      INTEGER AFIT_ROW1     ! First row to be extracted and fitted
      INTEGER AFIT_ROW2     ! Second row to be extracted and fitted

      COMMON /CRED4RED5/
     :  PF_DEGREE, PF_NREJECT, PF_SAYS1, PF_SAYE1, PF_SAYS2, PF_SAYE2,
     :  PF_SAYS3, PF_SAYE3, PF_SAYS4, PF_SAYE4, ORDER, BOXSIZE, 
     :  AFIT_NROWS, AFIT_ROW1, AFIT_ROW2
      SAVE /CRED4RED5/

      INTEGER
     :  PF_DEGREE_ID, PF_NREJECT_ID, PF_SAYS1_ID, PF_SAYE1_ID, 
     :  PF_SAYS2_ID, PF_SAYE2_ID, PF_SAYS3_ID, PF_SAYE3_ID, 
     :  PF_SAYS4_ID, PF_SAYE4_ID, ORDER_ID, BOXSIZE_ID, 
     :  AFIT_NROWS_ID, AFIT_ROW1_ID, AFIT_ROW2_ID

      COMMON /CRED4NBS5/
     :  PF_DEGREE_ID, PF_NREJECT_ID, PF_SAYS1_ID, PF_SAYE1_ID, 
     :  PF_SAYS2_ID, PF_SAYE2_ID, PF_SAYS3_ID, PF_SAYE3_ID, 
     :  PF_SAYS4_ID, PF_SAYE4_ID, ORDER_ID, BOXSIZE_ID, 
     :  AFIT_NROWS_ID, AFIT_ROW1_ID, AFIT_ROW2_ID
      SAVE /CRED4NBS5/

* Data reduction control parameters and notice board IDs :
      LOGICAL VARIANCE_WT    ! T if variance weighting enabled
      LOGICAL ADD_IN_PAIRS   ! T if OBJECT and SKY are added  into pairs
      LOGICAL PF_WEIGHT      ! Weight the data according to errors?
      LOGICAL SPC_INVERT     ! Invert the spectrum

      COMMON /CRED4RED6/
     :  VARIANCE_WT, ADD_IN_PAIRS, PF_WEIGHT, SPC_INVERT
      SAVE /CRED4RED6/

      INTEGER
     :  VARIANCE_WT_ID, ADD_IN_PAIRS_ID, PF_WEIGHT_ID, SPC_INVERT_ID

      COMMON /CRED4NBS6/
     :  VARIANCE_WT_ID, ADD_IN_PAIRS_ID, PF_WEIGHT_ID, SPC_INVERT_ID
      SAVE /CRED4NBS6/

* Data reduction control parameters and notice board IDs :
      REAL SKY_WT        ! Weighting factor to be applied to all SKY frames
      REAL SPC_ROW1S     ! First extracted spectrum row start
      REAL SPC_ROW1E     ! First extracted spectrum row end
      REAL SPC_ROW2S     ! Second extracted spectrum row start
      REAL SPC_ROW2E     ! Second extracted spectrum row end
      REAL SPC_ROW3S     ! Third extracted spectrum row start
      REAL SPC_ROW3E     ! Third extracted spectrum row end
      REAL AFIT_XSTART   ! First X value to be searched for lines
      REAL AFIT_XEND     ! Last X value to be searched for lines

      COMMON /CRED4RED7/ 
     :  SKY_WT, SPC_ROW1S, SPC_ROW1E, SPC_ROW2S, SPC_ROW2E, SPC_ROW3S, 
     :  SPC_ROW3E, AFIT_XSTART, AFIT_XEND
      SAVE /CRED4RED7/

      INTEGER
     :  SKY_WT_ID, SPC_ROW1S_ID, SPC_ROW1E_ID, SPC_ROW2S_ID, SPC_ROW2E_ID, 
     :  SPC_ROW3S_ID, SPC_ROW3E_ID, AFIT_XSTART_ID, AFIT_XEND_ID

      COMMON /CRED4NBS7/
     :  SKY_WT_ID, SPC_ROW1S_ID, SPC_ROW1E_ID, SPC_ROW2S_ID, SPC_ROW2E_ID, 
     :  SPC_ROW3S_ID, SPC_ROW3E_ID, AFIT_XSTART_ID, AFIT_XEND_ID
      SAVE /CRED4NBS7/

* NBS flags IDs
      INTEGER
     :  ABORT_REDUCTION_ID, STOP_REDUCTION_ID, PAUSE_REDUCTION_ID,
     :  REDUCING_ID, CRED4_BUSY_ID, RED4_BUSY_ID, P4_BUSY_ID

      COMMON /CRED4NBS8/ 
     :  ABORT_REDUCTION_ID, STOP_REDUCTION_ID, PAUSE_REDUCTION_ID, 
     :  REDUCING_ID, CRED4_BUSY_ID, RED4_BUSY_ID, P4_BUSY_ID
      SAVE /CRED4NBS8/

*    End of common block
