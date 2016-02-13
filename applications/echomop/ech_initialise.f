      SUBROUTINE ECH_INITIALISE( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_INITIALISE

*  Purpose:
*     Initialise environment interfaces etc.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     25-JUL-1996 (MJC):
*       Moved DATA statements for F77 compliance.
*     26-OCT-1996 (MJC):
*       Initialise GRAPHICS_SETUP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_SERVER.INC'
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_ECHOMOP.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Arguments:
      INTEGER STATUS

*  Local Variables:
      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER DINDEX
      INTEGER ECH_OBJ_IND

      CHARACTER*255 AUTO_OPTIONS

*  Data statements:
      DATA NEXT_1D_OPTION
     :     / 0 ,
     :       2 ,  3  ,  4  ,  5  ,   6  ,
     :       7 ,  8  ,  9  , 10  ,  11  ,
     :      12 , 14  ,  0  , 30  ,  -4  ,
     :      -4 , -5  ,  0  , -4  , -10  ,
     :       0 ,  0  ,  0  ,  0  ,   0  ,
     :       0 ,  0  ,  0  ,  0  ,   0  ,
     :       0 ,  0  /

      DATA NEXT_2D_OPTION
     :     / 0 ,
     :       2 ,  3  ,  4  ,  5  ,   6  ,
     :       7 ,  8  ,  9  , 10  ,  13  ,
     :       0 ,  0  , 14  , 30  ,  -4  ,
     :      -4 , -5  ,  0  , -4  , -10  ,
     :       0 ,  0  ,  0  ,  0  ,   0  ,
     :       0 ,  0  ,  0  ,  0  ,   0  ,
     :       0 ,  0  /

      DATA SUBMENU_OPTIONS
     :   /
     :   'Submenu_1' ,             ! 1
     :   'ECH_CHECK_FRAME' ,       ! 2
     :   'ECH_DECOSMIC_1',         ! 3
     :   'ECH_DETERMINE_SLOPE',    ! 4
     :   'ECH_COUNT_ORDERS',       ! 5
     :   'ECH_LOCATE_ORDERS',      ! 6
     :   'Submenu_2',              ! 7
     :   'ECH_GET_SLIT_EXTENT',    ! 8
     :   'ECH_GET_OBJ_PROFILE',    ! 9
     :   'Submenu_3',              ! 10
     :   'ECH_GET_REF_FWHM',       ! 11
     :   'ECH_LOCATE_REF_LINES',   ! 12
     :   'Submenu_4',              ! 13
     :   'ECH_FIT_REF_FWHMS',      ! 14
     :   'ECH_GEN_REBIN_SCALE',    ! 15
     :   'ECH_SCRUNCH_OBJ',        ! 16
     :   'ECH_SCRUNCH_ARC',        ! 17
     :   'Submenu_5',              ! 18
     :   'ECH_GEN_REBIN_SCALE',    ! 19
     :   'ECH_FIT_2D_DISTORTION',  ! 20
     :   'ECH_REBIN_ORDER',        ! 21
     :   'ECH_REBIN_ARCORDER',     ! 22
     :   'ECH_MODEL_SSKY',         ! 23
     :   'ECH_2DEXT_OPT',          ! 24
     :   'Submenu_6',              ! 25
     :   'ECH_FIT_ORDER_BLAZE',    ! 26
     :   'ECH_FLATTEN_ORDERS',     ! 27
     :   'ECH_NORMAL_YBLAZE',      ! 28
     :   'ECH_FLATTEN_YBLAZE',     ! 29
     :   'Submenu_7'               ! 30
     :   /

      DATA SUBMENU_TEXT
     :   /
     :   'Submenu_1                      (ECH_LOCATE)',
     :   'Check frame                    (ECH_FCHECK)',
     :   'Pre-trace Cosmic Ray locate    (ECH_DECOS1)',
     :   'Determine slope                 (ECH_SLOPE)',
     :   'Count orders                    (ECH_COUNT)',
     :   'Locate orders                  (ECH_LOCATE)',
     :   'Submenu_2                     (ECH_SPATIAL)',
     :   'Determine dekker extent        (ECH_DEKKER)',
     :   'Determine object limits        (ECH_OBJECT)',
     :   'Submenu_3                      (ECH_LINLOC)',
     :   'Estimate arc-line fwhm           (ECH_FWHM)',
     :   'Locate arc line candidates      (ECH_LINES)',
     :   'Submenu_4                     (ECH_SCRUNCH)',
     :   'Fit resolution using FWHMs    (ECH_FITFWHM)',
     :   'Calculate a linear scale       (ECH_WSCALE)',
     :   'Scrunch 1-D object orders      (ECH_SCROBJ)',
     :   'Scrunch 1-D arc orders         (ECH_SCRARC)',
     :   'Submenu_5                      (ECH_WSCALE)',
     :   'Calculate a linear scale      (ECH_SCRSCAL)',
     :   'Model 2-D distortion            (ECH_2DFIT)',
     :   'Rebin object orders             (ECH_REB2D)',
     :   'Rebin arc orders                (ECH_REB2D)',
     :   'Model rebinned sky             (ECH_REBSKY)',
     :   'Extract 2-D corrected data      (ECH_EXT2D)',
     :   'Submenu_6                       (ECH_BLAZE)',
     :   'Model order blaze function     (ECH_FITBLZ)',
     :   'Apply blaze correction          (ECH_DOBLZ)',
     :   'Calculate Y blaze correction  (ECH_FITYBLZ)',
     :   'Apply Y blaze correction       (ECH_DOYBLZ)',
     :   'Submenu_7'
     :   /

*   The array OPTION_MODULE consists of a list of action names or
*   special values.  The index is the `echmenu' menu number, so that
*   this array is what maps menu selections to operations.  If the array
*   value is not a special value, then echomop.f (around line 814) calls
*   ech_echomop_option with the array value as argument.  That
*   subroutine consists (more or less) of a big if statement, which performs
*   the action; the subroutines it uses to do this typically, but
*   not necessarily, have the same name as the action.  If the array
*   value starts `Submenu_', then echomop.f (line 747) searches through
*   array SUBMENU_OPTIONS above for an entry with the same name, then
*   performs each of the subsequent actions (up to the next entry
*   starting `Submenu_') via calls to ech_echomop_option.  The other
*   special values, such as `ECHOMOP_Per_Order' or `ECHOMOP_$Command',
*   are handled specially in echomop.f (line 695).
      DATA OPTION_MODULE
     :   /
     :   ' ',                      ! 0
     :   'Submenu_1',              ! 1
     :   'ECH_TRACE_ORDER',        ! 2
     :   'ECH_FIT_ORDER_TRACE',    ! 3
     :   'Submenu_2',              ! 4
     :   'ECH_MODEL_FLAT',         ! 5
     :   'ECH_MODEL_SKY',          ! 6
     :   'ECH_MODEL_PROFILE',      ! 7
     :   'ECH_EXT_OPT',            ! 8
     :   'Submenu_3',              ! 9
     :   'ECH_WAVELENGTH_CALIB',   ! 10
     :   'Submenu_6',              ! 11
     :   'Submenu_4',              ! 12
     :   'Submenu_5',              ! 13
     :   'ECH_WRITE_RESULTS',      ! 14
     :   'ECH_PLOT_TRACES',        ! 15
     :   'ECH_TRACE_CONSISTENCY',  ! 16
     :   'ECH_DECOSMIC_2',         ! 17
     :   'ECH_IMAGE_COSMIC',       ! 18
     :   'ECH_EXTR_QUICK',         ! 19
     :   'ECH_WAVE_CONSISTENCY',   ! 20
     :   'ECH_MULTI_MERGE',        ! 21
     :   'ECH_MODEL_BACK',         ! 22
     :   'ECH_ACTIVE_TUNE',        ! 23
     :   'ECHOMOP_Per_Order',      ! 24
     :   'ECHOMOP_All_Orders',     ! 25
     :   'ECH_DISABLE_ORDER',      ! 26
     :   'ECH_PLOTTER',            ! 27
     :   'ECHOMOP_Full_Menu',      ! 28
     :   'ECHOMOP_$Command',       ! 29
     :   'ECH_GEN_FLAT',           ! 30
     :   'ECHOMOP_Exit',           ! 31
     :   ' '                       ! 32
     :   /

      DATA OPTION_TEXT
     : / 'HELP/HYPER (ASCII or hypertext help).        ',
*      0
     :   'Start a reduction.               (ECH_LOCATE)',
     :   'Trace orders.                     (ECH_TRACE)',
     :   'Clip fitted traces.              (ECH_FITORD)',
     :   'Determine dekker/object extent. (ECH_SPATIAL)',
     :   'Model flat field.                (ECH_FFIELD)',
*      5
     :   'Model sky.                          (ECH_SKY)',
     :   'Model object profile.           (ECH_PROFILE)',
     :   'Extract orders 1-D.              (ECH_EXTRCT)',
     :   'Locate arc line candidates.      (ECH_LINLOC)',
     :   'Identify features.               (ECH_IDWAVE)',
*      10
     :   'Flatten order shape.             (ECH_FITBLZ)',
     :   'Scrunch to linear scale.        (ECH_SCRUNCH)',
     :   'Model/Extract orders 2-D.         (ECH_2DEXT)',
     :   'Save reduced data.               (ECH_RESULT)',
     :   'Plot order traces.                (ECH_TRPLT)',
*      15
     :   'Check trace consistency.         (ECH_TRCSIS)',
     :   'Post-trace Cosmic Ray locate.    (ECH_DECOS2)',
     :   'Image cosmic ray pixels.         (ECH_DECIMG)',
     :   'Quick-look Extraction.            (ECH_QEXTR)',
     :   'Check wavelength scales.         (ECH_WVCSIS)',
*      20
     :   'Scrunch and merge multiple.      (ECH_MULMRG)',
     :   'Model scattered light.           (ECH_MDLBCK)',
     :   'ADJUST tuning parameters.         (ECH_TUNER)',
     :   'Set single-order operations.        (ECH_ONE)',
     :   'Set all-order operations.           (ECH_ALL)',
*      25
     :   'DISABLE an order.               (ECH_DISABLE)',
     :   'PLOT reduction arrays.             (ECH_PLOT)',
     :   'Full MENU.                                   ',
     :   'System ($) commands.                         ',
     :   'Output balance-factor frame.    (ECH_GENFLAT)',
*      30
     :   'EXIT (alias Q/E/QUIT/EXIT/99).               ',
     :   ' ' /
*.

      STATUS = 0
      REPORT_MODE = RPM_INFO

*  Reset global open-files flags
      RDCTN_FILE_OPEN = .FALSE.
      DATA_FILES_OPEN = .FALSE.
      NUM_REG_VARS = 0
      ACCESS_COUNT = 0
      GRAPHICS_SETUP = .FALSE.

      CALL ECH_DEFINE_MODULES

      CSTR_RDCTN_INPTIM = ' '
      CSTR_RDCTN_SLITIM = ' '
      CSTR_RDCTN_TRACIM = ' '
      CSTR_RDCTN_FFIELD = ' '
      CSTR_RDCTN_ARC = ' '
      CSTR_RDCTN_OBJECT = ' '
      CSTR_RDCTN_STAR = ' '

      AUTO_OPTIONS = ' '

      IF ( CONTEXT_MODE .EQ. CTX_ECHOMOP_SHELL ) THEN
         DINDEX = DEFAULTS_INDICES(
     :        ECH_OBJ_IND( 'TUNE_AUTOMATE' ) )
         CALL ECH_GET_PARAMETER( 'TUNE_AUTOMATE', 'CHAR',
     :        0., .FALSE., AUTO_OPTIONS, DINDEX, STATUS )
      END IF

      IF ( AUTO_OPTIONS .EQ. 'SERVER' ) THEN
         USR_TUNE_SERVER = .TRUE.
         CALL ECH_ACCESS_OBJECT( 'SERVER', 'DECLARE', 'SOCKET',
     :        0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
         CALL ECH_ACCESS_OBJECT( ' ', 'CLOSE', 'ENVIRONMENT',
     :        0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )

      ELSE
         CALL ECH_MODULE_INIT( 'ECHOMOP', STATUS )
      ENDIF

 999  CONTINUE

      END
