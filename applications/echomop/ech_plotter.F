#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE ECH_PLOTTER( N_ORDERS, MAXIMUM_POLY, POLYNOMIALS,
     :           MXSKYPIX, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_PLOTTER

*  Purpose:
*     Interactive plotting of database objects.

*  Description:
*     The user can interactively select objects for plotting.
*     The style of the plot can be selected.  Two objects can be
*     plotted against each other.  Plots can be overlaid on earlier
*     plots.

*  Invocation:
*     CALL ECH_PLOTTER( N_ORDERS, MAXIMUM_POLY, POLYNOMIALS,
*     :                 MXSKYPIX, STATUS )

*  Arguments:
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     POLYNOMIALS = DOUBLE  (Given)
*        Polynomials for trace fits.
*     MXSKYPIX = INTEGER (Given)
*        Maximum pixels in dekker.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     BLY: Martin Bly (Starlink, RAL)
*     NG: Norman Gray (Starlink, Glasgow)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     26-MAR-1997 (MJC):
*       Complete reworking.
*     02-MAR-1998 (BLY):
*       Correction of READ ( ... '( F )' ...) statements to '( F14.4 )'.
*     05-JUL-1999 (NG):
*       Removed READONLY control from OPEN (unsupported in g77)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_USE_DIMEN.INC'

*  Arguments Given:
      INTEGER N_ORDERS
      INTEGER MXSKYPIX
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION POLYNOMIALS( MAXIMUM_POLY, N_ORDERS )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER UMAX_POINTS_PER_AXIS
      PARAMETER ( UMAX_POINTS_PER_AXIS = 16378 )

      INTEGER NUM_ABBREV
      PARAMETER ( NUM_ABBREV = 17 )

*  Local Variables:
      DOUBLE PRECISION YCO

      REAL XDATA( UMAX_POINTS_PER_AXIS )
      REAL YTRACE
      REAL DIST
      REAL VALUE
      REAL XM
      REAL XH
      REAL YM
      REAL YH
      REAL DATA_XM
      REAL DATA_XH
      REAL DATA_YM
      REAL DATA_YH
      REAL DATA_MIN
      REAL DATA_MAX

      INTEGER IDIMS( MAX_DIMENSIONS )
      INTEGER REQ_DIMS( MAX_DIMENSIONS )
      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER INUM_DIM
      INTEGER IDIM
      INTEGER IEND
      INTEGER ILEN
      INTEGER MAP_SIZE
      INTEGER MAP_OFFSET
      INTEGER I
      INTEGER REBIN
      INTEGER IWX
      INTEGER IWY
      INTEGER IORD
      INTEGER SET_OBJECT_SIZE
      INTEGER IDUM
      INTEGER ADDR_X
      INTEGER ADDR_Y
      INTEGER PLT_ADDRX
      INTEGER PLT_ADDRY
      INTEGER IMG_ADDRESS
      INTEGER IMG_ADDRESS2
      INTEGER IMG_HANDLE
      INTEGER IMG_HANDLE2
      INTEGER IMG_SIZE
      INTEGER BIM_ADDRESS
      INTEGER BIM_HANDLE
      INTEGER BIM_SIZE
      INTEGER ISTAT
      INTEGER IPLEN
      INTEGER IADD
      INTEGER NEAREST
      INTEGER IIND
      INTEGER ISCAN
      INTEGER NEWLEN
      INTEGER OPTIONS
      INTEGER NEW_OBJECT_SIZE
      INTEGER VM_SIZE
      INTEGER TYPE_CODE
      INTEGER IP
      INTEGER IPIND1
      INTEGER IPIND2
      INTEGER IPATH_LEN
      INTEGER AUNIT
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER UNIT

      LOGICAL IGNORE
      LOGICAL HARDCOPY
      LOGICAL OVERLAY
      LOGICAL OVERIMAGE
      LOGICAL CURSOR
      LOGICAL XLIMITS
      LOGICAL DUMP_ASCII
      LOGICAL YLIMITS
      LOGICAL BROWSE
      LOGICAL GRAPH
      LOGICAL UWINDOW
      LOGICAL FROM_FILE
      LOGICAL MENU
      LOGICAL SWAP_AXES

      CHARACTER*128 LIST
      CHARACTER*128 FULL_OBJECT_PATH
      CHARACTER*128 FULL_OBJECT_PATH2
      CHARACTER*128 IMG_NAME
      CHARACTER*128 RAWSTRING
      CHARACTER*128 USTRING_1
      CHARACTER*100 CINDICES
      CHARACTER*80 XLABEL
      CHARACTER*80 YLABEL
      CHARACTER*80 DMP_FORMAT
      CHARACTER*80 DMP_FILE
      CHARACTER*80 DUMMY_REC
      CHARACTER*80 PLOT_COMMANDS
      CHARACTER*40 HELP( NUM_ABBREV )
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*8 STYLE
      CHARACTER*8 NEW_STYLE
      CHARACTER*6 ABBREV( NUM_ABBREV )
      CHARACTER*6 NEW_OBJECT_TYPE
      CHARACTER*6 NEW_OBJECT_TYPE2

*  Functions Called:
      INTEGER CHR_LEN
      INTEGER ECH_WORD_LEN
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR

*  Data Statements:
      DATA HELP / 'Sky spectrum (per order)',
     :            'Sky spectrum variance (per order)',
     :            'Extracted object (per order)',
     :            'Extracted object variance (per order)',
     :            'Scrunched object (per order)',
     :            'Scrunched object variance (per order)',
     :            'Extracted arc (per order)',
     :            'Scrunched arc (per order)',
     :            'Fitted wavelengths (per order)',
     :            'Fitted flat field balances (per order)',
     :            'Fitted sky (per order)',
     :            'Fitted sky variances (per order)',
     :            'Blaze function (per order)',
     :            'Scrunched wavelength (per order)',
     :            'Merged order spectrum',
     :            'Merged order variances',
     :            'Wavelengths for merged spectrum' /
      DATA ABBREV / 'SKY',   'SKYV',  'OBJ',   'OBJV',  'SOBJ',
     :              'SOBJV', 'ARC',   'SARC',  'FWAV',  'FFLT',
     :              'FSKY',  'FSKYV', 'BLZ',   'SWAV',  '1D',
     :              '1DV',   'WAV' /
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )
      IF ( STATUS .NE. 0 ) THEN
         GO TO 999
      END IF

      DO I = 1, MAX_DIMENSIONS
         DUMDIM( I ) = 0
      END DO
      CALL ECH_SETUP_GRAPHICS( STATUS )
      MENU = .TRUE.
      BROWSE = .FALSE.
      HARDCOPY = .FALSE.
      XLIMITS = .FALSE.
      YLIMITS = .FALSE.
      CURSOR = .FALSE.
      SWAP_AXES = .FALSE.
      REBIN = 0
      SET_OBJECT_SIZE = 0
      USTRING_1 = ' '
      STYLE = 'BINS'
      FULL_OBJECT_PATH = ' '
      DMP_FILE = 'ech_plotted.dmp'
      DMP_FORMAT = '( 1X, F20.4, 1X, F20.4 )'
      GRAPH = .TRUE.
      UWINDOW = .FALSE.
      FROM_FILE = .FALSE.

      CALL ECH_TYPEINFO( 'FLOAT', TYPE_CODE, AUNIT )

*  Start of main loop.
 100  CONTINUE
      IF ( REBIN .GT. 1 ) THEN
         CALL CHR_ITOC( REBIN, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Rebin factor is ' // REF_STR1( :NCHAR1 ) //
     :         '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

      ELSE IF ( REBIN .LT. -1 ) THEN
         CALL CHR_ITOC( -REBIN, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Smooth factor is ' // REF_STR1( :NCHAR1 ) //
     :         '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF
      OVERLAY = .FALSE.
      OVERIMAGE = .FALSE.

 102  CONTINUE
      DUMP_ASCII = .FALSE.
      IF ( HARDCOPY ) THEN
         HARDCOPY = .FALSE.
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0., 0., 0., 0., ' ', ' ',
     :        ' ', 0., 0., GRPH_CLOSE_DEV, ' ', STATUS )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, FLOAT( IWX ), FLOAT( IWY ),
     :        0., 0., ' ', ' ', ' ', 0., 0., GRPH_OPEN_DEV,
     :        GRAPHICS_DEVICE_NAME, STATUS )
      END IF

*  Check for BROWSE mode.
      IF ( BROWSE ) GO TO 200

*  Normal mode, display the menu.
      CALL ECH_PLOTTER_MAIN_MENU( MENU, CURSOR, GRAPH, XLIMITS, YLIMITS,
     :     UWINDOW, XM, XH, YM, XH, STYLE )

*  Get next plot command.
      IF ( FROM_FILE ) THEN
         READ ( UNIT, '( A )', END = 998 ) FULL_OBJECT_PATH
         REPORT_STRING = PLOT_COMMANDS( :CHR_LEN( PLOT_COMMANDS ) ) //
     :         ':' // FULL_OBJECT_PATH( :CHR_LEN( FULL_OBJECT_PATH ) )
         CALL ECH_REPORT( 0, REPORT_STRING )

      ELSE
         FULL_OBJECT_PATH = USTRING_1

*     Try to move the default value onto the next order if applicable.
         IF ( FULL_OBJECT_PATH .NE. ' ' ) THEN
            IPATH_LEN = CHR_LEN( FULL_OBJECT_PATH )
            IF ( FULL_OBJECT_PATH( IPATH_LEN: ) .EQ. ']' ) THEN
               IP = IPATH_LEN
               DO WHILE ( IP .GT. 0 )
                  IF ( FULL_OBJECT_PATH( IP:IP ) .EQ. '[' .OR.
     :                 FULL_OBJECT_PATH( IP:IP ) .EQ. ',' ) THEN
                     GO TO 42
                  END IF
                  IP = IP - 1
               END DO
   42          CONTINUE
               IF ( IP .GT. 0 ) THEN
                  IP = IP + 1
                  CALL CHR_CTOI( FULL_OBJECT_PATH( IP: IPATH_LEN - 1 ),
     :                 IPIND1, STATUS )
                  IPIND1 = IPIND1 + 1
                  IF ( IPIND1 .EQ. N_ORDERS + 1 ) THEN
                     IPIND1 = 1
                  END IF
                  CALL CHR_ITOC( IPIND1, FULL_OBJECT_PATH( IP: ),
     :                 IPIND2 )
                  IPIND2 = IP + IPIND2
                  FULL_OBJECT_PATH( IPIND2: ) = '] '
               END IF
            END IF
         END IF
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Option',
     :        'CHAR', 0., .FALSE., FULL_OBJECT_PATH, 0, STATUS )
      END IF
      IF ( FULL_OBJECT_PATH .EQ. ' ' ) FULL_OBJECT_PATH = USTRING_1
      RAWSTRING = FULL_OBJECT_PATH
      CALL CHR_UCASE( FULL_OBJECT_PATH )
      GO TO 300

*  Browse mode, display the menu.
  200 CALL ECH_PLOTTER_BROWSE_MENU( MENU )

  105 USER_INPUT_CHAR = '|'
      GRAPH = .TRUE.
      CALL ECH_READ_GRPH_CURSOR( ISTAT )

*  'N' - set number of samples to plot.
      IF ( USER_INPUT_CHAR .EQ. 'N' ) THEN
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=No. X samples',
     :        'INT', VALUE, .FALSE., ' ', 0, STATUS )
         SET_OBJECT_SIZE = INT( VALUE )
         GO TO 200

*  'R' - range of Z values.
      ELSE IF ( USER_INPUT_CHAR .EQ. 'R' ) THEN
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Image minimum',
     :        'REAL', VALUE, .FALSE., ' ', 0, STATUS )
         DATA_MIN = VALUE
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Image maximum',
     :        'REAL', VALUE, .FALSE., ' ', 0, STATUS )
         DATA_MAX = VALUE
         GO TO 200

*  'L' - Set display limits in X and Y.
      ELSE IF ( USER_INPUT_CHAR .EQ. 'L' ) THEN
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Minimum X',
     :        'INT', XM, .FALSE., ' ', 0, STATUS )
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Maximum X',
     :        'INT', XH, .FALSE., ' ', 0, STATUS )
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Minimum Y',
     :        'INT', YM, .FALSE., ' ', 0, STATUS )
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Maximum Y',
     :        'INT', YH, .FALSE., ' ', 0, STATUS )
         IF ( INT( XM ) .EQ. 0 .AND. INT( XH ) .EQ. 0 ) THEN
            XLIMITS = .FALSE.
            CALL ECH_REPORT( 0,
     :           ' X-range will be scaled automatically.' )

         ELSE
            CALL CHR_ITOC( INT( XM ), REF_STR1, NCHAR1 )
            CALL CHR_ITOC( INT( XH ), REF_STR2, NCHAR2 )
            REPORT_STRING = ' X-axis limited from ' //
     :            REF_STR1( :NCHAR1 ) // ' to ' //
     :            REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            XLIMITS = .TRUE.
         END IF
         IF ( INT( YM ) .EQ. 0 .AND. INT( YH ) .EQ. 0 ) THEN
            YLIMITS = .FALSE.
            CALL ECH_REPORT( 0,
     :           ' Y-range will be scaled automatically.' )

         ELSE
            CALL CHR_ITOC( INT( YM ), REF_STR1, NCHAR1 )
            CALL CHR_ITOC( INT( YH ), REF_STR2, NCHAR2 )
            REPORT_STRING = ' Y-axis limited from ' //
     :            REF_STR1( :NCHAR1 ) // ' to ' //
     :            REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            YLIMITS = .TRUE.
         END IF
         GO TO 200

*  'M' - Display full menu.
      ELSE IF ( USER_INPUT_CHAR .EQ. 'M' ) THEN
         MENU = .TRUE.
         GO TO 200

*  'Q' or error - Quit or error.
      ELSE IF ( USER_INPUT_CHAR .EQ. 'Q' .OR. ISTAT .NE. 0 ) THEN
         IF ( ISTAT .NE. 0 ) THEN
            CALL ECH_REPORT( 0,
     :      ' Failed to read cursor position: exiting browse mode.' )
         END IF
         CALL ECH_ACCESS_OBJECT( 'PLOTIM', 'UNMAP', 'IMAGE-DATA',
     :        BIM_SIZE, BIM_ADDRESS, BIM_HANDLE, IDIMS,
     :        MAX_DIMENSIONS, INUM_DIM, 'READ', STATUS )
         CALL ECH_ACCESS_OBJECT( 'PLOTIM', 'UNMAP', 'STRUCTURE',
     :        BIM_SIZE, BIM_ADDRESS, BIM_HANDLE, IDIMS,
     :        MAX_DIMENSIONS, INUM_DIM, 'READ', STATUS )
         BROWSE = .FALSE.
         CALL PGVSTD
         GO TO 100

      ELSE IF ( USER_INPUT_CHAR .EQ. 'S' ) THEN
         FULL_OBJECT_PATH = 'FSKY'

      ELSE IF ( USER_INPUT_CHAR .EQ. 'O' ) THEN
         FULL_OBJECT_PATH = 'OBJ'

      ELSE IF ( USER_INPUT_CHAR .EQ. 'A' ) THEN
         FULL_OBJECT_PATH = 'ARC'

      ELSE IF ( USER_INPUT_CHAR .EQ. 'F' ) THEN
         FULL_OBJECT_PATH = 'FFLT'

      ELSE IF ( USER_INPUT_CHAR .EQ. 'B' ) THEN
         FULL_OBJECT_PATH = 'BLZ'

      ELSE IF ( USER_INPUT_CHAR .NE. '|' ) THEN
         WRITE ( REPORT_STRING, 1005 ) X_CURSOR, Y_CURSOR
         CALL ECH_REPORT( 0, REPORT_STRING )
         GO TO 105
      END IF
      IF ( USER_INPUT_CHAR .EQ. '|' ) GO TO 105

*  Normal mode handler.
  300 CONTINUE

*  Exit/quit selected.
      IF ( FULL_OBJECT_PATH .EQ. 'E' .OR.
     :     FULL_OBJECT_PATH .EQ. 'Q' ) GO TO 998

*  'M' - Full menu display.
      IF ( FULL_OBJECT_PATH( :1 ) .EQ. 'M' ) THEN
         MENU = .TRUE.
         GO TO 102

*  '+' - Plot overlay.
      ELSE IF ( FULL_OBJECT_PATH( :1 ) .EQ. '+' ) THEN
         FULL_OBJECT_PATH = FULL_OBJECT_PATH( 2: )
         CALL ECH_REPORT( 0, ' Overlay mode set.' )
         OVERLAY = .TRUE.
         IF ( FULL_OBJECT_PATH .EQ. ' ' ) GO TO 102

*  '!' - Image overlay.
      ELSE IF ( FULL_OBJECT_PATH( :1 ) .EQ. '!' ) THEN
         FULL_OBJECT_PATH = FULL_OBJECT_PATH( 2: )
         CALL ECH_REPORT( 0, ' Image Overlay mode set.' )
         OVERIMAGE = .TRUE.
         IF ( FULL_OBJECT_PATH .EQ. ' ' ) GO TO 102

*  'H'/'?' - Help requested.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'H' .OR.
     :          FULL_OBJECT_PATH .EQ. '?' ) THEN
         CALL ECH_HELP( 'ECHMENU_options ECH_PLOT Options',
     :        .TRUE., STATUS )
         GO TO 102

*  'G' - Toggle GRAPH mode.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'G' ) THEN
         GRAPH = .NOT. GRAPH
         GO TO 102

*  'U' - Toggle windowing.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'U' ) THEN
         UWINDOW = .NOT. UWINDOW
         GO TO 102

*  '@' - Read plot commands from a file.
      ELSE IF ( FULL_OBJECT_PATH( :1 ) .EQ. '@' ) THEN
         FROM_FILE = .FALSE.
         PLOT_COMMANDS = RAWSTRING( 2: )
         IF ( PLOT_COMMANDS .EQ. ' ' ) THEN
            CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Command file',
     :           'CHAR', VALUE, .FALSE., PLOT_COMMANDS, 0, STATUS )
         END IF
         STATUS = 0
         CALL FIO_GUNIT( UNIT, STATUS )
         OPEN ( UNIT=UNIT, FILE=PLOT_COMMANDS, IOSTAT=ISTAT,
     :          STATUS='OLD')
         IF ( ISTAT .EQ. 0 ) THEN
            FROM_FILE = .TRUE.
            REPORT_STRING = ' Reading commands from ' // PLOT_COMMANDS
            CALL ECH_REPORT( 0, REPORT_STRING )

         ELSE
            REPORT_STRING =  ' Cannot open file ' // PLOT_COMMANDS
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
         GO TO 102

*  '>' - Send output to a new device.
      ELSE IF ( FULL_OBJECT_PATH( :1 ) .EQ. '>' ) THEN
         GRAPHICS_DEVICE_NAME = RAWSTRING( 2: )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0.0, 0.0, 0.0, 0.0,
     :        ' ', ' ', ' ', 0.0, 0.0, GRPH_CLOSE_DEV, ' ', STATUS )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, FLOAT( IWX ), FLOAT( IWY ),
     :        0.0, 0.0, ' ', ' ', ' ', 0.0, 0.0, GRPH_OPEN_DEV,
     :        GRAPHICS_DEVICE_NAME, STATUS )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0.0, 0.0, 0.0, 0.0,
     :        ' ', ' ', ' ', 0.0, 0.0, GRPH_SET_NOPROMPT, ' ', STATUS )
         CALL ECH_SET_PARAMETER( 'SOFT', 'CHAR', VALUE,
     :        0, GRAPHICS_DEVICE_NAME, STATUS )
         FULL_OBJECT_PATH = ' '
         GO TO 102

*  'B' - Switch to browse mode.
      ELSE IF ( FULL_OBJECT_PATH( :2 ) .EQ. 'B ' ) THEN
         BROWSE = .TRUE.
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0., 0., 0., 0.,
     :        ' ', ' ', ' ', 0., 0.,
     :        GRPH_CLOSE_DEV, ' ', STATUS )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, FLOAT( IWX ),
     :        FLOAT( IWY ), 0., 0., ' ', ' ', ' ', 0., 0.,
     :        GRPH_OPEN_DEV, GRAPHICS_DEVICE_NAME, STATUS )
         OPTIONS = GRPH_SET_NOPROMPT
         IMG_NAME = RAWSTRING( 3: )
         IF ( IMG_NAME .EQ. ' ' ) THEN
 333        CONTINUE
            CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Image',
     :           'CHAR', VALUE, .FALSE., IMG_NAME, 0, STATUS )
         END IF
         CALL ECH_ACCESS_OBJECT( 'PLOTIM', 'OPEN', 'STRUCTURE',
     :        BIM_SIZE, BIM_ADDRESS, BIM_HANDLE,
     :        DUMDIM, MAX_DIMENSIONS, 0, IMG_NAME, STATUS )
         CALL ECH_ACCESS_OBJECT( 'PLOTIM', 'READ-SIZE', 'IMAGE-DATA',
     :        BIM_SIZE, BIM_ADDRESS, BIM_HANDLE,
     :        IDIMS, MAX_DIMENSIONS, INUM_DIM, ' ', STATUS )
         IF ( STATUS .NE. 0 ) GO TO 333
         CALL ECH_ACCESS_OBJECT( 'PLOTIM', 'MAP-IMAGE', '_REAL',
     :        BIM_SIZE, BIM_ADDRESS, BIM_HANDLE,
     :        IDIMS, MAX_DIMENSIONS, INUM_DIM, 'READ', STATUS )
         DATA_MIN = 0.0
         DATA_MAX = 0.0
         MENU = .TRUE.

*     Go and plot the image.
         GO TO 800

*  'I' - Toggle cursor interactive mode.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'I' ) THEN
         FULL_OBJECT_PATH = FULL_OBJECT_PATH( 2: )
         CURSOR = .NOT. CURSOR
         IF ( CURSOR )
     :      CALL ECH_REPORT( 0, ' Interactive cursor mode set.' )
         IF ( FULL_OBJECT_PATH .EQ. ' ' ) GO TO 102

*  'S' - Set plot style.
      ELSE IF ( FULL_OBJECT_PATH( :2 ) .EQ. 'S ' ) THEN
         IF ( FULL_OBJECT_PATH( 3: ) .NE. ' ' ) THEN
            NEW_STYLE = FULL_OBJECT_PATH( 3: )

         ELSE
            IF ( FROM_FILE ) THEN
               READ ( UNIT, '( A )', END = 998 ) NEW_STYLE

            ELSE
               CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Style',
     :              'CHAR', VALUE, .FALSE., NEW_STYLE, 0, STATUS )
            END IF
         END IF
         CALL CHR_UCASE( NEW_STYLE )
         CALL ECH_PLOTTER_STYLE( STYLE, NEW_STYLE, STATUS )
         GO TO 102

*  'A' - ASCII dump of last plot.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'A' ) THEN
         FULL_OBJECT_PATH = USTRING_1
         DUMP_ASCII = .TRUE.
         IF ( FROM_FILE ) THEN
            READ ( UNIT, '( A )', END = 998 ) DMP_FILE

         ELSE
            CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=File',
     :           'CHAR', VALUE, .FALSE., DMP_FILE, 0, STATUS )
         END IF
 104     CONTINUE
         IF ( FROM_FILE ) THEN
            READ ( UNIT, '( A )', END = 998 ) DMP_FORMAT

         ELSE
            CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Format',
     :           'CHAR', VALUE, .FALSE., DMP_FORMAT, 0, STATUS )
         END IF
         IF ( DMP_FORMAT .EQ. ' ' ) DMP_FORMAT = '(1X,F20.4,1X,F20.4)'
         WRITE ( DUMMY_REC, DMP_FORMAT, IOSTAT=ISTAT ) 0., 0.
         IF ( ISTAT .NE. 0 ) THEN
            CALL ECH_REPORT( 0, ' Bad format for two REAL numbers.' )
            GO TO 104
         END IF
         CALL ECH_REPORT( 0, ' listing plotted values to file.' )

*  'D' - Brief directory of plottable objects.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'D' ) THEN
         CALL ECH_REPORT( 0, ' ' )
         CALL ECH_REPORT( 0,
     :        ' Brief directory of reduction objects: ' )
         DO ISCAN = 1, NUM_ABBREV
            WRITE ( REPORT_STRING, '( 1X, A6, 1X, A )' )
     :            ABBREV( ISCAN ), HELP( ISCAN )
            CALL ECH_REPORT( 0, REPORT_STRING )
         END DO
         GO TO 102

*  'FD' - Full directory of plottable objects.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'FD' ) THEN
         list = '>> '
         CALL ECH_REPORT( 0, ' ' )
         CALL ECH_REPORT( 0,
     :        ' Full directory of reduction objects: ' )
         DO iscan = 2, max_objects
            IF ( object_ref_name ( iscan ) .NE. ' ' .AND.
     :           object_ref_name ( iscan ) .NE.
     :                          object_ref_name ( iscan-1 )  ) THEN
               newlen = CHR_LEN(list) + 2 +
     :                  CHR_LEN(object_ref_name(iscan))
               IF ( newlen .LT. 72 ) THEN
                  list = list( :CHR_LEN(list))//'  '//
     :  object_ref_name(iscan)( :CHR_LEN(object_ref_name(iscan)))
               ELSE
                  iplen = CHR_LEN(list)
                  IF ( iplen .GT. 3 ) CALL ECH_REPORT ( 0, list )
                  iplen = CHR_LEN(object_ref_name(iscan))
                  IF ( iplen .GT. 0 )
     :               list = '>> '//object_ref_name(iscan)//' '
               END IF
            END IF
         END DO
         CALL ECH_REPORT ( 0, ' ' )
         GO TO 102

*  'W' - Subwindowing.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'W' ) THEN
         IF ( FROM_FILE ) THEN
            READ ( UNIT, '( F14.4 )', END = 998 ) VALUE

         ELSE
            CALL ECH_GET_PARAMETER(
     :           'INSTANT-PROMPT=Horizontal panes',
     :           'INT', VALUE, .FALSE., ' ', 0, STATUS )
         END IF
         IWX = MIN( MAX( 1, INT( VALUE ) ), 8 )
         IF ( FROM_FILE ) THEN
            READ ( UNIT, '( F14.4 )', END = 998 ) VALUE

         ELSE
            CALL ECH_GET_PARAMETER(
     :           'INSTANT-PROMPT=Vertical panes',
     :           'INT', VALUE, .FALSE., ' ', 0, STATUS )
         END IF
         IWY = MIN( MAX( 1, INT( VALUE ) ), 8 )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0., 0., 0., 0.,
     :        ' ', ' ', ' ', 0., 0., GRPH_CLOSE_DEV, ' ', STATUS )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, FLOAT( IWX ),
     :        FLOAT( IWY ), 0., 0., ' ', ' ', ' ', 0., 0.,
     :        GRPH_OPEN_DEV, GRAPHICS_DEVICE_NAME, STATUS )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0., 0., 0., 0.,
     :        ' ', ' ', ' ', 0., 0., GRPH_SET_NOPROMPT, ' ', STATUS )
         WRITE ( REPORT_STRING, 1001 ) IWX, IWY
         CALL ECH_REPORT( 0, REPORT_STRING )
         GO TO 102

*  'L' - Range limiting.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'L' ) THEN
         IF ( from_file ) THEN
            READ ( UNIT, '( F14.4 )', END = 998 ) XM
            READ ( UNIT, '( F14.4 )', END = 998 ) XH
            READ ( UNIT, '( F14.4 )', END = 998 ) YM
            READ ( UNIT, '( F14.4 )', END = 998 ) YH

         ELSE
           CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Minimum X',
     :          'INT', XM, .FALSE., ' ', 0, STATUS )
           CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Maximum X',
     :          'INT', XH, .FALSE., ' ', 0, STATUS )
           CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Minimum Y',
     :          'INT', YM, .FALSE., ' ', 0, STATUS )
           CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Maximum Y',
     :          'INT', YH, .FALSE., ' ', 0, STATUS )
         END IF
         IF ( .NOT. GRAPH ) THEN
            IF ( FROM_FILE ) THEN
              READ ( UNIT, '( F14.4 )', END = 998 ) DATA_MIN
              READ ( UNIT, '( F14.4 )', END = 998 ) DATA_MAX

            ELSE
              CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Minimum data',
     :             'INT', DATA_MIN, .FALSE., ' ', 0, STATUS )
              CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Maximum data',
     :             'INT', DATA_MAX, .FALSE., ' ', 0, STATUS )
            END IF
         END IF
         IF ( INT( XM ) .EQ. 0 .AND. INT( XH ) .EQ. 0 ) THEN
            XLIMITS = .FALSE.
            CALL ECH_REPORT( 0,
     :           ' X-range will be automatically scaled.' )
         ELSE
            CALL CHR_ITOC( INT( XM ), REF_STR1, NCHAR1 )
            CALL CHR_ITOC( INT( XH ), REF_STR2, NCHAR2 )
            REPORT_STRING = ' X-axis limited from ' //
     :            REF_STR1( :NCHAR1 ) // ' to ' //
     :            REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            XLIMITS = .TRUE.
         END IF
         IF ( INT( YM ) .EQ. 0 .AND. INT( YH ) .EQ. 0 ) THEN
            YLIMITS = .FALSE.
            CALL ECH_REPORT( 0,
     :           ' Y-range will be automatically scaled.' )
         ELSE
            CALL CHR_ITOC( INT( YM ), REF_STR1, NCHAR1 )
            CALL CHR_ITOC( INT( YH ), REF_STR2, NCHAR2 )
            REPORT_STRING = ' Y-axis limited from ' //
     :            REF_STR1( :NCHAR1 ) // ' to ' //
     :            REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            YLIMITS = .TRUE.
         END IF
         GO TO 102

*  'N' - Number of samples to plot.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'N' ) THEN
         IF ( FROM_FILE ) THEN
            READ ( UNIT, '( F14.4 )', END = 998 ) VALUE

         ELSE
            CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=No. X samples',
     :           'INT', VALUE, .FALSE., ' ', 0, STATUS )
         END IF
         SET_OBJECT_SIZE = INT( VALUE )
         GO TO 102

*  'R' - Rebin factor.
      ELSE IF ( FULL_OBJECT_PATH .EQ. 'R' ) THEN
         IF ( FROM_FILE ) THEN
            READ ( UNIT, '( F14.4 )', END = 998 ) VALUE

         ELSE
            CALL ECH_GET_PARAMETER(
     :           'INSTANT-PROMPT=Bin(+)/smooth(-) factor',
     :           'INT', VALUE, .FALSE., ' ', 0, STATUS )
         END IF
         REBIN = INT( VALUE )
         GO TO 102

*  '^' - Last plot to hardcopy.
      ELSE IF ( FULL_OBJECT_PATH .EQ. '^' ) THEN
         HARDCOPY = .TRUE.
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 0., 0., 0., 0.,
     :        ' ', ' ', ' ', 0., 0., GRPH_CLOSE_DEV, ' ',STATUS )
         CALL ECH_PLOT_GRAPH( 1, IDUM, IDUM, 1.0, 1.0, 0., 0.,
     :        ' ', ' ', ' ', 0., 0., GRPH_OPEN_DEV,
     :        HARDCOPY_DEVICE_NAME, STATUS )
         FULL_OBJECT_PATH = USTRING_1
         REPORT_STRING = ' Copying last plot to hardcopy device ' //
     :   HARDCOPY_DEVICE_NAME( :CHR_LEN(HARDCOPY_DEVICE_NAME)) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

*   Save a copy of this request for next time.
      USTRING_1 = FULL_OBJECT_PATH

*   Check for two objects to be plotted against each other.
      FULL_OBJECT_PATH2 = ' '
      IGNORE = .FALSE.
      DO I = 1, CHR_LEN( FULL_OBJECT_PATH )
         IF ( FULL_OBJECT_PATH( I:I ) .EQ. '[' ) IGNORE = .TRUE.
         IF ( FULL_OBJECT_PATH( I:I ) .EQ. ']' ) IGNORE = .FALSE.
         IF ( FULL_OBJECT_PATH( I:I ) .EQ. ',' .AND. .NOT. IGNORE ) THEN
            FULL_OBJECT_PATH2 = FULL_OBJECT_PATH( I + 1 : ) // ' '
            FULL_OBJECT_PATH = FULL_OBJECT_PATH( : I - 1 ) // ' '
         END IF
      END DO

*   First object to be plotted, get type.
      IF ( FULL_OBJECT_PATH .NE. ' ' ) THEN
         CALL ECH_PLOTTER_UNMAP( FULL_OBJECT_PATH, NEW_OBJECT_TYPE,
     :        STATUS )
         IF ( STATUS .NE. 0 ) THEN
            GO TO 100
         END IF
      END IF

*  Second object to be plotted, get type.
      IF ( FULL_OBJECT_PATH2 .NE. ' ' ) THEN
         CALL ECH_PLOTTER_UNMAP( FULL_OBJECT_PATH2, NEW_OBJECT_TYPE2,
     :        STATUS )
         IF ( STATUS .NE. 0 ) THEN
            GO TO 100
         END IF
      END IF

*  Get type of first object.
      CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :     'READ-TYPE', NEW_OBJECT_TYPE, 0, 0, 0,
     :     DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      IF ( STATUS .NE. 0 ) THEN
         REPORT_STRING = ' Failed to find ' // FULL_OBJECT_PATH
         CALL ECH_REPORT( 0, REPORT_STRING )
         GO TO 100
      END IF

*  We can't plot character objects.
      IF ( NEW_OBJECT_TYPE .EQ. 'CHAR' ) THEN
         CALL ECH_REPORT( 0, ' Cannot plot character objects.' )
         GO TO 100
      END IF

*  Get size of first object.
      CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'READ-SIZE',
     :     NEW_OBJECT_TYPE, 0, 0, 0, DIMENSIONS, MAX_DIMENSIONS,
     :     NUM_DIM, ' ', STATUS )

*  Browse mode.
      IF ( BROWSE ) THEN

*     Find order nearest to cursor.
         DIST = 1.0E20
         NEAREST = 1
         DO IORD = 1, NUM_ORDERS
            CALL ECH_CALC_TRACE_AT_X( MAXIMUM_POLY,
     :           POLYNOMIALS( 1, IORD ), DBLE( X_CURSOR ),
     :           YCO, STATUS )
            IF ( ABS( REAL( YCO ) - Y_CURSOR ) .LT. DIST ) THEN
               DIST = ABS( REAL( YCO ) - Y_CURSOR )
               YTRACE = REAL( YCO )
               NEAREST = IORD
            END IF
         END DO

*     Calculate indices for the request.
         CINDICES = '['
         IADD = 2
         DO I = 1, NUM_DIM
            IF ( DIMEN_INDEX( I ) .EQ. 'NX' ) THEN
               IF ( SET_OBJECT_SIZE .GT. 0 ) THEN
                  IIND = MAX( 1,
     :                  INT( X_CURSOR ) - SET_OBJECT_SIZE / 2 )

               ELSE
                  IIND = 1
               END IF

            ELSE IF ( DIMEN_INDEX( I ) .EQ. 'NUM_ORDERS' ) THEN
               IIND = NEAREST

            ELSE IF ( DIMEN_INDEX( I ) .EQ. 'TUNE_MXSKYPIX' ) THEN
               IIND = MIN( MAX( 1, INT( Y_CURSOR - YTRACE ) +
     :               MXSKYPIX / 2 + 1 ), MXSKYPIX )

            ELSE
               IIND = 1
            END IF
            CALL CHR_ITOC( IIND, CINDICES( IADD: ), NCHAR1 )
            IADD = IADD + NCHAR1 + 1
            CINDICES( IADD - 1 : IADD ) = ', '
         END DO
         IADD = ECH_WORD_LEN( CINDICES )
         IF ( CINDICES( IADD : IADD ) .EQ. ',' )
     :      CINDICES( IADD : IADD ) = ']'

*     Append indices to path.
         FULL_OBJECT_PATH = FULL_OBJECT_PATH(
     :         :ECH_WORD_LEN( FULL_OBJECT_PATH )) // CINDICES // ' '

*     Prepare the screen for the plot.
         CALL PGPAGE
         CALL PGSVP( 0.66, 0.97, 0.4, 0.85 )
      END IF

*  Determine size of object to be plotted.
*  Force 1-D objects to be plotted as "spectra" rather than images.
      IF ( GRAPH .OR. DIMENSIONS( 2 ) .EQ. 1 ) THEN
         IF ( SET_OBJECT_SIZE .EQ. 0 ) THEN
            NEW_OBJECT_SIZE = DIMENSIONS( 1 )

         ELSE
            NEW_OBJECT_SIZE = SET_OBJECT_SIZE
            IF ( NEW_OBJECT_SIZE .GT. DIMENSIONS( 1 ) ) THEN
               NEW_OBJECT_SIZE = DIMENSIONS( 1 )
               WRITE ( REPORT_STRING, 1006 ) NEW_OBJECT_SIZE
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
         END IF

      ELSE
         NEW_OBJECT_SIZE = DIMENSIONS( 1 ) * MAX( 1, DIMENSIONS( 2 ) )
      END IF
      IMG_SIZE = NEW_OBJECT_SIZE
      MAP_SIZE = MAX( 1, DIMENSIONS( 1 ) ) *
     :      MAX( 1, DIMENSIONS( 2 ) ) * MAX( 1, DIMENSIONS( 3 ) )

*  Determine offset within object to selected part.
*  Range limit X selection to avoid overrun when partial.
      ILEN = CHR_LEN( FULL_OBJECT_PATH )
      CALL ECH_GET_DIMS( FULL_OBJECT_PATH, REQ_DIMS, 0, ILEN, IEND,
     :     IDIM )
      IF ( SET_OBJECT_SIZE .NE. 0 ) THEN
         IF ( REQ_DIMS( 1 ) + SET_OBJECT_SIZE .GT. DIMENSIONS( 1 ) )
     :        THEN
            REQ_DIMS( 1 ) = DIMENSIONS( 1 ) - SET_OBJECT_SIZE + 1
         END IF

      ELSE
         REQ_DIMS( 1 ) = 1
      END IF
      MAP_OFFSET = AUNIT * MAX( 0, REQ_DIMS( 1 ) - 1 )
      IF ( IDIM .EQ. 3 )
     :   MAP_OFFSET = MAP_OFFSET +
     :         MAX( 0, REQ_DIMS( 3 ) - 1 ) *
     :         DIMENSIONS( 2 ) * DIMENSIONS( 1 ) * AUNIT
      IF ( IDIM .GE. 2 )
     :   MAP_OFFSET = MAP_OFFSET +
     :         MAX( 0, REQ_DIMS( 2 ) - 1 ) * DIMENSIONS( 1 ) * AUNIT

*  Determine options for this plot.
      OPTIONS = 0
      IF ( OVERLAY ) OPTIONS = OPTIONS + GRPH_OVERLAY
      IF ( BROWSE ) OPTIONS = OPTIONS + GRPH_WINDOW
      IF ( UWINDOW ) OPTIONS = GRPH_WINDOW + GRPH_USER_WINDOW
      IF ( .NOT. XLIMITS ) OPTIONS = OPTIONS + GRPH_CALC_XMINMAX
      IF ( .NOT. YLIMITS ) OPTIONS = OPTIONS + GRPH_CALC_YMINMAX
      IF ( CURSOR ) OPTIONS = OPTIONS + GRPH_LOOP_EXAMINE

*  "cludge" by MJC 17-APR-1996 - so ech_plotter gets title.
      XLABEL = GRAPH_TITLE( : 80 )
      YLABEL = 'Value'
      IF ( DUMP_ASCII ) THEN
         OPTIONS = OPTIONS + GRPH_DUMP_ASCII
         XLABEL = DMP_FORMAT
         YLABEL = DMP_FILE
      END IF

*  Map the first object.
      IF ( FULL_OBJECT_PATH .EQ. ' ' ) THEN
         OPTIONS = OPTIONS + GRPH_GEN_YAXIS

      ELSE
         CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'MAP-READ',
     :        'FLOAT', MAP_SIZE, IMG_ADDRESS, IMG_HANDLE,
     :        DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
         IMG_ADDRESS = IMG_ADDRESS + MAP_OFFSET
         IF ( STATUS .NE. 0 ) THEN
            REPORT_STRING = ' Cannot read ' // FULL_OBJECT_PATH
            CALL ECH_REPORT( 0, REPORT_STRING )
            USTRING_1 = FULL_OBJECT_PATH
            GO TO 100
         END IF
      END IF

*  Map the second object, or fill in the X-axis.
      IF ( FULL_OBJECT_PATH2 .EQ. ' ' ) THEN
#if HAVE_PERCENTLOC
         IMG_ADDRESS2 = %LOC( XDATA )
#elif HAVE_INTRINSIC_LOC
         IMG_ADDRESS2 = LOC( XDATA )
#else
          Do not have LOC or %LOC
#endif
         IF ( SET_OBJECT_SIZE .NE. 0 ) THEN
            DO I = 1, SET_OBJECT_SIZE
               XDATA( I ) = REQ_DIMS( 1 ) + I - 1
            END DO

         ELSE
            OPTIONS = OPTIONS + GRPH_GEN_XAXIS
         END IF

      ELSE
         CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH2,
     :        'MAP-READ', 'FLOAT', MAP_SIZE,
     :        IMG_ADDRESS2, IMG_HANDLE2,
     :        DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
         IMG_ADDRESS2 = IMG_ADDRESS2 + MAP_OFFSET
         IF ( STATUS .NE. 0 ) THEN
            REPORT_STRING = ' Cannot read ' // FULL_OBJECT_PATH2
            CALL ECH_REPORT( 0, REPORT_STRING )
            USTRING_1 = FULL_OBJECT_PATH
            GO TO 100
         END IF
      END IF

*  Allocate space for copies of objects.
      VM_SIZE = NEW_OBJECT_SIZE * AUNIT
      CALL PSX_MALLOC( VM_SIZE, ADDR_Y, STATUS )
      CALL PSX_MALLOC( VM_SIZE, ADDR_X, STATUS )

*  Copy the objects.
      CALL ECH_COPY_BYTES( VM_SIZE, %VAL( IMG_ADDRESS ),
     :     %VAL( ADDR_Y ) )
      CALL ECH_COPY_BYTES( VM_SIZE, %VAL( IMG_ADDRESS2 ),
     :     %VAL( ADDR_X ) )

*  Swap the X- and Y-axes if requested.
      IF ( SWAP_AXES ) THEN
         PLT_ADDRY = ADDR_X
         PLT_ADDRX = ADDR_Y

      ELSE
         PLT_ADDRX = ADDR_X
         PLT_ADDRY = ADDR_Y
      END IF

*  Rebin or smooth the plot as needed.
      IF ( REBIN .LT. -1 .OR. REBIN .GT. 1 ) THEN
         CALL ECH_PLOTTER_REMBAD( %VAL( PLT_ADDRX ),
     :        %VAL( PLT_ADDRY ), NEW_OBJECT_SIZE )
         IF ( REBIN .GT. 1 ) THEN
            CALL ECH_PLOTTER_REBIN( REBIN, %VAL( PLT_ADDRX ),
     :           %VAL( PLT_ADDRY ), NEW_OBJECT_SIZE )

         ELSE
            CALL ECH_PLOTTER_SMOOTH( -REBIN,
     :           %VAL( PLT_ADDRY ), NEW_OBJECT_SIZE )
         END IF
      END IF

*  Plot a 1-D dataset.
      IF ( GRAPH .OR. DIMENSIONS( 2 ) .EQ. 1 ) THEN
         IF ( FULL_OBJECT_PATH2 .NE. ' ' ) THEN
            CALL ECH_PLOTTER_ZERO_TRUNC( NEW_OBJECT_SIZE,
     :           %VAL( PLT_ADDRX ) )
         END IF
         IF ( OVERIMAGE ) THEN
            CALL ECH_PLOT_GRAPH( NEW_OBJECT_SIZE,
     :           %VAL( PLT_ADDRX ), %VAL( PLT_ADDRY ),
     :           XM, XH, YM, YH, ' ', ' ', ' ',
     :           0.0, 0.0, OPTIONS, 'IMAGE-OVERLAY',
     :           STATUS )

         ELSE
            CALL ECH_PLOT_GRAPH( NEW_OBJECT_SIZE,
     :           %VAL( PLT_ADDRX ), %VAL( PLT_ADDRY ),
     :           XM, XH, YM, YH, XLABEL, YLABEL,
     :           FULL_OBJECT_PATH,
     :           0.0, 0.0, OPTIONS,
     :           STYLE, STATUS )
         END IF

*  Display a 2-D dataset.
      ELSE
         DATA_XM = XM
         DATA_XH = XH
         DATA_YM = YM
         DATA_YH = YH
         DATA_XM = MIN( MAX( 1.0, DATA_XM ), FLOAT( DIMENSIONS( 1 ) ) )
         IF ( DATA_XH .EQ. 0.0 ) DATA_XH = FLOAT( DIMENSIONS( 1 ) )
         DATA_XH = MIN( MAX( DATA_XM + 1.0, DATA_XH ),
     :         FLOAT( DIMENSIONS( 1 ) ) )
         DATA_YM = MIN( MAX( 1.0, DATA_YM ), FLOAT( DIMENSIONS( 2 ) ) )
         IF ( DATA_YH .EQ. 0.0 ) DATA_YH = FLOAT( DIMENSIONS( 2 ) )
         DATA_YH = MIN( MAX( DATA_YM + 1.0, DATA_YH ),
     :         FLOAT( DIMENSIONS( 2 ) ) )
         CALL ECH_PLOT_GRAPH( IMG_SIZE, %VAL( PLT_ADDRX ),
     :        %VAL( PLT_ADDRY ), DATA_XM, DATA_XH,
     :        DATA_YM, DATA_YH, XLABEL, '2-D array image',
     :        FULL_OBJECT_PATH, DATA_MIN, DATA_MAX,
     :        OPTIONS, 'IMAGING', STATUS )
      END IF

*  Unmap the objects.
      IF ( FULL_OBJECT_PATH .NE. ' ' ) THEN
         CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :        'UNMAP', 'FLOAT', IMG_SIZE, IMG_ADDRESS, IMG_HANDLE,
     :        DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      END IF
      IF ( FULL_OBJECT_PATH2 .NE. ' ' ) THEN
         CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH2, 'UNMAP',
     :        'FLOAT', IMG_SIZE, IMG_ADDRESS2, IMG_HANDLE2,
     :        DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      END IF

*  Free workspace.
      CALL PSX_FREE( ADDR_X, STATUS )
      CALL PSX_FREE( ADDR_Y, STATUS )

*  Display the master image.
  800 IF ( BROWSE ) THEN
         DATA_XM = 1.0
         DATA_XH = FLOAT( IDIMS( 1 ) )
         DATA_YM = 1.0
         DATA_YH = FLOAT( IDIMS( 2 ) )
         BIM_SIZE = IDIMS( 1 ) * IDIMS( 2 )
         CALL PGSVP( 0.05, 0.58, 0.1, 0.7 )
         CALL PGSWIN( DATA_XM, DATA_XH, DATA_YM, DATA_YH )
         CALL PGPTXT( DATA_XH / 3.0, DATA_YH * 1.2, 0.0, 0.0,
     :        'Image browse mode' )
         CALL ECH_PLOT_GRAPH( BIM_SIZE, %VAL( BIM_ADDRESS ),
     :        %VAL( BIM_ADDRESS ), DATA_XM, DATA_XH,
     :        DATA_YM, DATA_YH, ' ', ' ', IMG_NAME, DATA_MIN,
     :        DATA_MAX, OPTIONS, 'IMAGING', STATUS )
      END IF
      GO TO 100

  998 CONTINUE
      IF ( FROM_FILE ) THEN
         CLOSE( UNIT )
         STATUS = 0
         CALL FIO_PUNIT( UNIT, STATUS )
         FROM_FILE = .FALSE.
         REPORT_STRING = ' End of file ' // PLOT_COMMANDS
         CALL ECH_REPORT( 0, REPORT_STRING )
         GO TO 102
      END IF

  999 CONTINUE

 1001 FORMAT ( 1X, 'Initialised graphics with ',
     :       I2, ' by ', I2, ' subwindows.' )
 1005 FORMAT ( 1X, 'Cursor over pixel X=', F7.1, ' Y=', F7.1 )
 1006 FORMAT ( 1X,
     :     'Number of samples > array dimension, resetting to ', I5 )

      END

      SUBROUTINE ECH_PLOTTER_ZERO_TRUNC( N, ARRAY )
      IMPLICIT NONE
      INTEGER N
      REAL ARRAY( N )
      INTEGER I
      LOGICAL LASTZERO

      LASTZERO = .FALSE.

      DO I = 1, N
         IF ( LASTZERO .AND. ARRAY( I ) .EQ. 0.0 ) THEN
            N = I - 2
            GO TO 100
         END IF
         LASTZERO = ( ARRAY( I ) .EQ. 0.0 )
      END DO

  100 CONTINUE

      END
