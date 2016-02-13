      SUBROUTINE ECHOMOP( ACTION, STATUS )
*+
*  Name:
*     ECHOMOP

*  Purpose:
*     Main menu shell task.

*  Description:
*     This routine provides a semi-helpful shell to drive the multitude of
*     ECH_?????? tasks available. A simple menu of tasks available plus
*     a few utilities is provided. The user is guided through a reduction
*     by the program which always suggest a likely next option as a default.
*     Options are selected either by typing <return> to accept the default,
*     or by typing an option number followed by <return>.
*
*     The shell also accepts single letter and keyword selections for
*     common functions such as H(ELP),E(XIT),P(LOT) etc.
*
*     Additional faciltites include the provision of a system command
*     capability (any input starting with a $ is passed to the system
*     for execution), and a `cloning' option whereby any numerical
*     processing option may be followed by the character `<' plus the
*     name of an extant reduction database. This syntax requests
*     the shell to copy the information from the named file, instead of
*     invoking the processing option to calculate it.
*
*     Eg.      9<OLD_FILE
*
*           would request that the results of processing step 9
*           (locate arc lines) be copied from the reduction database
*           file `OLD_FILE', rather than actually running the processing
*           step.
*
*     This general mechanism of result cloning is available at all
*     processing steps and provides a simple mechanism for the efficient
*     sharing of intermediate results (eg order traces) amongst a large
*     number of similar reductions thus saving CPU time.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)
*     Tim Jenness (JAC, Hawaii)

*  History:
*     1990 Apr 4 (DMILLS):
*       Initial release
*     2004 Sept 29 (TIMJ):
*       Use ONE_SYSTEM rather than ECH_SYSTEM

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_ECHOMOP.INC'
      INCLUDE 'ECH_SERVER.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_PSX.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Input/Output variables used:
      CHARACTER*( * ) ACTION

*  Global Status:
      INTEGER STATUS

*  Local variables:
      REAL value

      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER I
      INTEGER II
      INTEGER I1
      INTEGER I2
      INTEGER SUBOPTION
      INTEGER IVALUE
      INTEGER AUTO_IDX
      INTEGER ORDER_NUMBER
      INTEGER OPTION
      INTEGER ISTAT
      INTEGER ISUBMENU
      INTEGER DEFAULT_OPTION
      INTEGER LAST_OPTION
      INTEGER ADDOPTION
      INTEGER IIOPTIONS
      INTEGER XLEN
      INTEGER DINDEX
      INTEGER NCHAR1

      LOGICAL EXTRACT_2D
      LOGICAL HINTS
      LOGICAL INTERACTIVE
      LOGICAL EXIT_SELECTED
      LOGICAL GOT_AOPT
      LOGICAL PREVIEW
      LOGICAL FULL_MENU
      LOGICAL EXPLAINED_MENU
      LOGICAL SOME_AUTO
      LOGICAL STARTED
      LOGICAL HYPER

      CHARACTER*132 AUTO_OPTIONS
      CHARACTER*80 WORK_STRING
      CHARACTER*80 ORIG
      CHARACTER*80 TOPIC
      CHARACTER*80 CLONE_FROM
      CHARACTER*80 SYS_COMMAND
      CHARACTER*80 XLATE
      CHARACTER*40 CVALUE
      CHARACTER*8 REF_STR1
      CHARACTER*( ECH__MNMSIZ ) SUBOPT_MODULE
      CHARACTER*( ECH__MNMSIZ ) SUBOPT_TEXT
      CHARACTER CVALUECLASS

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER ECH_OBJ_IND
      INTEGER ECH_WORD_LEN
      INTEGER CHR_LEN
      INTEGER CHR_INDEX

*  Data Statements:
      DATA STARTED / .FALSE. /
*.


*  Set context variable denoting control mechanism to denote monolith
      CALL ECH_SET_CONTEXT( 'CONTROL', 'MONOLITH' )

      EXTRACT_2D = .FALSE.
      HINTS = .FALSE.

*  Get host system details.
      CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :     STATUS )

*  Initialise common areas etc.
      IF ( SYSNAME .EQ. 'VMS' ) THEN
         IF ( .NOT. STARTED ) THEN
            CALL ECH_INITIALISE( STATUS )
            STARTED = .TRUE.
         END IF
      END IF

*  Return immediately if initialisation failed.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

      DINDEX = DEFAULTS_INDICES( ECH_OBJ_IND( 'TUNE_CLONE' ) )
      CALL ECH_GET_PARAMETER( 'TUNE_CLONE', 'CHAR', 0.0, .FALSE.,
     :     CLONE_FROM, DINDEX, STATUS )

*  Read any automated steps requested via parameter TUNE_AUTOMATE.
      ORDER_NUMBER = 0
      SOME_AUTO = .FALSE.
      DINDEX = DEFAULTS_INDICES( ECH_OBJ_IND( 'TUNE_AUTOMATE' ) )
      CALL ECH_GET_PARAMETER( 'TUNE_AUTOMATE', 'CHAR', 0.0, .FALSE.,
     :     AUTO_OPTIONS, DINDEX, STATUS )
      IF ( CHR_LEN( AUTO_OPTIONS ) .GT. 0 ) SOME_AUTO = .TRUE.

*  Process requested ACTION.
      CALL CHR_UCASE( ACTION )
      IF ( ACTION .NE. 'ECHMENU' .AND. ACTION .NE. 'ECHOMOP') THEN
         CALL ECH_GET_PARAMETER( 'IDX_NUM_ORDERS', 'INT', VALUE,
     :        .FALSE., ' ', 0, STATUS )
         ORDER_NUMBER = INT( ABS( VALUE ) )
         SOME_AUTO = .TRUE.
         IF ( ACTION .EQ. 'ECH_LOCATE' ) THEN
            AUTO_OPTIONS = '1,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_FCHECK' ) THEN
            AUTO_OPTIONS = '1.1,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_SLOPE' ) THEN
            AUTO_OPTIONS = '1.3,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_DECOS1' ) THEN
            AUTO_OPTIONS = '1.2,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_FIND' ) THEN
            AUTO_OPTIONS = '1.4,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_TRACE' ) THEN
            AUTO_OPTIONS = '2,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_FITORD' ) THEN
            AUTO_OPTIONS = '3,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_SPATIAL' ) THEN
            AUTO_OPTIONS = '4,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_DEKKER' ) THEN
            AUTO_OPTIONS = '4.1,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_OBJECT' ) THEN
            AUTO_OPTIONS = '4.2,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_FFIELD' ) THEN
            AUTO_OPTIONS = '5,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_SKY' ) THEN
            AUTO_OPTIONS = '6,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_PROFILE' ) THEN
            AUTO_OPTIONS = '7,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_EXTRCT' ) THEN
            AUTO_OPTIONS = '8,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_LINLOC' ) THEN
            AUTO_OPTIONS = '9,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_FWHM' ) THEN
            AUTO_OPTIONS = '9.1,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_LINES' ) THEN
            AUTO_OPTIONS = '9.2,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_IDWAVE' ) THEN
            AUTO_OPTIONS = '10,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_BLAZE' ) THEN
            AUTO_OPTIONS = '11,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_FITBLZ' ) THEN
            AUTO_OPTIONS = '11.1,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_DOBLZ' ) THEN
            AUTO_OPTIONS = '11.2,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_SCRUNCH' ) THEN
            AUTO_OPTIONS = '12,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_FITFWHM' ) THEN
            AUTO_OPTIONS = '12.1,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_WSCALE' ) THEN
            AUTO_OPTIONS = '12.2,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_SCROBJ' ) THEN
            AUTO_OPTIONS = '12.3,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_SCRARC' ) THEN
            AUTO_OPTIONS = '12.4,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_2DEXT' ) THEN
            AUTO_OPTIONS = '13,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_RESULT' ) THEN
            AUTO_OPTIONS = '14,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_TRPLT' ) THEN
            AUTO_OPTIONS = '15,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_TRCSIS' ) THEN
            AUTO_OPTIONS = '16,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_DECOS2' ) THEN
            AUTO_OPTIONS = '17,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_DECIMG' ) THEN
            AUTO_OPTIONS = '18,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_QEXTR' ) THEN
            AUTO_OPTIONS = '19,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_WVCSIS' ) THEN
            AUTO_OPTIONS = '20,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_MULMRG' ) THEN
            AUTO_OPTIONS = '21,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_MDLBCK' ) THEN
            AUTO_OPTIONS = '22,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_TUNE' ) THEN
            AUTO_OPTIONS = '23,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_ONE' ) THEN
            AUTO_OPTIONS = '24,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_ALL' ) THEN
            AUTO_OPTIONS = '25,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_IGNORE' ) THEN
            AUTO_OPTIONS = '26,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_PLOT' ) THEN
            AUTO_OPTIONS = '27,EXIT'

         ELSE IF ( ACTION .EQ. 'ECH_GENFLAT' ) THEN
            AUTO_OPTIONS = '30,EXIT'

         ELSE IF ( ACTION .EQ. 'KILL'.OR.
     :             ACTION .EQ. 'ECH_EXIT' ) THEN
            CALL ECH_REPORT( 0, ' ECHOMOP monolith closing down.' )
            CALL ECH_MODULE_TIDYUP( 'ECHOMOP', STATUS )
            CALL ECH_CLOSEDOWN( STATUS )
            RETURN

         ELSE
            SOME_AUTO = .FALSE.
         END IF

      ELSE IF ( AUTO_OPTIONS .EQ. 'KILL' ) THEN
         CALL ECH_REPORT( 0, ' ECHOMOP monolith closing down.' )
         CALL ECH_MODULE_TIDYUP( 'ECHOMOP', STATUS )
         CALL ECH_CLOSEDOWN( STATUS )
         RETURN
      END IF

*  Setup first recommended operation.
      IF ( .NOT. USR_TUNE_SERVER ) THEN
         CALL ECH_TRANSFER_INT( %VAL( IADDR_SO_FAR ), DEFAULT_OPTION )
      END IF
      DEFAULT_OPTION = MAX( DEFAULT_OPTION, 1 )
      IF ( DEFAULT_OPTION .GT. OPTION_STD_END ) DEFAULT_OPTION = 1
      LAST_OPTION = DEFAULT_OPTION

*  If QUICK/BATCH modes then setup defaults.
      IF ( USR_TUNE_QUICK ) CALL ECH_TUNE_QUICK( STATUS )
      IF ( USR_TUNE_BATCH ) CALL ECH_TUNE_BATCH( STATUS )

*  Determine interactive/automatic status.
      INTERACTIVE = .NOT. USR_TUNE_BATCH
      SYS_COMMAND = ' '
      IF ( INTERACTIVE ) THEN
         FULL_MENU = .TRUE.
      END IF
      IF ( AUTO_OPTIONS .EQ. 'SERVER' ) THEN
         INTERACTIVE = .FALSE.
      END IF
      EXIT_SELECTED = .FALSE.

*  Loop processing options until asked to exit or a fatal error occurs.
      DO WHILE ( .NOT. ECH_FATAL_ERROR( STATUS ) .AND.
     :           .NOT. EXIT_SELECTED )

*     If running interactively then
         IF ( INTERACTIVE ) THEN
            IF ( DEFAULT_OPTION .EQ. 0 )
     :         DEFAULT_OPTION = MIN( LAST_OPTION, OPTION_STD_END )

*        If there are no pre-specified automatic options to be done then
            IF ( .NOT. SOME_AUTO ) THEN

*       Present options (full menu first time, limited subset subsequently)
            IF ( FULL_MENU ) THEN
             CALL ECH_REPORT( 0, ' ' )
             CALL ECH_REPORT( 0,
     :' Menu options containing a capitalised keyword may be' )
             CALL ECH_REPORT( 0,
     :' selected by typing the keyword or its first letter.' )
             CALL ECH_REPORT( 0,
     :' Any option can be selected by typing its number.' )
             CALL ECH_REPORT( 0,
     :' Use negative values to start parameter editing for an option.' )
             CALL ECH_REPORT( 0,
     :' For example, "-3" starts the parameter editor for option 3.' )
             CALL ECH_REPORT( 0, ' ' )
             CALL ECH_REPORT( 0, ' Main menu options:' )
             WRITE ( report_string, 1005 ) 0, option_text( 0 )
             CALL ECH_REPORT( 0, report_string )
             DO i = 1, options_in_use / 2
                WRITE ( report_string, 1007 )
     :                i,
     :                option_text(i)( :32),
     :                i + options_in_use/2,
     :                option_text(i+options_in_use/2)( :32)
                CALL ECH_REPORT( 0, report_string )
             END DO
             iioptions = options_in_use
             IF ( (options_in_use/2)*2 .NE.iioptions ) THEN
               WRITE ( report_string, 1008 ) options_in_use,
     :               option_text(options_in_use)
               CALL ECH_REPORT( 0, report_string )
             END IF
          ELSE

             IF  ( .NOT. explained_menu ) THEN
                CALL ECH_REPORT( 0, ' ' )
                CALL ECH_REPORT( 0,
     :' From now on the main menu will be in a short form.' )
                CALL ECH_REPORT( 0,
     :' Only the most likely next options, and general-' )
                CALL ECH_REPORT( 0,
     :' purpose options will be displayed.' )
                CALL ECH_REPORT( 0,
     :' The full menu can be displayed using option M.' )
                CALL ECH_REPORT( 0, ' ' )
                explained_menu = .TRUE.
             END IF

             WRITE ( report_string, 1005 ) 0, option_text(0)
             CALL ECH_REPORT( 0, report_string )
             DO i = MAX ( option_std_start, default_option - 1 ),
     :              MIN ( option_std_end, default_option + 1 )
                WRITE ( report_string, 1005 ) i, option_text(i)
                CALL ECH_REPORT( 0, report_string )
             END DO

             DO i = default_option+2, special_options_at-1
                IF ( extract_2d )  THEN
                   addoption = next_2d_option ( i )
                ELSE
                   addoption = next_1d_option ( i )
                END IF
                IF ( -addoption .EQ. default_option ) THEN
                   WRITE ( report_string, 1005 ) i,
     :                                                option_text(i)
                   CALL ECH_REPORT( 0, report_string )
                END IF
             END DO

             DO i = special_options_at, options_in_use
                WRITE ( report_string, 1005 ) i, option_text(i)
                CALL ECH_REPORT( 0, report_string )
             END DO
           END IF

           IF ( ORDER_NUMBER .GT. 0 ) THEN
              CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
              REPORT_STRING = ' Set to process order ' //
     :              REF_STR1( :NCHAR1 ) // ' only.'
              CALL ECH_REPORT( 0, REPORT_STRING )
           END IF
           CALL ECH_REPORT( 0,
     :          ' Use -nn to edit/view option parameters.' )
           CALL ECH_REPORT( 0, ' ' )
           FULL_MENU = .FALSE.
          END IF

*      Calculate default option number
          value = FLOAT( default_option )
          IF ( option .LE. option_std_end ) last_option = option
          IF ( default_option .GT. 9 ) THEN
             WRITE ( cvalue, '(I2,1X)' ) default_option

          ELSE
             WRITE ( cvalue, '(I1,2X)' ) default_option
          END IF

*      If there are some automatic options left to be done then
          IF ( some_auto .AND. auto_options .NE. ' ' ) THEN
           auto_idx = 0
           got_aopt = .FALSE.

*          Loop until parsed whole list of automatic options
           DO WHILE ( auto_idx .LE. CHR_LEN ( auto_options )
     :                                 .AND. .NOT. got_aopt )
             auto_idx = auto_idx + 1

*            If seperator found then snip out option to be done next
             IF ( auto_options ( auto_idx:auto_idx ) .EQ. ',' .OR.
     :            auto_options ( auto_idx:auto_idx ) .EQ. ' '  ) THEN
                cvalue = auto_options( 1:auto_idx-1 ) // ' '
                got_aopt =.TRUE.
                auto_options = auto_options ( auto_idx+1: ) // ' '
             END IF
           END DO

           IF ( .NOT. got_aopt ) clone_from='NULL'

*      Else clear auto options flag
          ELSE
             SOME_AUTO = .FALSE.
          END IF


*      If no automatic options remaining then prompt user for option
          IF ( .NOT. some_auto ) THEN
             orig = cvalue
             cvalue = 'or Y for default=' // orig
             DO WHILE ( cvalue( :16 ) .EQ. 'or Y for default' .OR.
     :                  cvalue( :16 ) .EQ. 'OR Y FOR DEFAULT'     )
              cvalue = 'or Y for default=' // orig
              CALL ECH_GET_PARAMETER(
     :             'INSTANT-PROMPT=Option number ', 'CHAR', 0.0,
     :             .FALSE., CVALUE, 0, STATUS )
             END DO
          END IF

*      Trim any leading spaces from option text and report if automatic.
          I = 1
          DO WHILE ( CVALUE( I:I ) .EQ. ' ' )
             I = I + 1
          END DO
          IF ( I .GT. 1 ) THEN
             CVALUE = CVALUE( I: )
          END IF
          IF ( SOME_AUTO .AND. ( ACTION .EQ. 'ECHMENU' .OR.
     :         ACTION .EQ. 'ECHOMOP' ) ) THEN
             REPORT_STRING = ' Auto-running: Option ' //
     :             CVALUE( :CHR_LEN( CVALUE ) ) // '.'
             CALL ECH_REPORT( 0, REPORT_STRING )
          END IF
          SUBOPTION = 0

*      Check for `clone-from' syntax (Eg. 9<OLD_FILE )
          IF ( CLONE_FROM .EQ. 'NULL' ) THEN
             DO I = 1, CHR_LEN( CVALUE )
                IF ( CVALUE( I:I ) .EQ. '<' ) THEN
                   CLONE_FROM = CVALUE( I + 1 : ) // ' '
                   CVALUE = CVALUE( : I - 1 ) // ' '
                   REPORT_STRING = ' Results are to be cloned from' //
     :                ' reduction file ' // CLONE_FROM
                   CALL ECH_REPORT( 0, REPORT_STRING )
                END IF
             END DO
          END IF

*      Cloning cannot be done during client-server operation as the
*      results are ALWAYS read directly from a server response message
*      when operating in this way.
          IF ( .NOT. USR_TUNE_CLIENT .AND. .NOT. USR_TUNE_SERVER )
     :        CALL ECH_SET_PARAMETER( 'TUNE_CLONE', 'CHAR',
     :             0., 0, CLONE_FROM, STATUS )

*      If  `system' command (first character is a $) then get command text.
          ISTAT = 0
          SYS_COMMAND = ' '
          IF ( CVALUE( :1 ) .EQ. '$' ) THEN
             SYS_COMMAND = CVALUE( 2: )

          ELSE
*          Parse the string in cvalue (it might be good to consolidate
*          rather more of the above ad-hoc parsing into a routine such
*          as this, but this is probably complicated enough as it is)
             call ech_parse_option (cvalue, cvalueclass, i1, i2)

             if (cvalueclass .eq. 'y') then ! default
                option = default_option

             else if (cvalueclass .eq. '1') then ! single option
                option = i1

             else if (cvalueclass .eq. '2') then ! option plus suboption
                option = i1
                suboption = i2

             else if (cvalueclass .eq. 'h') then ! help
                option = 0

             else if (cvalueclass .eq. 'Y') then ! hyper
                option = 0
                hyper = .true.

             else if (cvalueclass .eq. 'q') then ! quit/exit
                option = options_in_use

             else if (cvalueclass .eq. 'm') then ! menu
                full_menu = .true.
                option = menu_option

             else if (cvalueclass .eq. 'a') then ! adjust
                option = adjust_option

             else if (cvalueclass .eq. 'p') then ! plot
                option = plot_option

             else if (cvalueclass .eq. 'd') then ! disable
                option = disable_option

             else               ! errors
                option = 0
                suboption = 0
             end if

*          Option 99 means the same as `q', so special-case this
             if ( option .eq. 99 ) option = options_in_use

*          Check for negative option number indicating that `preview' of
*          option parameters is what is required
             preview = .false.
             if ( option .lt. 0 ) then
                option = - option
                preview = .true.
             end if

*        write (report_string, '("DBG:2:option=",i2," suboption=",i3,
*     :       " preview=",l1)') option, suboption, preview
*        call ech_report (0, report_string)

          end if

        ELSE

*         If task is running as a Server then
*            Read a processing request from a client task (waits until one appears)
*            Exit immediately if any problem receiving message
*            Decode processing option required (specified as module name)
*            Call option processor routine
*            Write a processing response message to the client task
          IF ( usr_tune_server ) THEN
             CALL ECH_ACCESS_OBJECT( 'SERVER', 'READ', 'MESSAGE',
     :            0, 0, 0, dumdim, MAX_DIMENSIONS, 0, ' ', status )
             IF ( status .NE. 0 ) RETURN
             CALL ECH_ACCESS_OBJECT( 'MODULE', 'RECEIVE', ' ',
     :            0, 0, 0,  dumdim, MAX_DIMENSIONS, 0, subopt_module,
     :            status )
             order_number = 1
             CALL ECH_ECHOMOP_OPTION( subopt_module,
     :            order_number, status )
             CALL ECH_ACCESS_OBJECT( 'SERVER', 'WRITE', 'MESSAGE',
     :            0, 0, 0, dumdim, MAX_DIMENSIONS, 0, ' ', status )

*         Else calculate next option number to setup as default

          ELSE IF ( extract_2d )  THEN
             option = next_2d_option ( option )
          ELSE
             option = next_1d_option ( option )
          END IF
        END IF

*        write (report_string, '("DBG:1:option=",i2," suboption=",i3,
*     :       " subopt_module=",a," option_module()=",a)')
*     :       option, suboption, subopt_module, option_module(option)
*        call ech_report (0, report_string)

*       If a suboption number has been specified then
*          Locate main option for which it is specified
*          Check that specified suboption exists and calculate default next option
*       Endif
        IF ( suboption .GT. 0 ) THEN
           isubmenu = 0
           subopt_module = ' '
           DO i = 1, num_suboptions
              IF ( option_module(option) .EQ. submenu_options(i) )
     :                                              isubmenu = i + 1
           END DO
           IF ( isubmenu .GT. 0 ) THEN
              ii = 1
              DO WHILE ( submenu_options(isubmenu)(1:8) .NE.
     :                   'Submenu_' )
                 IF ( ii .EQ. suboption ) THEN
                    subopt_module = submenu_options(isubmenu)
                    subopt_text = submenu_text(isubmenu)
                    IF ( submenu_options(isubmenu+1)(1:8)
     :                                         .EQ. 'Submenu_' ) THEN
                     IF ( extract_2d ) THEN
                       default_option = MAX( default_option, 0,
     :                               next_2d_option ( option ) )
                     ELSE
                       default_option = MAX( default_option, 0,
     :                               next_1d_option ( option ) )
                     END IF
                    END IF
                 END IF
                 isubmenu = isubmenu + 1
                 ii = ii + 1
              END DO

           ELSE
              CALL ECH_REPORT( 0,
     :             ' internal error: submenu not found.' )
           END IF
        END IF

*     If operating as a server then carry on with loop.
         IF ( USR_TUNE_SERVER ) THEN
            CONTINUE

*     If option number too big, set hints-needed flag.
         ELSE IF ( OPTION .GT. OPTIONS_IN_USE ) THEN
            FULL_MENU = .TRUE.
            HINTS = .TRUE.

*     If EXIT requested, set flag.
         ELSE IF ( OPTION_MODULE( OPTION ) .EQ.
     :             'ECHOMOP_Exit' ) THEN
            EXIT_SELECTED = .TRUE.

*     If full menu requested, set flag.
         ELSE IF ( OPTION_MODULE( OPTION ) .EQ.
     :             'ECHOMOP_Full_Menu' ) THEN
            FULL_MENU = .TRUE.

*     If system command option, execute it.
         ELSE IF ( OPTION_MODULE( OPTION )
     :             .EQ. 'ECHOMOP_$Command' .OR.
     :             SYS_COMMAND .NE. ' ' ) THEN
            IF ( SYS_COMMAND .EQ. ' ' ) THEN
               CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=System_$',
     :              'CHAR', VALUE, .FALSE., SYS_COMMAND, 0, STATUS )
            END IF
            CALL ECH_REPORT( 0, ' ' )
            CALL ECH_REPORT( 0,
     :           'Executing operating system level command(s).' )
            REPORT_STRING = '---------------------------------------' //
     :           '-----------------------------------------'
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL ONE_EXEC( SYS_COMMAND, STATUS )
            REPORT_STRING = '---------------------------------------' //
     :            '-----------------------------------------'
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL ECH_REPORT( 0, ' ' )

*     Set all-order mode.
         ELSE IF ( OPTION_MODULE( OPTION ) .EQ.
     :             'ECHOMOP_All_Orders' ) THEN
            ORDER_NUMBER = 0
            CALL ECH_REPORT( 0,
     :           ' Set to process all orders automatically.' )

*     Set single-order mode.
         ELSE IF ( OPTION_MODULE( OPTION ) .EQ.
     :             'ECHOMOP_Per_Order' ) THEN
            CALL ECH_REPORT( 0, ' Set to process single orders' )
            VALUE = FLOAT( ORDER_NUMBER + 1 )
            CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Order to process',
     :           'INT', VALUE, .FALSE., ' ', 0, STATUS )
            ORDER_NUMBER = INT( ABS( VALUE ) )

         ELSE IF ( OPTION_MODULE( OPTION ) .EQ. 'ECH_ACTIVE_TUNE' ) THEN
            CALL ECH_ACTIVE_TUNE( OPTION_MODULE( DEFAULT_OPTION ),
     :           OPTION_TEXT( DEFAULT_OPTION ), STATUS )

*     If a parameter preview requested, call parameter editor routine.
         ELSE IF ( PREVIEW ) THEN
            IF ( SUBOPTION .GT. 0 ) THEN
               CALL ECH_EDIT_PARAMETERS( SUBOPT_MODULE, SUBOPT_TEXT,
     :              STATUS )

            ELSE
               CALL ECH_EDIT_PARAMETERS( OPTION_MODULE( OPTION ),
     :              OPTION_TEXT( OPTION ), STATUS )
            END IF
            DEFAULT_OPTION = OPTION

*     If option has suboptions.
         ELSE IF ( OPTION_MODULE( OPTION )( :8 ) .EQ. 'Submenu_' ) THEN
            IF ( SUBOPTION .GT. 0 ) THEN
               WRITE ( REPORT_STRING, 1002 )
               CALL ECH_REPORT( 0, REPORT_STRING )
               IF ( CLONE_FROM .NE. 'NULL' ) THEN
                  WRITE ( REPORT_STRING, 1004 ) SUBOPT_TEXT

               ELSE
                  WRITE ( REPORT_STRING, 1003 ) SUBOPT_TEXT
               END IF
               CALL ECH_REPORT( 0, REPORT_STRING )
               CALL ECH_REPORT( 0, ' ' )
               CALL ECH_ECHOMOP_OPTION( SUBOPT_MODULE, ORDER_NUMBER,
     :              STATUS )
               WRITE ( REPORT_STRING, 1002 )
               CALL ECH_REPORT( 0, REPORT_STRING )
               CALL ECH_REPORT( 0, ' ' )

            ELSE
               WRITE ( REPORT_STRING, 1002 )
               CALL ECH_REPORT( 0, REPORT_STRING )
               IF ( CLONE_FROM .NE. 'NULL' ) THEN
                  WRITE ( REPORT_STRING, 1004 ) OPTION_TEXT( OPTION )

               ELSE
                  WRITE ( REPORT_STRING, 1003 ) OPTION_TEXT( OPTION )
               END IF
               CALL ECH_REPORT( 0, REPORT_STRING )
               CALL ECH_REPORT( 0, ' ' )
               ISUBMENU = 0
               DO I = 1, NUM_SUBOPTIONS
                  IF ( OPTION_MODULE( OPTION ) .EQ.
     :                 SUBMENU_OPTIONS( I ) )
     :            ISUBMENU = I + 1
               END DO
               IF ( ISUBMENU .GT. 0 ) THEN
                  DO WHILE ( SUBMENU_OPTIONS( ISUBMENU )( :8 ) .NE.
     :                   'Submenu_' .AND. STATUS .EQ. 0 )
                     CALL ECH_ECHOMOP_OPTION(
     :                    SUBMENU_OPTIONS( ISUBMENU ),
     :                    ORDER_NUMBER, STATUS )
                     ISUBMENU = ISUBMENU + 1
                  END DO

               ELSE
                  CALL ECH_REPORT( 0,
     :                 ' Internal error: submenu not found.' )
               END IF

               WRITE ( REPORT_STRING, 1002 )
               CALL ECH_REPORT( 0, REPORT_STRING )
               CALL ECH_REPORT( 0, ' ' )
               IF ( EXTRACT_2D ) THEN
                  DEFAULT_OPTION = MAX( DEFAULT_OPTION, 0,
     :                  NEXT_2D_OPTION( OPTION ) )
               ELSE
                  DEFAULT_OPTION = MAX( DEFAULT_OPTION, 0,
     :                  NEXT_1D_OPTION( OPTION ) )
               END IF
            END IF

            IF ( .NOT. ECH_FATAL_ERROR( STATUS ) .AND.
     :           .NOT. USR_TUNE_SERVER )
     :         CALL ECH_TRANSFER_INT( OPTION, %VAL( IADDR_SO_FAR ) )

*     If valid option number provided then call the option processor
*     routine for option.
         ELSE IF ( OPTION .GT. 0 ) THEN
            WRITE ( REPORT_STRING, 1002 )
            CALL ECH_REPORT( 0, REPORT_STRING )
            IF ( CLONE_FROM .NE. 'NULL' ) THEN
               WRITE ( REPORT_STRING, 1004 ) OPTION_TEXT( OPTION )

            ELSE
               WRITE ( REPORT_STRING, 1003 ) OPTION_TEXT( OPTION )
            END IF
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL ECH_REPORT( 0, ' ' )
            CALL ECH_ECHOMOP_OPTION( OPTION_MODULE( OPTION ),
     :           ORDER_NUMBER, STATUS )

            IF ( .NOT. ECH_FATAL_ERROR( STATUS )  .AND.
     :           .NOT. USR_TUNE_SERVER  )
     :         CALL ECH_TRANSFER_INT( OPTION, %VAL( IADDR_SO_FAR ) )
            WRITE ( REPORT_STRING, 1002 )
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL ECH_REPORT( 0, ' ' )
            IF ( EXTRACT_2D ) THEN
               DEFAULT_OPTION = MAX( DEFAULT_OPTION, 0,
     :               NEXT_2D_OPTION( OPTION ) )

            ELSE
               DEFAULT_OPTION = MAX( DEFAULT_OPTION, 0,
     :               NEXT_1D_OPTION( OPTION ) )
            END IF

*     HELP request.
         ELSE IF ( OPTION .EQ. 0  ) THEN
            IF ( HYPER ) THEN
               XLATE = 'ECHOMOP_HYPER:'
               CALL ECH_PARSE_ENV( XLATE, XLEN )
               CALL ONE_EXEC( XLATE, STATUS )
               HYPER = .FALSE.

            ELSE
               TOPIC = OPTION_TEXT( DEFAULT_OPTION )
               I1 = CHR_INDEX( TOPIC, '(' )
               I2 = CHR_INDEX( TOPIC, ')' )
               IF ( I2 .GT. I1 ) THEN
                  TOPIC = OPTION_TEXT( DEFAULT_OPTION )( I1+1 : I2-1 )

               ELSE
                  TOPIC = OPTION_MODULE( DEFAULT_OPTION )
               END IF
               WORK_STRING = 'ECHMENU_Option ' // TOPIC
               CALL ECH_HELP( WORK_STRING, .TRUE., STATUS )
            END IF
            FULL_MENU = .TRUE.

         ELSE
            FULL_MENU = .TRUE.
            HINTS = .TRUE.
         END IF

*     Give hints if they are needed.
         IF ( HINTS ) THEN
            CALL ECH_REPORT( 0, ' ' )
            REPORT_STRING = ' Unknown option: "' //
     :            CVALUE( :ECH_WORD_LEN( CVALUE ) ) // '".'
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL ECH_REPORT( 0, ' Please type either:' )
            CALL ECH_REPORT( 0,
     :      '  -  an option number.  For example, "23".')
            CALL ECH_REPORT( 0,
     :      '  -  an option-parameter view/edit number.  E.g., "-23".' )
            CALL ECH_REPORT( 0,
     :      '  -  0, H or HELP for default-option HELP.' )
            CALL ECH_REPORT( 0,
     :      '  -  HYPER to start the hypertext HELP browser.' )
            CALL ECH_REPORT( 0,
     :      '  -  99, E or EXIT to leave the program.' )
            CALL ECH_REPORT( 0,
     :      '  -  $<text>, where <text> is a system command.' )
            HINTS = .FALSE.
         END IF

*     Check for rewind to previous option request.
         IF ( STATUS .EQ. ECH__NEED_RDCOBJ ) THEN
            STATUS = 0
            DEFAULT_OPTION = CONTEXT_MODE
            CONTEXT_MODE = CTX_ECHOMOP_SHELL

*     Check for abort option (but continue ECHOMOP) request,
*     or cloning problem.
         ELSE IF ( STATUS .EQ. ECH__ABORT_OPTION .OR.
     :             STATUS .EQ. ECH__NO_CLONE ) THEN
            STATUS = 0
         END IF

*     If abort request from parameter system, setup return to option menu.
         IF ( ECH_FATAL_ERROR( STATUS ) .AND. INTERACTIVE ) THEN
            STATUS = 0
            DEFAULT_OPTION = OPTION
         END IF
      END DO

*  Closedown environment specifics.
      AUTO_OPTIONS = ' '
      CALL ECH_SET_PARAMETER( 'TUNE_AUTOMATE', 'CHAR',
     :     0., 0, AUTO_OPTIONS, STATUS )

*  Default `clone' parameter to NULL.
      CLONE_FROM = 'NULL'
      CALL ECH_SET_PARAMETER( 'TUNE_CLONE', 'CHAR',
     :     0., 0, CLONE_FROM, STATUS )

 1002 FORMAT ( 1X, 15(5H-----) )
 1003 FORMAT ( 1X, 'Starting processing task: ', A52 )
 1004 FORMAT ( 1X, 'Copying results of: ', A52 )
 1005 FORMAT ( 1X, I2, '. ', A48 )
 1007 FORMAT ( 1X, I2, '. ', A32, 3X, I2, '. ', A32 )
 1008 FORMAT ( 1X, 2X,  2X, 32X, 3X, I2, '. ', A32 )

      END
