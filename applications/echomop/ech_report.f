      SUBROUTINE ECH_REPORT( CODE, STRING )

*  History:
*     2-MAR-1998 (BLY):
*        Corrected format in WRITE statement - illegal use of `I' format.
*
*     Description : Report all error/info to user
*     Keywords : UTILITY,ENVIRONMENT
*
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_SERVER.INC'

      byte byt_return
      byte byt_space
      byte byt_lb
      byte byt_ld
      byte byt_lf
      byte byt_ln
      byte byt_lp
      byte byt_lu
      byte byt_B
      byte byt_D
      byte byt_F
      byte byt_N
      byte byt_P
      byte byt_U
      byte byt_kpdup
      byte byt_kpddown
      byte byt_kpd0
      byte byt_prevscr
      byte byt_nextscr
      byte byt_kpdenter
      byte byt_uparrow
      byte byt_downarrow

      character*1 chr_return
      character*1 chr_space
      character*1 chr_lb
      character*1 chr_ld
      character*1 chr_lf
      character*1 chr_ln
      character*1 chr_lp
      character*1 chr_lu
      character*1 chr_B
      character*1 chr_D
      character*1 chr_F
      character*1 chr_N
      character*1 chr_P
      character*1 chr_U
      character*1 chr_kpdup
      character*1 chr_kpddown
      character*1 chr_kpd0
      character*1 chr_prevscr
      character*1 chr_nextscr
      character*1 chr_kpdenter
      character*1 chr_downarrow
      character*1 chr_uparrow

      EQUIVALENCE ( byt_return, chr_return )
      EQUIVALENCE ( byt_kpdup, chr_kpdup )
      EQUIVALENCE ( byt_kpddown, chr_kpddown )
      EQUIVALENCE ( byt_kpd0, chr_kpd0 )
      EQUIVALENCE ( byt_prevscr, chr_prevscr )
      EQUIVALENCE ( byt_nextscr, chr_nextscr )
      EQUIVALENCE ( byt_kpdenter, chr_kpdenter )
      EQUIVALENCE ( byt_lb, chr_lb )
      EQUIVALENCE ( byt_ld, chr_ld )
      EQUIVALENCE ( byt_lf, chr_lf )
      EQUIVALENCE ( byt_ln, chr_ln )
      EQUIVALENCE ( byt_lp, chr_lp )
      EQUIVALENCE ( byt_lu, chr_lu )
      EQUIVALENCE ( byt_B, chr_B )
      EQUIVALENCE ( byt_D, chr_D )
      EQUIVALENCE ( byt_F, chr_F )
      EQUIVALENCE ( byt_N, chr_N )
      EQUIVALENCE ( byt_P, chr_P )
      EQUIVALENCE ( byt_U, chr_U )
      EQUIVALENCE ( byt_space, chr_space )
      EQUIVALENCE ( byt_uparrow, chr_uparrow )
      EQUIVALENCE ( byt_downarrow,chr_downarrow)

      INTEGER CODE
      INTEGER LINES
      INTEGER ISTAT
      INTEGER STATUS
      INTEGER START
      INTEGER REM_START

      LOGICAL GO_ON

      CHARACTER*( * ) STRING
      CHARACTER*80 OPENED_NAME

*  Functions Called:
      INTEGER CHR_LEN

*  Data Statements:
      DATA BYT_RETURN     /  13 /
      DATA BYT_KPDUP      /  12 /
      DATA BYT_KPDDOWN    /   6 /
      DATA BYT_KPD0       /   4 /
      DATA BYT_PREVSCR    /  59 /
      DATA BYT_NEXTSCR    /  60 /
      DATA BYT_KPDENTER   /  13 /
      DATA BYT_UPARROW    /  18 /
      DATA BYT_DOWNARROW  /  19 /
      DATA CHR_SPACE      / ' ' /
      DATA CHR_LB         / 'b' /
      DATA CHR_LD         / 'd' /
      DATA CHR_LF         / 'f' /
      DATA CHR_LN         / 'n' /
      DATA CHR_LP         / 'p' /
      DATA CHR_LU         / 'u' /
      DATA CHR_B          / 'B' /
      DATA CHR_D          / 'D' /
      DATA CHR_F          / 'F' /
      DATA CHR_N          / 'N' /
      DATA CHR_P          / 'P' /
      DATA CHR_U          / 'U' /
      DATA LINES          /   0 /
*.

*  Check for diagnostic logging, if needed open log file.
      DIAGNOSTICS_ACTIVE = USR_TUNE_DIAGNOSE
      IF ( DIAGNOSTICS_ACTIVE .AND. DIAGNOSTICS_LUN .EQ. 0 ) THEN
         CALL ECH_OPEN_FILE( 'echomop_diagnostics.log', 'TEXT',
     :        'NEW', .FALSE., DIAGNOSTICS_LUN, OPENED_NAME, STATUS )
      ENDIF

*  Check for output paging.
      IF ( USR_TUNE_PAGE .GT. 0 ) THEN
         IF ( code .NE. rpm_nopage ) THEN
            rpm_line_index = rpm_line_index + 1
            IF ( rpm_line_index .GT. rpm_lindex_size )
     :         rpm_line_index = 1
            lines = lines + 1
            IF ( lines .EQ. usr_tune_page ) THEN
               lines = 0
               CALL MSG_OUT(' ','<space> to continue',status )
               go_on = .FALSE.
               start = rpm_line_index - usr_tune_page + 1
               rem_start = start
               IF ( start .LT. 1 ) start = start + rpm_lindex_size
               DO WHILE ( .NOT. go_on )
                  CALL ECH_READ_KEYBOARD( istat )
                  IF ( user_input_char(1:1) .EQ. ' ' ) THEN
                     go_on = .TRUE.

                  ELSE IF ( user_input_char(1:1)
     :                      .EQ. chr_return ) THEN
                     lines = usr_tune_page - 1
                     go_on = .TRUE.

                  ELSE IF ( user_input_char .EQ. chr_uparrow .OR.
     :                      user_input_char .EQ. chr_kpdup .OR.
     :                      user_input_char .EQ. chr_lb .OR.
     :                      user_input_char .EQ. chr_lp .OR.
     :                      user_input_char .EQ. chr_lu .OR.
     :                      user_input_char .EQ. chr_B .OR.
     :                      user_input_char .EQ. chr_P .OR.
     :                      user_input_char .EQ. chr_U ) THEN
                     start = start - 1
                     IF ( start .LT. 1 ) start = start + rpm_lindex_size
                     CALL ECH_REPORT_PAGE( start, usr_tune_page )

                  ELSE IF ( user_input_char .EQ. chr_downarrow .OR.
     :                      user_input_char .EQ. chr_kpddown .OR.
     :                      user_input_char .EQ. chr_ld .OR.
     :                      user_input_char .EQ. chr_lf .OR.
     :                      user_input_char .EQ. chr_ln .OR.
     :                      user_input_char .EQ. chr_D .OR.
     :                      user_input_char .EQ. chr_F .OR.
     :                      user_input_char .EQ. chr_N ) THEN
                     start = start + 1
                     IF ( start .GT. rpm_lindex_size )
     :                  start = start - rpm_lindex_size
                     CALL ECH_REPORT_PAGE ( start, usr_tune_page )

                  ELSE IF ( user_input_char .EQ. chr_prevscr ) THEN
                     start = start - usr_tune_page + 1
                     IF ( start .LT. 1 ) start = start + rpm_lindex_size
                     CALL ECH_REPORT_PAGE ( start, usr_tune_page )

                  ELSE IF ( user_input_char .EQ. chr_nextscr ) THEN
                     start = start + usr_tune_page - 1
                     IF ( start .GT. rpm_lindex_size )
     :                  start = start - rpm_lindex_size
                     CALL ECH_REPORT_PAGE( start, usr_tune_page )
                  ENDIF
               END DO

               IF ( start .NE. rem_start ) THEN
                  CALL ECH_REPORT_PAGE( rem_start, usr_tune_page )
               ENDIF
            ENDIF
            rpm_buffer( rpm_line_index ) = string
         ENDIF
      ENDIF

      IF ( code .EQ. rpm_log ) THEN
         IF ( usr_tune_diagnose ) THEN
            WRITE ( diagnostics_lun, '( 1X, A )' )
     :              string( :MAX( 1, CHR_LEN( string ) ) )
         ENDIF

      ELSE IF ( code .EQ. ECH__TXT_INFO ) THEN
         ISTAT = SAI__OK
         CALL MSG_OUT(' ',string(1:MAX(1,CHR_LEN(string))),istat)
         IF ( usr_tune_diagnose ) THEN
            WRITE ( diagnostics_lun, '( 1X, A )' )
     :            string(1:MAX(1,CHR_LEN(string)))
         ENDIF

      ELSE IF ( code .NE. 0 ) THEN
         ISTAT = SAI__OK
         CALL MSG_SETI( 'ECHCODE', CODE )
         CALL MSG_OUT( ' ', ' ECH_REPORT called with code ^ECHCODE.',
     :        ISTAT )
         IF ( USR_TUNE_DIAGNOSE ) THEN
            WRITE ( DIAGNOSTICS_LUN, '( 1X, A, I8 )' )
     :            'ECH_REPORT called with code ', code
         ENDIF

      ELSE IF ( LEN( string ) .GT. 0 ) THEN
         ISTAT = SAI__OK
         CALL MSG_OUT(' ',string(1:MAX(1,CHR_LEN(string))),istat)
         IF ( usr_tune_diagnose ) THEN
            WRITE ( diagnostics_lun, '( 1X, A )' )
     :            string(1:MAX(1,CHR_LEN(string)))
         ENDIF
      ENDIF

      END


      SUBROUTINE ECH_REPORT_PAGE ( START, COUNT )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'ECH_REPORT.INC'

      INTEGER START, COUNT, I, II, ISTAT

      I = 0
      II = START - 1

      ISTAT = SAI__OK
      DO WHILE ( I .LT. COUNT .AND. II .NE. RPM_LINE_INDEX )
         I = I + 1
         IF ( II+1 .GT. RPM_LINDEX_SIZE ) THEN
            II = 0
         END IF
         II = II + 1
         CALL MSG_OUT( ' ', RPM_BUFFER( II ), ISTAT )
      END DO

      END
