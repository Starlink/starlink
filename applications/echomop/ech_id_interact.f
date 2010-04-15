      SUBROUTINE ECH_ID_INTERACT(
     :           N_ORDERS,
     :           MAXIMUM_POLY,
     :           OPTION,
     :           ORDER,
     :           WAVE_NPOLY,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_ID_INTERACT

*  Purpose:
*     Allows interactive selection of main options for calibration.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine allows selection of  the major optional operations
*     during wavelength calibration. Options are presented in a menu
*     form and selected by typing a one- or two-character string,
*     followed by carriage return.  The following options are
*     supported:
*
*     AI (automatic identification), initiates a search and match of
*     the feature database. Any preset limitations on the wavelength
*     and dispersion range will be taken into account. When a solution
*     is found, a report to the user includes a probable status and the
*     user may then opt to investigate the solution further (I), or
*     search for further solutions by re-selecting the AI option.
*
*     AF ( automatic polynomial fitting), performs a cycle of fitting
*     polynomials to identified lines, then clipping away any
*     badly deviant lines, and re-fitting, etc. Fitting ceases when a
*     stable set of identified features remains.
*
*     E (exit), leaves the line identification menu and updates the
*     reduction database copy of the wavelength polynomial to reflect
*     the lastest calculated fit.
*
*     F (fit), does a single shot polynomial fit to the currently
*     identified lines. A report of deviations is generated but no
*     automatic  clipping of deviant lines is performed.
*
*     H (help), provides interactive browsing of the relevant sections
*     of the help library for line identification.
*
*     I  (interactive identification), enters the interactive line
*     specification, examination section. This section provides features
*     for addition/deletion/re-fitting/listing etc of identified lines.
*
*     IM (import ECHARC data), is used to provide an interface to the
*     ECHARC arc line identification program. In general the XP option
*     would first be used to export data for ECHARC processing. ECHARC
*     would then be run,and finally the data IMported back into this
*     package.
*
*     O (order selection), is used to change the currently selected order
*     when operating manually. The order number which is to be selected
*     will be prompted for.
*
*     P (polynomial degree), is used to alter the degree of polynomial
*     to be used for the wavelength fitting. This may vary up to the
*     maximum specified by the tunable parameter TUNE_MAXPOLY.
*
*     XP (export data for ECHARC), is used to provide an interface to the
*     ECHARC arc line identification program. In general the XP option
*     would first be used to export data for ECHARC processing. ECHARC
*     would then be run,and finally the data IMported back into this
*     package.

*  Invocation:
*     CALL ECH_ID_INTERACT(
*     :    N_ORDERS,
*     :    MAXIMUM_POLY,
*     :    OPTION,
*     :    ORDER,
*     :    WAVE_NPOLY,
*     :    STATUS
*     :   )

*  Arguments:
*     N_ORDERS = INTEGER (Given)
*        Number of orders in frame.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum polynomial degree allowed.
*     OPTION = CHAR*( * ) (Returned)
*        The option(s) required.
*     ORDER = INTEGER (Returned)
*        Order number to be processed.
*     WAVE_NPOLY = INTEGER (Returned)
*        Selected degree of polynomial.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     16-MAY-1997 (MJC):
*       Added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      INTEGER N_ORDERS
      INTEGER ORDER
      INTEGER MAXIMUM_POLY
      INTEGER WAVE_NPOLY

*  Arguments Returned:
      CHARACTER*( * ) OPTION

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL OVALUE
      REAL PVALUE

      LOGICAL GOTIT

      CHARACTER*80 REPLY

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      OVALUE = FLOAT( ORDER )
      PVALUE = FLOAT( WAVE_NPOLY )
      REPLY = 'I'
      STATUS = 0
      OPTION = 'NULL'

      WRITE ( REPORT_STRING, 1000 ) ORDER
      CALL ECH_REPORT( 0, REPORT_STRING )
      WRITE ( REPORT_STRING, 1001 ) WAVE_NPOLY
      CALL ECH_REPORT( 0, REPORT_STRING )
      CALL ECH_REPORT( 0, ' ' )
      CALL ECH_REPORT( 0, ' Options are as follows:' )
      CALL ECH_REPORT( 0, ' ' )
      CALL ECH_REPORT( 0, '  AI - Auto-identify features.' )
      CALL ECH_REPORT( 0, '  I  - Manually identify features.')
      CALL ECH_REPORT( 0, '  H  - Help.' )
      CALL ECH_REPORT( 0, '  Q  - Quit, ignoring any new fits.' )
      CALL ECH_REPORT( 0, '  E  - Exit, saving new fits.' )
      CALL ECH_REPORT( 0, '  O  - Change selected order.' )
      CALL ECH_REPORT( 0, '  P  - Change degree of fit.' )
      CALL ECH_REPORT( 0, '  XP - Export to ECHARC.' )
      CALL ECH_REPORT( 0, '  IM - Import from ECHARC.' )
      CALL ECH_REPORT( 0, ' ' )

      CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Please select option',
     :     'CHAR', 0.0, .FALSE., REPLY, 0, STATUS )

      CALL CHR_UCASE( REPLY )
      IF ( REPLY .EQ. 'O' ) THEN
         GOTIT = .FALSE.
         DO WHILE ( .NOT. GOTIT )
            CALL ECH_REPORT( 0, ' Change selected order to:' )
            CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Order number ',
     :           'INT', OVALUE, .FALSE., ' ', 0, STATUS )

            IF  ( INT( OVALUE ) .GT. 0 .AND.
     :          INT( OVALUE ) .LE. N_ORDERS ) THEN
               ORDER = INT( OVALUE )
               GOTIT = .TRUE.

            ELSE
               CALL ECH_REPORT( 0,
     :              ' Requested order number out of range.' )
            END IF
         END DO
         REPLY = 'I'

      ELSE IF ( REPLY .EQ. 'P' ) THEN
         GOTIT = .FALSE.
         DO WHILE ( .NOT. GOTIT )
            CALL ECH_REPORT( 0, ' Change number of coefficients to:' )
            CALL ECH_GET_PARAMETER(
     :           'INSTANT-PROMPT=Number of coefficients ',
     :           'INT', PVALUE, .FALSE., ' ', 0, STATUS )
            IF ( INT( PVALUE ) .GT. 1 .AND.
     :           INT( PVALUE ) .LE. MAXIMUM_POLY )  THEN
               WAVE_NPOLY = INT( PVALUE )
               GOTIT = .TRUE.

            ELSE
               CALL ECH_REPORT( 0,' Requested number out of range.' )
            END IF
         END DO
         REPLY = 'I'

      ELSE IF ( REPLY .EQ. 'H' ) THEN
         OPTION = 'HELP'
         REPLY = 'I'

      ELSE IF ( REPLY .EQ. 'Q' ) THEN
         OPTION = 'QUIT'
         STATUS = ECH__QUIT_WFIT

      ELSE IF ( REPLY .EQ. 'XP' ) THEN
         OPTION = 'SAVE FIGARO=ECHARC'
         REPLY = 'Q'

      ELSE IF ( REPLY .EQ. 'IM' ) THEN
         OPTION = 'LOAD FIGARO=ECHARC'
         REPLY = 'I'

      ELSE IF ( REPLY .EQ. 'E' ) THEN
         OPTION = 'EXIT'

      ELSE IF ( REPLY .EQ. 'AI' ) THEN
         OPTION = 'AUTOIDENTIFY ORDER'

      ELSE IF ( REPLY .EQ. 'I' ) THEN
         OPTION = 'IDENTIFY'

      ELSE
         REPORT_STRING = ' Unknown option: ' // REPLY
         CALL ECH_REPORT( 0, REPORT_STRING )
         OPTION = 'NULL'
         REPLY = 'H'
      END IF


 1000 FORMAT ( 1X, 'Current scope: selected order is ', I3, '.' )
 1001 FORMAT ( 1X, '               ',
     :             'number of fit coefficients is ', I2, '.' )

      END
