      SUBROUTINE ECH_HELP( HELP_TOPIC, INTERACTIVE, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_HELP

*  Purpose:
*     Interfaces with help system.

*  Description:
*     This routine provides information from the prevailing Help system.

*  Invocation:
*     CALL ECH_HELP( HELP_TOPIC, INTERACTIVE, STATUS )

*  Arguments:
*     HELP_TOPIC = CHAR (Given)
*        A help topic identifier.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if user can interact with help system.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_further_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     16-APR_1996 (MJC):
*       Tidy up.  Some modifications to add library suffix.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      CHARACTER*( * ) HELP_TOPIC
      LOGICAL INTERACTIVE

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER HLPLUN
      INTEGER LLEN
      INTEGER JFLAGS

      CHARACTER*255 HELP_FILE_PATH
      CHARACTER*80 DUMMY

      COMMON / HELP_FILE_LUN / HLPLUN

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR

      INTEGER HLP_INSUB
      EXTERNAL HLP_INSUB

      INTEGER HLP_HELP
      EXTERNAL HLP_HELP

      INTEGER HLP_OUTSUB
      EXTERNAL HLP_OUTSUB

      EXTERNAL HLP_NAMETR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Look up the location of the help library.
      HELP_FILE_PATH = 'ECHOMOP_HELP:'
      CALL ECH_PARSE_ENV( HELP_FILE_PATH, LLEN )

*  On first call, initialise the help system.
      IF ( HLPLUN .EQ. 0 ) THEN

*     Get an I/O unit number for accessing the library.
         CALL ECH_ACCESS_OBJECT( 'HELP-FILE-LUN', 'CREATE', 'LUN', 0,
     :        0, HLPLUN, DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )

*     Specify the extrension for the library file.
         CALL HLP_NAMETR( 2, '.shl', DUMMY, STATUS )
      END IF

      IF ( INTERACTIVE ) THEN
         JFLAGS = 1

      ELSE
         JFLAGS = 0
      END IF

*  Call the help system.
      STATUS = HLP_HELP( HLP_OUTSUB, 80, HELP_TOPIC, HLPLUN,
     :         HELP_FILE_PATH, JFLAGS, HLP_INSUB, HLP_NAMETR )

*  Trailing message for non-interactive HELP look-up.
      IF ( .NOT. INTERACTIVE ) THEN
         REPORT_STRING = ' from HELP @ECHOMOP ' // HELP_TOPIC
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

      END
