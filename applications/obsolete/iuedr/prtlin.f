      SUBROUTINE PRTLIN( STR, STATUS )
*+
*  Name:
*     SUBROUTINE PRTLIN

*  Purpose:
*     Put a terminated string out to a file and finish line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRTLIN( STR, STATUS )

*  Arguments:
*     STR = BYTE( * ) (Given)
*        The string to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-81 (JRG):
*       AT4 version.
*     31-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     13-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*   Implicit:
      IMPLICIT NONE

*   Global variables:
      INCLUDE 'CMPSX'

*   Local constants:
      INTEGER MAXREC           ! Maximum length of printed record.
      INTEGER ARB              ! Arbitrary string length.
      PARAMETER ( MAXREC = 200, ARB = 100 )

*   Import:
      BYTE STR( ARB )          ! String to be written.

*   Export:
      INTEGER STATUS           ! Global status.

*   External references:
      INTEGER STR_LEN          ! String length.

*   Local variables:
      CHARACTER*( MAXREC ) STRING ! CHARACTER version of string.

      INTEGER NCHAR            ! String length.
*.

      IF ( STR_LEN( STR ) .GT. 0 ) THEN
         CALL GEN_STOC( STR, MAXREC, STRING, NCHAR )

*      Write message text to the file.
         IF ( ISLOG ) THEN
            WRITE( LOGFILE, '( A )' ) STRING( : NCHAR )
         END IF

*      Write to user terminal.
         CALL MSG_OUT( ' ' , STRING( : NCHAR ), STATUS )
      END IF

      END
