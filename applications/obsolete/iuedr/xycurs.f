      SUBROUTINE XYCURS( STATUS )
*+
*  Name:
*     SUBROUTINE XYCURS

*  Purpose:
*     Get information using cursor on current diagram.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL XYCURS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-AUG-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-JUN-94 (MJC):
*       IUEDR Vn. 3.1-1.
*       Changed to use MSG library.
*       Changed to use CHR library.
*     08-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'
      INCLUDE 'CHR_ERR'

*  Global Variables:
      INCLUDE 'CMGRAF'

*  Status:
      INTEGER STATUS      ! Global status.

*  Local Constants:
      INTEGER CURMOD      ! GKS cursor mode (0 default).
      INTEGER MAXLABEL    ! Maximum length of label string.
      PARAMETER ( CURMOD = 0, MAXLABEL = 40 )

*  Local Variables:
      REAL*8 DX           ! X cursor hit position.
      REAL*8 DY           ! Y cursor hit position.

      REAL X              ! X cursor hit position.
      REAL Y              ! Y cursor hit position.

      CHARACTER*80 AMESSAGE
      CHARACTER*60 TABTITLE
      CHARACTER*( MAXLABEL ) CXLAB
      CHARACTER*( MAXLABEL ) CXUN
      CHARACTER*( MAXLABEL ) CYLAB
      CHARACTER*( MAXLABEL ) CYUN

      BYTE ASTRING( 80 )

      INTEGER KEY         ! Hit key.
      INTEGER NCHAR
      INTEGER IPOSN
      INTEGER CHR_LEN
      INTEGER LSTAT
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Axis labels and units.
      TABTITLE = ' '

      CALL GEN_STOC( XLAB, MAXLABEL, CXLAB, LSTAT )
      CALL GEN_STOC( XUN, MAXLABEL, CXUN, LSTAT )
      IPOSN = CHR_LEN( CXLAB )
      CALL CHR_APPND( CXUN, CXLAB, IPOSN )
      IPOSN = 0
      CALL CHR_PUTC( CXLAB, TABTITLE, IPOSN )

      CALL GEN_STOC( YLAB, MAXLABEL, CYLAB, LSTAT )
      CALL GEN_STOC( YUN, MAXLABEL, CYUN, LSTAT )
      IPOSN = CHR_LEN( CYLAB )
      CALL CHR_APPND( CYUN, CYLAB, IPOSN )
      IF ( CYLAB( 1:7 ) .EQ. 'Net(FN)' ) THEN
         IPOSN = 19

      ELSE IF ( CYLAB( 1:3 ) .EQ. 'Net' ) THEN
         IPOSN = 20

      ELSE
         IPOSN = 23
      END IF
      CALL CHR_PUTC( CYLAB, TABTITLE, IPOSN )

      CALL MSG_SETC( 'TLAB', TABTITLE )
      CALL MSG_LOAD( ' ', ' ^TLAB', AMESSAGE, NCHAR, STATUS )
      CALL GEN_CTOS( AMESSAGE, 80, ASTRING, NCHAR )
      CALL LINE_WRITS( '%p%s\\', ASTRING )
      CALL PRTBUF( STATUS )

      DO WHILE ( .TRUE. )
         CALL GRF_CUZONE( '12', CURMOD, KEY, X, Y, STATUS )
         DX = DBLE( X )
         DY = DBLE( Y )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: reading cursor\\', STATUS )
            GO TO 100

         ELSE IF ( KEY .LE. 0 ) THEN
            GO TO 100
         END IF

         CALL MSG_FMTD( 'OLINE', 'F7.2', DX )
         CALL MSG_FMTD( 'OLINE', 'F23.5', DY )
         CALL MSG_LOAD( ' ', ' ^OLINE', AMESSAGE, NCHAR, STATUS )
         CALL GEN_CTOS( AMESSAGE, 80, ASTRING, NCHAR )
         CALL LINE_WRITS( '%p%s\\', ASTRING )
         CALL PRTBUF( STATUS )
      END DO

 100  CONTINUE

      END
