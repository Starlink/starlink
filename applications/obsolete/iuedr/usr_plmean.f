      SUBROUTINE USR_PLMEAN( STATUS )
*+
*  Name:
*     SUBROUTINE USR_PLMEAN

*  Purpose:
*     The graphics display is opened, and the current combined spectrum
*     is plotted.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PLMEAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     10-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     07-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMGRAF'
      INCLUDE 'CMPLOT'
      INCLUDE 'CMCOLR'
      INCLUDE 'CMTEMP'

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of plot label.
      INTEGER MAXPOINT   ! Maximum number of points plotted.
      INTEGER MAXTITLE   ! Maximum length of plot title.
      PARAMETER ( MAXLABEL = 40, MAXPOINT = 27800, MAXTITLE = 80 )

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER ACTVAL     ! Parameter value count.
      INTEGER I          ! Do loop index.
      INTEGER N          ! Number of data in plot.

      LOGICAL QUAL       ! Whether data quality plotted.
      LOGICAL ZERO       ! Whether zero line is plotted.
      LOGICAL SELFLG     ! Whether SELINE is called during plotting.
      LOGICAL RELFLG     ! Whether RELINE is called during plotting.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that there is a spectrum.
      CALL RDCOMB( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: accessing dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NOCOMB ) THEN
         CALL ERROUT( 'Error: no spectrum\\', STATUS )
         GO TO 999
      END IF

*  Automatic RESET sequence.
      ZCLEAR = .FALSE.
      CALL GRF_CLZONE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: clearing graphics specification\\',
     :                STATUS )
         GO TO 999
      END IF

*  Open graphics display.
      CALL GRF_OPZONE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics unavailable\\', STATUS )
         GO TO 999
      END IF

*  Reset line style.
      CALL GRF_RSLINE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Add the new data arrays into storage so that they can be used
*  to define display limits if needed (only use).
      NPOINT = 0

      N = MIN( ( MAXPOINT - NPOINT ), NCOMB )
      IF ( N .LT. NCOMB ) THEN
         CALL ERROUT( 'Too many points for autoscale\\', STATUS )
         STATUS = SAI__OK
      END IF

      DO I = 1, N
         XPLOT( NPOINT + I ) = REAL( XCOMB( I ) )
         QPLOT( NPOINT + I ) = QCOMB( I )
         YPLOT( NPOINT + I ) = REAL( YCOMB( I ) )
      END DO
      NPOINT = NPOINT + N

*  HIST.
      CALL RDPARL( 'HIST\\', .FALSE., 1, HISTLN, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'HIST\\', STATUS )
         GO TO 999
      END IF

*  QUAL.
      CALL RDPARL( 'QUAL\\', .FALSE., 1, QUAL, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'QUAL\\', STATUS )
         GO TO 999
      END IF

*  Design axes and annotation if undefined.
      IF ( .NOT. DRAWN ) THEN

*     XL - x-limits, UNDEF means use data defined limits.
         CALL XLGET( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*     YL - y-limits, UNDEF means use data defined limits.
         CALL YLGET( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*     Plant axis labels.
         CALL STR_MOVE( XMLAB, MAXLABEL, XLAB )
         CALL STR_MOVE( XMUN, MAXLABEL, XUN )
         CALL STR_MOVE( YMLAB, MAXLABEL, YLAB )
         CALL STR_MOVE( YMUN, MAXLABEL, YUN )
         CALL STR_MOVE( MTITLE, MAXTITLE, GTITLE )

*     Determine axis limits one way or another.
         CALL GRF_DEAXES( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: defining axis limits\\', STATUS )
            GO TO 999
         END IF

*     Force zero level to be included (unless YL specified).
         IF ( NOYL ) THEN
            IF ( YLIM( 1 ) .GT. 0.0 ) THEN
               YLIM( 1 ) = 0.0

            ELSE IF ( YLIM( 2 ) .LT. 0.0 ) THEN
               YLIM( 2 ) = 0.0
            END IF
         END IF
      END IF

*  Set remaining potting flags.
      ZERO = .TRUE.
      SELFLG = .TRUE.
      RELFLG = .TRUE.

      DO I = 1, NCOMB
         TMPWRK1( I ) = XCOMB( I )
         TMPWRK2( I ) = YCOMB( I )
      END DO

*  Plot mean spectrum.
      IF ( HISTLN ) THEN
         CALL GRF_HICURV( 3, NCOMB, XPLOT, YPLOT, QCOMB, CHAR( 0 ), 0,
     :                    QUAL, ZERO, SELFLG, RELFLG )

      ELSE
         CALL GRF_POCURV( 3, NCOMB, XPLOT, YPLOT, QCOMB, CHAR( 0 ), 0,
     :                    QUAL, ZERO, SELFLG, RELFLG )
      END IF

 999  CONTINUE

      END
