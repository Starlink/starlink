      SUBROUTINE PRSPC( I, STATUS )
*+
*  Name:
*     SUBROUTINE PRSPC

*  Description:
*     Print spectrum and calibrations on a single output line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRSPC( I, STATUS )

*  Arguments:
*     I = INTEGER (Given)
*        Wavelength array index of point to be displayed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Print Wavelength, Net and Flux values for a point in the
*     current spectrum.
*     DQ is printed as Severity IUEVAL severity code (key assumed).
*     Non-existent values are printed as blank.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     09-MAY-83 (JRG):
*       IUEDR Vn. 1.3
*     06-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER I                 ! index

*  Global Variables:
      INCLUDE 'CMWAV'
      INCLUDE 'CMNET'
      INCLUDE 'CMFLX'

*  Status:
      INTEGER STATUS            ! Global status.

*  External References:
      INTEGER DQ_AND            ! bitwise DQ and

*  Local Variables:
      BYTE SLINE(400)           ! SWT line string

      CHARACTER*(400) CLINE     ! output character string

      INTEGER NCHAR             ! character count
      INTEGER Q                 ! dq temporary
      INTEGER QSEV              ! IUE severity code

*.

*  Encode basic values.
      WRITE ( CLINE, '(I5, 2X, 0PF12.3, 2( 2X, 1PE12.4 ) )',
     :        IOSTAT=STATUS ) I, WAVAIR( I ), SNET( I ), SFLX( I )

*  Blank out non-existent NET (and hence flux).
      IF ( QNET( I ) .NE. 0 ) THEN
         IF ( DQ_AND( QNET( I ), 1 ) .NE. 0 ) THEN
            CLINE( 22:33 ) = ' '
            CLINE( 36:47 ) = ' '
         END IF
      END IF

*  Blank out non-existent FLX (only).
      IF ( QFLX( I ) .NE. 0 ) THEN
         IF ( DQ_AND( QFLX( I ), 1 ) .NE. 0 ) THEN
            CLINE( 36:47 ) = ' '
         END IF
      END IF

*  Convert to SWT.
      CALL GEN_CTOS( CLINE( 1:47 ), 400, SLINE, NCHAR )
      CALL LINE_WRITS( '%p %-47s\\', SLINE )

*  Append data quality explanation based on QNET.
      Q = QNET( I )
      IF ( Q .NE. 0 ) THEN
         CALL LINE_WCONT( '%4w\\' )
         CALL DQ_RDPK( Q, 5, 4, QSEV )
         IF ( QSEV .EQ. 8 ) THEN
            CALL LINE_WCONT( '*\\' )

         ELSE IF ( QSEV .GT. 0 ) THEN
            CALL LINE_WRITI( '%i\\', QSEV )

         ELSE IF ( DQ_AND( Q, 1 ) .NE. 0 ) THEN
            CALL LINE_WCONT( '*\\' )

         ELSE IF ( DQ_AND( Q, 3 ) .NE. 0 ) THEN
            CALL LINE_WCONT( 'MARKED\\' )
         END IF
      END IF

*  Write out the line.
      CALL PRTBUF( STATUS )

      END
