      SUBROUTINE PRGRS( I, STATUS )
*+
*  Name:
*     SUBROUTINE PRGRS

*  Purpose:
*     Print recently extracted spectrum on a single output line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRGRS( I, STATUS )

*  Arguments:
*     I = INTEGER (Given)
*        Index of value to be displayed in spectrum arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Print Wavelength, Background, Gross, Net and Flux values for a
*     point in the recently extracted spectrum.
*     DQ is expanded to include a mnemonic for the IUE spectrum flag.
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
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER I                 ! Index.

*  Status:
      INTEGER STATUS            ! Global status.

*  External References:
      INTEGER DQ_AND            ! Bitwise DQ and.

*  Global Variables:
      INCLUDE 'CMWAV'
      INCLUDE 'CMNET'
      INCLUDE 'CMFLX'
      INCLUDE 'CMBKG'
      INCLUDE 'CMEXTP'

*  Local Constants:
      INTEGER MAXLINE
      PARAMETER ( MAXLINE = 400 )

*  Local Variables:
      REAL*8 SBKC               ! Scaled smooth background.
      REAL*8 SCALE              ! Scaling factor.
      REAL*8 SGRS               ! Generated gross.

      BYTE SLINE( MAXLINE )     ! SWT line string.

      CHARACTER*( MAXLINE ) CLINE  ! Output character string.

      INTEGER NCHAR             ! Character count.
      INTEGER Q                 ! DQ temporary.
      INTEGER QSEV              ! IUE severity code.
      INTEGER QBKC              ! SBKC data quality.
      INTEGER QGRS              ! SGRS data quality.
*.

*   Generate gross and scaled smooth background.
      SCALE = ABS( ROBJ( 2 ) - ROBJ( 1 ) )

      IF ( DQ_AND( QBKG( I ), 1 ) .EQ. 0 ) THEN
         SBKC = SBKG( I ) * SCALE
         QBKC = QBKG( I )
         IF ( DQ_AND( QNET( I ), 1 ) .EQ. 0 ) THEN
            SGRS = SNET( I ) + SBKC
            QGRS = QNET( I )

         ELSE
            SGRS = 0.0
            QGRS = 1
         END IF

      ELSE
         SBKC = 0.0
         QBKC = 1
         SGRS = 0.0
         QGRS = 1
      END IF

*   Encode basic values.
      WRITE ( CLINE, '(I5, 2X, 0PF12.3, 4(2X, 1PE12.4) )',
     :        IOSTAT = STATUS) I, WAVAIR( I ), SGRS, SBKC, SNET( I ),
     :        SFLX( I )

*   Blank out non-existent GROSS.
      IF ( QGRS .NE. 0 ) THEN
         IF ( DQ_AND( QGRS, 1 ) .NE. 0 ) THEN
            CLINE( 22:33 ) = ' '
         END IF
      END IF

*   Blank out non-existent BKG.
      IF ( QBKC .NE. 0 ) THEN
         IF ( DQ_AND( QBKC, 1 ) .NE. 0 ) THEN
            CLINE( 36:47 ) = ' '
         END IF
      END IF

*   Blank out non-existent NET (and hence flux).
      IF ( QNET( I ) .NE. 0 ) THEN
         IF ( DQ_AND( QNET( I ), 1 ) .NE. 0 ) THEN
            CLINE( 50:61 ) = ' '
            CLINE( 64:75 ) = ' '
         END IF
      END IF

*   Blank out non-existent FLX (only).
      IF ( QFLX( I ) .NE. 0 ) THEN
         IF ( DQ_AND( QFLX( I ), 1) .NE. 0 ) THEN
            CLINE( 64:75 ) = ' '
         END IF
      END IF

*   Convert to SWT.
      CALL GEN_CTOS( CLINE( :75 ), MAXLINE, SLINE, NCHAR )
      CALL LINE_WRITS( '%p %-75s\\', SLINE )

*   Append data quality explanation based on QNET.
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

      CALL PRTBUF( STATUS )

      END
