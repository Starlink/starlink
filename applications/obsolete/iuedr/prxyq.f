      SUBROUTINE PRXYQ( I, X, Y, Q, STATUS )
*+
*  Name:
*     SUBROUTINE PRXYQ

*  Purpose:
*     Print a wavelength/flux table entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRXYQ( I, X, Y, Q, STATUS )

*  Arguments:
*     I = INTEGER (Given)
*        Index into source array.
*     X = REAL*8 (Given)
*        Wavelength of point.
*     Y = REAL*8 (Given)
*        Flux at that point.
*     Q = INTEGER (Given)
*        Data quality information at that point.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Print a single line to output containing the (X,Y,DQ) values.
*     DQ is expanded to include a mnemonic for the IUE spectrum flag.
*     non-existent values are printed as blank.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink)
*     {enter_new_authors_here}

*  History:
*     09-MAY-83 (JRG):
*       IUEDR Vn. 1.0
*     06-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     17-APR-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER I                 ! Index

      REAL*8 X                  ! X-value
      REAL*8 Y                  ! Y-value

      INTEGER Q                 ! Data quality value

*  Status:
      INTEGER STATUS            ! Global status.

*  External References:
      INTEGER DQ_AND            ! Bitwise DQ and.

*  Local Variables:
      BYTE SLINE( 400 )         ! SWT line string.

      CHARACTER*( 400 ) CLINE   ! Output character string.

      INTEGER NCHAR             ! Character count.
      INTEGER QSEV              ! IUE severity code.
*.

      WRITE ( CLINE, '( I5, 2X, 0PF12.3, 2X, 1PE12.4 )',
     :        IOSTAT = STATUS ) I, X, Y

      IF ( Q .NE. 0 ) THEN
         IF ( DQ_AND( Q, 1 ) .NE. 0 ) THEN
            CLINE( 22 : 33 ) = ' '
         END IF
      END IF

      CALL GEN_CTOS( CLINE( 1 : 37 ), 400, SLINE, NCHAR )
      CALL LINE_WRITS( '%p %s\\', SLINE )

      IF ( Q .NE. 0 ) THEN
         CALL LINE_WCONT( '%4w\\' )
         CALL DQ_RDPK( Q, 5, 4, QSEV )

         IF ( QSEV .EQ. 8 ) THEN
            CALL LINE_WCONT( '*\\' )

         ELSE IF ( QSEV .GT. 0) THEN
            CALL LINE_WRITI( '%i\\', QSEV )

         ELSE IF ( DQ_AND( Q, 1 ) .NE. 0 ) THEN
            CALL LINE_WCONT( '*\\' )

         ELSE IF ( DQ_AND( Q, 3 ) .NE. 0 ) THEN
            CALL LINE_WCONT( 'MARKED\\' )

         END IF
      END IF

      CALL PRTBUF( STATUS )

      END
