      SUBROUTINE USR_PRLBLS( STATUS )
*+
*  Name:
*     SUBROUTINE USR_PRLBLS

*  Description:
*     The current LBLS array is printed as a table suitable for "working"
*     by hand.
*     The output should be diverted to a file!

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PRLBLS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     08-MAY-83 (JRG):
*       IUEDR Vn. 1.3
*     06-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     20-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMLBLS'
      INCLUDE 'CMPAN'

*  Status:
      INTEGER STATUS            ! Global status.

*  External References:
      INTEGER DQ_AND            ! Data quality AND.

*  Local Variables:
      CHARACTER*( 800 ) CLINE   ! F77 character string for line.
      CHARACTER*( 128 ) QLINE   ! F77 character string for quality.

      INTEGER FORM( 120 )       ! Maximum I-field for each array column.
      INTEGER IFIELD            ! Format field parameter.
      INTEGER IU                ! Loop index.
      INTEGER IV                ! Loop index.
      INTEGER IVALUE            ! Format field parameter.
      INTEGER NCHAR             ! Length of CLINE.
      INTEGER Q                 ! Local data quality.
      INTEGER QSEV              ! Data quality severity.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NOLBLS ) THEN
         CALL ERROUT( 'Error: no LBLS\\', STATUS )
         GO TO 999

      END IF

*  1st label (CAMERA,IMAGE,RESOLUTION).
      CALL LINE_WRITS( '%p %s\\', CAMERA )
      CALL LINE_WRITI( '%i\\', IMAGE )
      CALL LINE_WRITS( ' %s\\', RESOL )
      CALL LINE_WRITF( ',LBLS\\' )
      CALL PRTBUF( STATUS )

*  2nd label (OBJECT).
      CALL LINE_WRITS( '%p OBJECT=''%S''\\', TITLE )
      CALL PRTBUF( STATUS )

*  Determine format field lengths required.
      DO IU = 1, NU
         FORM( IU ) = 0
      END DO

      DO IV = 1, NV
         DO IU = 1, NU
            IVALUE = NINT( REAL( FPAN( IU, IV ) ) )
            IF ( IVALUE .EQ. 0 ) THEN
               IFIELD = 1

            ELSE
               IF ( IVALUE .LT. 0 ) THEN
                  IFIELD = 1

               ELSE
                  IFIELD = 0
               END IF

 110           CONTINUE

               IF ( .NOT. ( IVALUE .NE. 0 ) ) THEN
                  FORM( IU ) = MAX( FORM( IU ), IFIELD )
                  GO TO 150

               ELSE
                  IFIELD = IFIELD + 1
                  IVALUE = IVALUE / 10
                  GO TO 110
               END IF
            END IF

            FORM( IU ) = MAX( FORM( IU ), IFIELD )
 150        CONTINUE
         END DO
      END DO

*  Data quality key.
      CALL PRTEOL( STATUS )
      CALL LINE_WCONT( '%p Data Quality Key:\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI( '%p%2w %2i\\', 1 )
      CALL LINE_WCONT( ' affected by extrapolated ITF\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI( '%p%2w %2i\\', 2 )
      CALL LINE_WCONT( ' affected by microphonics\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI( '%p%2w %2i\\', 3 )
      CALL LINE_WCONT( ' affected by spike\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI( '%p%2w %2i\\', 4 )
      CALL LINE_WCONT( ' affected by bright point\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI( '%p%2w %2i\\', 5 )
      CALL LINE_WCONT( ' affected by reseau mark\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI( '%p%2w %2i\\', 6 )
      CALL LINE_WCONT( ' affected by ITF truncation\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI( '%p%2w %2i\\', 7 )
      CALL LINE_WCONT( ' affected by saturation\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%p end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

*  Column values.
      CALL LINE_WRITS( '%p %s\\', ULAB )
      CALL LINE_WRITS( '%s:\\', UUNT )
      CALL PRTBUF( STATUS )

      DO IU = 1, NU
         CALL LINE_WRITI( '%p%2w %5i\\', IU )
         CALL LINE_WRITF( '%2w %7.2f\\', US( IU ) )
         CALL PRTBUF( STATUS )
      END DO

      CALL LINE_WCONT( '%p end.\\', STATUS )
      CALL PRTBUF( STATUS )

*  Flux label/units.
      CALL PRTEOL( STATUS )
      CALL LINE_WRITS( '%p%48w%s\\', DLAB )
      CALL LINE_WRITS( '%s\\', DUNT )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

*  Row label.
      CALL LINE_WRITS( '%p %s\\', VLAB )
      CALL PRTBUF( STATUS )

*  Row units.
      CALL LINE_WRITS( '%p %s\\', VUNT )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

*  Column indices.
      CLINE = ' '
      NCHAR = 8

      DO IU = 1, NU
         WRITE ( CLINE( NCHAR + 1: ), '(1X, I<FORM(IU)>)',
     :           IOSTAT = STATUS ) IU
         NCHAR = NCHAR + FORM( IU ) + 1
      END DO

      CALL PRTCHR( CLINE( :NCHAR ), STATUS )
      CALL PRTEOL( STATUS )

*  Values.
      DO IV = 1, NV
         NCHAR = 0
         CLINE = ' '
         QLINE = ' '
         WRITE ( CLINE( NCHAR + 1: ), '( 1X, f7.2 )',
     :           IOSTAT = STATUS ) VS( IV )
         NCHAR = NCHAR + 8

         DO IU = 1, NU
            IF ( ( NCHAR + FORM( IU ) ) .GT. 128 ) THEN
               CALL PRTCHR( CLINE( :NCHAR ), STATUS )
               CALL PRTCHR( QLINE( :NCHAR ), STATUS )
               NCHAR = 8
               CLINE = ' '
               QLINE = ' '
            END IF

            CALL DQ_UTOI( QPAN( IU, IV ), Q )
            CALL DQ_RDPK( Q, 5, 4, QSEV )

            IF ( DQ_AND( Q, 1 ) .EQ. 0 ) THEN
               IVALUE = NINT( REAL( FPAN( IU, IV ) ) )
               WRITE ( CLINE( NCHAR + 1: ), '(1X, I<FORM(IU)>)',
     :                 IOSTAT = STATUS ) IVALUE

               IF ( QSEV .GT. 0 ) THEN
                  WRITE ( QLINE( NCHAR + 1: ), '(1X, I<FORM(IU)>)',
     :                    IOSTAT = STATUS ) QSEV

               ELSE
                  QLINE( NCHAR + 1:NCHAR + FORM( IU ) + 1 ) = ' '
               END IF

            ELSE
               CLINE( NCHAR + 1:NCHAR + FORM( IU ) + 1 ) = ' '
               QLINE( NCHAR + 1:NCHAR + FORM( IU ) + 1 ) = ' '
            END IF

            NCHAR = NCHAR + FORM( IU ) + 1
         END DO

         CALL PRTCHR( CLINE( :NCHAR ), STATUS )
         CALL PRTCHR( QLINE( :NCHAR ), STATUS )
      END DO

      CALL PRTEOL( STATUS )

 999  CONTINUE

      END
