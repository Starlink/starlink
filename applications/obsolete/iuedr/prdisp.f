      SUBROUTINE PRDISP( STATUS )
*+
*  Name:
*     SUBROUTINE PRDISP

*  Purpose:
*     Print dispersion constants.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRDISP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     05-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
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

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISP'
      INCLUDE 'CMWCOR'
      INCLUDE 'CMECOR'
      INCLUDE 'CMVEL'

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

*  Local Variables:
      INTEGER I          ! Loop index.
      INTEGER IDAY       ! Day in month.
      INTEGER IMON       ! Month in year.
      INTEGER IYEAR      ! Year.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Dispersion proper.
      IF ( .NOT. NODISP ) THEN

*     Banner.
         CALL LINE_WCONT( '%p%2w Dispersion:\\' )
         CALL PRTBUF( STATUS )

*     Basics.
         CALL LINE_WRITS( '%p%4w Type ''%s'',\\', DISPTP )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITS( '%p%4w Source ''%s''.\\', DISPID )
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p%4w Constants:\\' )
         CALL PRTBUF( STATUS )

         DO I = 1, NDISP
            CALL LINE_WRITI( '%p%6w A%i \\', I )
            CALL LINE_WRITF( '%13.6e\\', DISPS( I ) )
            CALL LINE_WRITI( '%5w B%i \\', I )
            CALL LINE_WRITF( '%13.6e\\', DISPL( I ) )
            CALL PRTBUF( STATUS )
         END DO

         CALL LINE_WCONT( '%p%4w end.\\' )
         CALL PRTBUF( STATUS )

*     Old format dispersion data.
         IF ( .NOT. STR_SIMLR( 'IUE_DISPN\\', DISPTP ) ) THEN

*        THDA derivatives.
            CALL LINE_WRITF( '%p%4w Standard THDA %.2f (C),\\', DISPT0 )
            CALL LINE_WRITF( ' derivatives (%g,\\', DISPST )
            CALL LINE_WRITF( '%g).\\', DISPLT )
            CALL PRTBUF( STATUS )

*        DATE derivativies.
            CALL LINE_WCONT( '%p%4w Standard Date \\' )
            CALL MSC_DATE( IFIX( REAL( DISPD0 ) ), IDAY, IMON, IYEAR,
     :                     STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL LINE_WRITI( '%i/\\', IDAY )
               CALL LINE_WRITI( '%i/\\', IMON )
               CALL LINE_WRITI( '%i,\\', IYEAR )

            ELSE
               CALL LINE_WCONT( '??/??/??,\\' )
            END IF

            CALL LINE_WRITF( ' derivatives (%g,\\', DISPSD )
            CALL LINE_WRITF( '%g).\\', DISPLD )
            CALL PRTBUF( STATUS )

*     New format dispersion data.
         ELSE

*        THDA zero point.
            CALL LINE_WRITF( '%p%4w Standard THDA %.2f (C).\\', DISPT0 )
            CALL PRTBUF( STATUS )

*        DATE zero point.
            CALL LINE_WCONT( '%p%4w Standard Date \\' )
            CALL MSC_DATE( IFIX( REAL( DISPD0 ) ), IDAY, IMON, IYEAR,
     :                     STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL LINE_WRITI( '%i/\\', IDAY )
               CALL LINE_WRITI( '%i/\\', IMON )
               CALL LINE_WRITI( '%i.\\', IYEAR )

            ELSE
               CALL LINE_WCONT( '??/??/??.\\' )
            END IF
            CALL PRTBUF( STATUS )

*        Correlation Coefficients.
            CALL LINE_WCONT( '%p%4w Correlation Coefficients:\\' )
            CALL PRTBUF( STATUS )
            CALL LINE_WRITF( '%p%6w WS (%g,\\', DISPWS1 )
            CALL LINE_WRITF( ' %g,\\', DISPWS2 )
            CALL LINE_WRITF( ' %g,\\', DISPWS3 )
            CALL LINE_WRITF( ' %g).\\', DISPWS4 )
            CALL PRTBUF( STATUS )
            CALL LINE_WRITF( '%p%6w WL (%g,\\', DISPWL1 )
            CALL LINE_WRITF( ' %g,\\', DISPWL2 )
            CALL LINE_WRITF( ' %g,\\', DISPWL3 )
            CALL LINE_WRITF( ' %g).\\', DISPWL4 )
            CALL PRTBUF( STATUS )
         END IF

*     Ripcon.
         IF ( NDISP .EQ. 7 ) THEN
            CALL LINE_WRITF(
     :           '%p%4w Global Ripple Constant, K=%.1f (A).\\',
     :           RIPCON )
            CALL PRTBUF( STATUS )
         END IF

*     Aperture positions.
         CALL LINE_WCONT( '%p%4w Aperture offsets: \\' )
         DO I = 1, NAPER
            CALL LINE_WCONT( APERS( 1, I ) )
            CALL LINE_WRITF( ' (%.2f,\\', DISPDS( I ) )
            CALL LINE_WRITF( '%.2f),\\', DISPDL( I ) )
         END DO
         CALL PRTBUF( STATUS )

*     Peculiar shifts.
         CALL LINE_WCONT( '%p%4w Spectrum shifts: \\' )
         DO I = 1, NAPER
            CALL LINE_WCONT( APERS( 1, I ) )
            CALL LINE_WRITF( ' (%.2f,\\', DISPSG( I ) )
            CALL LINE_WRITF( '%.2f),\\', DISPLG( I ) )
         END DO
         CALL PRTBUF( STATUS )

*     End.
         CALL LINE_WCONT( '%p%2w end.\\' )
         CALL PRTBUF( STATUS )
         CALL PRTEOL( STATUS )
      END IF

*  Wavelength corrections.
      IF ( .NOT.NOECOR .OR. .NOT.NOWCOR .OR. .NOT.NOVEL ) THEN
         CALL LINE_WCONT( '%p%2w Wavelengths:\\' )
         CALL PRTBUF( STATUS )

*     Wavelength shifts.
         IF ( .NOT. NOWCOR ) THEN
            CALL LINE_WCONT( '%p%4w Wavelength shifts: \\' )
            DO I = 1, NAPER
               CALL LINE_WCONT( APERS( 1, I ) )
               CALL LINE_WRITF( ' %.2f\\', WCOR( I ) )
               IF ( I .LT. NAPER ) THEN
                  CALL LINE_WCONT( ',  \\' )
               END IF
            END DO
            CALL PRTBUF( STATUS )

         ELSE IF ( .NOT. NOECOR ) THEN
            CALL LINE_WCONT( '%p%4w Echelle shifts: \\' )
            DO I = 1, NAPER
               CALL LINE_WCONT( APERS( 1, I ) )
               CALL LINE_WRITF( ' %.2f\\', ECOR( I ) )
               IF ( I .LT. NAPER ) THEN
                  CALL LINE_WCONT( ',  \\' )
               END IF
            END DO
            CALL PRTBUF( STATUS )
         END IF

*     Velocity correction.
         IF ( .NOT. NOVEL ) THEN
            CALL LINE_WCONT( '%p%4w Velocity shifts: \\' )
            DO I = 1, NAPER
               CALL LINE_WCONT( APERS( 1, I ) )
               CALL LINE_WRITF( ' %.1f\\', VEL( I ) )
               IF ( I .LT. NAPER ) THEN
                  CALL LINE_WCONT(',  \\')
               END IF
            END DO
            CALL LINE_WCONT( ' (km/s).\\' )
            CALL PRTBUF( STATUS )
         END IF

*     End.
         CALL LINE_WCONT( '%p%2w end.\\' )
         CALL PRTBUF( STATUS )
         CALL PRTEOL( STATUS )
      END IF

      END
