      SUBROUTINE LOGRID( IAPER, STATUS )
*+
*  Name:
*     SUBROUTINE LOGRID

*  Purpose:
*     Define wavelength grid for LORES spectrum.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOGRID( IAPER, STATUS )

*  Arguments:
*     IAPER = INTEGER (Given)
*        Which aperture is to be used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The wavelength grid is determined from (CAMERA, APERTURE)
*     internally.  The sample rate is determined from that given in
*     geometric pixels and from the dispersion at the central
*     wavelength.  The grid is delimited at either end by the
*     requirement to be contained within the circular face-plate
*     boundary.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC)
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     THIS IS NOT DEBUGGED!
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER IAPER      ! aperture index

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISH'
      INCLUDE 'CMFACE'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMWAV'

*  Local Variables:
      REAL*8 DIST        ! distance from face-plate centre
      REAL*8 L           ! L-coordinate
      REAL*8 S           ! S-coordinate
      REAL*8 W           ! W-coordinate
      REAL*8 W1          ! start wavelength guess
      REAL*8 W2          ! end wavelength guess

      INTEGER I          ! loop index
      INTEGER ICAM       ! camera index
      INTEGER IW1        ! start index
      INTEGER IW2        ! end index

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set EXTP undefined.
      NOEXTP = .TRUE.

*   Wavelength step.
      DWAV = GSAMP / DRDW

*   Choose extreme limits based on CAMERA.
      CALL IUE_CAMN( CAMERA, ICAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: IUE camera invalid\\', STATUS )
         GO TO 999

      ELSE IF ( ICAM.EQ.3 ) THEN
         W1 = 1100.0
         W2 = 2000.0

      ELSE IF ( ICAM.EQ.1 .OR. ICAM.EQ.2 ) THEN
         W1 = 1500.0
         W2 = 3400.0

      ELSE
         CALL ERROUT( 'Error: IUE camera unknown\\', STATUS )
         GO TO 999
      END IF

*   Start and end indices.
      IW1 = NINT( W1 / DWAV )
      IW2 = NINT( W2 / DWAV )

*   Contain start of grid.
      DO WHILE ( .TRUE. )
         IF ( .NOT. ( IW1 .LE. IW2 ) ) THEN
            GO TO 100
         END IF
         W = DBLE( IW1 ) * DWAV
         CALL WTOR( 0.0d0, W, S, L )
         DIST = SQRT((S - CENTRE( 1 )) ** 2 + (L - CENTRE( 2 )) ** 2)
         IF ( DIST .LE. DBLE( RADIUS ) ) THEN
            GO TO 100

         ELSE
            IW1 = IW1 + 1
         END IF
      END DO
 100  CONTINUE

*   Contain end of grid.
      DO WHILE ( .TRUE. )
         IF ( .NOT. ( IW2 .GE. IW1 ) ) THEN
            GO TO 200
         END IF

         W = DBLE( IW2 ) * DWAV
         CALL WTOR( 0.0d0, W, S, L )
         DIST = SQRT((S - CENTRE( 1 )) ** 2 + (L - CENTRE( 2 )) ** 2)
         IF ( DIST .LE. DBLE( RADIUS ) ) THEN
            GO TO 200

         ELSE
            IW2 = IW2 - 1
         END IF
      END DO
 200  CONTINUE

*   Check that there is a grid at all.
      IF ( IW1 .GE. IW2 ) THEN
         CALL ERROUT( 'Error: order is not within image\\', STATUS )
         GO TO 999
      END IF

*   Form a proper grid.
      NWAV = IW2 - IW1 + 1
      IF ( NWAV .GT. 1200 ) THEN
         CALL LINE_WCONT( 'Too many wavelengths, some lost.\\' )
         CALL PRTBUF( STATUS )
         NWAV = 1200

      ELSE IF ( NWAV .LT. 2 ) THEN
         NWAV = 2
      END IF

      WAV1 = DBLE( IW1 ) * DWAV

      DO I = 1, NWAV
         WAV( I ) = DBLE( I - 1 ) * DWAV + WAV1
      END DO

*   Form bin sizes.
      CALL CAGRID( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: forming bin sizes\\', STATUS )
         GO TO 999
      END IF

*   Centroid and Background Folding Ranges.
      BBASE = BKGAV / DRDW
      CBASE = CENAV / DRDW
      NOEXTP = .FALSE.

 999  CONTINUE
      END
