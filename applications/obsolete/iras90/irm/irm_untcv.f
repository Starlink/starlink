      SUBROUTINE IRM_UNTCV( UNITS1, UNITS2, NDET, DET, SCALE, STATUS )
*+
*  Name:
*     IRM_UNTCV

*  Purpose:
*     Calculate scale factor for valid CRDD units conversions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_UNTCV( UNITS1, UNITS2, NDET, DET, SCALE, STATUS )

*  Description:
*     This routine calculates the scale factors needed to convert CRDD
*     data in UNITS1 to that in UNITS2. Systems of units should be
*     specified using the symbolic constants listed below, rather than
*     by the corresponding character strings. These constants are made
*     available by including the IRC_PAR file. Presently, this routine
*     supports conversion between any pair of following units:
*
*     IRC__F - Flux value in units of Pico-Watts per square metre
*
*     IRC__J - Flux density values in Janskys.
*
*     IRC__JPS - Surface brightness values in Janskys per steradian.
*
*     IRC__MJPS - Surface brightness values in Mega-Janskys per steradian.
*
*     IRC__FPS - Surface brightness values in Pico-Watts per square
*                    metre, per steradian.
*
*     The conversion between flux and surface brightness is via the
*     effective solid angle of each detector. Since the dead detectors
*     have zero effective solid angle, the conversion scale for the dead
*     detectors will be the starlink bad value "VAL__BADR", but no error
*     is reported.

*  Arguments:
*     UNITS1 = CHARACTER*( * ) (Given)
*        The original units of the CRDD data.
*     UNITS2 = CHARACTER*( * ) (Given)
*        The new units to which the CRDD data will be converted.
*     NDET = INTEGER (Given)
*        The number of detectors whose data units are to be converted.
*     DET( NDET ) = INTEGER (Given)
*        Detector numbers of the detectors whose data units are to be
*        converted.
*     SCALE( NDET ) = REAL (Returned)
*        The scale factor needed to convert CRDD data of each detector
*        from UNITS1 to UNITS2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-APR-1991 (WG):
*        Original version.
*     29-APR-1992 (WG):
*        Including conversions for more units.
*     30-SEP-1992 (DSB):
*        Use symbolic constants rather than literal character strings.
*        Make check on supplied units case sensitive. Re-structured.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink Primitive constants
      INCLUDE 'I90_DAT'          ! IRAS Satellite and mission data
      INCLUDE 'IRC_PAR'          ! IRC_ package constants

*  Arguments Given:
      CHARACTER*(*) UNITS1
      CHARACTER*(*) UNITS2
      INTEGER NDET
      INTEGER DET( NDET )

*  Arguments Returned:
      REAL SCALE( NDET )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if two strings are equal apart
                                 ! from case.

*  Local Constants:
      INTEGER BAND               ! Current waveband index.
      INTEGER DETNO              ! Current detector number.
      LOGICAL DIVSOL             ! True if the scale should be divided
				 ! by the solid angle.
      INTEGER I                  ! Do loop index
      LOGICAL MULSOL             ! True if the scale should be multiplied
				 ! by the solid angle.
      REAL    SOLAN              ! Current detector solid angle.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each detector.
      DO I = 1, NDET

*  Get the detector number, and check it is OK.
         DETNO = DET( I )
         IF( DETNO .LE. 0 .OR. DETNO .GT. I90__DETS ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'D', DETNO )
            CALL ERR_REP( 'IRM_UNTCV_ERR1',
     :               'IRM_UNTCV: Invalid detector number supplied (^D)',
     :                    STATUS )
            GO TO 999
         END IF

*  Get the detectors waveband index and solid angle.
         BAND = I90__DBAND( DETNO )
         SOLAN = I90__SOLAN( DETNO )

*  If the detector is dead, return VAL__BADR.
         IF( SOLAN .EQ. 0.0 ) THEN
            SCALE( I ) = VAL__BADR

*  If the two system of units are the same, return 1.
         ELSE IF( CHR_SIMLR( UNITS1, UNITS2 ) ) THEN
            SCALE( I ) = 1.0

*  Otherwise, find the scaling factor  (excluding solid angle factor)
*  for converting input CRDD values to units of Jy. Also determine if
*  the solid angle factor needs to be included.
         ELSE
            IF( CHR_SIMLR( UNITS1, IRC__F ) ) THEN        ! pW/(M**2)
               SCALE( I ) = I90__JY( BAND )
               MULSOL = .FALSE.

            ELSE IF( CHR_SIMLR( UNITS1, IRC__FPS ) ) THEN ! (pW/M**2)/Sr
               SCALE( I ) = I90__JY( BAND )
               MULSOL = .TRUE.

            ELSE IF( CHR_SIMLR( UNITS1, IRC__J ) ) THEN   ! Jy
               SCALE( I ) = 1.0
               MULSOL = .FALSE.

            ELSE IF( CHR_SIMLR( UNITS1, IRC__JPS ) ) THEN ! Jy/Sr
               SCALE( I ) = 1.0
               MULSOL = .TRUE.

            ELSE IF( CHR_SIMLR( UNITS1, IRC__MJPS) ) THEN ! MJy/Sr
               SCALE( I ) = 1.0E6
               MULSOL = .TRUE.

            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'U', UNITS1 )
               CALL ERR_REP( 'IRM_UNTCV_ERR2',
     :                       'IRM_UNTCV: CRDD units "^U" not supported',
     :                        STATUS )
               GO TO 999

            END IF

*  Modify the flux scaling factor to produce the requested output units,
*  instead of Jy, and note if the factor should be divided by the solid
*  angle.
            IF( CHR_SIMLR( UNITS2, IRC__F ) ) THEN
               SCALE( I ) = SCALE( I )/I90__JY( BAND )
               DIVSOL = .FALSE.

            ELSE IF( CHR_SIMLR( UNITS2, IRC__FPS ) ) THEN
               SCALE( I ) = SCALE( I )/I90__JY( BAND )
               DIVSOL = .TRUE.

            ELSE IF( CHR_SIMLR( UNITS2, IRC__J ) ) THEN
               DIVSOL = .FALSE.

            ELSE IF( CHR_SIMLR( UNITS2, IRC__JPS ) ) THEN
               DIVSOL = .TRUE.

            ELSE IF( CHR_SIMLR( UNITS2, IRC__MJPS ) ) THEN
               SCALE( I ) = SCALE( I )*1.0E-6
               DIVSOL = .TRUE.

            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'U', UNITS2 )
               CALL ERR_REP( 'IRM_UNTCV_ERR3',
     :                       'IRM_UNTCV: CRDD units "^U" not supported',
     :                        STATUS )
               GO TO 999

            END IF

*  Incorporate the solid angle factor into the scaling factor.
            IF( MULSOL .AND. .NOT. DIVSOL ) THEN
               SCALE( I ) = 1.0E-7*SCALE( I )*SOLAN

            ELSE IF( DIVSOL .AND. .NOT. MULSOL ) THEN
               SCALE( I ) = 1.0E7*SCALE( I )/SOLAN

            END IF

         END IF

      END DO

 999  CONTINUE

      END
