      SUBROUTINE IRC_TRUNC( IDC, NDETS, DETIND, SAMPLO, SAMPHI, STATUS )
*+
*  Name:
*     IRC_TRUNC

*  Purpose:
*     Find the section of a scan containing data from all available
*     detectors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_TRUNC( IDC, NDETS, DETIND, SAMPLO, SAMPHI, STATUS )

*  Description:
*     Each detector data stream starts and finishes at a different
*     in-scan position, resulting in there being a section at each end
*     of the scan which is not covered by data from all available
*     detectors. This routine returns the upper and lower sample number
*     limits (for each requested detector data stream) of the section
*     of the CRDD file for which there is data from all the requested
*     detectors.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     NDETS = INTEGER (Given)
*        The number of detector data streams to be included.
*     DETIND( NDETS ) = INTEGER (Given)
*        A list of detector indices defining the detector data streams
*        to be included.
*     SAMPLO( NDETS ) = REAL (Returned)
*        The sample numbers defining the start of a section from each
*        detector data stream for which there is corresponding data
*        from all the other included detectors.
*     SAMPHI( NDETS ) = REAL (Returned)
*        The sample numbers defining the end of a section from each
*        detector data stream for which there is corresponding data
*        from all the other included detectors.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JAN-1992 (DSB):
*        Orginal version.
*     {original_version_entry}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_SLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest sample number in the DATA array.
*        CCM_SHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest sample number in the DATA array.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.

*  Arguments Given:
      INTEGER IDC
      INTEGER NDETS
      INTEGER DETIND( NDETS )

*  Arguments Given and Returned:
      REAL SAMPLO( NDETS )
      REAL SAMPHI( NDETS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL      DIST             ! Distance from reference sample.
      INTEGER   I                ! Loop count.
      REAL      MAXDIS           ! Maximum distance from reference
                                 ! sample.
      INTEGER   REFDET           ! Reference detector index.
      REAL      REFSMP           ! Reference sample number.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that at least one detector is to be included.
      IF( NDETS .LE. 0 .OR. NDETS .GT. I90__MAXDT ) THEN
         STATUS = IRC__NVAL
         CALL MSG_SETI( 'ND', NDETS )
         CALL ERR_REP( 'IRC_TRUNC_ERR1',
     :         'IRC_TRUNC: Illegal number of detectors specified (^ND)',
     :                 STATUS )
         GO TO 999
      END IF

*  Check the supplied IRC identifier is valid. If not, report an error
*  and quit.
      IF( IDC .LE. 0 .OR. IDC .GT. IRC__MAX ) THEN
         STATUS = IRC__INVID

      ELSE IF( .NOT. CCM_VALID( IDC ) ) THEN
         STATUS = IRC__INVID

      END IF

      IF( STATUS .EQ. IRC__INVID ) THEN
         CALL ERR_REP( 'IRC_TRUNC_ERR2',
     :                 'IRC_TRUNC: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that all detector indices are within the bounds of the NDF
*  second dimension
      DO I = 1, NDETS
         IF( DETIND( I ) .LT. CCM_DLOW( IDC ) .OR.
     :       DETIND( I ) .GT. CCM_DHIGH( IDC ) ) THEN
            STATUS = IRC__BADDI
            CALL MSG_SETI( 'D', DETIND( I ) )
            CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
            CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
            CALL ERR_REP( 'IRC_TRUNC_ERR3',
     :        'IRC_TRUNC: Detector index ^D is out of bounds [^DL,^DH]',
     :                  STATUS )
            GO TO 999
         END IF
      END DO

*  Find the greatest in-scan distance (in the negative focal plane Y
*  direction) between the first sample in the first detector, and the
*  first sample in any other detector.
      REFDET = DETIND( 1 )
      REFSMP = REAL( CCM_SLOW( IDC ) )
      MAXDIS = 0.0

      DO I = 2, NDETS
         CALL IRC1_DISTI( IDC, REFSMP, REFDET, REFSMP, DETIND( I ),
     :                    DIST, STATUS )
         MAXDIS = MAX( MAXDIS, -DIST )
      END DO

*  Store the sample number at which each detector reaches the in-scan
*  position found above.
      DO I = 1, NDETS
         CALL IRC1_OFFSI( IDC, REFSMP, REFDET, DETIND( I ), -MAXDIS,
     :                    SAMPLO( I ), STATUS )
      END DO

*  Find the greatest in-scan distance (in the positive focal plane Y
*  direction) between the last sample in the first detector, and the
*  last sample in any other detector.
      REFSMP = REAL( CCM_SHIGH( IDC ) )
      MAXDIS = 0.0

      DO I = 2, NDETS
         CALL IRC1_DISTI( IDC, REFSMP, REFDET, REFSMP, DETIND( I ),
     :                    DIST, STATUS )
         MAXDIS = MAX( MAXDIS, DIST )
      END DO

*  Store the sample number at which each detector reaches the in-scan
*  position found above.
      DO I = 1, NDETS
         CALL IRC1_OFFSI( IDC, REFSMP, REFDET, DETIND( I ), MAXDIS,
     :                    SAMPHI( I ), STATUS )
      END DO

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_TRUNC_ERR4',
     :      'IRC_TRUNC: Unable to find image area covered by CRDD file',
     :                 STATUS )
      END IF

      END
