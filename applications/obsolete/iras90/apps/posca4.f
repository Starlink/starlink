      SUBROUTINE POSCA4( APAR, BPAR, SCS, IGRP, NCRDDF, A, B, STATUS )
*+
*{subroutine_prologue}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Arguments Given:
      CHARACTER APAR*(*)
      CHARACTER BPAR*(*)
      CHARACTER SCS*(*)
      INTEGER IGRP
      INTEGER NCRDDF

*  Arguments Returned:
      DOUBLE PRECISION A
      DOUBLE PRECISION B

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION ANGLE     ! Scan angle at a sample.
      DOUBLE PRECISION REFDEC    ! DEC of reference point.
      DOUBLE PRECISION REFRA     ! RA of reference point.


      INTEGER BAND               ! Waveband index of CRDD file.
      INTEGER IDC                ! IRC identifier for CRDD file.
      INTEGER INDEX              ! Index of NDF within input group.
      INTEGER INDF               ! Identifier for input NDF.
      INTEGER OBS                ! OBS number.
      INTEGER SOP                ! SOP number.


      LOGICAL FOUND              ! True if a good CRDD file has been
                                 ! found.


      REAL NOMSPD                ! Nominal scan speed.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find a valid CRDD file.
      INDEX = 1
      FOUND = .FALSE.
      DO WHILE( .NOT. FOUND )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP, INDEX, 'READ', INDF, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to import it into the IRC CRDD file handling system.
         CALL IRC_IMPRT( INDF, IDC, STATUS )

*  If the NDF is a valid CRDD file, get the RA and DEC of the reference
*  point.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP, OBS,
     :                     STATUS )

*  Convert the reference position coordinates to the required coordinate
*  system.
            CALL IRA_CONVT( 1, REFRA, REFDEC, 'EQU(B1950)', SCS,
     :                      IRA__IRJEP, A, B, STATUS )

*  Release the CRDD file from the IRC system.
            CALL IRC_ANNUL( IDC, STATUS )

*  Indicate that a valid CRDD file has been found.
            FOUND = .TRUE.

*  If the NDF is not a valid CRDD file, release the CRDD file from the
*  IRC system and annul the error.
         ELSE
            CALL IRC_ANNUL( IDC, STATUS )
            CALL ERR_ANNUL( STATUS )

*  Increment the index of the next input NDF.
            INDEX = INDEX + 1

*  If all input NDFs have been tried, report an error.
            IF( INDEX .GT. NCRDDF .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'POSCA4_ERR1',
     :      'POSCA4: Cannot access any of the input NDFs as CRDD files',
     :                       STATUS )
               GO TO 999
            END IF

         END IF

*  Annul the NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

      END DO

*  Get the sky coordinates of the required position, using the
*  coordinates of the reference position of the first valid CRDD file as
*  the suggested defaults.
      CALL IRA_GETCO( APAR, BPAR, ' of the required position', SCS,
     :                .TRUE.,  A, B, STATUS )

*  Finish
 999  CONTINUE

      END
