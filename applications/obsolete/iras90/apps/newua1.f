      SUBROUTINE NEWUA1( VAR, IDC, UNITS, OLDUN, SLOW, SHIGH, DLOW,
     :                   DHIGH, DATIN, VARIN, DATOUT, VAROUT, DBAD,
     :                   VBAD, STATUS )
*+
*  Name:
*     NEWUA1

*  Purpose:
*     Scale a CRDD file to new units.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NEWUA1( VAR, IDC, UNITS, OLDUN, SLOW, SHIGH, DLOW,
*                  DHIGH, DATIN, VARIN, DATOUT, VAROUT, DBAD,
*                  VBAD, STATUS )

*  Description:
*     The effective solid angle of each detector is taken into account
*     by processing each detector separately.

*  Arguments:
*     VAR = LOGICAL (Given)
*        True if the variance arrays are to be scaled.
*     IDC = INTEGER (Given)
*        The IRC identifier for the input CRDD file.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units required for the output.
*     OLDUN = CHARACTER * ( * ) (Given)
*        The units of the input.
*     SLOW = INTEGER (Given)
*        The low sample bound.
*     SHIGH = INTEGER (Given)
*        The high sample bound.
*     DLOW = INTEGER (Given)
*        The low detector bound.
*     DHIGH = INTEGER (Given)
*        The high detector bound.
*     DATIN( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Given)
*        The input data array.
*     VARIN( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Given)
*        The input variance array. Only accessed if VAR is true.
*     DATOUT( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Returned)
*        The output data array.
*     VAROUT( SLOW:SHIGH, DLOW:DHIGH ) = REAL (Returned)
*        The output variance array. Only accessed if VAR is true.
*     DBAD = LOGICAL (Returned)
*        True if any bad values are found in the data array.
*     VBAD = LOGICAL (Returned)
*        True if any bad values are found in the variance array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1992 (DSB):
*        Original version.
*     12-OCT-1993 (DSB):
*        Check scale factor returned by irm_untcv is not bad.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      LOGICAL VAR
      INTEGER IDC
      CHARACTER UNITS*(*)
      CHARACTER OLDUN*(*)
      INTEGER SLOW
      INTEGER SHIGH
      INTEGER DLOW
      INTEGER DHIGH
      REAL DATIN( SLOW:SHIGH, DLOW:DHIGH )
      REAL VARIN( SLOW:SHIGH, DLOW:DHIGH )

*  Arguments Returned:
      REAL DATOUT( SLOW:SHIGH, DLOW:DHIGH )
      REAL VAROUT( SLOW:SHIGH, DLOW:DHIGH )
      LOGICAL DBAD
      LOGICAL VBAD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Return the detector number stored at
                                 ! a given index.

*  Local Variables:
      INTEGER DETIND             ! The current detector index.
      INTEGER DETNO              ! The current detector number.
      INTEGER SAMP               ! The current sample number.

      REAL SCALE                 ! Factor for unit conversion.
      REAL VAL                   ! Input value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the bad value flags.
      DBAD = .FALSE.
      VBAD = .FALSE.

*  Loop round each detector in the CRDD file.
      DO DETIND = DLOW, DHIGH

*  Get the detector number stored at this index.
         DETNO = IRC_DETNO( IDC, DETIND, STATUS )

*  Get the conversion factor for this detector.
         CALL IRM_UNTCV( OLDUN, UNITS, 1, DETNO, SCALE, STATUS )

*  If the detector is dead, store bad values in the output.
         IF( SCALE .EQ. VAL__BADR ) THEN
            DO SAMP = SLOW, SHIGH
               DATOUT( SAMP, DETIND ) = VAL__BADR
               IF( VAR ) VAROUT( SAMP, DETIND ) = VAL__BADR
            END DO
            DBAD = .TRUE.
            IF( VAR ) VBAD = .TRUE.

*  Otherwise, loop round each sample.
         ELSE
            DO SAMP = SLOW, SHIGH

*  If valid, write the input value to the output in the new units.
               VAL = DATIN( SAMP, DETIND )
               IF( VAL .NE. VAL__BADR ) then
                  DATOUT( SAMP, DETIND ) = VAL*SCALE

*  Store a bad value in the output if the input is bad, and set the bad
*  pixel flag.
               ELSE
                  DATOUT( SAMP, DETIND ) = VAL__BADR
                  DBAD = .TRUE.
               END IF

*  If required, do the same with the variance value.
               IF( VAR ) THEN

                  VAL = VARIN( SAMP, DETIND )
                  IF( VAL .NE. VAL__BADR ) then
                     VAROUT( SAMP, DETIND ) = VAL*SCALE*SCALE
                  ELSE
                     VAROUT( SAMP, DETIND ) = VAL__BADR
                     VBAD = .TRUE.
                  END IF

               END IF

* Do the next sample from the currrent detector.
            END DO

         END IF

*  Do the next detector.
      END DO

      END
