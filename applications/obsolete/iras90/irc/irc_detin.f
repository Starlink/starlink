      INTEGER FUNCTION IRC_DETIN( IDC, DETNO, STATUS )
*+
*  Name:
*     IRC_DETIN

*  Purpose:
*     Get the detector index for a given detector number.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = IRC_DETIN( IDC, DETNO, STATUS )

*  Description:
*     The detector index (if any) which corresponds to the given
*     detector number (in the range 1 to 62) is returned.  The detector
*     index is just the row number within the NDF DATA array, and
*     varies between the bounds of the second dimension of the NDF. If
*     the requested detector number is not included in the CRDD file,
*     the Starlink "BAD" value (VAL__BADI) is returned, but no error is
*     reported. If an error state exists on entry, or if any error
*     occurs within the function, VAL__BADI is returned.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     DETNO = INTEGER (Given)
*        The detector number for which the corresponding detector index
*        is required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     IRC_DETIN = INTEGER
*        The detector index corresponding to the given detector number.
*        This is set to VAL__BADI if STATUS indicates an error on entry,
*        or if an error is generated within IRC_DETIN.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-FEB-1991 (DSB):
*        Original version.
*     {enter_changes_here}

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
      INCLUDE 'IRC_ERR'          ! IRC error values.
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_DETIN( I90__DETS, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.

*  Arguments Given:
      INTEGER IDC
      INTEGER DETNO

*  Status:
      INTEGER STATUS             ! Global status
*.

*  If a bad status value exists on entry, set the function value to the
*  Starlink "BAD" value (VAL__BADI).
      IF( STATUS .NE. SAI__OK ) THEN
         IRC_DETIN = VAL__BADI

*  Otherwise check that the given identifier is valid.
      ELSE
         IF( IDC .GT. 0 .AND. IDC .LE. IRC__MAX ) THEN
            IF( CCM_VALID( IDC ) ) THEN

*  Check that the requested detector number is within the allowed
*  bounds.
               IF( DETNO .GT. 0 .AND. DETNO .LE. I90__DETS ) THEN

*  Get the value from common. This value will be VAL__BADI if the
*  detector is not present within the CRDD file.
                  IRC_DETIN = CCM_DETIN( DETNO, IDC )

*  Give an error report if the given detector index was out of bounds.
               ELSE
                  IRC_DETIN = VAL__BADI
                  STATUS = IRC__BADDN
                  CALL MSG_SETI( 'I', DETNO )
                  CALL ERR_REP( 'IRC_DETIN_ERR1',
     :                'IRC_DETIN: Detector number #^I does not exists',
     :                 STATUS )
               END IF

*  If an invalid IRC identifier was supplied, give an error report.
            ELSE
               IRC_DETIN = VAL__BADI
               STATUS = IRC__INVID
               CALL ERR_REP( 'IRC_DETIN_ERR2',
     :                     'IRC_DETIN: Invalid IRC identifier supplied',
     :                       STATUS )
            END IF

         ELSE
            IRC_DETIN = VAL__BADI
            STATUS = IRC__INVID
            CALL ERR_REP( 'IRC_DETIN_ERR3',
     :                    'IRC_DETIN: Invalid IRC identifier supplied',
     :                    STATUS )
         END IF

      END IF

      END
