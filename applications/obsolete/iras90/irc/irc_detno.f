      INTEGER FUNCTION IRC_DETNO( IDC, DETIND, STATUS )
*+
*  Name:
*     IRC_DETNO

*  Purpose:
*     Get the detector number for a given detector index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = IRC_DETNO( IDC, DETIND, STATUS )

*  Description:
*     The detector number (in the range 1 to 62) of the detector which
*     generated the data held at the detector index given by argument
*     DETIND is found. The detector index is just the row number within
*     the NDF DATA array, and varies between the bounds of the second
*     dimension of the NDF. An error is reported if a detector index is
*     given outside this range. If any error occurs, the function
*     returns the Starlink "BAD" values (VAL__BADI).

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     DETIND = INTEGER (Given)
*        The detector index for which the corresponding detector number
*        is required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     IRC_DETNO = INTEGER
*        The detector number corresponding to the given detector index.
*        This is set to VAL__BADI if STATUS indicates an error on entry,
*        or if an error is generated within IRC_DETNO.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-FEB-1991 (DSB):
*        Original version.
*     5-NOV-1991 (DSB):
*        Index into the common array CCM_DETNO corrected by adding on
*        " - CCM_DLOW( IDC ) + 1 ".
*     11-FEB-1992 (DSB):
*        Index into the common array CCM_DETNO modified to use CCM_DETOR
*     {enter_further_changes_here}

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
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.
*        CCM_DETNO( IRC__MXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

*  Arguments Given:
      INTEGER IDC
      INTEGER DETIND

*  Status:
      INTEGER STATUS             ! Global status
*.

*  If a bad status value exists on entry, set the function value to the
*  Starlink "BAD" value (VAL__BADI).
      IF( STATUS .NE. SAI__OK ) THEN
         IRC_DETNO = VAL__BADI

*  Otherwise check that the given identifier is valid.
      ELSE
         IF( IDC .GT. 0 .AND. IDC .LE. IRC__MAX ) THEN
            IF( CCM_VALID( IDC ) ) THEN

*  Check that the requested detector index is within the bounds of the
*  second dimension of the NDF.
               IF( DETIND .GE. CCM_DLOW( IDC ) .AND.
     :             DETIND .LE. CCM_DHIGH( IDC ) ) THEN

*  Get the value from common.
                  IRC_DETNO = CCM_DETNO( DETIND + CCM_DETOR( IDC ),
     :                                   IDC )

*  Give an error report if the given detector index was out of bounds.
               ELSE
                  IRC_DETNO = VAL__BADI
                  STATUS = IRC__BADDI
                  CALL MSG_SETI( 'I', DETIND )
                  CALL ERR_REP( 'IRC_DETNO_ERR1',
     :                  'IRC_DETNO: Detector index ^I is out of bounds',
     :                   STATUS )
               END IF

*  If an invalid IRC identifier was supplied, give an error report.
            ELSE
               IRC_DETNO = VAL__BADI
               STATUS = IRC__INVID
               CALL ERR_REP( 'IRC_DETNO_ERR2',
     :                     'IRC_DETNO: Invalid IRC identifier supplied',
     :                       STATUS )
            END IF

         ELSE
            IRC_DETNO = VAL__BADI
            STATUS = IRC__INVID
            CALL ERR_REP( 'IRC_DETNO_ERR3',
     :                    'IRC_DETNO: Invalid IRC identifier supplied',
     :                    STATUS )
         END IF

      END IF

      END
