      SUBROUTINE IRC1_GETID( IDC, STATUS )
*+
*  Name:
*     IRC1_GETID

*  Purpose:
*     Obtain a free IRC identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_GETID( IDC, STATUS )

*  Description:
*     The lowest free IRC identifier is returned. If no free identifiers
*     can be found then an error is reported and status is returned
*     equal to IRC__NOMOR. Note, this routine does NOT flag the
*     returned identifier as used, in common.

*  Arguments:
*     IDC = INTEGER (Returned)
*        The lowest free IRC identifier. If an error occurs IDC is
*        returned equal to the value IRC__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JAN-1991 (DSB):
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
      INCLUDE 'I90_DAT'          ! IRCS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the elements of the corresponding elements of
*           all the common arrays contain valid CRDD information.

*  Arguments Returned:
      INTEGER IDC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the identifier to the invalid value, IRC__NOID.
      IDC = IRC__NOID

*  Set the identifier to the lowest invalid identifier found.
      DO I = 1, IRC__MAX
         IF( .NOT.CCM_VALID (I) .AND. IDC .EQ. IRC__NOID) IDC = I
      END DO

*  If no invalid identifier was found, give an error.
      IF( IDC .EQ. IRC__NOID ) THEN
         STATUS = IRC__NOMOR
         CALL ERR_REP( 'IRC1_GETID_ERR1',
     :             'IRC1_GETID: No room for any more CRDD information',
     :                 STATUS )
      END IF

      END
