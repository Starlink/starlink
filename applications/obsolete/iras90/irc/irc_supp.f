      SUBROUTINE IRC_SUPP( IDC, NAME, THERE, LOC, STATUS )
*+
*  Name:
*     IRC_SUPP

*  Purpose:
*     Locate an item of support information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_SUPP( IDC, NAME, THERE, LOC, STATUS )

*  Description:
*     CRDD files may contain "support information". This is information
*     which is supplied with the data but which is not actually needed
*     for the operation of any other IRC routine. The support
*     information available will in general depend on the type of CRDD
*     contained in the CRDD file, but may contain things like the
*     geographic longitude and latitude at various times throughout the
*     scan. The exact support information available is listed in the
*     appendices of ID1 describing each individual CRDD type. This
*     routine returns an HDS locator to a particular item of support
*     information. No error is reported if the item does not exist, but
*     the argument THERE is returned false. The correct interpretation
*     of the support information is the responsibility of the calling
*     application.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     NAME = CHARACTER * ( * ) (Given)
*        The HDS name of the required item of support information.
*     THERE = LOGICAL (Returned)
*        Returned true if the item of support information was found, and
*        false otherwise.
*     LOC = CHARACTER * ( * ) (Returned)
*        An HDS locator to the required item of support information. The
*        variable supplied for this argument should have a declared
*        length given by the symbolic constant DAT__SZLOC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-OCT-1991 (DSB):
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
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.

*  Arguments Given:
      INTEGER IDC
      CHARACTER NAME*(*)

*  Arguments Returned:
      LOGICAL THERE
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DETLOC*(DAT__SZLOC) ! Locator to the DETAILS structure.
      CHARACTER SUPLOC*(DAT__SZLOC) ! Locator to the SUPPORT_INFO
                                    ! structure.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied IRC identifier is valid. If not, report an error
*  and quit.
      IF( IDC .LE. 0 .OR. IDC .GT. IRC__MAX ) THEN
         STATUS = IRC__INVID

      ELSE IF( .NOT. CCM_VALID( IDC ) ) THEN
         STATUS = IRC__INVID

      END IF

      IF( STATUS .EQ. IRC__INVID ) THEN
         CALL ERR_REP( 'IRC_SUPP_ERR1',
     :                 'IRC_SUPP: Invalid IRC identifier supplied',
     :                 STATUS )
      END IF

*  Get a locator to the DETAILS component of the CRDD_INFO structure.
      CALL DAT_FIND( CCM_CRDDL( IDC ), IRC__DNAME, DETLOC, STATUS )

*  Get a locator to the SUPPORT_INFO component of the DETAILS structure.
*  The actual name is stored in symbolic constant IRC__SNAME.
      CALL DAT_FIND( DETLOC, IRC__SNAME, SUPLOC, STATUS )

*  See if the requested component of the SUPPORT_INFO structure exists.
      CALL DAT_THERE( SUPLOC, NAME, THERE, STATUS )

*  If it does exist, get a locator to it.
      IF( THERE .AND. STATUS .EQ. SAI__OK ) THEN
         CALL DAT_FIND( SUPLOC, NAME, LOC, STATUS )
      END IF

*  Annul the locators to the DETAILS and SUPPORT_INFO structures.
      CALL DAT_ANNUL( SUPLOC, STATUS )
      CALL DAT_ANNUL( DETLOC, STATUS )

*  If an error occured, annul the returned locator and give a contextual
*  message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( LOC, STATUS )
         CALL MSG_SETC( 'N', NAME )
         CALL ERR_REP( 'IRC_SUPP_ERR2',
     :            'IRC_SUPP: Unable to locate support information "^N"',
     :                 STATUS )
      END IF

      END
