      SUBROUTINE IRH_NEW( TITLE, IDH, STATUS )
*+
*  Name:
*     IRH_NEW

*  Purpose:
*     Create a new empty group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_NEW( TITLE, IDH, STATUS )

*  Description:
*     A new empty group is created and an identifier to it is returned
*     in IDH. Names can be stored in this group by calling routines
*     IRH_APPND (for names obtained through the ADAM parameter system)
*     or IRH_PUT (for names specified by the application).  The string
*     supplied in TITLE is associated with the group. It can be
*     retrieved by IRH_GTTL, or modified by IRG_PTTL.

*  Arguments:
*     TITLE = CHARACTER (Given)
*        A descriptive string to be associated with the group.
*     IDH = INTEGER (Returned)
*        An identifier for the created group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-JUN-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Added DAT_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_STATE = CHARACTER (Read)
*           If HCM_STATE = IRH__GOING then IRH has been initialised.

*  Arguments Given:
      CHARACTER TITLE*(*)

*  Arguments Returned:
      INTEGER IDH

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Set the IRH identifier to an invalid value before checking the
*  status.
      IDH = IRH__NOID

*  Check inherited global status. If bad, return with an invalid IRH
*  identifier.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first call to an IRH routine, initialize the common
*  blocks and data structures associated with IRH.
      IF( HCM_STATE .NE. IRH__GOING ) THEN
         CALL IRH1_INIT( STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999
      END IF

*  Get the next free IRH identifier. A new empty group structure is
*  created within a temporary HDS object to hold the group contents.
      CALL IRH1_GTIDH( TITLE, IDH, STATUS )

*  If an error occured, annul the group and give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL IRH_ANNUL( IDH, STATUS )
         CALL ERR_REP( 'IRH_NEW_ERR1',
     :                 'IRH_NEW: Unable to create a new group', STATUS )
      END IF

      END
* $Id$
