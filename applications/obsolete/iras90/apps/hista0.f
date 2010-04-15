      SUBROUTINE HISTA0( LOC, THERE, HLOC, RLOC, EXTSIZ, NEXT, NRECS,
     :                   STATUS )
*+
*  Name:
*     HISTA0

*  Purpose:
*     Read global information from HISTORY structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HISTA0( LOC, THERE, HLOC, RLOC, EXTSIZ, NEXT, NRECS, STATUS )

*  Description:
*     This routine checks to see if the supplied object contains a
*     HISTORY structure. If it does, various items of information are
*     returned describing the structure together with locators to the
*     structure and the RECORDS array.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        A locator to an HDS object, potentially containing a HISTORY
*        structure.
*     THERE = LOGICAL (Returned)
*        True if the object located by LOC contains a HISTORY structure.
*        False otherwise.
*     HLOC = CHARACTER * ( * ) (Returned)
*        A locator to the HISTORY structure. Set blank if no such
*        structure exists.
*     RLOC = CHARACTER * ( * )  (Returned)
*        A locator to the RECORDS array. Set blank if no HISTORY
*        structure exists.
*     EXTSIZ = INTEGER (Returned)
*        The value of the EXTEND_SIZE component. Set to 5 if no
*        HISTORY structure exists.
*     NEXT = INTEGER (Returned)
*        The index of the next HISTORY record to be written. One more
*        than the value of the CURRENT_RECORD component (interpreted as
*        the most recently completed HISTORY record). Set to one if no
*        HISTORY structure exists.
*     NRECS = INTEGER (Returned)
*        The current size of the RECORDS array. Set to zero if no
*        HISTORY structure exists.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1992 (DSB):
*        Original version.
*     10-FEB-1992 (DSB):
*        Changed interpretation of CURRENT_RECORD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER LOC*(*)

*  Arguments Returned:
      LOGICAL THERE
      CHARACTER HLOC*(*)
      CHARACTER RLOC*(*)
      INTEGER EXTSIZ
      INTEGER NEXT
      INTEGER NRECS

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the NDF already has a HISTORY component.
      CALL DAT_THERE( LOC, 'HISTORY', THERE, STATUS )

*  If it does, get a locator to it.
      IF( THERE ) THEN
         CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*  Get values for the EXTEND_SIZE and CURRENT_RECORD components.
         CALL CMP_GET0I( HLOC, 'EXTEND_SIZE', EXTSIZ, STATUS )
         CALL CMP_GET0I( HLOC, 'CURRENT_RECORD', NEXT, STATUS )
         NEXT = NEXT + 1

*  Get a locator to the RECORDS component, and get its current size.
         CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )
         CALL DAT_SIZE( RLOC, NRECS, STATUS )

*  Return appropriate values if no HISTORY structure exists.
      ELSE
         HLOC = ' '
         RLOC = ' '
         EXTSIZ = 5
         NEXT = 1
         NRECS = 0
      END IF

      END
