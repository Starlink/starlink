      SUBROUTINE IRH1_IRMBL( SIZE, NAMES, MODGP, MODGI, LEVEL, FILE,
     :                       NEWSIZ, STATUS )
*+
*  Name:
*     IRH1_IRMBL

*  Purpose:
*     Remove unused elements from a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_IRMBL( SIZE, NAMES, MODGP, MODGI, LEVEL, FILE, NEWSIZ,
*                      STATUS )

*  Description:
*     Unused elements can be introduced into a group by calling
*     IRH1_ERELM.  This routine re-organises the contents of a group to
*     remove such unused elements. An element is unused if the 
*     corresponding name is equal to the symbolic constant IRH__BLANK.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the arrays holding group information.
*     NAMES( SIZE ) = CHARACTER (Given and Returned)
*        The names forming the group.
*     MODGP( SIZE ) = INTEGER (Given and Returned)
*        The IRH identifiers of the groups used as a basis for any
*        names in the group which were created as a result of a
*        modification element.
*     MODGI( SIZE ) = INTEGER (Given and Returned)
*        The indices within the groups specified by MODGP1 of the names
*        used as a basis for any names in the group which were created
*        as a result of a modification element.
*     LEVEL( SIZE ) = INTEGER (Given and Returned)
*        The indirection depth at which each name in the group was
*        specified.
*     FILE( SIZE ) = CHARACTER (Given and Returned)
*        The name of the indirection file in which each name in the
*        group was specified. 
*     NEWSIZ = INTEGER (Returned)
*        The size of the group after removal of blank elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRH_PAR'          ! IRH constants

*  Arguments Given:
      INTEGER SIZE
      CHARACTER NAMES( SIZE )*(*)
      INTEGER MODGP( SIZE )
      INTEGER MODGI( SIZE )
      INTEGER LEVEL( SIZE )
      CHARACTER FILE( SIZE )*(*)

*  Arguments Returned:
      INTEGER NEWSIZ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index into group.
      INTEGER J                  ! Lowest free slot in the group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the position of the lowest free slot.
      J = 1

*  Loop round all elements in the group.
      DO I = 1, SIZE

*  If the NAMES entry is not blank...
         IF( NAMES( I ) .NE. IRH__BLANK ) THEN

*  ...shift all the element information to the lowest free slot (unless
*  the lowest free slot is actually the current slot).
            IF( I .GT. J ) THEN
               NAMES( J ) = NAMES( I )            
               MODGP( J ) = MODGP( I )
               MODGI( J ) = MODGI( I )
               LEVEL( J ) = LEVEL( I )
               FILE( J ) = FILE( I )
            END IF

*  Update the position of the lowest free slot.
            J = J + 1

         END IF

      END DO

*  Return the new group size.
      NEWSIZ = J - 1

      END
* $Id$
