      SUBROUTINE IRH_PUT( IDH, SIZE, NAMES, INDEX, STATUS )
*+
*  Name:
*     IRH_PUT

*  Purpose:
*     Put a given set of names into a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_PUT( IDH, SIZE, NAMES, INDEX, STATUS )

*  Description:
*     The given names are stored in the group, overwriting the previous
*     names. The group is extended if necessary. The names can be
*     appended to the end of the group by giving INDEX a value of zero.

*  Arguments:
*     IDH = INTEGER (Given)
*        An IRH identifier for the group.
*     SIZE = INTEGER (Given)
*        The size of the NAMES array.
*     NAMES( SIZE ) = CHARACTER*(*) (Given)
*        The names to be stored in the group. The first name is stored
*        at the index given by INDEX, the last is stored at index
*        INDEX+SIZE-1. The previous names with these indices are
*        overwritten. The group is extended as necessary to make room
*        for the new names.
*     INDEX = INTEGER (Given)
*        The index at which to store the first name. A value of zero
*        causes the names to be appended to the end of the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference. Added DAT_PAR.
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
      INCLUDE 'IRH_ERR'          ! IRH error values.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Arguments Given:
      INTEGER IDH
      INTEGER SIZE
      CHARACTER NAMES( SIZE )*(*)
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index into the group NAMES array.
      INTEGER START              ! Lowest index at which to store a new
                                 ! name.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the IRH identifier is not valid, report an error.
      IF( IDH .LT. 1 .OR. IDH .GT. IRH__MAXG ) THEN
         STATUS = IRH__INVID

      ELSE IF( .NOT. HCM_VALID( IDH ) ) THEN
         STATUS = IRH__INVID

      END IF

      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_REP( 'IRH_PUT_ERR1',
     :                 'IRH_PUT: Invalid IRH identifier supplied',
     :                 STATUS )
      END IF


*  If an index of zero is given, use one more than the current group 
*  size.
      IF( INDEX .EQ. 0 ) THEN
         START = HCM_GSIZE( IDH ) + 1

*  If the given index was outside the bounds of thr group, report an 
*  error.
      ELSE IF( INDEX .LT. 0 .OR. INDEX .GT. HCM_GSIZE( IDH ) ) THEN
         STATUS = IRH__OUTBN
         CALL ERR_REP( 'IRH_PUT_ERR2', 
     :     'IRH_PUT: Supplied index is outside the bounds of the group',
     :     STATUS )
         GO TO 999

*  If the index was inside the bounds of the group, use it.
      ELSE
         START = INDEX

      END IF

*  Loop round each index value. Start at the highest index so that the
*  group will only be extended once.
      DO I = START + SIZE - 1, START, -1

*  Call IRH1_PTELM to do the work.  
         CALL IRH1_PTELM( IDH, I, NAMES( I - START + 1 ), 0, ' ', 0, 0,
     :                    STATUS )

      END DO

*  If an error occured give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRH_PUT_ERR3',
     :                 'IRH_PUT: Unable to add names to a group',
     :                  STATUS )
      END IF

      END
* $Id$
