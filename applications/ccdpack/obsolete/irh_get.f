      SUBROUTINE IRH_GET( IDH, INDEX, SIZE, NAMES, STATUS )
*+
*  Name:
*     IRH_GET

*  Purpose:
*     Returns a set of names contained in a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_GET( IDH, INDEX, SIZE, NAMES, STATUS )

*  Description:
*     The names with indices between INDEX and INDEX+SIZE-1 (inclusive)
*     contained in the given group are returned.

*  Arguments:
*     IDH = INTEGER (Given)
*        An IRH identifier for the group.
*     INDEX = INTEGER (Given)
*        The lowest index for which the corresponding name is required.
*     SIZE = INTEGER (Given)
*        The number of names required.
*     NAMES( SIZE ) = CHARACTER*(*) (Returned)
*        The names held at the given positions in the group. The 
*        corresponding character variables should have declared length
*        specified by the symbolic constant IRH__SZNAM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed reference to I90_PAR, added trailing string lengths and
*        DAT_PAR.
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
*        HCM_NMPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        HCM_SIZE( IRH__MAXG ) = INTEGER (Read)
*           The size of the mapped NAMES array of each group.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding IRH identifier is valid (i.e. in
*           use).
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IDH
      INTEGER INDEX
      INTEGER SIZE

*  Arguments Returned:
      CHARACTER NAMES( SIZE )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index into the group NAMES array.
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
         CALL ERR_REP( 'IRH_GET_ERR1',
     :                 'IRH_GET: Invalid IRH identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Loop round each index value.
      DO I = INDEX, INDEX + SIZE - 1

*  If the index is outside the bounds of the group, report an error.
         IF( I .LE. 0 .OR. I .GT. HCM_GSIZE( IDH ) ) THEN
            STATUS = IRH__OUTBN
            CALL ERR_REP( 'IRH_GET_ERR2', 
     :      'IRH_GET: Attempt to access a name outside the bounds of'//
     :      ' the group.', STATUS )
            GO TO 999

*  Otherwise, call IRH1_IGET to do the work.  NB, the final argument
*  specifies the length of each character string in the mapped NAMES
*  array, and is required by UNIX. There is no corresponding dummy
*  argument in the code for IRH1_IGET. 
         ELSE
            CALL IRH1_IGET( HCM_GSIZE( IDH ), %VAL( HCM_NMPNT( IDH ) ),
     :                   I, NAMES( I - INDEX + 1 ), STATUS,
     :                   %VAL( IRH__SZNAM ) )

         END IF

      END DO

*  If an error has occured give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRH_GET_ERR3',
     :                 'IRH_GET: Unable to get names from a group',
     :                  STATUS )
      END IF
      
      END
* $Id$
