      SUBROUTINE IRH_INDEX( NAME, IDH, INDEX, STATUS )
*+
*  Name:
*     IRH_INDEX

*  Purpose:
*     Searches a group for a given name and if found, returns its
*     index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_INDEX( NAME, IDH, INDEX, STATUS )

*  Description:
*     The given group is searched for the given name. If it is found
*     then the corresponding index within the group is returned. If it
*     is not found, the index is set to zero, but no error status is
*     generated. The search is case insensitive. If the group contains 
*     the name more than once then the lowest index is returned.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        The name to be searched for.
*     IDH = INTEGER (Given)
*        An IRH identifier for the group to be searched.
*     INDEX = INTEGER (Returned)
*        The index of the name within the group. This number is greater
*        than or equal to one if the name is found, and zero if it is
*        not found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference, added triling string lengths and
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
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Arguments Given:
      INTEGER IDH
      CHARACTER NAME*(*)

*  Arguments Returned:
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

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
         CALL ERR_REP( 'IRH_INDEX_ERR1',
     :                 'IRH_INDEX: Invalid group identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  If the required name is blank, return immediately with index set to
*  zero.
      INDEX = 0
      IF( NAME .EQ. ' ' ) GO TO 999

*  Call IRH1_FIND to do the work.  NB, the final argument specifies the
*  length of each character string in the mapped NAMES array, and is
*  required by UNIX. There is no corresponding dummy argument in the
*  code for IRH1_FIND.
      CALL IRH1_FIND( HCM_SIZE( IDH ), %VAL( HCM_NMPNT( IDH ) ),
     :                NAME, INDEX, STATUS, %VAL( IRH__SZNAM ) )

 999  CONTINUE

      END
* $Id$
