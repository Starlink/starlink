      SUBROUTINE GRP_INDEX( NAME, IGRP, START, INDEX, STATUS )
*+
*  Name:
*     GRP_INDEX

*  Purpose:
*     Searches for a given name and if found, returns its index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_INDEX( NAME, IGRP, START, INDEX, STATUS )

*  Description:
*     The group is searched for the given name, starting at the name
*     with index given by START, and continuing to the end of the
*     group.  If it is found then the corresponding index within the
*     group is returned. If it is not found, the index is set to zero,
*     but no error status is generated. The search is case sensitive
*     unless GRP_SETCS has been called to indicate that the group is
*     case insensitive.  If the section of the group searched contains
*     the name more than once then the lowest index is returned.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name to be searched for.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group to be searched.
*     START = INTEGER (Given)
*        The lowest index to be checked.
*     INDEX = INTEGER (Returned)
*        The index of the name within the group. This number is greater
*        than or equal to START if the name is found, and zero if it is
*        not found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        CMN_SIZE( GRP__MAXG ) = INTEGER (Read)
*           The size of the mapped NAMES array of each group.
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER IGRP
      CHARACTER NAME*(*)
      INTEGER START

*  Arguments Returned:
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Ensure that an index of 1 is returned if an error condition
*  exists on entry.
      INDEX = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Call GRP1_FIND to do the work.  NB, the final argument specifies the
*  length of each character string in the mapped NAMES array, and is
*  required by UNIX. There is no corresponding dummy argument in the
*  code for GRP1_FIND.
      CALL GRP1_FIND( CMN_UPPER( SLOT ), CMN_SIZE( SLOT ), START,
     :                %VAL( CMN_NMPNT( SLOT ) ), NAME, INDEX, STATUS,
     :                %VAL( GRP__SZNAM ) )

*  If an error occurred, give a context message and ensure that the
*  returned index is 1.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'NM', NAME )
         CALL ERR_REP( 'GRP_INDEX_ERR1',
     :      'GRP_INDEX: Error searching for name "^NM" within a group.',
     :                 STATUS )
         INDEX = 1
      END IF

      END
