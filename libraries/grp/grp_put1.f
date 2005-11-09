      SUBROUTINE GRP_PUT1( IGRP, NAME, INDEX, STATUS )
*+
*  Name:
*     GRP_PUT1

*  Purpose:
*     Put a single name into a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_PUT1( IGRP, NAME, INDEX, STATUS )

*  Description:
*     The given name is stored in the group in the form in which it is
*     supplied (including any control characters). It overwrites
*     any previous name stored at the specified index. The name can be 
*     appended to the end of the group by giving INDEX a value of zero 
*     or one greater than the current size of the group. An error is 
*     reported if the name is added beyond the end of the group (i.e.  
*     if adding the name would result in a gap within the group for 
*     which no names would be defined).

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     NAME = CHARACTER * ( * ) (Given)
*        The name to be stored in the group. 
*     INDEX = INTEGER (Given)
*        The index at which to store the name. A value of zero
*        causes the name to be appended to the end of the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-NOV-2005 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER IGRP
      CHARACTER NAME*(*)
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Just call GRP_PUT to do the work. No need for a status check since 
*  GRP_PUT will do it.
      CALL GRP_PUT( IGRP, 1, NAME, INDEX, STATUS )

      END
