      SUBROUTINE GRP_SETSZ( IGRP, SIZE, STATUS )
*+
*  Name:
*     GRP_SETSZ

*  Purpose:
*     Reduce the size of a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_SETSZ( IGRP, SIZE, STATUS )

*  Description:
*     This routine sets the size of the given group to the specified
*     value. The new size must be less than or equal to the old size.
*     The names with indices greater than the new size are lost. Other
*     names remain unaltered.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     SIZE = INTEGER (Given)
*        The new group size. If a negative value is given, then zero
*        is used.
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
      INCLUDE 'GRP_ERR'          ! GRP errors

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the new size is greater than the old size, report an error.
      IF( SIZE .GT. CMN_GSIZE( SLOT ) ) THEN
         STATUS = GRP__SZINC
         CALL MSG_SETI( 'NEW', SIZE )
         CALL MSG_SETI( 'OLD', CMN_SIZE )
         CALL ERR_REP( 'GRP_SETSZ_ERR1',
     : 'GRP_SETSZ: New size (^NEW) is greater than the old size (^OLD)',
     :                  STATUS )

*  Otherwise, store the new size, limited to zero.
      ELSE
         CMN_GSIZE( SLOT ) = MAX( SIZE, 0 )

*  Truncate the arrays to this size.
         CALL GRP1_TRUNC( SLOT, STATUS )

      END IF

*  If an error has occured, give a context message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_SETSZ_ERR2',
     :                 'GRP_SETSZ: Error reducing the size of a group.',
     :                 STATUS )
      END IF

      END
