      SUBROUTINE NDG1_APPEN( IGRP1, IGRP2, TEMPLT, REST, STATUS )
*+
*  Name:
*     NDG1_APPEN

*  Purpose:
*     Append matching files to a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_APPEN( IGRP1, IGRP2, TEMPLT, REST, STATUS )

*  Description:
*     All files matching the supplied file template are appended to IGRP1. 
*     For each such file appended to IGRP1, a copy of REST is also appended 
*     to IGRP2.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        An identifier for the group to which the matching file names should
*        be appended.
*     IGRP2 = INTEGER (Given)
*        An identifier for the group to which copies of REST should
*        be appended.
*     TEMPLT = CHARACTER * ( * ) (Given)
*        The wild card file template.
*     REST = CHARACTER * ( * ) (Given)
*        Text to be added to IGRP2 for each matching file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1999 (DSB):
*        Original version.
*     26-AUG-1999 (DSB):
*        Changed to use new NDG1_WILD which returns all matching files,
*        not just ones which are known data files.
*     2-SEP-2004 (TIMJ):
*        Switch to using ONE_FIND_FILE
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'ONE_ERR'          ! ONE_ constants

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IGRP2
      CHARACTER TEMPLT*(*)
      CHARACTER REST*(*)
      
*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL ONE_FIND_FILE
      EXTERNAL ONE_FIND_FILE

*  Local Variables:
      CHARACTER FILE*(GRP__SZFNM) ! The file spec of the matching file
      INTEGER ICONTX             ! Context for ndg1_wild
      LOGICAL FOUND              ! Matched a filename
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ignore blank templates.
      IF( TEMPLT .NE. ' ' ) THEN

*  Initialise the context value used by ONE_FIND_FILE so that a new file
*  searching context will be started.
         ICONTX = 0

*  Loop round looking for matching files until we get bad status
*  (which may include ONE__NOFILES)
         DO WHILE( STATUS .EQ. SAI__OK ) 

*  Attempt to find the next matching file.
            FILE = ' '
            FOUND = ONE_FIND_FILE( TEMPLT, .TRUE., FILE, ICONTX, 
     :           STATUS )

*  If another file was found which matches the name...
            IF( FOUND .AND. STATUS .EQ. SAI__OK ) THEN

*  Append it to the group.
               CALL GRP_PUT( IGRP1, 1, FILE, 0, STATUS )

*  Append a copy of REST to the second group.
               CALL GRP_PUT( IGRP2, 1, REST, 0, STATUS )
           
            END IF

         END DO

*  Clear status if no more files
         IF (STATUS .EQ. ONE__NOFILES) CALL ERR_ANNUL( STATUS )

*  End the search context.
         CALL ONE_FIND_FILE_END( ICONTX, STATUS )

      END IF

      END
