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
*     {enter_new_authors_here}

*  History:
*     23-FEB-1999 (DSB):
*        Original version.
*     26-AUG-1999 (DSB):
*        Changed to use new NDG1_WILD which returns all matching files,
*        not just ones which are known data files.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'        ! NDG private constants.
      INCLUDE 'NDG_ERR'          ! NDG error constants.

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IGRP2
      CHARACTER TEMPLT*(*)
      CHARACTER REST*(*)
      
*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER NDG1_WILD
      INTEGER NDG1_EWILD

*  Local Variables:
      CHARACTER FILE*(GRP__SZFNM) ! The file spec of the matching file
      INTEGER ICONTX             ! Context for ndg1_wild
      INTEGER ISTAT              ! Local status value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ignore blank templates.
      IF( TEMPLT .NE. ' ' ) THEN

*  Initialise the context value used by NDG1_WILD so that a new file
*  searching context will be started.
         ICONTX = 0

*  Loop round looking for matching files.
         ISTAT = NDG__OK
         DO WHILE( ISTAT .EQ. NDG__OK .AND. STATUS .EQ. SAI__OK ) 

*  Attempt to find the next matching file.
            FILE = ' '
            ISTAT = NDG1_WILD( TEMPLT, FILE, ICONTX )

*  If another file was found which matches the name...
            IF( ISTAT .EQ. NDG__OK ) THEN

*  Append it to the group.
               CALL GRP_PUT( IGRP1, 1, FILE, 0, STATUS )

*  Append a copy of REST to the second group.
               CALL GRP_PUT( IGRP2, 1, REST, 0, STATUS )
           
*  If a system error was detected by NDG1_WILD, report it.
            ELSE IF ( ISTAT .EQ. NDG__WPER ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'NDG1_APPEN_ERR1', 'NDG1_APPEN: Error'//
     :                       ' getting pipe from forked process', 
     :                       STATUS )
      
            ELSE IF ( ISTAT .EQ. NDG__WMER ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'NDG1_APPEN_ERR2', 'NDG1_APPEN: '//
     :                       'Cannot allocate memory', STATUS )
      
            END IF

         END DO

*  End the search context.
         ISTAT = NDG1_EWILD( ICONTX )

      END IF

      END
