      SUBROUTINE CTG1_APPEN( IGRP1, IGRP2, TEMPLT, REST, STATUS )
*+
*  Name:
*     CTG1_APPEN

*  Purpose:
*     Append matching files to a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_APPEN( IGRP1, IGRP2, TEMPLT, REST, STATUS )

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

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1999 (DSB):
*        Original version.
*     2-DEC-1999 (DSB):
*        Added options argument to ctg1_wild call.
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
      INCLUDE 'ONE_ERR'          ! ONE error constants

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
      INTEGER ICONTX             ! Context for one_find_file
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
            IF ( FOUND .AND. STATUS .EQ. SAI__OK ) THEN

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
