      SUBROUTINE ARD_PTWCS( IWCS, IGRP, STATUS )
*+
*  Name:
*     ARD_PTWCS

*  Purpose:
*     Construct an ARD WCS statement and append it to a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_PTWCS( IWCS, IGRP, STATUS )

*  Description:
*     This routine creates a WCS statement describing the supplied
*     FrameSet, and appends the statement to the end of the supplied GRP
*     group.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet.
*     IGRP = INTEGER (Given and Returned)
*        A GRP group identifier. If GRP__NOID is supplied, a new group is
*        created and its identifier returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'ARD_ERR'          ! ARD error constants
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Global Constants:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_AGRP= INTEGER (Write)
*           GRP identifier for the group holding AST_ data.
*        CMN_ASTLN = INTEGER (Write)
*           Next element to use in the group holding AST_ data.

*  Arguments Given:
      INTEGER IWCS
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_SNKTA

*  Local Variables:
      INTEGER CHAN               ! Pointer to an AST Channel
      INTEGER SIZE0              ! Initial size of the supplied group
      LOGICAL NEWGRP             ! Create a new GRP group?
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  If no group was supplied, create one now. Otherwise, note the
*  original size of the group.
      NEWGRP = ( IGRP .EQ. GRP__NOID )
      IF( NEWGRP ) THEN
         CALL GRP_NEW( ' ', IGRP, STATUS )
         SIZE0 = 0
      ELSE
         CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )
      END IF

*  Append the opening "WCS(" string.
      CALL GRP_PUT( IGRP, 1, 'WCS(<!!', 0, STATUS )

*  Create an AST_ Channel to write the supplied Object to the group.
*  Supply the ARD1_SNKTA routine as the "sink" routine for storing the
*  data, and specify that only essential information be included.
      CHAN = AST_CHANNEL( AST_NULL, ARD1_SNKTA, 'Full=-1,Comment=0',
     :                    STATUS )

*  Initialise the index of the first element in the group to be
*  used by the sink function.
      CMN_ASTLN = SIZE0 + 2

*  Store the group identifier in common.
      CMN_AGRP = IGRP

*  Write the copy of the supplied AST_ object to the Channel, thus
*  transferring the data to the catalogue. Report an error if the Object
*  could not be written.
      IF( AST_WRITE( CHAN, IWCS, STATUS ) .EQ. 0 .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__BADAR
         CALL ERR_REP( 'ARD_PTWCS_ERR1', 'ARD_PTWCS: No Objects '//
     :                 'written.', STATUS )
      END IF

*  Annul the Channel pointer, thus deleting the Channel.
      CALL AST_ANNUL( CHAN, STATUS )

*  Finish off with the final ")" string.
      CALL GRP_PUT( IGRP, 1, '!!>)', 0, STATUS )

*  If an error occurred, tidy up.
      IF( STATUS .NE. SAI__OK ) THEN
         IF( NEWGRP ) THEN
            CALL GRP_DELET( IGRP, STATUS )
         ELSE
            CALL ERR_BEGIN( STATUS )
            CALL GRP_SETSZ( IGRP, SIZE0, STATUS )
            CALL ERR_END( STATUS )
         END IF
      END IF

      END
