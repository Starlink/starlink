      SUBROUTINE NDG1_HNDLR( EVNAME, EVTEXT, STATUS )
*+
*  Name:
*     NDG1_HNDLR

*  Purpose:
*     Handle an NDF event.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_HNDLR( EVNAME, EVTEXT, STATUS )

*  Description:
*     NDG_BEGPV registers this routine with the NDF library as an event
*     handler to be called whenever an NDF is opened or closed.

*  Arguments:
*     EVNAME = CHARACTER * ( * ) (Given)
*        The type of NDF event that has occurred.
*     EVTEXT = CHARACTER * ( * ) (Given)
*        Descriptive information associated with the event. This will be
*        the path to the file that was opened or closed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-NOV-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'DAT_PAR'          ! AST constants and functions

*  Global Variables:
      INTEGER RDGRP              ! Group holding input NDFs
      INTEGER WRGRP              ! Group holding output NDFs
      COMMON /NDG_PRV/ RDGRP, WRGRP

*  Arguments Given:
      CHARACTER EVNAME*(*)
      CHARACTER EVTEXT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the event was the opening of an input NDF( i.e. an existing NDF 
*  opened for READ or UPDATE mode), add the path to the NDF to the RDGRP
*  group.
      IF( EVNAME .EQ. 'READ_EXISTING_NDF' .OR. 
     :    EVNAME .EQ. 'UPDATE_EXISTING_NDF' ) THEN
         CALL GRP_PUT1( RDGRP, EVTEXT, 0, STATUS )
      END IF

*  If the event was the opening of an output NDF( i.e. an existing NDF 
*  opened for UPDATE mode or a new NDF opened), add the path to the NDF 
*  to the RDGRP group.
      IF( EVNAME .EQ. 'UPDATE_EXISTING_NDF' .OR. 
     :    EVNAME .EQ. 'OPEN_NEW_NDF' ) THEN
         CALL GRP_PUT1( WRGRP, EVTEXT, 0, STATUS )
      END IF

      END
