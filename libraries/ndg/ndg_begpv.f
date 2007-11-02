      SUBROUTINE NDG_BEGPV( STATUS )
*+
*  Name:
*     NDG_BEGPV

*  Purpose:
*     Begin an NDF provenance block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_BEGPV( STATUS )

*  Description:
*     This routine should be called to mark the start of an NDF 
*     provenance block. The block should be ended by a matching 
*     call to NDG_ENDPV. See NDG_ENDPV for more details.
*
*     Note - provenance blocks must not be nested.

*  Arguments:
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
*     31-OCT-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INTEGER RDGRP              ! Group holding input NDFs
      INTEGER WRGRP              ! Group holding output NDFs
      COMMON /NDG_PRV/ RDGRP, WRGRP

*  External References:
      EXTERNAL NDG1_HNDLR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Indicate that the routine NDG1_HNDLR should be called whenever an NDF
*  is opened or closed.
      CALL NDF_HNDLR( "READ_EXISTING_NDF", NDG1_HNDLR, .TRUE., STATUS )
      CALL NDF_HNDLR( "WRITE_EXISTING_NDF", NDG1_HNDLR, .TRUE., STATUS )
      CALL NDF_HNDLR( "UPDATE_EXISTING_NDF", NDG1_HNDLR, .TRUE., 
     :                 STATUS )
      CALL NDF_HNDLR( "OPEN_NEW_NDF", NDG1_HNDLR, .TRUE., STATUS )
      CALL NDF_HNDLR( "CLOSE_NDF", NDG1_HNDLR, .TRUE., STATUS )

*  Indicate that the PROVENANCE extension should not be propagated by
*  default when NDF_PROP or NDF_SCOPY is called.
      CALL NDF_TUNE( 0, 'PXTPROVENANCE', STATUS )

*  Create a GRP group to hold the paths to the NDFs that are read during 
*  the provenance block.
      CALL GRP_NEW( 'Input NDFs', RDGRP, STATUS )

*  Create a GRP group to hold the paths to the NDFs that are written during 
*  the provenance block.
      CALL GRP_NEW( 'Output NDFs', WRGRP, STATUS )

      END
