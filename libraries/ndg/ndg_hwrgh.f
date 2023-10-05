      SUBROUTINE NDG_HWRGH( INDF, STATUS )
*+
*  Name:
*     NDG_HWRGH

*  Purpose:
*     Write GRP history to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_HWRGH( INDF, STATUS )

*  Description:
*     This routine appends a description of each currently registered GRP
*     group to the current History record in the supplied NDF. See
*     NDF_ENDGH. It returns without action if 1) the NDF library has not
*     yet written a default history record to the NDF, or 2) all GRP
*     history has already been written to the NDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-OCT-2009 (DSB):
*        Original version.
*     19-AUG-2010 (DSB):
*        Do not remove the NDF from the KeyMap before exiting as new
*        groups may be registered subsequently and may need to be written
*        out by a later call to this function.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'

*  Global Variables:
      INCLUDE 'NDG_COM2'         ! Global GRP history information

*  Arguments Given:
      INTEGER INDF

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER NDFNAM*255
      INTEGER NDFLEN
      LOGICAL UDHKMP
*.

*  Check inherited status
      IF( STATUS .NE. SAI__OK ) RETURN

*  Lock the mutex that serialises access to NDG globals
      CALL NDG1_GLOCK( .TRUE. )

*  Now lock the required global AST objects
      CALL NDG1_ALOCK( .TRUE., DHKMP_COM2, UDHKMP, STATUS )

*  Check that the supplied NDF is in the KeyMap holding the paths of NDFs
*  to which default history has been written.
      IF( DHKMP_COM2 .NE. AST__NULL ) THEN
         CALL NDF_MSG( 'T', INDF )
         CALL MSG_LOAD( ' ', '^T', NDFNAM, NDFLEN, STATUS )
         IF( AST_MAPHASKEY( DHKMP_COM2, NDFNAM( : NDFLEN ),
     :                      STATUS ) ) THEN

*  Append the contents of registered groups to the current history record
*  in the NDF.
            CALL NDG1_HWRGH( INDF, STATUS )

         END IF
      END IF

*  Now unlock the global AST objects so that they can be used by other
*  threads.
      IF(UDHKMP) CALL NDG1_ALOCK( .FALSE., DHKMP_COM2, UDHKMP, STATUS )

*  Unlock the mutex that serialises access to NDG globals
      CALL NDG1_GLOCK( .FALSE. )

      END
