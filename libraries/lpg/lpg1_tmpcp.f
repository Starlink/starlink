      SUBROUTINE LPG1_TMPCP( STATUS )
*+
*  Name:
*     LPG1_TMPCP

*  Purpose:
*     Copy temporary output NDFs totheir correct location.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG1_TMPCP( STATUS )

*  Description:
*     This routine copies any temporary output NDFs created by the
*     previous invocation of the application to the locations specified
*     when the output parameter was acccessed. It should be called once
*     the application has completed.
*
*     Temporary output NDFs are created if the output NDF specified by
*     the parameter is currently in use by the application.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAR-2004 (DSB):
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
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'LPG_CONST'        ! LPG private constants

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG global variables
*        TMPLST = INTEGER (Read)
*           A GRP identifier for a group holding the full specification
*           for any temporary output NDFs created during the previous
*           invocation of the application. A temporary output NDF is
*           created if the output NDF requested by the user may already
*           be open by the NDF system. In this case the temporary NDF
*           is copied to the requested position once the application has
*           finished.  The TMPLST group holds adjacent pairs of file
*           specs; the first one in each pair is the spec of the temporary
*           output NDF, the second is the spec of the requested output NDF.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER REQSPC*(GRP__SZNAM)! Full spec for requested output NDF
      CHARACTER TMPSPC*(GRP__SZNAM)! Full spec for temporary output NDF
      INTEGER I                  ! Index of current element
      INTEGER INDF1              ! Identifier for temporary output NDF
      INTEGER INDF2              ! Identifier for requested output NDF
      INTEGER JUNK               ! Un-used argument
      INTEGER PLACE2             ! Placeholder for requested output NDF
      INTEGER SIZGRP             ! No. of elements in the group
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin a new NDF context.
      CALL NDF_BEGIN

*  Loop round each of the temporary NDFs. These are stored in a GRP group.
*  The full spec of each temporary NDF is followed immediately in the group
*  by the full spec of the NDF which is to receive the output.
      CALL GRP_GRPSZ( TMPLST, SIZGRP, STATUS )
      DO I = 1, SIZGRP, 2

*  Get the name of the temporary NDF and open it for update access
*  (update not read, so that we can delete it).
         CALL GRP_GET( TMPLST, I, 1, TMPSPC, STATUS )
         CALL NDF_OPEN( DAT__ROOT, TMPSPC, 'UPDATE', 'OLD', INDF1,
     :                  JUNK, STATUS )

*  Get the name of the requested output NDF.
         CALL GRP_GET( TMPLST, I + 1, 1, REQSPC, STATUS )

*  First delete the existing NDF. This means opening it and then deleting
*  it. We need to do this to ensure that any HDS structure into which we
*  are writing the NDF is empty.
         CALL NDF_OPEN( DAT__ROOT, REQSPC, 'UPDATE', 'UNKNOWN', INDF2,
     :                  PLACE2, STATUS )
         IF( INDF2 .NE. NDF__NOID ) THEN
            CALL NDF_DELET( INDF2, STATUS )

*  Now open a new NDF in the place of the original NDF.
            CALL NDF_OPEN( DAT__ROOT, REQSPC, 'WRITE', 'NEW', JUNK,
     :                     PLACE2, STATUS )
         END IF

*  Copy the temporary NDF to the new location.
         CALL NDF_COPY( INDF1, PLACE2, INDF2, STATUS )

*  Annul the requested NDF identifiers.
         CALL NDF_ANNUL( INDF2, STATUS )

*  Delete the temporary NDF.
         CALL NDF_DELET( INDF1, STATUS )

      END DO

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
