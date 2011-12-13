      SUBROUTINE ASTSHOWMESH( STATUS )
*+
*  Name:
*     ASTSHOWMESH

*  Purpose:
*     Display a mesh of points covering the surface of a Region.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTSHOWMESH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application writes a table to standard output containing the axis
*     values at a mesh of points covering the surface of the supplied Region.
*     Each row of output contains a tab-separated list of axis values, one
*     for each axis in the Frame encapsulated by the Region. The number of
*     points in the mesh is determined by the MeshSize attribute.
*
*     The table is preceeded by a given title string, and followed by a
*     single line containing the word "ENDMESH".

*  Usage:
*     astshowmesh this format ttl

*  ADAM Parameters:
*     THIS = LITERAL (Read)
*        An NDF, FITS file or text file holding the Frame. If an NDF is
*        supplied, the current Frame of the WCS FrameSet will be used. If a
*        FITS file is supplied, the Frame corresponding to the primary axis
*        descriptions will be used.
*     FORMAT = _LOGICAL (Read)
*         If TRUE, then the output table contains axis values that have
*         been formatted using thew AST_FORMAT method. Otherwise, the
*         table contains floating point values displayed with a default format.
*     TITLE = LITERAL (Read)
*        A text string to display before the table.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
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
*     5-JUN-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  External References:
      EXTERNAL AST_ISAREGION

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS
      CHARACTER TTL*255
      LOGICAL FORMAT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Region.
      CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS,
     :                 STATUS )

*  Get theother parameters
      CALL PAR_GET0L( 'FORMAT', FORMAT, STATUS )
      CALL PAR_GET0C( 'TTL', TTL, STATUS )

*  Produce the dump.
      CALL AST_SHOWMESH( THIS, FORMAT, TTL, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTSHOWMESH_ERR', 'Error displaying a Region '//
     :                 'mesh.', STATUS )
      END IF

      END
