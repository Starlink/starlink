      SUBROUTINE ASTZOOMMAP( STATUS )
*+
*  Name:
*     ASTZOOMMAP

*  Purpose:
*     Create a ZoomMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTZOOMMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new ZoomMap and optionally initialises
*     its attributes. A ZoomMap is a linear Mapping which performs a "zoom"
*     transformation by multiplying all coordinate values by the same
*     scale factor (the inverse transformation is performed by
*     dividing by this scale factor). The number of coordinate values
*     representing each point is unchanged.

*  Usage:
*     astzoommap ncoord zoom options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     NCOORD = _INTEGER (Read)
*        The number of coordinate values for the ZoomMap (the same value
*        is used for both input and output axes).
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new ZoomMap.
*     RESULT = LITERAL (Read)
*        A text file to receive the new ZoomMap.
*     ZOOM = _DOUBLE (Read)
*        Initial scale factor by which coordinate values should be
*        multiplied (by the forward transformation) or divided (by the
*        inverse transformation). This factor may subsequently be
*        changed via the ZoomMap's Zoom attribute. It may be positive
*        or negative, but should not be zero.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     12-MAR-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION ZOOM
      INTEGER NCOORD
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the required parameter values.
      CALL PAR_GET0I( 'NCOORD', NCOORD, STATUS )
      CALL PAR_GET0D( 'ZOOM', ZOOM, STATUS )

*  Create the required ZoomMap.
      RESULT = AST_ZOOMMAP( NCOORD, ZOOM, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTZOOMMAP_ERR', 'Error creating a new '//
     :                 'ZoomMap.', STATUS )
      END IF

      END
