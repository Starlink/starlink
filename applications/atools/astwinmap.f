      SUBROUTINE ASTWINMAP( STATUS )
*+
*  Name:
*     ASTWINMAP

*  Purpose:
*     Create a WinMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTWINMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new WinMap and optionally initialises
*     its attributes. A Winmap is a linear Mapping which transforms a
*     rectangular window in one coordinate system into a similar window
*     in another coordinate system by scaling and shifting each axis (the
*     window edges being parallel to the coordinate axes).
*
*     A WinMap is specified by giving the coordinates of two opposite
*     corners (A and B) of the window in both the input and output
*     coordinate systems.

*  Usage:
*     astwinmap ncoord ina inb outa outb options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     INA() = _DOUBLE (Read)
*        The coordinates of corner A of the window in the input
*        coordinate system.
*     INB() = _DOUBLE (Read)
*        The coordinates of corner B of the window in the input
*        coordinate system.
*     NCOORD = _INTEGER (Read)
*        The number of coordinate values for the WinMap (the same value
*        is used for both input and output axes).
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new WinMap.
*     OUTA() = _DOUBLE (Read)
*        The coordinates of corner A of the window in the output
*        coordinate system.
*     OUTB() = _DOUBLE (Read)
*        The coordinates of corner B of the window in the output
*        coordinate system.
*     RESULT = LITERAL (Read)
*        A text file to receive the new WinMap.

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
*     14-FEB-2001 (DSB):
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
      DOUBLE PRECISION INA( NDF__MXDIM )
      DOUBLE PRECISION INB( NDF__MXDIM )
      DOUBLE PRECISION OUTA( NDF__MXDIM )
      DOUBLE PRECISION OUTB( NDF__MXDIM )
      INTEGER NCOORD
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the required parameter values.
      CALL PAR_GET0I( 'NCOORD', NCOORD, STATUS )
      CALL PAR_EXACD( 'INA', NCOORD, INA, STATUS )
      CALL PAR_EXACD( 'INB', NCOORD, INB, STATUS )
      CALL PAR_EXACD( 'OUTA', NCOORD, OUTA, STATUS )
      CALL PAR_EXACD( 'OUTB', NCOORD, OUTB, STATUS )

*  Create the required WinMap.
      RESULT = AST_WINMAP( NCOORD, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTWINMAP_ERR', 'Error creating a new '//
     :                 'WinMap.', STATUS )
      END IF

      END
