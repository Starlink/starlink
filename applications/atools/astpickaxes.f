      SUBROUTINE ASTPICKAXES( STATUS )
*+
*  Name:
*     ASTPICKAXES

*  Purpose:
*     Create a new Frame by picking axes from an existing one.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTPICKAXES( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new Frame whose axes are copied from an
*     existing Frame along with other Frame attributes, such as its
*     Title. Any number (zero or more) of the original Frame's axes
*     may be copied, in any order, and additional axes with default
*     attributes may also be included in the new Frame.
*
*     A Mapping that converts between the coordinate systems described
*     by the two Frames may also be created.

*  Usage:
*     astpickaxes this axes map result

*  ADAM Parameters:
*     AXES() = _INTEGER (Read)
*        A vector of integers which lists the axes to be copied. These
*        should be given in the order required in the new Frame, using
*        the axis numbering in the original Frame (which starts at 1
*        for the first axis). Axes may be selected in any order, but
*        each may only be used once.  If additional (default) axes are
*        also to be included, the corresponding elements of this array
*        should be set to zero.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     MAP = LITERAL (Read)
*        A text file to receive a new Mapping. This will be a PermMap
*        (or a UnitMap as a special case) that describes the axis
*        permutation that has taken place between the original and new
*        Frames. The Mapping's forward transformation will convert
*        coordinates from the original Frame into the new one, and vice
*        versa. If this Mapping is not required, a null (!) value may be
*        supplied for this parameter. [!]
*     RESULT = LITERAL (Read)
*        A text file to receive the new Frame.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the original FrameSet to which a
*        new Frame is to be added. If an NDF is supplied, the current
*        Frame of the WCS FrameSet will be used.

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
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAFRAME

*  Local Variables:
      INTEGER AXES( NDF__MXDIM )
      INTEGER MAP
      INTEGER NAXES
      INTEGER RESULT
      INTEGER THIS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a Frame.
      CALL KPG1_GTOBJ( 'THIS', 'Frame or FrameSet', AST_ISAFRAME, THIS,
     :                 STATUS )

*  Get the axis indices for the new frame
      CALL PAR_GET1I( 'AXES', NDF__MXDIM, AXES, NAXES, STATUS )

*  Create the new Frame.
      RESULT = AST_PICKAXES( THIS, NAXES, AXES, MAP, STATUS )

*  If succesful, write the Mapping out if required.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL ATL1_PTOBJ( 'MAP', ' ', MAP, STATUS )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

*  Write the new Frame out.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTPICKAXES_ERR', 'Error adding a Frame into '//
     :                 'a FrameSet.', STATUS )
      END IF

      END
