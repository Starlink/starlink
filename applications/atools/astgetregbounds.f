      SUBROUTINE ASTGETREGBOUNDS( STATUS )
*+
*  Name:
*     ASTGETREGBOUNDS

*  Purpose:
*     Returns the bounding box of Region.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTGETREGBOUNDS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns the upper and lower limits of a box which
*     just encompasses the supplied Region. The limits are returned as axis
*     values within the Frame represented by the Region. The value of the
*     Negated  attribute is ignored (i.e. it is assumed that the Region has not
*     been negated).

*     The corresponding AST function is AST_GETREGIONBOUDS, but the name
*     has been contracted to "astgetregbounds" for the purposes of this
*     ATOOLS command in order not to exceed the allowed length of HDS
*     component names.

*  Usage:
*     ast_getregbounds this

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     LBND() = _DOUBLE (Write)
*        An array in which to return the lower axis bounds covered by the
*        Region. It should have at least as many elements as there are axes
*        in the Region. If an axis has no lower limit, the returned value
*        will be the largest possible negative value.
*     THIS = LITERAL (Read)
*        A text file holding the Region.
*     UBND() = _DOUBLE (Write)
*        An array in which to return the upper axis bounds covered by the
*        Region. It should have at least as many elements as there are axes
*        in the Region. If an axis has no upper limit, the returned value
*        will be the largest possible positive value.

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
*     28-MAY-3007 (DSB):
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

*  External References:
      EXTERNAL AST_ISAREGION

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS
      INTEGER NAXES, I
      DOUBLE PRECISION XL( NDF__MXDIM ), XU( NDF__MXDIM )
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Region.
      CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS,
     :                 STATUS )

*  Find the bounding box.
      CALL AST_GETREGIONBOUNDS( THIS, XL, XU, STATUS )

*  Determine the Naxes attribute of the Region
      NAXES = AST_GETI( THIS, 'Naxes', STATUS)

*  Display the results.
      CALL MSG_BLANK( STATUS )
      DO I = 1, NAXES
         CALL MSG_SETD( 'XL', XL( I ) )
         IF( I .NE. NAXES ) CALL MSG_SETC( 'XL', ',' )
      END DO

      CALL MSG_OUT( ' ', 'Region lower bounds: (^XL).', STATUS )
      CALL PAR_PUT1D( 'LBND', NAXES, XL, STATUS )

      DO I = 1, NAXES
         CALL MSG_SETD( 'XU', XU( I ) )
         IF( I .NE. NAXES ) CALL MSG_SETC( 'XU', ',' )
      END DO

      CALL MSG_OUT( ' ', 'Region upper bounds: (^XU).', STATUS )
      CALL PAR_PUT1D( 'UBND', NAXES, XU, STATUS )

      CALL MSG_BLANK( STATUS )

*  Tidy up.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error finding a bounding box for a '//
     :                 'Region.', STATUS )
      END IF

      END
