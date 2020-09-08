      SUBROUTINE ASTGETREGDISC( STATUS )
*+
*  Name:
*     ASTGETREGDISC

*  Purpose:
*     Returns  the centre and radius of a disc containing a 2D Region.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTGETREGDISC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns the centre and radius of a disc that just
*     encloses the supplied 2-dimensional Region. The centre is returned
*     as a pair of axis values within the Frame represented by the Region.
*     The value of the Negated attribute is ignored (i.e. it is assumed
*     that the Region has not been negated).
*
*     The corresponding AST function is AST_GETREGIONDISC, but the name
*     has been contracted to "astgetregdisc" for the purposes of this
*     ATOOLS command in order not to exceed the allowed length of HDS
*     component names.

*  Usage:
*     astgetregdisc this

*  ADAM Parameters:
*     CENTRE(2) = _DOUBLE (Write)
*        An array in which to return the axis values at the centre of the
*        bounding disc.
*     RADIUS = DOUBLE (Write)
*        The radius of the bounding disc, as a geodesic distance within the
*        Frame represented by the Region. It will be returned holding
*        AST__BAD If the Region is unbounded.
*     THIS = LITERAL (Read)
*        A text file holding the Region.

*  Notes:
*    - An error is reported if the Region is not 2-dimensional.
*    - The value of the Negated attribute is ignored (i.e. it is assumed
*    that the Region has not been negated).
*    - If the Region is unbounded, the radius will be returned set to
*    AST__BAD and the supplied centre axis values will be returned unchanged.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     8-SEP-2020 (DSB):
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

*  External References:
      EXTERNAL AST_ISAREGION

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS
      DOUBLE PRECISION CENTRE( 2 ), RADIUS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Region.
      CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS,
     :                 STATUS )

*  Find the bounding disc.
      CALL AST_GETREGIONDISC( THIS, CENTRE, RADIUS, STATUS )

*  Display the results.
      CALL MSG_BLANK( STATUS )
      IF( RADIUS .NE. AST__BAD ) THEN
         CALL MSG_SETD( 'CX', CENTRE( 1 ) )
         CALL MSG_SETD( 'CY', CENTRE( 2 ) )
         CALL MSG_OUT( ' ', 'Centre of bounding disc: (^CX,^CY)',
     :                 STATUS )
         CALL MSG_SETD( 'R', RADIUS )
         CALL MSG_OUT( ' ', 'Radius of bounding disc: ^R', STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'Centre of bounding disc: (<bad>,<bad>)',
     :                 STATUS )
         CALL MSG_OUT( ' ', 'Radius of bounding disc: <bad>', STATUS )
         CENTRE( 1 ) = AST__BAD
         CENTRE( 2 ) = AST__BAD
      END IF
      CALL MSG_BLANK( STATUS )

*  Store the results in output parameters.
      CALL PAR_PUT1D( 'CENTRE', 2, CENTRE, STATUS )
      CALL PAR_PUT0D( 'RADIUS', RADIUS, STATUS )

*  Tidy up.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error finding a bounding disc for a '//
     :                 'Region.', STATUS )
      END IF

      END
