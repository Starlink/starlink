      SUBROUTINE KPG1_CPSTY( IPLOT, IN, OUT, ATTRS, STATUS )
*+
*  Name:
*     KPG1_CPSTY

*  Purpose:
*     Copies the plotting style for an AST element to another AST
*     element.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CPSTY( IPLOT, IN, OUT, ATTRS, STATUS )

*  Description:
*     This routine copies the AST attributes which determine the colour,
*     style, width, font and size. If IN is non-blank, then the
*     attributes are copied from the element given by IN, to the
*     element given by OUT--the original values of these attributes for
*     element OUT are returned in ATTRS. If IN is blank, then the
*     values supplied in ATTRS are copied to the element given by
*     OUT-- the original values of these attributes for element OUT are
*     again returned in ATTRS.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        The AST Plot.
*     IN = CHARACTER * ( * ) (Returned)
*        The name of the source element (e.g. "CURVES", "AXIS1"), or
*        blank.
*     OUT = CHARACTER * ( * ) (Returned)
*        The name of the destination element (e.g. "CURVES", "AXIS1").
*     ATTRS( 5 ) = REAL (Given and Returned)
*        The entry values are ignored unless IN is blank, in which case
*        the entry values should be the colour, width, style, size and font
*        attribute values to associate with element OUT. On exit, the
*        original colour, width, style, size and font attribute values
*        associated with element OUT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-AUG-2002 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST functions and parameter values

*  Arguments Given:
      INTEGER IPLOT
      CHARACTER IN*(*)
      CHARACTER OUT*(*)

*  Arguments Given and Returned:
      REAL ATTRS( 5 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FULLATT*40       ! Fully qualified attribute name
      CHARACTER ATROOT( 5 )*7    ! Root name of attribute
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of string
      REAL NEWVAL                ! New attribute value

      DATA ATROOT / 'Colour(', 'Width(', 'Style(', 'Size(', 'Font(' /

*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Do each attribute in turn.
      DO I = 1, 5

*  Get the new value.
         IF( IN .EQ. ' ' ) THEN
            NEWVAL = ATTRS( I )
         ELSE
            IAT = 0
            CALL CHR_APPND( ATROOT( I ), FULLATT, IAT )
            CALL CHR_APPND( IN, FULLATT, IAT )
            CALL CHR_APPND( ')', FULLATT, IAT )
            NEWVAL = AST_GETR( IPLOT, FULLATT( : IAT ), STATUS )
         END IF

*  Get the original value of element OUT.
         IAT = 0
         CALL CHR_APPND( ATROOT( I ), FULLATT, IAT )
         CALL CHR_APPND( OUT, FULLATT, IAT )
         CALL CHR_APPND( ')', FULLATT, IAT )

         ATTRS( I ) = AST_GETR( IPLOT, FULLATT( : IAT ), STATUS )

* Store the new value
         CALL AST_SETR( IPLOT, FULLATT( : IAT ), NEWVAL, STATUS )

      END DO

      END
