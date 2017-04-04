      SUBROUTINE ASTCHEBYDOMAIN( STATUS )
*+
*  Name:
*     ASTCHEBYDOMAIN

*  Purpose:
*     Returns the bounding box of the domain of a ChebyMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCHEBYDOMAIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns the upper and lower limits of the box
*     defining the domain of either the forward or inverse transformation
*     of a ChebyMap. These are the values that were supplied when the
*     ChebyMap was created.

*  Usage:
*     astchebydomain this forward

*  ADAM Parameters:
*     FORWARD = _LOGICAL (Read)
*        A TRUE value indicates that the domain of the ChebyMap's
*        forward transformation is to be returned, while a FALSE
*        value indicates that the domain of the inverse transformation
*        should be returned.
*     LBND() = _DOUBLE (Write)
*        An array in which to return the lower axis bounds of the ChebyMap
*        domain. The number of elements should be at least equal to the
*        number of ChebyMap inputs (if FORWARD is .TRUE.), or outputs
*        (if FORWARD is .FALSE.).
*     THIS = LITERAL (Read)
*        A text file holding the Region.
*     UBND() = _DOUBLE (Write)
*        An array in which to return the upper axis bounds of the ChebyMap
*        domain. The number of elements should be at least equal to the
*        number of ChebyMap inputs (if FORWARD is .TRUE.), or outputs
*        (if FORWARD is .FALSE.).

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
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
*     4-MAY-2017 (DSB):
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
      EXTERNAL AST_ISACHEBYMAP

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS
      INTEGER NAXES, I
      LOGICAL FORWRD
      DOUBLE PRECISION XL( NDF__MXDIM ), XU( NDF__MXDIM )
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the ChebyMap
      CALL KPG1_GTOBJ( 'THIS', 'ChebyMap', AST_ISACHEBYMAP, THIS,
     :                 STATUS )

*  Get the FORWARD parameter.
      CALL PAR_GET0L( 'FORWARD', FORWRD, STATUS )

*  Find the bounding box.
      CALL AST_CHEBYDOMAIN( THIS, FORWRD, XL, XU, STATUS )

*  Determine the number of input or output axes.
      IF( FORWRD ) THEN
         NAXES = AST_GETI( THIS, 'Nin', STATUS )
      ELSE
         NAXES = AST_GETI( THIS, 'Nout', STATUS )
      END IF

*  Display the results.
      CALL MSG_BLANK( STATUS )
      DO I = 1, NAXES
         CALL MSG_SETD( 'XL', XL( I ) )
         IF( I .NE. NAXES ) CALL MSG_SETC( 'XL', ',' )
      END DO

      CALL MSG_OUT( ' ', 'Lower bounds: (^XL).', STATUS )
      CALL PAR_PUT1D( 'LBND', NAXES, XL, STATUS )

      DO I = 1, NAXES
         CALL MSG_SETD( 'XU', XU( I ) )
         IF( I .NE. NAXES ) CALL MSG_SETC( 'XU', ',' )
      END DO

      CALL MSG_OUT( ' ', 'Upper bounds: (^XU).', STATUS )
      CALL PAR_PUT1D( 'UBND', NAXES, XU, STATUS )

      CALL MSG_BLANK( STATUS )

*  Tidy up.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error finding the domain of a '//
     :                 'ChebyMap.', STATUS )
      END IF

      END
