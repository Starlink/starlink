      SUBROUTINE CCD1_LNMAP( TR, MAP, STATUS )
*+
*  Name:
*     CCD1_LNMAP

*  Purpose:
*     Generate a linear AST mapping.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_LNMAP( TR, MAP, STATUS )

*  Description:
*     Generates a linear two-dimensional AST mapping from the six
*     parameters which specify it.  This is just a harness for a set
*     of AST calls, useful because it is a bit messy to generate a
*     linear map directly.

*  Arguments:
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        Six parameters of the linear mapping:
*           X' = TR( 1 ) + X * TR( 2 ) + Y * TR( 3 )
*           Y' = TR( 4 ) + X * TR( 5 ) + Y * TR( 6 )
*     MAP = INTEGER (Returned)
*        An AST pointer to the requested mapping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     22-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants

*  Arguments Given:
      DOUBLE PRECISION TR( 6 )

*  Arguments Returned:
      INTEGER MAP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION MATRIX( 4 ) ! Transformation matrix
      DOUBLE PRECISION PIA( 2 )  ! First point initial position for WinMap defn
      DOUBLE PRECISION PIB( 2 )  ! Second point initial position for WinMap defn
      DOUBLE PRECISION POA( 2 )  ! First point final position for WinMap defn
      DOUBLE PRECISION POB( 2 )  ! Second point final position for WinMap defn
      INTEGER MAPMAT
      INTEGER MAPWIN
      INTEGER MAPCMP

*  Local Data:
      DATA PIA/ 0D0, 0D0 /
      DATA PIB/ 1D0, 1D0 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Generate a mapping representing the non-translational parts of the
*  linear transformation.
      MATRIX( 1 ) = TR( 2 )
      MATRIX( 2 ) = TR( 3 )
      MATRIX( 3 ) = TR( 5 )
      MATRIX( 4 ) = TR( 6 )
      MAPMAT = AST_MATRIXMAP( 2, 2, 0, MATRIX, ' ', STATUS )

*  Generate a mapping representing the translational parts of the
*  linear transformation.
      POA( 1 ) = TR( 1 )
      POA( 2 ) = TR( 4 )
      POB( 1 ) = TR( 1 ) + 1D0
      POB( 2 ) = TR( 4 ) + 1D0
      MAPWIN = AST_WINMAP( 2, PIA, PIB, POA, POB, ' ', STATUS )

*  Combine the translational and non-translational mappings to generate
*  a single one which represents the whole linear transformation.
      MAPCMP = AST_CMPMAP( MAPMAT, MAPWIN, .TRUE., ' ', STATUS )

*  Have a go at simplifying the combined mapping (one or other of the
*  translational or non-translational parts may be Unit).
      MAP = AST_SIMPLIFY( MAPCMP, STATUS )

*  Tidy up.
      CALL AST_ANNUL( MAPMAT, STATUS )
      CALL AST_ANNUL( MAPWIN, STATUS )
      CALL AST_ANNUL( MAPCMP, STATUS )

*  Check that a valid mapping has been obtained.
      IF ( MAP .EQ. AST__NULL ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_LNMAP_NOMAP',
     :   'Failed to obtain a mapping from linear coefficients', STATUS )
      END IF

      END
* $Id$
