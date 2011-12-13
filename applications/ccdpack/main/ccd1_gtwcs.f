      SUBROUTINE CCD1_GTWCS( INDF, IWCS, STATUS )
*+
*  Name:
*     CCD1_GTWCS

*  Purpose:
*     Obtain world coordinate system information from an NDF.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GTWCS( INDF, IWCS, STATUS )

*  Description:
*     This routine returns a frameset describing the WCS information
*     in an NDF.  If there is an error, the value AST__NULL is returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the WCS frameset.  Returns AST__NULL if no
*        WCS frameset can be found, or if STATUS is set on entry, or if
*        some other error occurs.
*     STATUS = INTEGER (Given and returned)
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
*     08-FEB-1999 (MBT):
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
      INTEGER INDF

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Set return value to null pointer, so that if the routine fails to set
*  a non-null value then this will be returned.
      IWCS = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get AST pointer for NDF's WCS component.
      CALL NDF_GTWCS( INDF, IWCS, STATUS )

      END
* $Id$
