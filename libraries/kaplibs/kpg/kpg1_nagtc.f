      SUBROUTINE KPG1_NAGTC( CVALUE, LOC, NDIM, DIM, STATUS )
*+
*  Name:
*     KPG1_NAGTC

*  Purpose:
*     Swaps argument order when getting a mapped character array from
*     an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NAGTC( CVALUE, LOC, NDIM, DIM, STATUS )

*  Description:
*     This is just a dummy routine to swap the argument order when
*     obtaining the value of a mapped character array from an HDS
*     object.  It is needed for Unix systems.

*  Arguments:
*     CVALUE = CHARACTER * ( * ) (Returned)
*        The character value.
*     LOC = CHARACTER * (DAT__SZLOC) (Given)
*        The locator of the object whose value is required.
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     DIM( * ) = INTEGER (Given)
*        The dimensions of the character object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1992 (RFWS):
*        Original version.
*     1992 May 9 (MJC):
*        Renamed from NATGTC and added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LOC
      INTEGER NDIM
      INTEGER DIM( * )

*  Arguments Returned:
      CHARACTER * ( * ) CVALUE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the character value.
      CALL DAT_GETC( LOC, NDIM, DIM, CVALUE, STATUS )

      END
