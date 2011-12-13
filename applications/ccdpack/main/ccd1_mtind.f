      SUBROUTINE CCD1_MTIND( INVEC, NVEC, INDEXS, NIND, OUTVEC, STATUS )
*+
*  Name:
*     CCD1_MTIND

*  Purpose:
*     Reorders an array using a list of indexs into the input array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MTIND( INVEC, NVEC, INDEXS, NIND, OUTVEC, STATUS )

*  Description:
*     This routine uses a list of indices to re-order an array. The
*     indices are the positions within the input vector of the sequence
*     of output positions.

*  Arguments:
*     INVEC( NVEC ) = DOUBLE PRECISION (Given)
*        Array of values to be re-arranged.
*     NVEC = INTEGER (Given)
*        Number of values in INVEC.
*     INDEXS( NIND ) = INTEGER (Given)
*        The indices at which the output vectors values lie in the input
*        vector.
*     NIND = INTEGER (Given)
*        Number of values in INDEXS.
*     OUTVEC( NIND ) = DOUBLE PRECISION (Returned)
*        The rearranged data values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*     Notes:
*     -  Part of the list access routines in CCDPACK.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NVEC
      DOUBLE PRECISION INVEC( NVEC )
      INTEGER NIND
      INTEGER INDEXS( NIND )

*  Arguments Returned:
      DOUBLE PRECISION OUTVEC( NIND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over the indexs extracting the corresponding value from the
*  input array and placing it in the output array.
      DO 1 I = 1, NIND
         OUTVEC( I ) = INVEC( INDEXS( I ) )
 1    CONTINUE

      END
* $Id$
