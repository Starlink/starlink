      SUBROUTINE CCD1_XDAT( IN, NREC, NVAL, OUT, STATUS )
*+
*  Name:
*     CCD1_XDAT

*  Purpose:
*     Extracts extra data values from a list array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_XDAT( IN, NREC, NVAL, OUT, STATUS )

*  Description:
*     This routine extracts data from an input array and writes it
*     to an output array, discarding the first two columns and
*     transposing it.

*  Arguments:
*     IN( NREC, NVAL ) = DOUBLE PRECISION (Given)
*        Array of data values from which columns are to be extracted.
*     NREC = INTEGER (Given)
*        The number of records.
*     NVAL = INTEGER (Given)
*        The number of values in each record.
*     OUT( NVAL - 2, NREC ) = DOUBLE PRECISION (Returned)
*        The output array, containing a transposed copy of the input
*        array with the first two rows missing.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - part of list access routine in CCDPACK.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-MAY-2001 (MBT):
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
      INTEGER NREC
      INTEGER NVAL
      DOUBLE PRECISION IN( NREC, NVAL )

*  Arguments Returned:
      DOUBLE PRECISION OUT( NVAL - 2, NREC )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do the copy.
      DO I = 3, NVAL
         DO J = 1, NREC
            OUT( I - 2, J ) = IN( J, I )
         END DO
      END DO

      END
* $Id$
