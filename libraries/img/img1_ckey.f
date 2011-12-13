      SUBROUTINE IMG1_CKEY( NCARD, BLOCK, N, STATUS )
*+
* Name:
*    IMG1_CKEY

*  Purpose:
*     Counts the number of records in a FITS block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_CKEY( NCARD, BLOCK, N, STATUS )

*  Description:
*     This routine counts the number of valid records in a FITS block.
*     The record organization of the FITS block is taken into account.
*     This is necessary when records may be blank and/or the position of
*     the 'END' keyword isn't known. Blank records are taken to indicate
*     that they have been deleted and should not be counted.

*  Arguments:
*     NCARD = INTEGER (Given)
*        Number of real entries in FITS block (declared size).
*     BLOCK( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS block.
*     N = INTEGER (Returned)
*        The number of valid records.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-AUG-1994 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_ERR'          ! IMG error codes

*  Arguments Given:
      INTEGER NCARD
      CHARACTER * ( * ) BLOCK( NCARD )

*  Arguments Returned:
      INTEGER N

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look at each record and count only non-blank ones until the 'END'
*  keyword is reached.
      N = 0
      DO 1 I = 1, NCARD
         IF ( BLOCK( I ) .NE. ' ' ) THEN
            IF ( BLOCK( I )( 1: 3 ) .NE. 'END' ) THEN
               N = N + 1
            ELSE

*  Do not count anymore.
               GO TO 2
            END IF
         END IF
 1    CONTINUE
 2    CONTINUE
      END
* $Id$
