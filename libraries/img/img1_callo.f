      SUBROUTINE IMG1_CALLO( SIZE, QUAN, POINT, STATUS )
*+
*  Name:
*     IMG1_CALLO

*  Purpose:
*     Allocate character virtual memory (UNIX version).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_CALLO( SIZE, QUAN, POINT, STATUS )

*  Description:
*     This routine allocates arbitrary amounts of virtual memory for
*     character strings. It should be used as this allows encapsulation
*     of any system dependent problems with this process within this
*     routine. The inverse process of freeing memory should also be
*     performed with IMG1_CFREE and memory extension by IMG1_CREAL.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the character strings which are to be allocated
*        (declared size of strings).
*     QUAN = INTEGER (Given)
*        The number of character strings of SIZE which are to be
*        allocated (array dimension).
*     POINT = INTEGER (Returned)
*        The pointer to the virtual memory.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - UNIX specific. Does very little except call PSX routine.
*
*     - On UNIX systems the size of the character strings is passed
*     after the last real argument using the %VAL() mechanism, e.g.
*
*        call img1_callo( size, quan, ip, status )
*        call mysub( %val( ip ), quan, %val( size ) )
*        .
*        .
*        end
*
*        subroutine mysub( carray, size )
*
*        character * ( * ) carray( size )
*        .
*        .
*        end

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
*     PDRAPER: Peter Draper (Durham University - Starlink)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_ERR'          ! IMG error codes
      INCLUDE 'IMG_CONST'        ! IMG internal constants

*  Arguments Given:
      INTEGER SIZE
      INTEGER QUAN

*  Arguments Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NREQD              ! Number of byte-size elements actually
                                 ! required
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try allocating the required amount of memory.
      NREQD = QUAN * SIZE
      IF ( NREQD .GT. 0 ) THEN

*  Attempt to allocate memory.
         CALL PSX_MALLOC( NREQD, POINT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Failed to access the memory - set status and exit.
            CALL MSG_SETI( 'NREQD', NREQD )
            CALL ERR_REP( 'IMG1_MALLO_FAIL', 'IMG1_MALLO: ' //
     :           'Could not allocate ^NREQD bytes of memory.' , STATUS )

*  Set pointer to NULL.
            POINT = IMG__NOPTR
         END IF
      ELSE

*  Invalid input value - set status and exit.
         STATUS = IMG__FATIN
         CALL ERR_REP( 'IMG1_MALLO_BAD',
     :   'IMG1_MALLO: Requested memory less than 1 byte (possible ' //
     :        'programming error).', STATUS )
      END IF
      END
* $Id$
