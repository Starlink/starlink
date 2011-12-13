      SUBROUTINE IMG1_CREAL( SIZE, QUAN, POINT, STATUS )
*+
*  Name:
*     IMG1_CALLO

*  Purpose:
*     Reallocate character virtual memory (UNIX version).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_CREAL( SIZE, QUAN, POINT, STATUS )

*  Description:
*     This routine changes the size of a virtual memory character array
*     created by the routine IMG1_CALLO. The new size (dimension) of the
*     character array is QUAN. The original length of the elemental
*     strings should be given as SIZE, although this may also be
*     changed.
*
*     The contents of the strings are unchanged up to the memory address
*     which points to either the end of the adjusted array or the end of
*     the current array, whichever is the smaller. Note that if the
*     string size is changed then this will probably scramble any
*     information.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the character strings which were/are to be
*        allocated (declared size of strings). This may be different to
*        the original string size.
*     QUAN = INTEGER (Given)
*        The number of character strings of SIZE which are to be
*        allocated (array dimension). This is the whole size of the
*        character array.
*     POINT = INTEGER (Given and Returned)
*        The pointer to the original virtual memory on entry. Points to
*        the new memory on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - UNIX specific. Does very little except call PSX routine.
*
*     - On UNIX systems the size of the character strings is passed
*     after the last real argument using the %VAL() mechanism, e.g.
*
*        call img1_creal( size, quan, ip, status )
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
      INCLUDE 'IMG_ERR'          ! IMG error constants
      INCLUDE 'IMG_CONST'        ! IMG internal constants

*  Arguments Given:
      INTEGER SIZE
      INTEGER QUAN

*  Arguments Given and Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NREQD              ! Number of byte-size elements actually
                                 ! required
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Estimate the quantity of memory required.
      NREQD = QUAN * SIZE
      IF ( NREQD .GT. 0 ) THEN

*  Attempt to re-allocate memory.
         CALL PSX_REALLOC( NREQD, POINT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Failed to access the memory - set status and exit.
            CALL MSG_SETI( 'NREQD', NREQD )
            CALL ERR_REP( 'IMG1_CREAL_FAIL', 'IMG1_CREAL: Could not ' //
     :           're-allocate ^NREQD bytes of memory.' , STATUS )

*  Set pointer to NULL.
            POINT = IMG__NOPTR
         END IF
      ELSE

*  Invalid input value - set status and exit.
         STATUS = IMG__FATIN
         CALL ERR_REP( 'IMG1_CREAL_BAD',
     :   'IMG1_CREAL: Requested memory less than 1 byte  ' //
     :        '(possible programming error).', STATUS )
      END IF
      END
* $Id$
