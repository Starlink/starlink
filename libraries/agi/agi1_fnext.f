************************************************************************

      SUBROUTINE AGI_1FNEXT ( FRELEN, FRELIS, NEXFRE, FREEID, STATUS )

*+
*  Name:
*     AGI_1FNEXT

*  Purpose:
*     Get next member from free list.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, FREEID, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Get the next member from the free list. If there are no more
*     free then return an error in STATUS. The next free element in
*     the free list is copied from NEXFRE into FREEID and NEXFRE is
*     updated by copying the contents of the element of FRELIS pointed
*     to by NEXFRE. For example if the contents were
*                --------------          -----------------
*                | NEXFRE | 6 |          | FRELIS(3) | 9 |
*                --------------          -----------------
*                                        .               .
*                                        -----------------
*                                        | FRELIS(6) | 3 |
*                                        -----------------
*     Then the result will be
*     --------------          --------------          -----------------
*     | FREEID | 6 |          | NEXFRE | 3 |          | FRELIS(3) | 9 |
*     --------------          --------------          -----------------
*                                                     .               .
*                                                     ------------------
*                                                     | FRELIS(6) | -2 |
*                                                     ------------------

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     Aug 1988
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Arguments Given:
*     Length of the array containing the free list.
      INTEGER FRELEN


*  Arguments Given and Returned:
*     Array containing the free list
      INTEGER FRELIS( FRELEN )

*     Pointer indicating next free in free list array
      INTEGER NEXFRE


*  Arguments Returned:
*     Indicator to free member
      INTEGER FREEID


*  Status:
      INTEGER STATUS

*.


*   If there are no more free members then return an error status
      IF ( NEXFRE .LT. 0 ) THEN
         STATUS = 1

*   Else get the next free member
      ELSE
         FREEID = NEXFRE
         NEXFRE = FRELIS( NEXFRE )

*   Put a -2 into the old place. This is not strictly neccessary for
*   correct operation, but it allows the release routine to check that
*   the item has been previously given away.
         FRELIS( FREEID ) = -2

      ENDIF

      END

