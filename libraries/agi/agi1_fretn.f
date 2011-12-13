************************************************************************

      SUBROUTINE AGI_1FRETN ( FRELEN, FREEID, FRELIS, NEXFRE, STATUS )

*+
*  Name:
*     AGI_1FRETN

*  Purpose:
*     Return a member to the free list.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1FRETN ( FRELEN, FREEID, FRELIS, NEXFRE )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Return a member to the free list. The FRELIS and NEXFRE are
*     updated. If the element is already free then an error status is
*     returned. The FRELIS element pointed to by FREEID is filled with
*     NEXFRE, and then FREEID is copied into NEXFRE. For example if the
*     contents were
*     --------------          --------------          -----------------
*     | FREEID | 6 |          | NEXFRE | 3 |          | FRELIS(3) | 9 |
*     --------------          --------------          -----------------
*                                                     .               .
*                                                     ------------------
*                                                     | FRELIS(6) | -2 |
*                                                     ------------------
*     Then the result will be
*                --------------          -----------------
*                | NEXFRE | 6 |          | FRELIS(3) | 9 |
*                --------------          -----------------
*                                        .               .
*                                        -----------------
*                                        | FRELIS(6) | 3 |
*                                        -----------------

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

*     Indicator to free member
      INTEGER FREEID


*  Arguments Given and Returned:
*     Array containing the free list
      INTEGER FRELIS( FRELEN )

*     Pointer indicating next free in free list array
      INTEGER NEXFRE


*  Status:
      INTEGER STATUS

*.


*   Check that the given member is already free
      IF ( FRELIS( FREEID ) .LT. -1 ) THEN

*   Put NEXFRE into the location pointed to by FREEID
         FRELIS( FREEID ) = NEXFRE

*   Update NEXFRE
         NEXFRE = FREEID

      ELSE

*   The element is already free
         STATUS = 2

      ENDIF

      END

