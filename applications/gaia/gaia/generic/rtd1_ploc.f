      SUBROUTINE RTD1_PLOC( STR1, IRIGHT, STR2, STATUS )
*+
*  Name:
*     RTD1_PLOC

*  Purpose:
*     To place a copy of a string leftwards of a named column.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_PLOC( STR1, IRIGHT, STR2, STATUS )

*  Description:
*     This routine copies STR1 into STR2, starting from the point
*     IRIGHT. STR1 is copied into the region before IRIGHT, ie. leftward
*     from this point. Truncation thus occurs from the left ensuring
*     that the later elements of STR1 are copied into STR2.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The string to be copied.
*     IRIGHT = INTEGER (Given)
*        The starting point from which elements of STR1 are copied into
*        STR2.
*     STR2 = CHARACTER * ( * ) (Given and Returned)
*        Output string containing the copy of STR1, truncated from the
*        left.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-JUL-1994 (PWD):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STR1
      INTEGER IRIGHT

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER J
      INTEGER IRMOST
      INTEGER LENS1

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the IRIGHT value.
      IRMOST = MAX( 1, MIN( IRIGHT, LEN( STR2 ) ) )

*  Find the length of STR1
      LENS1 = LEN( STR1 )

*  Loop from this position to the start of the string copying any
*  character left in STR1.
      J = LENS1
      DO 1 I = IRMOST, 1, -1
         IF ( J .GT. 0 ) THEN
            STR2( I : I ) = STR1( J : J )
         END IF
         J = J - 1
 1    CONTINUE
      END
* @(#)img1_ploc.f   1.3   94/08/15 16:34:07   96/11/22 16:36:28
