      SUBROUTINE RTD1_SETAR( START, STEP, SIZE, ARRAY, STATUS )
*+
*  Name:
*     RTD1_SETAR

*  Purpose:
*     Sets the values of an array to an incremented range of values.

*  Language:
*     Starlink Fortran-77.

*  Invocation:
*     CALL RTD1_SETAR( START, STEP, SIZE, ARRAY, STATUS )

*  Description:
*     This routine fills an array with values starting at START and
*     incremented between each element by STEP. It's really just
*     a convience for setting dynamically allocated arrays.
*
*     The result is:
*        ARRAY( I ) = START + ( I - 1 ) * STEP

*  Arguments:
*     START = REAL (Given)
*        Value of first element in ARRAY.
*     STEP = REAL (Given)
*        Increment for each pixel.
*     SIZE = INTEGER (Given)
*        Size of the array ARRAY.
*     ARRAY( SIZE ) = REAL (Returned)
*        Array with all elements set up with incremented values.
*     STATUS = INTEGER ({status_access_mode})
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
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     14-MAR-1996 (PWD):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      REAL START
      REAL STEP
      INTEGER SIZE

*  Arguments Returned:
      REAL ARRAY( SIZE )

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER I                 ! Loop variable
      REAL CURVAL               ! Current array value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      CURVAL = START
      DO 1 I = 1, SIZE
         ARRAY( I ) = CURVAL
         CURVAL = CURVAL + STEP
 1    CONTINUE
      END
