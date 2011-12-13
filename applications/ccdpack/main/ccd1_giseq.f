      SUBROUTINE CCD1_GISEQ( FIRST, INCRE, NELEM, ARRAY, STATUS )
*+
*  Name:
*     CCD1_GISEQ

*  Purpose:
*     Generates a sequential array of integer values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GISEQ( FIRST, INCRE, NELEM, ARRAY, STATUS )

*  Description:
*     This routine creates a sequence of integer values which in writes
*     into the array ARRAY. The number of elements in the array is
*     determined by the NELEM argument. The sequence of numbers written
*     into the array is determined by the FIRST and INCRE
*     arguments. Basically the first value written into the array is
*     FIRST, the second FIRST+INCRE etc. until the array is filled.

*  Arguments:
*     FIRST = INTEGER (Given)
*        The starting value of the sequence to be written into the
*        array.
*     INCRE = INTEGER (Given)
*        The increment between successive values which are written into
*        the array.
*     NELEM = INTEGER (Given)
*        The actual number of elements in ARRAY.
*     ARRAY( NELEM ) = INTEGER (Returned)
*        On output this array contains the sequence of integers:
*        FIRST, FIRST+INCRE, FIRST+2*INCRE .... FIRST+(NELEM-1)*INCRE
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     5-OCT-1992 (PDRAPER):
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
      INTEGER FIRST
      INTEGER INCRE
      INTEGER NELEM

*  Arguments Returned:
      INTEGER ARRAY( NELEM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER  I                 ! Loop variable
      INTEGER VALUE              ! Current value of array element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the first value to the array.
      VALUE = FIRST
      ARRAY( 1 ) = VALUE
      IF ( NELEM .GT. 1 ) THEN

*  Loop over the rest of the array.
         DO 1 I = 2, NELEM

*  Add INCRE to the present value.
            VALUE = VALUE + INCRE

*  Enter into array.
            ARRAY( I ) = VALUE
 1       CONTINUE
      END IF

      END
* $Id$
