      SUBROUTINE CCD1_LEXT( ARRAY, NX, NY, GETY, EROW, STATUS )
*+
*  Name:
*     CCD1_LEXT

*  Purpose:
*     Extracts a row of values from a list array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LEXT( ARRAY, NX, NY, GETY, EROW, STATUS )

*  Description:
*     This routine extracts a row of data from a double precision
*     array.

*  Arguments:
*     ARRAY( NX, NY ) = DOUBLE PRECISION (Given)
*        Array of data values from which a row of values is to
*        be extracted.
*     NX = INTEGER (Given)
*        First dimension of ARRAY.
*     NY = INTEGER (Given)
*        Second dimension of ARRAY.
*     GETY = INTEGER (Given)
*        The number of the row to extract from ARRAY.
*     EROW( NROW ) = DOUBLE PRECISION (Returned)
*        The extracted row of data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - part of list access routine in CCDPACK.

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
*     22-SEP-1992 (PDRAPER):
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
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION ARRAY( NX, NY )
      INTEGER GETY

*  Arguments Returned:
      DOUBLE PRECISION EROW( NX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that GETCOL is in permitted range.
      IF ( GETY .GE. 1 .AND. GETY .LE. NY ) THEN

*  Loop and extract the data.
         DO 1 I = 1, NX
            EROW( I ) = ARRAY( I, GETY )
 1       CONTINUE
      ELSE

*  Report an error and exit.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_LEXT1',
     :   '  CCD1_LEXT: Cannot extract selected column of data.'//
     :   ' The given value lies outside of the array bounds', STATUS )
      END IF

      END
* $Id$
