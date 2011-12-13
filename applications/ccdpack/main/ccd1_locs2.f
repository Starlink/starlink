      SUBROUTINE CCD1_LOCS2( LIST, NCOL, NROW, CCOL, CHECK, VALUE,
     :                       LOCAT, NLOC, STATUS )
*+
*  Name:
*     CCD1_LOCS2

*  Purpose:
*     Locates the occurances of a string in a 2D list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LOCS2( LIST, NCOL, NROW, CCOL, CHECK, VALUE,
*                      LOCAT, NLOC, STATUS )

*  Description:
*     The routine looks at the values in the 2D character dimension
*     LIST( NCOL, NROW ) in column CCOL. It only looks in those rows
*     which are flagged as true in the CHECK array. If it locates a
*     matching value the row number is entered into LOCAT and NCOL is
*     incremented by one. So on exit a list of pointers to the
*     locations of a specified string in a given column is output.

*  Arguments:
*     LIST( NCOL, NROW ) = CHARACTER * ( * ) (Given)
*        Array of character values which are too be checked for
*        occurances of the string VALUE.
*     NCOL = INTEGER (Given)
*        First dimension of LIST.
*     NROW = INTEGER (Given)
*        Second dimension of LIST.
*     CCOL = INTEGER (Given)
*        Column number to look for values in.
*     CHECK( NROW ) = LOGICAL (Given)
*        Logical mask of rows to be checked for string occurance.
*     VALUE = CHARACTER * ( * ) (Given)
*        The value which is to be located.
*     LOCAT( NROW ) = INTEGER (Returned)
*        List of the row numbers which contain VALUE.
*     NLOC = INTEGER (Returned)
*        Number of occurances of VALUE located.
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
*     21-FEB-1992 (PDRAPER):
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
      INTEGER NCOL
      INTEGER NROW
      CHARACTER * ( * ) LIST( NCOL, NROW )
      INTEGER CCOL
      LOGICAL CHECK( NROW )
      CHARACTER * ( * ) VALUE

*  Arguments Returned:
      INTEGER LOCAT( NROW )
      INTEGER NLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      LOGICAL GOTONE             ! FLag for locating occurence of VALUE
      INTEGER CLEN               ! Maximum length of strings which may
                                 ! be usefully compared.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get lengths of input strings.
      CLEN  = MIN( LEN( VALUE ), LEN( LIST( 1, CCOL ) ) )

*  Set number of occurances.
      NLOC = 0

*  Loop of all rows of LIST.
      DO 1 I = 1, NROW

*  Initialise pointer list.
         LOCAT( I ) = 0

*  Do we need to check this one.
         IF ( CHECK ( I ) ) THEN
            GOTONE = VALUE( 1 : CLEN ) .EQ. LIST( CCOL, I )( 1 : CLEN )
            IF ( GOTONE ) THEN

*  Increment counter and enter pointer.
               NLOC = NLOC + 1
               LOCAT( NLOC ) = I
            END IF
         END IF
 1    CONTINUE

      END
* $Id$
