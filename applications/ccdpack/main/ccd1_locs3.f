      SUBROUTINE CCD1_LOCS3( LIST, NCOL, NROW, CCOL1, CCOL2, CHECK,
     :                       VALUE1, VALUE2, LOCAT, NLOC, STATUS )
*+
*  Name:
*     CCD1_LOCS3

*  Purpose:
*     Locates the occurances of two strings in a 2D list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LOCS3( LIST, NCOL, NROW, CCOL1, CCOL2, CHECK, VALUE1,
*                      VALUE2, LOCAT, NLOC, STATUS )

*  Description:
*     The routine looks at the values in the 2D character dimension
*     LIST( NCOL, NROW ) in columns CCOL1 and CCOL2 . It only looks in
*     those rows which are flagged as true in the CHECK array. If it
*     locates a pair of matching values the row number is entered into
*     LOCAT and NLOC is incremented by one. So on exit a list of
*     pointers to the entries in which VALUE1 and VALUE2 are present is
*     output.

*  Arguments:
*     LIST( NCOL, NROW ) = CHARACTER * ( * ) (Given)
*        Array of character values which are too be checked for
*        occurances of the string VALUE.
*     NCOL = INTEGER (Given)
*        First dimension of LIST.
*     NROW = INTEGER (Given)
*        Second dimension of LIST.
*     CCOL1 = INTEGER (Given)
*        First column number to look for matching values.
*     CCOL2 = INTEGER (Given)
*        Second column number to look for matching values.
*     CHECK( NROW ) = LOGICAL (Given)
*        Logical mask of rows to be checked for string occurance.
*     VALUE1 = CHARACTER * ( * ) (Given)
*        The first value which is to be located.
*     VALUE2 = CHARACTER * ( * ) (Given)
*        The second value which is to be located.
*     LOCAT( NROW ) = INTEGER (Returned)
*        List of the row numbers which contain both VALUE1 and VALUE2
*        in CCOL1 and CCOL2 respectively.
*     NLOC = INTEGER (Returned)
*        Number of occurances of VALUE1 and VALUE2 located.
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
*     24-FEB-1992 (PDRAPER):
*        Changed to accept two entries for matching
*     {enter_further_changes_here}

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
      INTEGER CCOL1
      INTEGER CCOL2
      LOGICAL CHECK( NROW )
      CHARACTER * ( * ) VALUE1
      CHARACTER * ( * ) VALUE2

*  Arguments Returned:
      INTEGER LOCAT( NROW )
      INTEGER NLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      LOGICAL GOTONE             ! Flag for locating occurence of VALUEs
      INTEGER CLEN1              ! Maximum length of strings which may
                                 ! be usefully compared.
      INTEGER CLEN2              ! Maximum length of strings which may
                                 ! be usefully compared.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get lengths of input strings.
      CLEN1  = MIN( LEN( VALUE1 ), LEN( LIST( 1, CCOL1 ) ) )
      CLEN2  = MIN( LEN( VALUE2 ), LEN( LIST( 1, CCOL2 ) ) )

*  Set number of occurances.
      NLOC = 0

*  Loop of all rows of LIST.
      DO 1 I = 1, NROW

*  Initialise pointer list.
         LOCAT( I ) = 0

*  Do we need to check this one.
         IF ( CHECK ( I ) ) THEN
            GOTONE = VALUE1( 1 : CLEN1 ) .EQ.
     :               LIST( CCOL1, I )( 1 : CLEN1 ) .AND.
     :               VALUE2( 1 : CLEN2 ) .EQ.
     :               LIST( CCOL2, I )( 1 : CLEN2 )
            IF ( GOTONE ) THEN

*  Increment counter and enter pointer.
               NLOC = NLOC + 1
               LOCAT( NLOC ) = I
            END IF
         END IF
 1    CONTINUE

      END
* $Id$
