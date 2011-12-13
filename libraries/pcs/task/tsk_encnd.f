      SUBROUTINE TASK_ENCND ( NDIMS, DIMS, DVALS, STRING, STATUS )
*+
*  Name:
*     TASK_ENCND

*  Purpose:
*     Encode an array as a character string

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_ENCND ( NDIMS, DIMS, DVALS, STRING, STATUS )

*  Description:
*     Convert the given multidimensional array into characters and
*     concatenate the values into a string with separators. The
*     dimensions of the array are delimited by [] following the ADAM
*     syntax.
*     There is a routine for each type C, D, I, L, R.

*  Arguments:
*     NDIMS=INTEGER (given)
*           number of dimensions of the given array
*     DIMS(NDIMS)=INTEGER (given)
*           the dimensions of the given array
*     DVALS(1:*)=DOUBLE PRECISION (given)
*           the given array, treated as a vector
*     STRING=CHARACTER*(*) (returned)
*           the returned string
*     STATUS=INTEGER

*  Algorithm:
*     Convert each of the given values into an element in a 1-D array of
*     strings. Then call the ADAM string building routine to concatenate
*     the strings into a single string with the correct syntax.

*  Copyright:
*     Copyright (C) 1987, 1989, 1992 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-NOV-1987 (REVAD::BDK):
*        Original
*     29-APR-1989 (AAOEPP::WFL):
*        Make it generic
*     04-OCT-1992 (RLVAD::AJC):
*        Use CHR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NDIMS        ! number of dimensions of the given array

      INTEGER DIMS(NDIMS)  ! the dimensions of the given array

      DOUBLE PRECISION DVALS(1:*) ! the given array, treated as a vector

*  Arguments Returned:
      CHARACTER*(*) STRING ! the returned string

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER*(40) CARRAY(20)  ! store for original type conversions
      INTEGER TOTNUM             ! number of items for conversion
      INTEGER J                  ! loop counter
      INTEGER NCHAR              ! size of encoded item
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      TOTNUM = 1
      DO J = 1, NDIMS
         TOTNUM = TOTNUM * DIMS(J)
      ENDDO

      J = 0
      DO WHILE ( ( J .LT. TOTNUM ) .AND. ( STATUS .EQ. SAI__OK ) )
         J = J + 1
         CALL CHR_DTOC( DVALS(J), CARRAY(J), NCHAR )
      ENDDO

      CALL STRING_BUILDARR ( NDIMS, DIMS, CARRAY, STRING, STATUS )

      END
