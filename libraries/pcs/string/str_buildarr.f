      SUBROUTINE STRING_BUILDARR ( NDIMS, DIMS, CARRAY, OUTSTRING,
     :  STATUS )
*+
*  Name:
*     STRING_BUILDARR

*  Purpose:
*     Build an array of strings into a string

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL STRING_BUILDARR ( NDIMS, DIMS, CARRAY, OUTSTRING,
*     :  STATUS )

*  Description:
*     Given an N-dimensional array of strings, build them into a single
*     string with dimensions and values separated by brackets and
*     commas.

*  Arguments:
*     NDIMS=INTEGER (given)
*           number of dimensions in CARRAY
*     DIMS(NDIMS)=INTEGER (given)
*           dimensions of CARRAY
*     CARRAY(*)=CHARACTER*(*) (given)
*           array containing the values
*     OUTSTRING=CHARACTER*(*) (returned)
*           generated string
*     STATUS=INTEGER

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council.
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     08-MAY-1987 (REVAD::BDK):
*        Original
*     31-OCT-1997 (50->100):
*        Increase size (80->250) and number
*                  of array elements (STARLINK::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NDIMS             ! number of dimensions in CARRAY

      INTEGER DIMS(NDIMS)       ! dimensions of CARRAY

      CHARACTER*(*) CARRAY(*)   ! array containing the values

*  Arguments Returned:
      CHARACTER*(*) OUTSTRING   ! generated string

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER*250 INARRAY(100)  ! work array

      CHARACTER*250 OUTARRAY(100) ! work array

      INTEGER TOTAL             ! total numnber of items

      INTEGER J                 ! loop counter

      INTEGER K                 ! dimension currently being packed

      INTEGER NUMPACK           ! number of packages being generated
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Copy the input array into a work array
*
      TOTAL = 1
      DO J = 1, NDIMS
         TOTAL = TOTAL * DIMS(J)
      ENDDO
      DO J = 1, TOTAL
         INARRAY(J) = CARRAY(J)
      ENDDO
*
*   Pack down the array one dimension at a time
*
      DO K = 1, NDIMS

         NUMPACK = 1
         DO J = NDIMS, K+1, -1
            NUMPACK = NUMPACK * DIMS(J)
         ENDDO

         CALL STRING_PACK ( NUMPACK, DIMS(K), INARRAY, OUTARRAY,
     :     STATUS )

         DO J = 1, NUMPACK
            INARRAY(J) = OUTARRAY(J)
         ENDDO

      ENDDO

      OUTSTRING = INARRAY(1)

      END
