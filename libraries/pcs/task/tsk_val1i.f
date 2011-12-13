      SUBROUTINE TASK_VAL1I ( NVALS, IVALS, STRING, STATUS )
*+
*  Name:
*     TASK_VAL1I

*  Purpose:
*     Encode a vector as a character string

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_VAL1I ( NVALS, IVALS, STRING, STATUS )

*  Description:
*     Convert the given 1-D array into characters and concatenate the
*     result into a string with the ADAM syntax, that is the elements of
*     the array are separated and the whole is surrounded by [].
*     There is a routine for each type C, D, I, L, R.

*  Arguments:
*     NVALS=INTEGER (given)
*           number of values in the 1-D array
*     IVALS(NVALS)=INTEGER (given)
*           the array to be converted
*     STRING=CHARACTER*(*) (returned)
*           the returned character string
*     STATUS=INTEGER

*  Algorithm:
*     Call TASK_ENC1I

*  Copyright:
*     Copyright (C) 1987, 1989 Science & Engineering Research Council.
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
*     06-NOV-1987 (REVAD::BDK):
*        Original
*     29-APR-1989 (AAOEPP::WFL):
*        Make it generic (same as TASK_ENC1I)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NVALS         ! number of values in the 1-D array

      INTEGER IVALS(NVALS) ! the array to be encoded

*  Arguments Returned:
      CHARACTER*(*) STRING  ! the returned character string

*  Status:
      INTEGER STATUS
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL TASK_ENC1I ( NVALS, IVALS, STRING, STATUS )

      END
