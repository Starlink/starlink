      SUBROUTINE CAT5_STNUL (NULVAL, ROWS, NULCOL, STATUS)
*+
*  Name:
*     CAT5_STNUL
*  Purpose:
*     Set a column of null value flags to a single value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STNUL (NULVAL, ROWS; NULCOL; STATUS)
*  Description:
*     Set a column of null value flags to a single value.
*  Arguments:
*     NULVAL  =  LOGICAL (Given)
*        Value to which the column is to be set.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     NULCOL(ROWS)  =  LOGICAL (Returned)
*        Column of null value flags.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set every row in the column to the required value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      LOGICAL
     :  NULVAL
      INTEGER
     :  ROWS
*  Arguments Returned:
      LOGICAL
     :  NULCOL(ROWS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP                     ! Loop index.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         DO LOOP = 1, ROWS
            NULCOL(LOOP) = NULVAL
         END DO

      END IF

      END
