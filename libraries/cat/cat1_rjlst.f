      SUBROUTINE CAT1_RJLST (NUMSEL, SELIST, ROWS, CATLST, NUMREJ,
     :  REJLST, STATUS)
*+
*  Name:
*     CAT1_RJLST
*  Purpose:
*     Create a list of rejected rows from a list of selected rows.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_RJLST (NUMSEL, SELIST, ROWS; CATLST; NUMREJ, REJLST;
*       STATUS)
*  Description:
*     Create a list of rejected rows in a catalogue from a list of
*     selected rows.
*  Arguments:
*     NUMSEL  =  INTEGER (Given)
*        Number of rows to be selected.
*     SELIST(NUMSEL)  =  INTEGER (Given)
*        Array of row numbers to be selected.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     CATLST(ROWS)  =  INTEGER (Work)
*        Work array with one entry for every row in the catalogue.
*     NUMREJ  =  INTEGER (Returned)
*        Number of rows rejected.
*     REJLST(ROWS)  =  INTEGER (Returned)
*        List of rejected rows.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise every row in the catalogue list to rejected.
*     For every selected row
*       Set the entry in the catalogue list to selected.
*     end for
*     For every row in the catalogue list
*       If the row is still rejected then
*         Increment the number of rejects.
*         Add the row to the list of rejects.
*       end if
*     end for
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
*     12/6/96 (ACD): Original version.
*     18/6/96 (ACD): Documentation error in prologue corrected and
*        un-necessary INCLUDE file removed.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  NUMSEL,
     :  SELIST(NUMSEL),
     :  ROWS
*  Arguments Used:
      INTEGER
     :  CATLST(ROWS)
*  Arguments Returned:
      INTEGER
     :  NUMREJ,
     :  REJLST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER REJECT          ! Flag indicating row is rejected.
      PARAMETER (REJECT = 1)

      INTEGER SELECT          ! Flag indicating row is selected.
      PARAMETER (SELECT = 2)
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  CURSEL  ! Current selected row.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise every row in the catalogue list to rejected.

         DO LOOP = 1, ROWS
            CATLST(LOOP) = REJECT
         END DO

*
*       Set those rows which are in the list of selected rows to
*       selected.

         DO LOOP = 1, NUMSEL
            CURSEL = SELIST(LOOP)

            IF (CURSEL .GE. 1  .AND.  CURSEL .LE. ROWS) THEN
               CATLST(CURSEL) = SELECT
            END IF
         END DO

*
*       Examine the catalogue list and extract those elements which are
*       still rejected.

         NUMREJ = 0

         DO LOOP = 1, ROWS
            IF (CATLST(LOOP) .EQ. REJECT) THEN
               NUMREJ = NUMREJ + 1
               REJLST(NUMREJ) = LOOP
            END IF
         END DO

      END IF

      END
