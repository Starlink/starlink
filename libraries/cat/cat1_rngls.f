      SUBROUTINE CAT1_RNGLS (ROWS, BEGROW, ENDROW, REJECT, NUMSEL,
     :  NUMREJ, SELLST, REJLST, STATUS)
*+
*  Name:
*     CAT1_RNGLS
*  Purpose:
*     Generate lists of selected and, optionally, rejected row numbers.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_RNGLS (ROWS, BEGROW, ENDROW, REJECT, NUMSEL, NUMREJ;
*       SELLST, REJLST; STATUS)
*  Description:
*     Generate lists of selected and, optionally, rejected row numbers.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        The total number of rows in the catalogue.
*     BEGROW  =  INTEGER (Given)
*        The number of the first selected row.
*     ENDROW  =  INTEGER (Given)
*        The number of the last selected row.
*     REJECT  =  LOGICAL (Given)
*        Flag indicating whether or not a list of rejected rows is to
*        be produced.  It is coded as follows:
*        .TRUE.  -  produce list of rejected rows,
*        .FALSE. -  do not produce a list of rejected rows.
*     NUMSEL  =  INTEGER (Given)
*        Number of selected rows.
*     NUMREJ  =  INTEGER (Given)
*        Number of rejected rows.
*     SELLST(NUMSEL)  =  INTEGER (Returned)
*        List of selected rows.
*     REJLST(NUMREJ)  =  INTEGER (Returned)
*        Optional list of rejected rows.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Generate the list of selected rows.
*     If the list of rejected rows is required then
*       Generate the list of rejected rows.
*     end if
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     7/9/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  ROWS,
     :  BEGROW,
     :  ENDROW,
     :  NUMSEL,
     :  NUMREJ
      LOGICAL
     :  REJECT
*  Arguments Returned:
      INTEGER
     :  SELLST(NUMSEL),
     :  REJLST(NUMREJ)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  CURSEL,  ! Count for current selected row.
     :  CURREJ,  ! Count for current rejected row.
     :  LOOP     ! Loop index.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Generate the list of selected rows.

         CURSEL = 0

         DO LOOP = BEGROW, ENDROW
            CURSEL = CURSEL + 1
            SELLST(CURSEL) = LOOP
         END DO

*
*       If required then generate the list of rejected rows.

         IF (REJECT) THEN

*
*          Note that these loops rely on the Fortran 77 'zero trip DO'
*          feature.  If BEGROW corresponds to the first row in the
*          catalogue or ENDROW corresponds to the last row in the
*          catalogue then one of the loops will will be skipped without
*          executing.

            CURREJ = 0

            DO LOOP = 1, BEGROW-1
               CURREJ = CURREJ + 1
               REJLST(CURREJ) = LOOP
            END DO

            DO LOOP = ENDROW+1, ROWS
               CURREJ = CURREJ + 1
               REJLST(CURREJ) = LOOP
            END DO
         END IF

      END IF

      END
