      SUBROUTINE CAT_RSET (CI, ROWS, STATUS)
*+
*  Name:
*     CAT_RSET
*  Purpose:
*     Set the number of rows which a new catalogue is expected to contain.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_RSET (CI, ROWS; STATUS)
*  Description:
*     Set the number of rows which a new catalogue is expected to contain.
*
*     The value set may be either used or ignored, depending on the type
*     of the catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows which the catalogue is to contain.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier corresponds to a catalogue then
*       Get the array element corresponding to the catalogue.
*       If the catalogue is'NEW' then
*         If creation of the catalogue is not finished then
*           If the specified number of rows is greater than zero then
*             Set the number of rows.
*           else
*             Set the status.
*             Report an error.
*           end if
*         else
*           Set the status.
*           Report an error.
*         end if
*       else
*         Set the status.
*         Report an error.
*       end if
*     else
*       Set the status.
*       Report an error.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/7/96 (ACD): Original version.
*     23/7/96 (ACD): First stable version.
*     22/1/97 (ACD): Fixed a spelling mistake in the prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  ROWS
*  Status:
      INTEGER STATUS   ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,   ! Type of the identifier.
     :  CIELM     ! Array element corresponding to the catalogue.
*.
      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check whether the identifier corresponds to a catalogue.

         CALL CAT_TIDTP (CI, IDTYPE, STATUS)
         IF (IDTYPE .EQ. CAT__CITYP) THEN

*
*          Find the array element corresponding to the catalogue.

            CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*          Check that the state of the catalogue is 'NEW' (that is,
*          a new catalogue is being created).

            IF (STATE__CAT1(CIELM) .EQ. CAT1__STNEW) THEN

*
*             Check that creation of the catalogue is not yet finished.

               IF (.NOT. FINSH__CAT1(CIELM) ) THEN

*
*                Check that the number of rows is a positive number.

                  IF (ROWS .GT. 0) THEN

*
*                   Set the number of rows.

                     EROW__CAT1(CIELM) = ROWS

                  ELSE
                     STATUS = CAT__ERROR

                     CALL CAT1_ERREP ('CAT_RSET_NZRW', 'Given '/
     :                 /'number of rows is zero or negative.',
     :                 STATUS)
                  END IF
               ELSE
                  STATUS = CAT__ERROR

                  CALL CAT1_ERREP ('CAT_RSET_CRFN', 'Creation '/
     :               /'of the catalogue is finished.', STATUS)
               END IF
            ELSE
               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_RSET_NTCR', 'Given catalogue '/
     :           /'is not being created.', STATUS)
            END IF
         ELSE
            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_RSET_CRFN', 'Given identifier '/
     :        /'does not correspond to a catalogue.', STATUS)
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_RSET_ERR', 'CAT_RSET: error'/
     :        /'setting the number of rows in the catalogue.',
     :        STATUS)
         END IF

      END IF

      END
