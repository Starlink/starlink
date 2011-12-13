      SUBROUTINE CAT_TDETL (CI, COLFLG, NUMROW, NUMCOL, NUMIND, NUMPAR,
     :   DATE, STATUS)
*+
*  Name:
*     CAT_TDETL
*  Purpose:
*     Get the details of a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TDETL (CI, COLFLG; NUMROW, NUMCOL, NUMIND, NUMPAR, DATE;
*       STATUS)
*  Description:
*     Get some summary details of a catalogue.  This routine will work
*     only if it is given a genuine catalogue identifier.  If it is
*     given a selection identifier (or any other sort of identifier)
*     it will return with an error status.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.  This routine will accept only a catalogue
*        identifier; not a selection identifier.
*     COLFLG  =  INTEGER (Given)
*        Flag indicating the type of columns for which the details are
*        to be obtained, coded as follows:
*        CAT__GVIRT - virtual columns only,
*        CAT__GPHYS - physical columns only,
*        CAT__GALL  - all columns (virtual and physical).
*     NUMROW  =  INTEGER (Returned)
*        Number of rows in the catalogue.
*     NUMCOL  =  INTEGER (Returned)
*        Number of columns in the catalogue.
*     NUMIND  =  INTEGER (Returned)
*        Number of indices to the catalogue.
*     NUMPAR  =  INTEGER (Returned)
*        Number of parameters in the catalogue.
*     DATE  =  DOUBLE PRECISION (Returned)
*        Creation date of the catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of the identifier.
*     If the identifier corresponds to a catalogue then
*       Attempt to find the array element corresponding to the catalogue.
*       If ok then
*         Extract the details required for the catalogue from the
*         catalogue arrays.
*         Inquire the modification date attribute for the catalogue.
*       end if
*     else
*       Set the status.
*       Report a message: the given identifier does not correspond to a
*       catalogue.
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
*     3/5/93  (ACD): Prologue only.
*     17/7/93 (ACD): First implementation.
*     24/1/94 (ACD): Modified error reporting.
*     15/4/94 (ACD): Added explicit checks to ensure that the given
*       identifier corresponds to a catalogue.
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
     :  COLFLG
*  Arguments Returned:
      INTEGER
     :  NUMROW,
     :  NUMCOL,
     :  NUMIND,
     :  NUMPAR
      DOUBLE PRECISION
     :  DATE
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      INTEGER
     :  CIELM,    ! Array element corresponding to the catalogue.
     :  IDTYP     ! Type of the given identifier.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier and proceed if it
*       corresponds to a catalogue.

         CALL CAT_TIDTP (CI, IDTYP, STATUS)

         IF (IDTYP .EQ. CAT__CITYP) THEN

*
*          Attempt to find the array element corresponding to the
*          catalogue and proceed if ok.

            CALL CAT1_CIELM (CI, CIELM, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Extract the details required for the catalogue from the
*             catalogue arrays.

               NUMROW = NROW__CAT1(CIELM)

               IF (COLFLG .EQ. CAT__GVIRT) THEN
                  NUMCOL = NVCOL__CAT1(CIELM)

               ELSE IF (COLFLG .EQ. CAT__GPHYS) THEN
                  NUMCOL = NPCOL__CAT1(CIELM)

               ELSE IF (COLFLG .EQ. CAT__GALL) THEN
                  NUMCOL = NVCOL__CAT1(CIELM) + NPCOL__CAT1(CIELM)

               ELSE
                  NUMCOL = 0
                  STATUS = CAT__INVGN

               END IF

               NUMIND = NIND__CAT1(CIELM)
               NUMPAR = NPAR__CAT1(CIELM)

*
*             Inquire the modification date attribute for the catalogue.

               CALL CAT_TIQAD (CI, 'DATE', DATE, STATUS)

            END IF

         ELSE

*
*          The given identifier does not correspond to a catalogue;
*          set the status and report an error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_TDETL_INVID', 'The given identifier '/
     :        /'does not correspond to a catalogue.', STATUS)
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TDETL_ERR', 'CAT_TDETL: error'/
     :        /'getting catalogue details.', STATUS)
         END IF

      END IF

      END
