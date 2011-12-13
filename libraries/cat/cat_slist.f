      SUBROUTINE CAT_SLIST (NUMSEL, SELIST, CRIT, REJFLG, CI, SI, SIR,
     :  NUMREJ, STATUS)
*+
*  Name:
*     CAT_SLIST
*  Purpose:
*     Create a selection from an array of row numbers.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SLIST (NUMSEL, SELIST, CRIT, REJFLG, CI; SI, SIR, NUMREJ;
*       STATUS)
*  Description:
*     Create a selection from an array of row numbers.
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*  Arguments:
*     NUMSEL  =  INTEGER (Given)
*        Number of rows to be selected.
*     SELIST(NUMSEL)  =  INTEGER (Given)
*        Array of row numbers to be selected.  These row numbers must
*        be rows in the catalogue or selection to which CI corresponds.
*        That is, if CI corresponds to a catalogue they will be absolute
*        row numbers.  However, if CI is a selection they will be row
*        numbers in the selection, NOT the corresponding row numbers in
*        the underlying catalogue.
*     CRIT  =  CHARACTER*(*) (Given)
*        Character string summarising the selection criteria.
*     REJFLG  =  LOGICAL (Given)
*        Flag indicating whether or not a second selection of the
*        rejected rows is to be created:
*        .TRUE.  -  create the catalogue of rejected rows,
*        .FALSE. -  do not create the catalogue of rejected rows.
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     SI  =  INTEGER (Returned)
*        Selection identifier to the set of selected rows.
*     SIR  =  INTEGER (Returned)
*        Optional selection identifier to the set of rejected rows.
*        If the rejected rows are not being retained then SIR is set
*        to the null identifier.
*     NUMREJ  =  INTEGER (Returned)
*        Number of rows rejected.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of the catalogue identifier.
*     If the identifier corresponds to either a catalogue or a
*     selection then
*       Determine the identifier of the parent catalogue.
*       Create workspace for the list of selected rows.
*       Copy the selected rows to this space.
*       Create a selection identifier for the selected rows.
*       If a list of rejected rows is to be created then
*         Generate the list of rejected rows.
*         Create space to hold the list of rejected rows.
*         If there are any rejected rows then
*           Copy the rejected rows to this space.
*           Create a selection identifier for the rejected rows.
*         else
*           Set the identifier to selection of rejected objects to null.
*         end if
*         Release the work space.
*       else
*         Set the identifier to selection of rejected objects to null.
*         Set the number of rejected objects to zero.
*       end if
*     else
*       Set the status.
*       Report error; the input catalogue identifer does not correspond to
*       a catalogue or a selection.
*     end if
*     If any error occurred then
*       Report the error.
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
*     13/6/96 (ACD): Original version.
*     17/3/00 (ACD): Fixed to handle correctly the case where a list of
*        rejected rows is required, but there are none.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CNF_PAR'           ! For CNF_PVAL function
*  Arguments Given:
      INTEGER
     :  NUMSEL,
     :  SELIST(NUMSEL),
     :  CI
      CHARACTER
     :  CRIT*(*)
      LOGICAL
     :  REJFLG
*  Arguments Returned:
      INTEGER
     :  SI,
     :  SIR,
     :  NUMREJ
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  ROWS,    ! Number of rows in the parent catalogue.
     :  WKPTR1,  ! Work space for generating rejected rows.
     :  WKPTR2,  !  "     "    "      "         "      "  .
     :  SELPTR,  ! Pointer to list of selected rows.
     :  REJPTR   !    "    "   "   "  rejected  "  .
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the identifier for the input catalogue or selection
*       actually corresponds to a catalogue or selection.

         CALL CAT_TIDTP (CI, CIDTYP, STATUS)

         IF (CIDTYP .EQ. CAT__CITYP  .OR.  CIDTYP .EQ. CAT__SITYP)
     :     THEN

*
*          Determine the identifier of the parent catalogue for the new
*          selection; copy the identifier if the input identifier
*          corresponds to a catalogue or get the parent identifier if
*          input identifier corresponds to a selection.

            IF (CIDTYP .EQ. CAT__CITYP) THEN
               CIC = CI
            ELSE
               CALL CAT_TIDPR (CI, CIC, STATUS)
            END IF

*
*          Create space to hold the list of selected rows and copy the
*          selected rows to it, converting relative row numbers in a
*          selection into the corresponding absolute row numbers as
*          appropriate.

            CALL CAT1_CRTAR (NUMSEL, '_INTEGER', SELPTR, STATUS)
            CALL CAT1_CATRA (CI, NUMSEL, SELIST, %VAL(CNF_PVAL(SELPTR)),
     :                       STATUS)

*
*          Create a selection identifier.

            CALL CAT1_CRTSL (CIC, CRIT, .TRUE., NUMSEL, SELPTR, SI,
     :        STATUS)

*
*          Check whether an additional selection of rejected rows is
*          required.

            IF (REJFLG) THEN

*
*             If so, then generate the list of rejected rows.

               CALL CAT_TROWS (CIC, ROWS, STATUS)

               CALL CAT1_CRTAR (ROWS, '_INTEGER', WKPTR1, STATUS)
               CALL CAT1_CRTAR (ROWS, '_INTEGER', WKPTR2, STATUS)

               CALL CAT1_RJLST (NUMSEL, SELIST, ROWS,
     :                          %VAL(CNF_PVAL(WKPTR1)),
     :           NUMREJ, %VAL(CNF_PVAL(WKPTR2)), STATUS)

               IF (NUMREJ .GT. 0) THEN
                  CALL CAT1_CRTAR (ROWS, '_INTEGER', REJPTR, STATUS)

                  CALL CAT1_CATRA (CI, NUMREJ, %VAL(CNF_PVAL(WKPTR2)),
     :              %VAL(CNF_PVAL(REJPTR)), STATUS)

*
*                Create the selection of rejected rows.

                  CALL CAT1_CRTSL (CIC, CRIT, .FALSE., NUMREJ, REJPTR,
     :              SIR, STATUS)

               ELSE
                  SIR = CAT__NOID

               END IF

*
*             Release the work space.

               CALL CAT1_FREAR (WKPTR1, STATUS)
               CALL CAT1_FREAR (WKPTR2, STATUS)

            ELSE
               SIR = CAT__NOID
               NUMREJ = 0

            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SLIST_INCT', 'The given '/
     :        /'catalogue identifier does not correspond to a '/
     :        /'catalogue or selection.', STATUS)
         END IF

*
*       If any error occurred then set the selections to null and
*       report the error.

         IF (STATUS .NE. CAT__OK) THEN
            SI = CAT__NOID
            NUMSEL = 0

            SIR = CAT__NOID
            NUMREJ = 0

            CALL CAT1_ERREP ('CAT_SLIST_ERR', 'CAT_SLIST: Error '/
     :        /'generating a selection.', STATUS)
         END IF

      END IF

      END
