      SUBROUTINE CAT_SELCT (CI, EI, REJFLG, SI, NUMSEL, SIR, NUMREJ,
     :  STATUS)
*+
*  Name:
*     CAT_SELCT
*  Purpose:
*     Create a selection of rows satisfying some expression.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SELCT (CI, EI, REJFLG; SI, NUMSEL, SIR, NUMREJ; STATUS)
*  Description:
*     Create a selection of rows satisfying some expression.
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The expression can involve both indexed and unindexed columns.
*     If indexed columns are involved, they are NOT used to optimise
*     the selection.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     EI  =  INTEGER (Given)
*        Identifier to the logical expression defining the selection
*        criteria.
*     REJFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not a second selection of the
*        rejected rows is to be created:
*        .TRUE.  -  create the catalogue of rejected rows,
*        .FALSE. -  do not create the catalogue of rejected rows.
*     SI  =  INTEGER (Returned)
*        Selection identifier to the set of selected rows.
*     NUMSEL  =  INTEGER (Returned)
*        Number of rows selected.
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
*       If the identifier corresponds to a catalogue then
*         Simply copy the identifier as the base catalogue.
*       else (the identifier corresponds to a selection) then
*         Obtain the identifier of the parent catalogue.
*       end if
*       Determine the type of the expression identifier.
*       If the identifier corresponds to an expression then
*         Determine the parent catalogue of the expression.
*         If the input catalogue and the parent catalogue of the
*         expression are the same then
*           Determine the number of rows in the catalogue.
*           If ok then
*             Create workspace for the list of selected rows.
*             If a  list of rejected rows is to be created then
*               Create workspace for the list of rejected rows.
*             end if
*             If ok then
*               Select the rows which satisfy the criterion.
*               If some objects were selected then
*                 Create space to hold the list of selected rows.
*                 Copy the selected rows to this space.
*                 Create a selection identifier for the selected rows.
*                 If a list of rejected rows is to be created then
*                   Create space to hold the list of rejected rows.
*                   Copy the rejected rows to this space.
*                   Create a selection identifier for the rejected rows.
*                 end if
*               else no rows were selected.
*                 Set the selection identifier to the null identifier.
*                 Set the number of rows selected to zero.
*                 If a list of rejected rows is to be created then
*                   Set the selection identifier for the list of rejects
*                   to the null identifier.
*                   Set the number of rows rejected to zero.
*                 end if
*               end if
*               Release the workspace for the selected rows.
*               If a list of rejected rows is to be created then
*                 Release the workspace for the rejected rows.
*               end if
*             end if
*           end if
*         else
*           Set the status.
*           Report error; input catalogue and expression do not correspond
*           to the same catalogue.
*         end if
*       else
*         Set the status.
*         Report error; the given expression identifier does not correspond
*         to an expression.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     14/4/94 (ACD): Original version.
*     6/9/94  (ACD): First stable version.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     2/5/96  (ACD): Fixed bug in generating selection of rejected
*       objects.
*     17/3/00 (ACD): Fixed to correctly handle the case where a
*       selection of rejected rows has been requeste, but not rows were
*       rejected.
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
     :  CI,
     :  EI
      LOGICAL
     :  REJFLG
*  Arguments Returned:
      INTEGER
     :  SI,
     :  NUMSEL,
     :  SIR,
     :  NUMREJ
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  EIDTYP,  ! Type of the given identifier for the expression.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  CIE,     ! Parent catalogue identifier for the expression.
     :  ROWS,    ! Number of rows in the parent catalogue.
     :  WSPTR,   ! Pointer to workspace for list of selected rows.
     :  WRPTR,   !    "    "      "      "   "   "  rejected  "  .
     :  SELPTR,  ! Pointer to final list of selected rows.
     :  REJPTR   !    "    "    "    "   "  rejected  "  .
      CHARACTER
     :  EXPR*(CAT__SZEXP)  ! Expression defining the selection.
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
*          Check that the expression identifier actually corresponds to
*          an expression.

            CALL CAT_TIDTP (EI, EIDTYP, STATUS)

            IF (EIDTYP .EQ. CAT__EITYP) THEN

*
*             Determine the parent catalogue of the expression.

               CALL CAT_TIDPR (EI, CIE, STATUS)

*
*             Check that the expression and the input catalogue or
*             selection correspond to the same catalogue.

               IF (CIE .EQ. CIC) THEN

*
*                Determine the number of rows in the input catalogue
*                or selection.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Proceed if all is ok.

                  IF (STATUS .EQ. CAT__OK) THEN

*
*                   Create workspace for the list of selected row and,
*                   if required, the list of rejected rows

                     CALL CAT1_CRTAR (ROWS, '_INTEGER', WSPTR, STATUS)
                     IF (REJFLG) THEN
                        CALL CAT1_CRTAR (ROWS, '_INTEGER', WRPTR,
     :                    STATUS)
                     END IF

*
*                   Proceed if all is ok.

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Select the rows which satisfy the criterion.

                        CALL CAT1_SELCT (EI, REJFLG, CI, ROWS, NUMSEL,
     :                    %VAL(CNF_PVAL(WSPTR)), NUMREJ,
     :                    %VAL(CNF_PVAL(WRPTR)), STATUS)

*
*                      Proceed if some objects were selected.

                        IF (NUMSEL .GT. 0) THEN

*
*                         Create space to hold the list of selected
*                         rows, copy the selected rows to this space and
*                         finally create a selection identifier for the
*                         selected rows.
*
*                         Repeat the process for the list of rejected rows,
*                         if required and there were any.

                           CALL CAT1_CRTAR (NUMSEL, '_INTEGER',
     :                       SELPTR, STATUS)
                           CALL CAT1_CPYAR (NUMSEL,
     :                                      %VAL(CNF_PVAL(WSPTR)),
     :                       %VAL(CNF_PVAL(SELPTR)), STATUS)
                           CALL CAT_TIQAC (EI, 'EXPR', EXPR, STATUS)
                           CALL CAT1_CRTSL (CIC, EXPR, .TRUE., NUMSEL,
     :                       SELPTR, SI, STATUS)

                           IF (REJFLG) THEN
                              IF (NUMREJ .GT. 0) THEN
                                 CALL CAT1_CRTAR (NUMREJ, '_INTEGER',
     :                             REJPTR, STATUS)
                                 CALL CAT1_CPYAR (NUMREJ,
     :                                            %VAL(CNF_PVAL(WRPTR)),
     :                             %VAL(CNF_PVAL(REJPTR)), STATUS)
                                 CALL CAT1_CRTSL (CIC, EXPR, .FALSE.,
     :                             NUMREJ, REJPTR, SIR, STATUS)
                              ELSE
                                 SIR = CAT__NOID

                              END IF

                           END IF

                        ELSE

*
*                         No rows were selected; set the identifer for the
*                         selected rows to null, and, if required, the
*                         identifier to the rejected rows to null.  Also
*                         set the number of rejected rows to null.

                           SI = CAT__NOID

                           IF (REJFLG) THEN
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        END IF

*
*                      Release the workspace for the selected rows and, if
*                      appropriate, the rejected rows.

                        CALL CAT1_FREAR (WSPTR, STATUS)

                        IF (REJFLG) THEN
                           CALL CAT1_FREAR (WRPTR, STATUS)
                        END IF
                     END IF
                  END IF

               ELSE

*
*                The input catalogue or selection and the expression do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SELCT_DIFF', 'The input '/
     :              /'and expression correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given expression identifier does not correspond to an
*             expression.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SELCT_INXP', 'The given '/
     :           /'expression identifier does not correspond to an '/
     :           /'expression.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SELCT_INCT', 'The given '/
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

            CALL CAT1_ERREP ('CAT_SELCT_ERR', 'CAT_SELCT: Error '/
     :        /'generating a selection.', STATUS)
         END IF

      END IF

      END
