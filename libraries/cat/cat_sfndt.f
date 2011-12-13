      SUBROUTINE CAT_SFNDB (CI, FI, MINRNG, MAXRNG, REJFLG,
     :  SI, NUMSEL, SIR, NUMREJ, STATUS)
*+
*  Name:
*     CAT_SFNDB
*  Purpose:
*     Create a selection of rows within a given range.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SFNDB (CI, FI, MINRNG, MAXRNG, REJFLG; SI, NUMSEL,
*       SIR, NUMREJ; STATUS)
*  Description:
*     Create a selection of rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     FI  =  INTEGER (Given)
*        Identifier to the column whose fields will be selected to
*        lie in the given range.  The column must be sorted into
*        ascending or descending order (and known by CAT to be so
*        sorted).
*     MINRNG  =  BYTE (Given)
*        Minimum value which a field must satisfy to be selected.
*     MAXRNG  =  BYTE (Given)
*        Maximum value which a field must satisfy to be selected.
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
*       Determine the type of the column identifier.
*       If the column identifier corresponds to a column then
*         Determine the parent catalogue of the column.
*         If the input catalogue and the parent catalogue of the
*         column are the same then
*           Determine the number of rows in the catalogue.
*           Determine the order of the column.
*           If the column is either ascending or descending then
*             Determine the row corresponding to the lower limit.
*             Determine the row corresponding to the upper limit.
*             If ok then
*               If any rows were selected then
*                 Switch the rows so that they are in ascending row
*                 order.
*                 Force the rows to lie in the catalogue bounds.
*                 Compute the number of rows selected.
*                 Map sufficient work space for the selected rows.
*                 If required then
*                   Map sufficient work space for the rejected rows.
*                 end if
*                 Generate the lists of selected and rejected rows.
*                 Generate the expression corresponding to the
*                 selection specified.
*                 Attempt to create an identifier for the selected
*                 objects.
*                 If required then
*                   Attempt to create an identifier for the rejected
*                   objects.
*                 else
*                   Set the rejection identifier to null.
*                   Set the number of rejected objects to zero.
*                 end if
*               else
*                 Set the selection identifier to null.
*                 Set the number of rows selected to 0.
*                 Set the rejection identifier to null.
*                 Set the number of rows rejected to 0.
*               end if
*             end if
*           else
*             Set the status.
*             Report error; attempting range selection on an
*             unsorted column.
*           end if
*         else
*           Set the status.
*           Report error; input catalogue and column do not correspond
*           to the same catalogue.
*         end if
*       else
*         Set the status.
*         Report error; the given column identifier does not correspond
*         to a column.
*       end if
*     else
*       Set the status.
*       Report error; the input catalogue identifer does not correspond
*       to a catalogue or a selection.
*     end if
*     If any error occurred then
*       Set the returned identifiers to null.
*       Set the returned number of rows to zero.
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     Only works on sorted columns, not indexed columns.
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
*     16/8/94 (ACD): Original version.
*     14/9/94 (ACD): First stable version.
*     17/2/95 (ACD): Fixed bug in error reporting.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     19/9/96 (ACD): Fixed bug in defining range of rows (which was
*        only manifest for descending columns).
*     17/3/00 (ACD): Fixed handling of the case where the rejected rows
*        were required, but no rows were rejected.
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
     :  FI
      BYTE
     :  MINRNG,
     :  MAXRNG
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
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  FIDTYP,  ! Type of the given identifier for the column.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  CIF,     ! Parent catalogue identifier for the column.
     :  ROWS,    ! Number of rows in the input cat. or selection.
     :  ORDER,   ! Order of the colunm (ascending, descening, none).
     :  MINROW,  ! Row corresponding to MINRNG.
     :  MAXROW,  !  "        "       "  MAXRNG
     :  BEGROW,  ! First (begining) selected row in the catalogue.
     :  ENDROW   ! Last  (end)         "      "  "   "      "    .
      INTEGER
     :  SELPTR,  ! Pointer to final list of selected rows.
     :  REJPTR,  !    "    "    "    "   "  rejected  "  .
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LEXPR,   !   "    "  EXPR   ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      LOGICAL
     :  LRJFLG   ! Local rejected rows flag.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP), ! Name of the column being selected.
     :  EXPR*(CAT__SZEXP)   ! Expression defining the selection.
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
*          Check that the column identifier actually corresponds to
*          a column.

            CALL CAT_TIDTP (FI, FIDTYP, STATUS)

            IF (FIDTYP .EQ. CAT__FITYP) THEN

*
*             Determine the parent catalogue of the column.

               CALL CAT_TIDPR (FI, CIF, STATUS)

*
*             Check that the column and the input catalogue or
*             selection correspond to the same catalogue.

               IF (CIF .EQ. CIC) THEN

*
*                Determine the number of rows in the input catalogue
*                or selection.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Determine the order of the column.

                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

*
*                Check that the column is in either ascending or
*                descending order.

                  IF (ORDER .EQ. CAT__ASCND  .OR.
     :              ORDER .EQ. CAT__DSCND) THEN

*
*                   Determine the rows in the catalogue (or selection)
*                   corresponding to the upper and lower limits and
*                   proceed if ok.

                     CALL CAT1_BHOPB (CI, FI, ROWS, ORDER, MINRNG,
     :                 MINROW, STATUS)
                     CALL CAT1_BHOPB (CI, FI, ROWS, ORDER, MAXRNG,
     :                 MAXROW, STATUS)

C                    print2201, minrow, maxrow
C2201                format(1x, 'minrow, maxrow: ', i5, i5)


                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        BEGROW = MIN(MINROW, MAXROW)
                        ENDROW = MAX(MINROW, MAXROW)
                        IF (ENDROW .LE. ROWS) THEN
                           ENDROW = ENDROW - 1
                        END IF

C                       print2202, begrow, endrow, rows
C2202                   format(1x, 'begrow, endrow, rows: ',
C    :                    i10, i10, i10)

                        IF (((BEGROW .GE. 1 .AND. BEGROW .LE. ROWS)  .OR
     :.
     :                      (ENDROW .GE. 1 .AND. ENDROW .LE. ROWS)  .OR.
     :                      (BEGROW .LT. 1 .AND. ENDROW .GT. ROWS))
     :                    .AND. (BEGROW .LE. ENDROW) ) THEN

*
*                         Force the selected rows to lie in the range
*                         of the catalogue.

                           BEGROW = MAX(BEGROW, 1)
                           ENDROW = MIN(ENDROW, ROWS)

*
*                         Compute the number of rows selected.

                           NUMSEL = ENDROW + 1 - BEGROW

*
*                         Map sufficient work space to hold the
*                         selected rows, and, if required, the
*                         rejected rows.

                           CALL CAT1_CRTAR (NUMSEL, '_INTEGER',
     :                       SELPTR, STATUS)

                           IF (REJFLG) THEN
                              NUMREJ = ROWS - NUMSEL

                              IF (NUMREJ .GT. 0) THEN
                                 LRJFLG = .TRUE.
                                 CALL CAT1_CRTAR (NUMREJ, '_INTEGER',
     :                             REJPTR, STATUS)
                              ELSE
                                 LRJFLG = .FALSE.
                                 REJPTR = 0
                              END IF
                           END IF

*
*                         Generate the lists of selected and rejected
*                         rows.

                           CALL CAT1_RNGLS (ROWS, BEGROW, ENDROW,
     :                       LRJFLG, NUMSEL, NUMREJ,
     :                       %VAL(CNF_PVAL(SELPTR)),
     :                       %VAL(CNF_PVAL(REJPTR)), STATUS)

*
*                         Generate the expression specifying the
*                         selection.  Note that this expression is
*                         stored as an attribute of the selection
*                         essentially as a comment; it is not used
*                         in any calculations.

                           EXPR = ' '
                           LEXPR = 0

                           CALL CAT_TIQAC (FI, 'NAME', CNAME, STATUS)
                           IF (CNAME .NE. ' ') THEN
                              LCNAME = CHR_LEN(CNAME)
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' > ', EXPR, LEXPR)
                           CALL CAT1_PUTB (MINRNG, EXPR, LEXPR)
C                          call chr_putc ('minrng', expr, lexpr)

                           CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                           IF (CNAME .NE. ' ') THEN
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' <= ', EXPR, LEXPR)
                           CALL CAT1_PUTB (MAXRNG, EXPR, LEXPR)
C                          call chr_putc ('maxrng', expr, lexpr)

*
*                         Attempt to create an identifier for the
*                         selected objects.

                           CALL CAT1_CRTSL (CIC, EXPR, .TRUE., NUMSEL,
     :                       SELPTR, SI, STATUS)

*
*                         If required and available, attempt to create
*                         an identifier for the rejected objects.
*                         Otherwise set the identifier for the rejected
*                         objects to null and the number of rejected
*                         objects to zero.

                           IF (LRJFLG) THEN
                              CALL CAT1_CRTSL (CIC, EXPR, .FALSE.,
     :                          NUMREJ, REJPTR, SIR, STATUS)
                           ELSE
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        ELSE

*
*                         No rows were selected; set the identifer for
*                         the selected rows to null, and, if required,
*                         the identifier to the rejected rows to null.
*                         Also set the number of rejected rows to null.

                           SI = CAT__NOID
                           NUMSEL = 0

                           IF (REJFLG) THEN
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        END IF
                     END IF
                  ELSE

*                   The column is neither ascending nor descending; set
*                   the status and report an error.  Note that the
*                   message text varies depending on whether the column
*                   is unsorted or an invalid sort code has been
*                   entered.

                     STATUS = CAT__INVSR

                     ERRTXT = ' '
                     ERRPOS = 0

                     CALL CHR_PUTC ('Invalid range selection ',
     :                 ERRTXT, ERRPOS)

                     IF (ORDER .EQ. CAT__NOORD) THEN
                        CALL CHR_PUTC ('(unsorted column).',
     :                    ERRTXT, ERRPOS)
                     ELSE
                        CALL CHR_PUTC ('(unknown sort code: ',
     :                    ERRTXT, ERRPOS)
                        CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
                        CALL CHR_PUTC (').', ERRTXT, ERRPOS)
                     END IF

                     CALL CAT1_ERREP ('CAT_SFNDB_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SFNDB_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SFNDB_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SFNDB_INCT', 'The given '/
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

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDB: error selecting range; ',
     :        ERRTXT, ERRPOS)
            CALL CAT1_PUTB (MINRNG, ERRTXT, ERRPOS)
C           call chr_putc ('minrng', expr, lexpr)
            CALL CHR_PUTC (' < ', ERRTXT, ERRPOS)

            LSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', CNAME, LSTAT)
            IF (CNAME .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), ERRTXT, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRTXT, ERRPOS)
            END IF

            CALL CHR_PUTC (' <= ', ERRTXT, ERRPOS)
            CALL CAT1_PUTB (MAXRNG, ERRTXT, ERRPOS)
C           call chr_putc ('maxrng', expr, lexpr)

            CALL CAT1_ERREP ('CAT_FNDB_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_SFNDC (CI, FI, MINRNG, MAXRNG, REJFLG,
     :  SI, NUMSEL, SIR, NUMREJ, STATUS)
*+
*  Name:
*     CAT_SFNDC
*  Purpose:
*     Create a selection of rows within a given range.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SFNDC (CI, FI, MINRNG, MAXRNG, REJFLG; SI, NUMSEL,
*       SIR, NUMREJ; STATUS)
*  Description:
*     Create a selection of rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     FI  =  INTEGER (Given)
*        Identifier to the column whose fields will be selected to
*        lie in the given range.  The column must be sorted into
*        ascending or descending order (and known by CAT to be so
*        sorted).
*     MINRNG  =  CHARACTER*(*) (Given)
*        Minimum value which a field must satisfy to be selected.
*     MAXRNG  =  CHARACTER*(*) (Given)
*        Maximum value which a field must satisfy to be selected.
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
*       Determine the type of the column identifier.
*       If the column identifier corresponds to a column then
*         Determine the parent catalogue of the column.
*         If the input catalogue and the parent catalogue of the
*         column are the same then
*           Determine the number of rows in the catalogue.
*           Determine the order of the column.
*           If the column is either ascending or descending then
*             Determine the row corresponding to the lower limit.
*             Determine the row corresponding to the upper limit.
*             If ok then
*               If any rows were selected then
*                 Switch the rows so that they are in ascending row
*                 order.
*                 Force the rows to lie in the catalogue bounds.
*                 Compute the number of rows selected.
*                 Map sufficient work space for the selected rows.
*                 If required then
*                   Map sufficient work space for the rejected rows.
*                 end if
*                 Generate the lists of selected and rejected rows.
*                 Generate the expression corresponding to the
*                 selection specified.
*                 Attempt to create an identifier for the selected
*                 objects.
*                 If required then
*                   Attempt to create an identifier for the rejected
*                   objects.
*                 else
*                   Set the rejection identifier to null.
*                   Set the number of rejected objects to zero.
*                 end if
*               else
*                 Set the selection identifier to null.
*                 Set the number of rows selected to 0.
*                 Set the rejection identifier to null.
*                 Set the number of rows rejected to 0.
*               end if
*             end if
*           else
*             Set the status.
*             Report error; attempting range selection on an
*             unsorted column.
*           end if
*         else
*           Set the status.
*           Report error; input catalogue and column do not correspond
*           to the same catalogue.
*         end if
*       else
*         Set the status.
*         Report error; the given column identifier does not correspond
*         to a column.
*       end if
*     else
*       Set the status.
*       Report error; the input catalogue identifer does not correspond
*       to a catalogue or a selection.
*     end if
*     If any error occurred then
*       Set the returned identifiers to null.
*       Set the returned number of rows to zero.
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     Only works on sorted columns, not indexed columns.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/8/94 (ACD): Original version.
*     14/9/94 (ACD): First stable version.
*     17/2/95 (ACD): Fixed bug in error reporting.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     19/9/96 (ACD): Fixed bug in defining range of rows (which was
*        only manifest for descending columns).
*     17/3/00 (ACD): Fixed handling of the case where the rejected rows
*        were required, but no rows were rejected.
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
     :  FI
      CHARACTER*(*)
     :  MINRNG,
     :  MAXRNG
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
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  FIDTYP,  ! Type of the given identifier for the column.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  CIF,     ! Parent catalogue identifier for the column.
     :  ROWS,    ! Number of rows in the input cat. or selection.
     :  ORDER,   ! Order of the colunm (ascending, descening, none).
     :  MINROW,  ! Row corresponding to MINRNG.
     :  MAXROW,  !  "        "       "  MAXRNG
     :  BEGROW,  ! First (begining) selected row in the catalogue.
     :  ENDROW   ! Last  (end)         "      "  "   "      "    .
      INTEGER
     :  SELPTR,  ! Pointer to final list of selected rows.
     :  REJPTR,  !    "    "    "    "   "  rejected  "  .
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LEXPR,   !   "    "  EXPR   ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      LOGICAL
     :  LRJFLG   ! Local rejected rows flag.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP), ! Name of the column being selected.
     :  EXPR*(CAT__SZEXP)   ! Expression defining the selection.
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
*          Check that the column identifier actually corresponds to
*          a column.

            CALL CAT_TIDTP (FI, FIDTYP, STATUS)

            IF (FIDTYP .EQ. CAT__FITYP) THEN

*
*             Determine the parent catalogue of the column.

               CALL CAT_TIDPR (FI, CIF, STATUS)

*
*             Check that the column and the input catalogue or
*             selection correspond to the same catalogue.

               IF (CIF .EQ. CIC) THEN

*
*                Determine the number of rows in the input catalogue
*                or selection.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Determine the order of the column.

                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

*
*                Check that the column is in either ascending or
*                descending order.

                  IF (ORDER .EQ. CAT__ASCND  .OR.
     :              ORDER .EQ. CAT__DSCND) THEN

*
*                   Determine the rows in the catalogue (or selection)
*                   corresponding to the upper and lower limits and
*                   proceed if ok.

                     CALL CAT1_BHOPC (CI, FI, ROWS, ORDER, MINRNG,
     :                 MINROW, STATUS)
                     CALL CAT1_BHOPC (CI, FI, ROWS, ORDER, MAXRNG,
     :                 MAXROW, STATUS)

C                    print2201, minrow, maxrow
C2201                format(1x, 'minrow, maxrow: ', i5, i5)


                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        BEGROW = MIN(MINROW, MAXROW)
                        ENDROW = MAX(MINROW, MAXROW)
                        IF (ENDROW .LE. ROWS) THEN
                           ENDROW = ENDROW - 1
                        END IF

C                       print2202, begrow, endrow, rows
C2202                   format(1x, 'begrow, endrow, rows: ',
C    :                    i10, i10, i10)

                        IF (((BEGROW .GE. 1 .AND. BEGROW .LE. ROWS)  .OR
     :.
     :                      (ENDROW .GE. 1 .AND. ENDROW .LE. ROWS)  .OR.
     :                      (BEGROW .LT. 1 .AND. ENDROW .GT. ROWS))
     :                    .AND. (BEGROW .LE. ENDROW) ) THEN

*
*                         Force the selected rows to lie in the range
*                         of the catalogue.

                           BEGROW = MAX(BEGROW, 1)
                           ENDROW = MIN(ENDROW, ROWS)

*
*                         Compute the number of rows selected.

                           NUMSEL = ENDROW + 1 - BEGROW

*
*                         Map sufficient work space to hold the
*                         selected rows, and, if required, the
*                         rejected rows.

                           CALL CAT1_CRTAR (NUMSEL, '_INTEGER',
     :                       SELPTR, STATUS)

                           IF (REJFLG) THEN
                              NUMREJ = ROWS - NUMSEL

                              IF (NUMREJ .GT. 0) THEN
                                 LRJFLG = .TRUE.
                                 CALL CAT1_CRTAR (NUMREJ, '_INTEGER',
     :                             REJPTR, STATUS)
                              ELSE
                                 LRJFLG = .FALSE.
                                 REJPTR = 0
                              END IF
                           END IF

*
*                         Generate the lists of selected and rejected
*                         rows.

                           CALL CAT1_RNGLS (ROWS, BEGROW, ENDROW,
     :                       LRJFLG, NUMSEL, NUMREJ,
     :                       %VAL(CNF_PVAL(SELPTR)),
     :                       %VAL(CNF_PVAL(REJPTR)), STATUS)

*
*                         Generate the expression specifying the
*                         selection.  Note that this expression is
*                         stored as an attribute of the selection
*                         essentially as a comment; it is not used
*                         in any calculations.

                           EXPR = ' '
                           LEXPR = 0

                           CALL CAT_TIQAC (FI, 'NAME', CNAME, STATUS)
                           IF (CNAME .NE. ' ') THEN
                              LCNAME = CHR_LEN(CNAME)
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' > ', EXPR, LEXPR)
                           CALL CAT1_PUTC (MINRNG, EXPR, LEXPR)
C                          call chr_putc ('minrng', expr, lexpr)

                           CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                           IF (CNAME .NE. ' ') THEN
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' <= ', EXPR, LEXPR)
                           CALL CAT1_PUTC (MAXRNG, EXPR, LEXPR)
C                          call chr_putc ('maxrng', expr, lexpr)

*
*                         Attempt to create an identifier for the
*                         selected objects.

                           CALL CAT1_CRTSL (CIC, EXPR, .TRUE., NUMSEL,
     :                       SELPTR, SI, STATUS)

*
*                         If required and available, attempt to create
*                         an identifier for the rejected objects.
*                         Otherwise set the identifier for the rejected
*                         objects to null and the number of rejected
*                         objects to zero.

                           IF (LRJFLG) THEN
                              CALL CAT1_CRTSL (CIC, EXPR, .FALSE.,
     :                          NUMREJ, REJPTR, SIR, STATUS)
                           ELSE
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        ELSE

*
*                         No rows were selected; set the identifer for
*                         the selected rows to null, and, if required,
*                         the identifier to the rejected rows to null.
*                         Also set the number of rejected rows to null.

                           SI = CAT__NOID
                           NUMSEL = 0

                           IF (REJFLG) THEN
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        END IF
                     END IF
                  ELSE

*                   The column is neither ascending nor descending; set
*                   the status and report an error.  Note that the
*                   message text varies depending on whether the column
*                   is unsorted or an invalid sort code has been
*                   entered.

                     STATUS = CAT__INVSR

                     ERRTXT = ' '
                     ERRPOS = 0

                     CALL CHR_PUTC ('Invalid range selection ',
     :                 ERRTXT, ERRPOS)

                     IF (ORDER .EQ. CAT__NOORD) THEN
                        CALL CHR_PUTC ('(unsorted column).',
     :                    ERRTXT, ERRPOS)
                     ELSE
                        CALL CHR_PUTC ('(unknown sort code: ',
     :                    ERRTXT, ERRPOS)
                        CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
                        CALL CHR_PUTC (').', ERRTXT, ERRPOS)
                     END IF

                     CALL CAT1_ERREP ('CAT_SFNDC_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SFNDC_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SFNDC_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SFNDC_INCT', 'The given '/
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

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDC: error selecting range; ',
     :        ERRTXT, ERRPOS)
            CALL CAT1_PUTC (MINRNG, ERRTXT, ERRPOS)
C           call chr_putc ('minrng', expr, lexpr)
            CALL CHR_PUTC (' < ', ERRTXT, ERRPOS)

            LSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', CNAME, LSTAT)
            IF (CNAME .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), ERRTXT, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRTXT, ERRPOS)
            END IF

            CALL CHR_PUTC (' <= ', ERRTXT, ERRPOS)
            CALL CAT1_PUTC (MAXRNG, ERRTXT, ERRPOS)
C           call chr_putc ('maxrng', expr, lexpr)

            CALL CAT1_ERREP ('CAT_FNDC_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_SFNDD (CI, FI, MINRNG, MAXRNG, REJFLG,
     :  SI, NUMSEL, SIR, NUMREJ, STATUS)
*+
*  Name:
*     CAT_SFNDD
*  Purpose:
*     Create a selection of rows within a given range.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SFNDD (CI, FI, MINRNG, MAXRNG, REJFLG; SI, NUMSEL,
*       SIR, NUMREJ; STATUS)
*  Description:
*     Create a selection of rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     FI  =  INTEGER (Given)
*        Identifier to the column whose fields will be selected to
*        lie in the given range.  The column must be sorted into
*        ascending or descending order (and known by CAT to be so
*        sorted).
*     MINRNG  =  DOUBLE PRECISION (Given)
*        Minimum value which a field must satisfy to be selected.
*     MAXRNG  =  DOUBLE PRECISION (Given)
*        Maximum value which a field must satisfy to be selected.
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
*       Determine the type of the column identifier.
*       If the column identifier corresponds to a column then
*         Determine the parent catalogue of the column.
*         If the input catalogue and the parent catalogue of the
*         column are the same then
*           Determine the number of rows in the catalogue.
*           Determine the order of the column.
*           If the column is either ascending or descending then
*             Determine the row corresponding to the lower limit.
*             Determine the row corresponding to the upper limit.
*             If ok then
*               If any rows were selected then
*                 Switch the rows so that they are in ascending row
*                 order.
*                 Force the rows to lie in the catalogue bounds.
*                 Compute the number of rows selected.
*                 Map sufficient work space for the selected rows.
*                 If required then
*                   Map sufficient work space for the rejected rows.
*                 end if
*                 Generate the lists of selected and rejected rows.
*                 Generate the expression corresponding to the
*                 selection specified.
*                 Attempt to create an identifier for the selected
*                 objects.
*                 If required then
*                   Attempt to create an identifier for the rejected
*                   objects.
*                 else
*                   Set the rejection identifier to null.
*                   Set the number of rejected objects to zero.
*                 end if
*               else
*                 Set the selection identifier to null.
*                 Set the number of rows selected to 0.
*                 Set the rejection identifier to null.
*                 Set the number of rows rejected to 0.
*               end if
*             end if
*           else
*             Set the status.
*             Report error; attempting range selection on an
*             unsorted column.
*           end if
*         else
*           Set the status.
*           Report error; input catalogue and column do not correspond
*           to the same catalogue.
*         end if
*       else
*         Set the status.
*         Report error; the given column identifier does not correspond
*         to a column.
*       end if
*     else
*       Set the status.
*       Report error; the input catalogue identifer does not correspond
*       to a catalogue or a selection.
*     end if
*     If any error occurred then
*       Set the returned identifiers to null.
*       Set the returned number of rows to zero.
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     Only works on sorted columns, not indexed columns.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/8/94 (ACD): Original version.
*     14/9/94 (ACD): First stable version.
*     17/2/95 (ACD): Fixed bug in error reporting.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     19/9/96 (ACD): Fixed bug in defining range of rows (which was
*        only manifest for descending columns).
*     17/3/00 (ACD): Fixed handling of the case where the rejected rows
*        were required, but no rows were rejected.
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
     :  FI
      DOUBLE PRECISION
     :  MINRNG,
     :  MAXRNG
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
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  FIDTYP,  ! Type of the given identifier for the column.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  CIF,     ! Parent catalogue identifier for the column.
     :  ROWS,    ! Number of rows in the input cat. or selection.
     :  ORDER,   ! Order of the colunm (ascending, descening, none).
     :  MINROW,  ! Row corresponding to MINRNG.
     :  MAXROW,  !  "        "       "  MAXRNG
     :  BEGROW,  ! First (begining) selected row in the catalogue.
     :  ENDROW   ! Last  (end)         "      "  "   "      "    .
      INTEGER
     :  SELPTR,  ! Pointer to final list of selected rows.
     :  REJPTR,  !    "    "    "    "   "  rejected  "  .
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LEXPR,   !   "    "  EXPR   ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      LOGICAL
     :  LRJFLG   ! Local rejected rows flag.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP), ! Name of the column being selected.
     :  EXPR*(CAT__SZEXP)   ! Expression defining the selection.
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
*          Check that the column identifier actually corresponds to
*          a column.

            CALL CAT_TIDTP (FI, FIDTYP, STATUS)

            IF (FIDTYP .EQ. CAT__FITYP) THEN

*
*             Determine the parent catalogue of the column.

               CALL CAT_TIDPR (FI, CIF, STATUS)

*
*             Check that the column and the input catalogue or
*             selection correspond to the same catalogue.

               IF (CIF .EQ. CIC) THEN

*
*                Determine the number of rows in the input catalogue
*                or selection.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Determine the order of the column.

                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

*
*                Check that the column is in either ascending or
*                descending order.

                  IF (ORDER .EQ. CAT__ASCND  .OR.
     :              ORDER .EQ. CAT__DSCND) THEN

*
*                   Determine the rows in the catalogue (or selection)
*                   corresponding to the upper and lower limits and
*                   proceed if ok.

                     CALL CAT1_BHOPD (CI, FI, ROWS, ORDER, MINRNG,
     :                 MINROW, STATUS)
                     CALL CAT1_BHOPD (CI, FI, ROWS, ORDER, MAXRNG,
     :                 MAXROW, STATUS)

C                    print2201, minrow, maxrow
C2201                format(1x, 'minrow, maxrow: ', i5, i5)


                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        BEGROW = MIN(MINROW, MAXROW)
                        ENDROW = MAX(MINROW, MAXROW)
                        IF (ENDROW .LE. ROWS) THEN
                           ENDROW = ENDROW - 1
                        END IF

C                       print2202, begrow, endrow, rows
C2202                   format(1x, 'begrow, endrow, rows: ',
C    :                    i10, i10, i10)

                        IF (((BEGROW .GE. 1 .AND. BEGROW .LE. ROWS)  .OR
     :.
     :                      (ENDROW .GE. 1 .AND. ENDROW .LE. ROWS)  .OR.
     :                      (BEGROW .LT. 1 .AND. ENDROW .GT. ROWS))
     :                    .AND. (BEGROW .LE. ENDROW) ) THEN

*
*                         Force the selected rows to lie in the range
*                         of the catalogue.

                           BEGROW = MAX(BEGROW, 1)
                           ENDROW = MIN(ENDROW, ROWS)

*
*                         Compute the number of rows selected.

                           NUMSEL = ENDROW + 1 - BEGROW

*
*                         Map sufficient work space to hold the
*                         selected rows, and, if required, the
*                         rejected rows.

                           CALL CAT1_CRTAR (NUMSEL, '_INTEGER',
     :                       SELPTR, STATUS)

                           IF (REJFLG) THEN
                              NUMREJ = ROWS - NUMSEL

                              IF (NUMREJ .GT. 0) THEN
                                 LRJFLG = .TRUE.
                                 CALL CAT1_CRTAR (NUMREJ, '_INTEGER',
     :                             REJPTR, STATUS)
                              ELSE
                                 LRJFLG = .FALSE.
                                 REJPTR = 0
                              END IF
                           END IF

*
*                         Generate the lists of selected and rejected
*                         rows.

                           CALL CAT1_RNGLS (ROWS, BEGROW, ENDROW,
     :                       LRJFLG, NUMSEL, NUMREJ,
     :                       %VAL(CNF_PVAL(SELPTR)),
     :                       %VAL(CNF_PVAL(REJPTR)), STATUS)

*
*                         Generate the expression specifying the
*                         selection.  Note that this expression is
*                         stored as an attribute of the selection
*                         essentially as a comment; it is not used
*                         in any calculations.

                           EXPR = ' '
                           LEXPR = 0

                           CALL CAT_TIQAC (FI, 'NAME', CNAME, STATUS)
                           IF (CNAME .NE. ' ') THEN
                              LCNAME = CHR_LEN(CNAME)
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' > ', EXPR, LEXPR)
                           CALL CAT1_PUTD (MINRNG, EXPR, LEXPR)
C                          call chr_putc ('minrng', expr, lexpr)

                           CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                           IF (CNAME .NE. ' ') THEN
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' <= ', EXPR, LEXPR)
                           CALL CAT1_PUTD (MAXRNG, EXPR, LEXPR)
C                          call chr_putc ('maxrng', expr, lexpr)

*
*                         Attempt to create an identifier for the
*                         selected objects.

                           CALL CAT1_CRTSL (CIC, EXPR, .TRUE., NUMSEL,
     :                       SELPTR, SI, STATUS)

*
*                         If required and available, attempt to create
*                         an identifier for the rejected objects.
*                         Otherwise set the identifier for the rejected
*                         objects to null and the number of rejected
*                         objects to zero.

                           IF (LRJFLG) THEN
                              CALL CAT1_CRTSL (CIC, EXPR, .FALSE.,
     :                          NUMREJ, REJPTR, SIR, STATUS)
                           ELSE
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        ELSE

*
*                         No rows were selected; set the identifer for
*                         the selected rows to null, and, if required,
*                         the identifier to the rejected rows to null.
*                         Also set the number of rejected rows to null.

                           SI = CAT__NOID
                           NUMSEL = 0

                           IF (REJFLG) THEN
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        END IF
                     END IF
                  ELSE

*                   The column is neither ascending nor descending; set
*                   the status and report an error.  Note that the
*                   message text varies depending on whether the column
*                   is unsorted or an invalid sort code has been
*                   entered.

                     STATUS = CAT__INVSR

                     ERRTXT = ' '
                     ERRPOS = 0

                     CALL CHR_PUTC ('Invalid range selection ',
     :                 ERRTXT, ERRPOS)

                     IF (ORDER .EQ. CAT__NOORD) THEN
                        CALL CHR_PUTC ('(unsorted column).',
     :                    ERRTXT, ERRPOS)
                     ELSE
                        CALL CHR_PUTC ('(unknown sort code: ',
     :                    ERRTXT, ERRPOS)
                        CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
                        CALL CHR_PUTC (').', ERRTXT, ERRPOS)
                     END IF

                     CALL CAT1_ERREP ('CAT_SFNDD_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SFNDD_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SFNDD_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SFNDD_INCT', 'The given '/
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

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDD: error selecting range; ',
     :        ERRTXT, ERRPOS)
            CALL CAT1_PUTD (MINRNG, ERRTXT, ERRPOS)
C           call chr_putc ('minrng', expr, lexpr)
            CALL CHR_PUTC (' < ', ERRTXT, ERRPOS)

            LSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', CNAME, LSTAT)
            IF (CNAME .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), ERRTXT, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRTXT, ERRPOS)
            END IF

            CALL CHR_PUTC (' <= ', ERRTXT, ERRPOS)
            CALL CAT1_PUTD (MAXRNG, ERRTXT, ERRPOS)
C           call chr_putc ('maxrng', expr, lexpr)

            CALL CAT1_ERREP ('CAT_FNDD_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_SFNDI (CI, FI, MINRNG, MAXRNG, REJFLG,
     :  SI, NUMSEL, SIR, NUMREJ, STATUS)
*+
*  Name:
*     CAT_SFNDI
*  Purpose:
*     Create a selection of rows within a given range.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SFNDI (CI, FI, MINRNG, MAXRNG, REJFLG; SI, NUMSEL,
*       SIR, NUMREJ; STATUS)
*  Description:
*     Create a selection of rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     FI  =  INTEGER (Given)
*        Identifier to the column whose fields will be selected to
*        lie in the given range.  The column must be sorted into
*        ascending or descending order (and known by CAT to be so
*        sorted).
*     MINRNG  =  INTEGER (Given)
*        Minimum value which a field must satisfy to be selected.
*     MAXRNG  =  INTEGER (Given)
*        Maximum value which a field must satisfy to be selected.
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
*       Determine the type of the column identifier.
*       If the column identifier corresponds to a column then
*         Determine the parent catalogue of the column.
*         If the input catalogue and the parent catalogue of the
*         column are the same then
*           Determine the number of rows in the catalogue.
*           Determine the order of the column.
*           If the column is either ascending or descending then
*             Determine the row corresponding to the lower limit.
*             Determine the row corresponding to the upper limit.
*             If ok then
*               If any rows were selected then
*                 Switch the rows so that they are in ascending row
*                 order.
*                 Force the rows to lie in the catalogue bounds.
*                 Compute the number of rows selected.
*                 Map sufficient work space for the selected rows.
*                 If required then
*                   Map sufficient work space for the rejected rows.
*                 end if
*                 Generate the lists of selected and rejected rows.
*                 Generate the expression corresponding to the
*                 selection specified.
*                 Attempt to create an identifier for the selected
*                 objects.
*                 If required then
*                   Attempt to create an identifier for the rejected
*                   objects.
*                 else
*                   Set the rejection identifier to null.
*                   Set the number of rejected objects to zero.
*                 end if
*               else
*                 Set the selection identifier to null.
*                 Set the number of rows selected to 0.
*                 Set the rejection identifier to null.
*                 Set the number of rows rejected to 0.
*               end if
*             end if
*           else
*             Set the status.
*             Report error; attempting range selection on an
*             unsorted column.
*           end if
*         else
*           Set the status.
*           Report error; input catalogue and column do not correspond
*           to the same catalogue.
*         end if
*       else
*         Set the status.
*         Report error; the given column identifier does not correspond
*         to a column.
*       end if
*     else
*       Set the status.
*       Report error; the input catalogue identifer does not correspond
*       to a catalogue or a selection.
*     end if
*     If any error occurred then
*       Set the returned identifiers to null.
*       Set the returned number of rows to zero.
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     Only works on sorted columns, not indexed columns.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/8/94 (ACD): Original version.
*     14/9/94 (ACD): First stable version.
*     17/2/95 (ACD): Fixed bug in error reporting.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     19/9/96 (ACD): Fixed bug in defining range of rows (which was
*        only manifest for descending columns).
*     17/3/00 (ACD): Fixed handling of the case where the rejected rows
*        were required, but no rows were rejected.
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
     :  FI
      INTEGER
     :  MINRNG,
     :  MAXRNG
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
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  FIDTYP,  ! Type of the given identifier for the column.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  CIF,     ! Parent catalogue identifier for the column.
     :  ROWS,    ! Number of rows in the input cat. or selection.
     :  ORDER,   ! Order of the colunm (ascending, descening, none).
     :  MINROW,  ! Row corresponding to MINRNG.
     :  MAXROW,  !  "        "       "  MAXRNG
     :  BEGROW,  ! First (begining) selected row in the catalogue.
     :  ENDROW   ! Last  (end)         "      "  "   "      "    .
      INTEGER
     :  SELPTR,  ! Pointer to final list of selected rows.
     :  REJPTR,  !    "    "    "    "   "  rejected  "  .
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LEXPR,   !   "    "  EXPR   ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      LOGICAL
     :  LRJFLG   ! Local rejected rows flag.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP), ! Name of the column being selected.
     :  EXPR*(CAT__SZEXP)   ! Expression defining the selection.
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
*          Check that the column identifier actually corresponds to
*          a column.

            CALL CAT_TIDTP (FI, FIDTYP, STATUS)

            IF (FIDTYP .EQ. CAT__FITYP) THEN

*
*             Determine the parent catalogue of the column.

               CALL CAT_TIDPR (FI, CIF, STATUS)

*
*             Check that the column and the input catalogue or
*             selection correspond to the same catalogue.

               IF (CIF .EQ. CIC) THEN

*
*                Determine the number of rows in the input catalogue
*                or selection.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Determine the order of the column.

                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

*
*                Check that the column is in either ascending or
*                descending order.

                  IF (ORDER .EQ. CAT__ASCND  .OR.
     :              ORDER .EQ. CAT__DSCND) THEN

*
*                   Determine the rows in the catalogue (or selection)
*                   corresponding to the upper and lower limits and
*                   proceed if ok.

                     CALL CAT1_BHOPI (CI, FI, ROWS, ORDER, MINRNG,
     :                 MINROW, STATUS)
                     CALL CAT1_BHOPI (CI, FI, ROWS, ORDER, MAXRNG,
     :                 MAXROW, STATUS)

C                    print2201, minrow, maxrow
C2201                format(1x, 'minrow, maxrow: ', i5, i5)


                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        BEGROW = MIN(MINROW, MAXROW)
                        ENDROW = MAX(MINROW, MAXROW)
                        IF (ENDROW .LE. ROWS) THEN
                           ENDROW = ENDROW - 1
                        END IF

C                       print2202, begrow, endrow, rows
C2202                   format(1x, 'begrow, endrow, rows: ',
C    :                    i10, i10, i10)

                        IF (((BEGROW .GE. 1 .AND. BEGROW .LE. ROWS)  .OR
     :.
     :                      (ENDROW .GE. 1 .AND. ENDROW .LE. ROWS)  .OR.
     :                      (BEGROW .LT. 1 .AND. ENDROW .GT. ROWS))
     :                    .AND. (BEGROW .LE. ENDROW) ) THEN

*
*                         Force the selected rows to lie in the range
*                         of the catalogue.

                           BEGROW = MAX(BEGROW, 1)
                           ENDROW = MIN(ENDROW, ROWS)

*
*                         Compute the number of rows selected.

                           NUMSEL = ENDROW + 1 - BEGROW

*
*                         Map sufficient work space to hold the
*                         selected rows, and, if required, the
*                         rejected rows.

                           CALL CAT1_CRTAR (NUMSEL, '_INTEGER',
     :                       SELPTR, STATUS)

                           IF (REJFLG) THEN
                              NUMREJ = ROWS - NUMSEL

                              IF (NUMREJ .GT. 0) THEN
                                 LRJFLG = .TRUE.
                                 CALL CAT1_CRTAR (NUMREJ, '_INTEGER',
     :                             REJPTR, STATUS)
                              ELSE
                                 LRJFLG = .FALSE.
                                 REJPTR = 0
                              END IF
                           END IF

*
*                         Generate the lists of selected and rejected
*                         rows.

                           CALL CAT1_RNGLS (ROWS, BEGROW, ENDROW,
     :                       LRJFLG, NUMSEL, NUMREJ,
     :                       %VAL(CNF_PVAL(SELPTR)),
     :                       %VAL(CNF_PVAL(REJPTR)), STATUS)

*
*                         Generate the expression specifying the
*                         selection.  Note that this expression is
*                         stored as an attribute of the selection
*                         essentially as a comment; it is not used
*                         in any calculations.

                           EXPR = ' '
                           LEXPR = 0

                           CALL CAT_TIQAC (FI, 'NAME', CNAME, STATUS)
                           IF (CNAME .NE. ' ') THEN
                              LCNAME = CHR_LEN(CNAME)
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' > ', EXPR, LEXPR)
                           CALL CAT1_PUTI (MINRNG, EXPR, LEXPR)
C                          call chr_putc ('minrng', expr, lexpr)

                           CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                           IF (CNAME .NE. ' ') THEN
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' <= ', EXPR, LEXPR)
                           CALL CAT1_PUTI (MAXRNG, EXPR, LEXPR)
C                          call chr_putc ('maxrng', expr, lexpr)

*
*                         Attempt to create an identifier for the
*                         selected objects.

                           CALL CAT1_CRTSL (CIC, EXPR, .TRUE., NUMSEL,
     :                       SELPTR, SI, STATUS)

*
*                         If required and available, attempt to create
*                         an identifier for the rejected objects.
*                         Otherwise set the identifier for the rejected
*                         objects to null and the number of rejected
*                         objects to zero.

                           IF (LRJFLG) THEN
                              CALL CAT1_CRTSL (CIC, EXPR, .FALSE.,
     :                          NUMREJ, REJPTR, SIR, STATUS)
                           ELSE
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        ELSE

*
*                         No rows were selected; set the identifer for
*                         the selected rows to null, and, if required,
*                         the identifier to the rejected rows to null.
*                         Also set the number of rejected rows to null.

                           SI = CAT__NOID
                           NUMSEL = 0

                           IF (REJFLG) THEN
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        END IF
                     END IF
                  ELSE

*                   The column is neither ascending nor descending; set
*                   the status and report an error.  Note that the
*                   message text varies depending on whether the column
*                   is unsorted or an invalid sort code has been
*                   entered.

                     STATUS = CAT__INVSR

                     ERRTXT = ' '
                     ERRPOS = 0

                     CALL CHR_PUTC ('Invalid range selection ',
     :                 ERRTXT, ERRPOS)

                     IF (ORDER .EQ. CAT__NOORD) THEN
                        CALL CHR_PUTC ('(unsorted column).',
     :                    ERRTXT, ERRPOS)
                     ELSE
                        CALL CHR_PUTC ('(unknown sort code: ',
     :                    ERRTXT, ERRPOS)
                        CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
                        CALL CHR_PUTC (').', ERRTXT, ERRPOS)
                     END IF

                     CALL CAT1_ERREP ('CAT_SFNDI_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SFNDI_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SFNDI_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SFNDI_INCT', 'The given '/
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

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDI: error selecting range; ',
     :        ERRTXT, ERRPOS)
            CALL CAT1_PUTI (MINRNG, ERRTXT, ERRPOS)
C           call chr_putc ('minrng', expr, lexpr)
            CALL CHR_PUTC (' < ', ERRTXT, ERRPOS)

            LSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', CNAME, LSTAT)
            IF (CNAME .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), ERRTXT, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRTXT, ERRPOS)
            END IF

            CALL CHR_PUTC (' <= ', ERRTXT, ERRPOS)
            CALL CAT1_PUTI (MAXRNG, ERRTXT, ERRPOS)
C           call chr_putc ('maxrng', expr, lexpr)

            CALL CAT1_ERREP ('CAT_FNDI_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_SFNDL (CI, FI, MINRNG, MAXRNG, REJFLG,
     :  SI, NUMSEL, SIR, NUMREJ, STATUS)
*+
*  Name:
*     CAT_SFNDL
*  Purpose:
*     Create a selection of rows within a given range.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SFNDL (CI, FI, MINRNG, MAXRNG, REJFLG; SI, NUMSEL,
*       SIR, NUMREJ; STATUS)
*  Description:
*     Create a selection of rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     FI  =  INTEGER (Given)
*        Identifier to the column whose fields will be selected to
*        lie in the given range.  The column must be sorted into
*        ascending or descending order (and known by CAT to be so
*        sorted).
*     MINRNG  =  LOGICAL (Given)
*        Minimum value which a field must satisfy to be selected.
*     MAXRNG  =  LOGICAL (Given)
*        Maximum value which a field must satisfy to be selected.
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
*       Determine the type of the column identifier.
*       If the column identifier corresponds to a column then
*         Determine the parent catalogue of the column.
*         If the input catalogue and the parent catalogue of the
*         column are the same then
*           Determine the number of rows in the catalogue.
*           Determine the order of the column.
*           If the column is either ascending or descending then
*             Determine the row corresponding to the lower limit.
*             Determine the row corresponding to the upper limit.
*             If ok then
*               If any rows were selected then
*                 Switch the rows so that they are in ascending row
*                 order.
*                 Force the rows to lie in the catalogue bounds.
*                 Compute the number of rows selected.
*                 Map sufficient work space for the selected rows.
*                 If required then
*                   Map sufficient work space for the rejected rows.
*                 end if
*                 Generate the lists of selected and rejected rows.
*                 Generate the expression corresponding to the
*                 selection specified.
*                 Attempt to create an identifier for the selected
*                 objects.
*                 If required then
*                   Attempt to create an identifier for the rejected
*                   objects.
*                 else
*                   Set the rejection identifier to null.
*                   Set the number of rejected objects to zero.
*                 end if
*               else
*                 Set the selection identifier to null.
*                 Set the number of rows selected to 0.
*                 Set the rejection identifier to null.
*                 Set the number of rows rejected to 0.
*               end if
*             end if
*           else
*             Set the status.
*             Report error; attempting range selection on an
*             unsorted column.
*           end if
*         else
*           Set the status.
*           Report error; input catalogue and column do not correspond
*           to the same catalogue.
*         end if
*       else
*         Set the status.
*         Report error; the given column identifier does not correspond
*         to a column.
*       end if
*     else
*       Set the status.
*       Report error; the input catalogue identifer does not correspond
*       to a catalogue or a selection.
*     end if
*     If any error occurred then
*       Set the returned identifiers to null.
*       Set the returned number of rows to zero.
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     Only works on sorted columns, not indexed columns.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/8/94 (ACD): Original version.
*     14/9/94 (ACD): First stable version.
*     17/2/95 (ACD): Fixed bug in error reporting.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     19/9/96 (ACD): Fixed bug in defining range of rows (which was
*        only manifest for descending columns).
*     17/3/00 (ACD): Fixed handling of the case where the rejected rows
*        were required, but no rows were rejected.
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
     :  FI
      LOGICAL
     :  MINRNG,
     :  MAXRNG
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
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  FIDTYP,  ! Type of the given identifier for the column.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  CIF,     ! Parent catalogue identifier for the column.
     :  ROWS,    ! Number of rows in the input cat. or selection.
     :  ORDER,   ! Order of the colunm (ascending, descening, none).
     :  MINROW,  ! Row corresponding to MINRNG.
     :  MAXROW,  !  "        "       "  MAXRNG
     :  BEGROW,  ! First (begining) selected row in the catalogue.
     :  ENDROW   ! Last  (end)         "      "  "   "      "    .
      INTEGER
     :  SELPTR,  ! Pointer to final list of selected rows.
     :  REJPTR,  !    "    "    "    "   "  rejected  "  .
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LEXPR,   !   "    "  EXPR   ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      LOGICAL
     :  LRJFLG   ! Local rejected rows flag.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP), ! Name of the column being selected.
     :  EXPR*(CAT__SZEXP)   ! Expression defining the selection.
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
*          Check that the column identifier actually corresponds to
*          a column.

            CALL CAT_TIDTP (FI, FIDTYP, STATUS)

            IF (FIDTYP .EQ. CAT__FITYP) THEN

*
*             Determine the parent catalogue of the column.

               CALL CAT_TIDPR (FI, CIF, STATUS)

*
*             Check that the column and the input catalogue or
*             selection correspond to the same catalogue.

               IF (CIF .EQ. CIC) THEN

*
*                Determine the number of rows in the input catalogue
*                or selection.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Determine the order of the column.

                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

*
*                Check that the column is in either ascending or
*                descending order.

                  IF (ORDER .EQ. CAT__ASCND  .OR.
     :              ORDER .EQ. CAT__DSCND) THEN

*
*                   Determine the rows in the catalogue (or selection)
*                   corresponding to the upper and lower limits and
*                   proceed if ok.

                     CALL CAT1_BHOPL (CI, FI, ROWS, ORDER, MINRNG,
     :                 MINROW, STATUS)
                     CALL CAT1_BHOPL (CI, FI, ROWS, ORDER, MAXRNG,
     :                 MAXROW, STATUS)

C                    print2201, minrow, maxrow
C2201                format(1x, 'minrow, maxrow: ', i5, i5)


                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        BEGROW = MIN(MINROW, MAXROW)
                        ENDROW = MAX(MINROW, MAXROW)
                        IF (ENDROW .LE. ROWS) THEN
                           ENDROW = ENDROW - 1
                        END IF

C                       print2202, begrow, endrow, rows
C2202                   format(1x, 'begrow, endrow, rows: ',
C    :                    i10, i10, i10)

                        IF (((BEGROW .GE. 1 .AND. BEGROW .LE. ROWS)  .OR
     :.
     :                      (ENDROW .GE. 1 .AND. ENDROW .LE. ROWS)  .OR.
     :                      (BEGROW .LT. 1 .AND. ENDROW .GT. ROWS))
     :                    .AND. (BEGROW .LE. ENDROW) ) THEN

*
*                         Force the selected rows to lie in the range
*                         of the catalogue.

                           BEGROW = MAX(BEGROW, 1)
                           ENDROW = MIN(ENDROW, ROWS)

*
*                         Compute the number of rows selected.

                           NUMSEL = ENDROW + 1 - BEGROW

*
*                         Map sufficient work space to hold the
*                         selected rows, and, if required, the
*                         rejected rows.

                           CALL CAT1_CRTAR (NUMSEL, '_INTEGER',
     :                       SELPTR, STATUS)

                           IF (REJFLG) THEN
                              NUMREJ = ROWS - NUMSEL

                              IF (NUMREJ .GT. 0) THEN
                                 LRJFLG = .TRUE.
                                 CALL CAT1_CRTAR (NUMREJ, '_INTEGER',
     :                             REJPTR, STATUS)
                              ELSE
                                 LRJFLG = .FALSE.
                                 REJPTR = 0
                              END IF
                           END IF

*
*                         Generate the lists of selected and rejected
*                         rows.

                           CALL CAT1_RNGLS (ROWS, BEGROW, ENDROW,
     :                       LRJFLG, NUMSEL, NUMREJ,
     :                       %VAL(CNF_PVAL(SELPTR)),
     :                       %VAL(CNF_PVAL(REJPTR)), STATUS)

*
*                         Generate the expression specifying the
*                         selection.  Note that this expression is
*                         stored as an attribute of the selection
*                         essentially as a comment; it is not used
*                         in any calculations.

                           EXPR = ' '
                           LEXPR = 0

                           CALL CAT_TIQAC (FI, 'NAME', CNAME, STATUS)
                           IF (CNAME .NE. ' ') THEN
                              LCNAME = CHR_LEN(CNAME)
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' > ', EXPR, LEXPR)
                           CALL CAT1_PUTL (MINRNG, EXPR, LEXPR)
C                          call chr_putc ('minrng', expr, lexpr)

                           CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                           IF (CNAME .NE. ' ') THEN
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' <= ', EXPR, LEXPR)
                           CALL CAT1_PUTL (MAXRNG, EXPR, LEXPR)
C                          call chr_putc ('maxrng', expr, lexpr)

*
*                         Attempt to create an identifier for the
*                         selected objects.

                           CALL CAT1_CRTSL (CIC, EXPR, .TRUE., NUMSEL,
     :                       SELPTR, SI, STATUS)

*
*                         If required and available, attempt to create
*                         an identifier for the rejected objects.
*                         Otherwise set the identifier for the rejected
*                         objects to null and the number of rejected
*                         objects to zero.

                           IF (LRJFLG) THEN
                              CALL CAT1_CRTSL (CIC, EXPR, .FALSE.,
     :                          NUMREJ, REJPTR, SIR, STATUS)
                           ELSE
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        ELSE

*
*                         No rows were selected; set the identifer for
*                         the selected rows to null, and, if required,
*                         the identifier to the rejected rows to null.
*                         Also set the number of rejected rows to null.

                           SI = CAT__NOID
                           NUMSEL = 0

                           IF (REJFLG) THEN
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        END IF
                     END IF
                  ELSE

*                   The column is neither ascending nor descending; set
*                   the status and report an error.  Note that the
*                   message text varies depending on whether the column
*                   is unsorted or an invalid sort code has been
*                   entered.

                     STATUS = CAT__INVSR

                     ERRTXT = ' '
                     ERRPOS = 0

                     CALL CHR_PUTC ('Invalid range selection ',
     :                 ERRTXT, ERRPOS)

                     IF (ORDER .EQ. CAT__NOORD) THEN
                        CALL CHR_PUTC ('(unsorted column).',
     :                    ERRTXT, ERRPOS)
                     ELSE
                        CALL CHR_PUTC ('(unknown sort code: ',
     :                    ERRTXT, ERRPOS)
                        CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
                        CALL CHR_PUTC (').', ERRTXT, ERRPOS)
                     END IF

                     CALL CAT1_ERREP ('CAT_SFNDL_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SFNDL_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SFNDL_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SFNDL_INCT', 'The given '/
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

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDL: error selecting range; ',
     :        ERRTXT, ERRPOS)
            CALL CAT1_PUTL (MINRNG, ERRTXT, ERRPOS)
C           call chr_putc ('minrng', expr, lexpr)
            CALL CHR_PUTC (' < ', ERRTXT, ERRPOS)

            LSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', CNAME, LSTAT)
            IF (CNAME .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), ERRTXT, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRTXT, ERRPOS)
            END IF

            CALL CHR_PUTC (' <= ', ERRTXT, ERRPOS)
            CALL CAT1_PUTL (MAXRNG, ERRTXT, ERRPOS)
C           call chr_putc ('maxrng', expr, lexpr)

            CALL CAT1_ERREP ('CAT_FNDL_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_SFNDR (CI, FI, MINRNG, MAXRNG, REJFLG,
     :  SI, NUMSEL, SIR, NUMREJ, STATUS)
*+
*  Name:
*     CAT_SFNDR
*  Purpose:
*     Create a selection of rows within a given range.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SFNDR (CI, FI, MINRNG, MAXRNG, REJFLG; SI, NUMSEL,
*       SIR, NUMREJ; STATUS)
*  Description:
*     Create a selection of rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     FI  =  INTEGER (Given)
*        Identifier to the column whose fields will be selected to
*        lie in the given range.  The column must be sorted into
*        ascending or descending order (and known by CAT to be so
*        sorted).
*     MINRNG  =  REAL (Given)
*        Minimum value which a field must satisfy to be selected.
*     MAXRNG  =  REAL (Given)
*        Maximum value which a field must satisfy to be selected.
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
*       Determine the type of the column identifier.
*       If the column identifier corresponds to a column then
*         Determine the parent catalogue of the column.
*         If the input catalogue and the parent catalogue of the
*         column are the same then
*           Determine the number of rows in the catalogue.
*           Determine the order of the column.
*           If the column is either ascending or descending then
*             Determine the row corresponding to the lower limit.
*             Determine the row corresponding to the upper limit.
*             If ok then
*               If any rows were selected then
*                 Switch the rows so that they are in ascending row
*                 order.
*                 Force the rows to lie in the catalogue bounds.
*                 Compute the number of rows selected.
*                 Map sufficient work space for the selected rows.
*                 If required then
*                   Map sufficient work space for the rejected rows.
*                 end if
*                 Generate the lists of selected and rejected rows.
*                 Generate the expression corresponding to the
*                 selection specified.
*                 Attempt to create an identifier for the selected
*                 objects.
*                 If required then
*                   Attempt to create an identifier for the rejected
*                   objects.
*                 else
*                   Set the rejection identifier to null.
*                   Set the number of rejected objects to zero.
*                 end if
*               else
*                 Set the selection identifier to null.
*                 Set the number of rows selected to 0.
*                 Set the rejection identifier to null.
*                 Set the number of rows rejected to 0.
*               end if
*             end if
*           else
*             Set the status.
*             Report error; attempting range selection on an
*             unsorted column.
*           end if
*         else
*           Set the status.
*           Report error; input catalogue and column do not correspond
*           to the same catalogue.
*         end if
*       else
*         Set the status.
*         Report error; the given column identifier does not correspond
*         to a column.
*       end if
*     else
*       Set the status.
*       Report error; the input catalogue identifer does not correspond
*       to a catalogue or a selection.
*     end if
*     If any error occurred then
*       Set the returned identifiers to null.
*       Set the returned number of rows to zero.
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     Only works on sorted columns, not indexed columns.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/8/94 (ACD): Original version.
*     14/9/94 (ACD): First stable version.
*     17/2/95 (ACD): Fixed bug in error reporting.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     19/9/96 (ACD): Fixed bug in defining range of rows (which was
*        only manifest for descending columns).
*     17/3/00 (ACD): Fixed handling of the case where the rejected rows
*        were required, but no rows were rejected.
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
     :  FI
      REAL
     :  MINRNG,
     :  MAXRNG
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
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  FIDTYP,  ! Type of the given identifier for the column.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  CIF,     ! Parent catalogue identifier for the column.
     :  ROWS,    ! Number of rows in the input cat. or selection.
     :  ORDER,   ! Order of the colunm (ascending, descening, none).
     :  MINROW,  ! Row corresponding to MINRNG.
     :  MAXROW,  !  "        "       "  MAXRNG
     :  BEGROW,  ! First (begining) selected row in the catalogue.
     :  ENDROW   ! Last  (end)         "      "  "   "      "    .
      INTEGER
     :  SELPTR,  ! Pointer to final list of selected rows.
     :  REJPTR,  !    "    "    "    "   "  rejected  "  .
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LEXPR,   !   "    "  EXPR   ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      LOGICAL
     :  LRJFLG   ! Local rejected rows flag.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP), ! Name of the column being selected.
     :  EXPR*(CAT__SZEXP)   ! Expression defining the selection.
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
*          Check that the column identifier actually corresponds to
*          a column.

            CALL CAT_TIDTP (FI, FIDTYP, STATUS)

            IF (FIDTYP .EQ. CAT__FITYP) THEN

*
*             Determine the parent catalogue of the column.

               CALL CAT_TIDPR (FI, CIF, STATUS)

*
*             Check that the column and the input catalogue or
*             selection correspond to the same catalogue.

               IF (CIF .EQ. CIC) THEN

*
*                Determine the number of rows in the input catalogue
*                or selection.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Determine the order of the column.

                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

*
*                Check that the column is in either ascending or
*                descending order.

                  IF (ORDER .EQ. CAT__ASCND  .OR.
     :              ORDER .EQ. CAT__DSCND) THEN

*
*                   Determine the rows in the catalogue (or selection)
*                   corresponding to the upper and lower limits and
*                   proceed if ok.

                     CALL CAT1_BHOPR (CI, FI, ROWS, ORDER, MINRNG,
     :                 MINROW, STATUS)
                     CALL CAT1_BHOPR (CI, FI, ROWS, ORDER, MAXRNG,
     :                 MAXROW, STATUS)

C                    print2201, minrow, maxrow
C2201                format(1x, 'minrow, maxrow: ', i5, i5)


                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        BEGROW = MIN(MINROW, MAXROW)
                        ENDROW = MAX(MINROW, MAXROW)
                        IF (ENDROW .LE. ROWS) THEN
                           ENDROW = ENDROW - 1
                        END IF

C                       print2202, begrow, endrow, rows
C2202                   format(1x, 'begrow, endrow, rows: ',
C    :                    i10, i10, i10)

                        IF (((BEGROW .GE. 1 .AND. BEGROW .LE. ROWS)  .OR
     :.
     :                      (ENDROW .GE. 1 .AND. ENDROW .LE. ROWS)  .OR.
     :                      (BEGROW .LT. 1 .AND. ENDROW .GT. ROWS))
     :                    .AND. (BEGROW .LE. ENDROW) ) THEN

*
*                         Force the selected rows to lie in the range
*                         of the catalogue.

                           BEGROW = MAX(BEGROW, 1)
                           ENDROW = MIN(ENDROW, ROWS)

*
*                         Compute the number of rows selected.

                           NUMSEL = ENDROW + 1 - BEGROW

*
*                         Map sufficient work space to hold the
*                         selected rows, and, if required, the
*                         rejected rows.

                           CALL CAT1_CRTAR (NUMSEL, '_INTEGER',
     :                       SELPTR, STATUS)

                           IF (REJFLG) THEN
                              NUMREJ = ROWS - NUMSEL

                              IF (NUMREJ .GT. 0) THEN
                                 LRJFLG = .TRUE.
                                 CALL CAT1_CRTAR (NUMREJ, '_INTEGER',
     :                             REJPTR, STATUS)
                              ELSE
                                 LRJFLG = .FALSE.
                                 REJPTR = 0
                              END IF
                           END IF

*
*                         Generate the lists of selected and rejected
*                         rows.

                           CALL CAT1_RNGLS (ROWS, BEGROW, ENDROW,
     :                       LRJFLG, NUMSEL, NUMREJ,
     :                       %VAL(CNF_PVAL(SELPTR)),
     :                       %VAL(CNF_PVAL(REJPTR)), STATUS)

*
*                         Generate the expression specifying the
*                         selection.  Note that this expression is
*                         stored as an attribute of the selection
*                         essentially as a comment; it is not used
*                         in any calculations.

                           EXPR = ' '
                           LEXPR = 0

                           CALL CAT_TIQAC (FI, 'NAME', CNAME, STATUS)
                           IF (CNAME .NE. ' ') THEN
                              LCNAME = CHR_LEN(CNAME)
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' > ', EXPR, LEXPR)
                           CALL CAT1_PUTR (MINRNG, EXPR, LEXPR)
C                          call chr_putc ('minrng', expr, lexpr)

                           CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                           IF (CNAME .NE. ' ') THEN
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' <= ', EXPR, LEXPR)
                           CALL CAT1_PUTR (MAXRNG, EXPR, LEXPR)
C                          call chr_putc ('maxrng', expr, lexpr)

*
*                         Attempt to create an identifier for the
*                         selected objects.

                           CALL CAT1_CRTSL (CIC, EXPR, .TRUE., NUMSEL,
     :                       SELPTR, SI, STATUS)

*
*                         If required and available, attempt to create
*                         an identifier for the rejected objects.
*                         Otherwise set the identifier for the rejected
*                         objects to null and the number of rejected
*                         objects to zero.

                           IF (LRJFLG) THEN
                              CALL CAT1_CRTSL (CIC, EXPR, .FALSE.,
     :                          NUMREJ, REJPTR, SIR, STATUS)
                           ELSE
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        ELSE

*
*                         No rows were selected; set the identifer for
*                         the selected rows to null, and, if required,
*                         the identifier to the rejected rows to null.
*                         Also set the number of rejected rows to null.

                           SI = CAT__NOID
                           NUMSEL = 0

                           IF (REJFLG) THEN
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        END IF
                     END IF
                  ELSE

*                   The column is neither ascending nor descending; set
*                   the status and report an error.  Note that the
*                   message text varies depending on whether the column
*                   is unsorted or an invalid sort code has been
*                   entered.

                     STATUS = CAT__INVSR

                     ERRTXT = ' '
                     ERRPOS = 0

                     CALL CHR_PUTC ('Invalid range selection ',
     :                 ERRTXT, ERRPOS)

                     IF (ORDER .EQ. CAT__NOORD) THEN
                        CALL CHR_PUTC ('(unsorted column).',
     :                    ERRTXT, ERRPOS)
                     ELSE
                        CALL CHR_PUTC ('(unknown sort code: ',
     :                    ERRTXT, ERRPOS)
                        CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
                        CALL CHR_PUTC (').', ERRTXT, ERRPOS)
                     END IF

                     CALL CAT1_ERREP ('CAT_SFNDR_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SFNDR_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SFNDR_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SFNDR_INCT', 'The given '/
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

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDR: error selecting range; ',
     :        ERRTXT, ERRPOS)
            CALL CAT1_PUTR (MINRNG, ERRTXT, ERRPOS)
C           call chr_putc ('minrng', expr, lexpr)
            CALL CHR_PUTC (' < ', ERRTXT, ERRPOS)

            LSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', CNAME, LSTAT)
            IF (CNAME .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), ERRTXT, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRTXT, ERRPOS)
            END IF

            CALL CHR_PUTC (' <= ', ERRTXT, ERRPOS)
            CALL CAT1_PUTR (MAXRNG, ERRTXT, ERRPOS)
C           call chr_putc ('maxrng', expr, lexpr)

            CALL CAT1_ERREP ('CAT_FNDR_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_SFNDW (CI, FI, MINRNG, MAXRNG, REJFLG,
     :  SI, NUMSEL, SIR, NUMREJ, STATUS)
*+
*  Name:
*     CAT_SFNDW
*  Purpose:
*     Create a selection of rows within a given range.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SFNDW (CI, FI, MINRNG, MAXRNG, REJFLG; SI, NUMSEL,
*       SIR, NUMREJ; STATUS)
*  Description:
*     Create a selection of rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be created from either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Input catalogue or selection from which the new selection is
*        to be generated.  Note that CI may be either a catalogue or
*        a selection identifier.
*     FI  =  INTEGER (Given)
*        Identifier to the column whose fields will be selected to
*        lie in the given range.  The column must be sorted into
*        ascending or descending order (and known by CAT to be so
*        sorted).
*     MINRNG  =  INTEGER*2 (Given)
*        Minimum value which a field must satisfy to be selected.
*     MAXRNG  =  INTEGER*2 (Given)
*        Maximum value which a field must satisfy to be selected.
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
*       Determine the type of the column identifier.
*       If the column identifier corresponds to a column then
*         Determine the parent catalogue of the column.
*         If the input catalogue and the parent catalogue of the
*         column are the same then
*           Determine the number of rows in the catalogue.
*           Determine the order of the column.
*           If the column is either ascending or descending then
*             Determine the row corresponding to the lower limit.
*             Determine the row corresponding to the upper limit.
*             If ok then
*               If any rows were selected then
*                 Switch the rows so that they are in ascending row
*                 order.
*                 Force the rows to lie in the catalogue bounds.
*                 Compute the number of rows selected.
*                 Map sufficient work space for the selected rows.
*                 If required then
*                   Map sufficient work space for the rejected rows.
*                 end if
*                 Generate the lists of selected and rejected rows.
*                 Generate the expression corresponding to the
*                 selection specified.
*                 Attempt to create an identifier for the selected
*                 objects.
*                 If required then
*                   Attempt to create an identifier for the rejected
*                   objects.
*                 else
*                   Set the rejection identifier to null.
*                   Set the number of rejected objects to zero.
*                 end if
*               else
*                 Set the selection identifier to null.
*                 Set the number of rows selected to 0.
*                 Set the rejection identifier to null.
*                 Set the number of rows rejected to 0.
*               end if
*             end if
*           else
*             Set the status.
*             Report error; attempting range selection on an
*             unsorted column.
*           end if
*         else
*           Set the status.
*           Report error; input catalogue and column do not correspond
*           to the same catalogue.
*         end if
*       else
*         Set the status.
*         Report error; the given column identifier does not correspond
*         to a column.
*       end if
*     else
*       Set the status.
*       Report error; the input catalogue identifer does not correspond
*       to a catalogue or a selection.
*     end if
*     If any error occurred then
*       Set the returned identifiers to null.
*       Set the returned number of rows to zero.
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     Only works on sorted columns, not indexed columns.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/8/94 (ACD): Original version.
*     14/9/94 (ACD): First stable version.
*     17/2/95 (ACD): Fixed bug in error reporting.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     19/9/96 (ACD): Fixed bug in defining range of rows (which was
*        only manifest for descending columns).
*     17/3/00 (ACD): Fixed handling of the case where the rejected rows
*        were required, but no rows were rejected.
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
     :  FI
      INTEGER*2
     :  MINRNG,
     :  MAXRNG
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
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIDTYP,  ! Type of the given identifier for the input catalogue.
     :  FIDTYP,  ! Type of the given identifier for the column.
     :  CIC,     ! Parent catalogue identifier for the input catalogue.
     :  CIF,     ! Parent catalogue identifier for the column.
     :  ROWS,    ! Number of rows in the input cat. or selection.
     :  ORDER,   ! Order of the colunm (ascending, descening, none).
     :  MINROW,  ! Row corresponding to MINRNG.
     :  MAXROW,  !  "        "       "  MAXRNG
     :  BEGROW,  ! First (begining) selected row in the catalogue.
     :  ENDROW   ! Last  (end)         "      "  "   "      "    .
      INTEGER
     :  SELPTR,  ! Pointer to final list of selected rows.
     :  REJPTR,  !    "    "    "    "   "  rejected  "  .
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LEXPR,   !   "    "  EXPR   ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      LOGICAL
     :  LRJFLG   ! Local rejected rows flag.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP), ! Name of the column being selected.
     :  EXPR*(CAT__SZEXP)   ! Expression defining the selection.
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
*          Check that the column identifier actually corresponds to
*          a column.

            CALL CAT_TIDTP (FI, FIDTYP, STATUS)

            IF (FIDTYP .EQ. CAT__FITYP) THEN

*
*             Determine the parent catalogue of the column.

               CALL CAT_TIDPR (FI, CIF, STATUS)

*
*             Check that the column and the input catalogue or
*             selection correspond to the same catalogue.

               IF (CIF .EQ. CIC) THEN

*
*                Determine the number of rows in the input catalogue
*                or selection.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Determine the order of the column.

                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

*
*                Check that the column is in either ascending or
*                descending order.

                  IF (ORDER .EQ. CAT__ASCND  .OR.
     :              ORDER .EQ. CAT__DSCND) THEN

*
*                   Determine the rows in the catalogue (or selection)
*                   corresponding to the upper and lower limits and
*                   proceed if ok.

                     CALL CAT1_BHOPW (CI, FI, ROWS, ORDER, MINRNG,
     :                 MINROW, STATUS)
                     CALL CAT1_BHOPW (CI, FI, ROWS, ORDER, MAXRNG,
     :                 MAXROW, STATUS)

C                    print2201, minrow, maxrow
C2201                format(1x, 'minrow, maxrow: ', i5, i5)


                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        BEGROW = MIN(MINROW, MAXROW)
                        ENDROW = MAX(MINROW, MAXROW)
                        IF (ENDROW .LE. ROWS) THEN
                           ENDROW = ENDROW - 1
                        END IF

C                       print2202, begrow, endrow, rows
C2202                   format(1x, 'begrow, endrow, rows: ',
C    :                    i10, i10, i10)

                        IF (((BEGROW .GE. 1 .AND. BEGROW .LE. ROWS)  .OR
     :.
     :                      (ENDROW .GE. 1 .AND. ENDROW .LE. ROWS)  .OR.
     :                      (BEGROW .LT. 1 .AND. ENDROW .GT. ROWS))
     :                    .AND. (BEGROW .LE. ENDROW) ) THEN

*
*                         Force the selected rows to lie in the range
*                         of the catalogue.

                           BEGROW = MAX(BEGROW, 1)
                           ENDROW = MIN(ENDROW, ROWS)

*
*                         Compute the number of rows selected.

                           NUMSEL = ENDROW + 1 - BEGROW

*
*                         Map sufficient work space to hold the
*                         selected rows, and, if required, the
*                         rejected rows.

                           CALL CAT1_CRTAR (NUMSEL, '_INTEGER',
     :                       SELPTR, STATUS)

                           IF (REJFLG) THEN
                              NUMREJ = ROWS - NUMSEL

                              IF (NUMREJ .GT. 0) THEN
                                 LRJFLG = .TRUE.
                                 CALL CAT1_CRTAR (NUMREJ, '_INTEGER',
     :                             REJPTR, STATUS)
                              ELSE
                                 LRJFLG = .FALSE.
                                 REJPTR = 0
                              END IF
                           END IF

*
*                         Generate the lists of selected and rejected
*                         rows.

                           CALL CAT1_RNGLS (ROWS, BEGROW, ENDROW,
     :                       LRJFLG, NUMSEL, NUMREJ,
     :                       %VAL(CNF_PVAL(SELPTR)),
     :                       %VAL(CNF_PVAL(REJPTR)), STATUS)

*
*                         Generate the expression specifying the
*                         selection.  Note that this expression is
*                         stored as an attribute of the selection
*                         essentially as a comment; it is not used
*                         in any calculations.

                           EXPR = ' '
                           LEXPR = 0

                           CALL CAT_TIQAC (FI, 'NAME', CNAME, STATUS)
                           IF (CNAME .NE. ' ') THEN
                              LCNAME = CHR_LEN(CNAME)
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' > ', EXPR, LEXPR)
                           CALL CAT1_PUTW (MINRNG, EXPR, LEXPR)
C                          call chr_putc ('minrng', expr, lexpr)

                           CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                           IF (CNAME .NE. ' ') THEN
                              CALL CHR_PUTC (CNAME(1 : LCNAME), EXPR,
     :                          LEXPR)
                           ELSE
                              CALL CHR_PUTC ('<unknown>', EXPR, LEXPR)
                           END IF

                           CALL CHR_PUTC (' <= ', EXPR, LEXPR)
                           CALL CAT1_PUTW (MAXRNG, EXPR, LEXPR)
C                          call chr_putc ('maxrng', expr, lexpr)

*
*                         Attempt to create an identifier for the
*                         selected objects.

                           CALL CAT1_CRTSL (CIC, EXPR, .TRUE., NUMSEL,
     :                       SELPTR, SI, STATUS)

*
*                         If required and available, attempt to create
*                         an identifier for the rejected objects.
*                         Otherwise set the identifier for the rejected
*                         objects to null and the number of rejected
*                         objects to zero.

                           IF (LRJFLG) THEN
                              CALL CAT1_CRTSL (CIC, EXPR, .FALSE.,
     :                          NUMREJ, REJPTR, SIR, STATUS)
                           ELSE
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        ELSE

*
*                         No rows were selected; set the identifer for
*                         the selected rows to null, and, if required,
*                         the identifier to the rejected rows to null.
*                         Also set the number of rejected rows to null.

                           SI = CAT__NOID
                           NUMSEL = 0

                           IF (REJFLG) THEN
                              SIR = CAT__NOID
                              NUMREJ = 0
                           END IF
                        END IF
                     END IF
                  ELSE

*                   The column is neither ascending nor descending; set
*                   the status and report an error.  Note that the
*                   message text varies depending on whether the column
*                   is unsorted or an invalid sort code has been
*                   entered.

                     STATUS = CAT__INVSR

                     ERRTXT = ' '
                     ERRPOS = 0

                     CALL CHR_PUTC ('Invalid range selection ',
     :                 ERRTXT, ERRPOS)

                     IF (ORDER .EQ. CAT__NOORD) THEN
                        CALL CHR_PUTC ('(unsorted column).',
     :                    ERRTXT, ERRPOS)
                     ELSE
                        CALL CHR_PUTC ('(unknown sort code: ',
     :                    ERRTXT, ERRPOS)
                        CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
                        CALL CHR_PUTC (').', ERRTXT, ERRPOS)
                     END IF

                     CALL CAT1_ERREP ('CAT_SFNDW_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SFNDW_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SFNDW_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SFNDW_INCT', 'The given '/
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

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDW: error selecting range; ',
     :        ERRTXT, ERRPOS)
            CALL CAT1_PUTW (MINRNG, ERRTXT, ERRPOS)
C           call chr_putc ('minrng', expr, lexpr)
            CALL CHR_PUTC (' < ', ERRTXT, ERRPOS)

            LSTAT = CAT__OK
            CALL CAT_TIQAC (FI, 'NAME', CNAME, LSTAT)
            IF (CNAME .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), ERRTXT, ERRPOS)
            ELSE
               CALL CHR_PUTC ('<unknown>', ERRTXT, ERRPOS)
            END IF

            CALL CHR_PUTC (' <= ', ERRTXT, ERRPOS)
            CALL CAT1_PUTW (MAXRNG, ERRTXT, ERRPOS)
C           call chr_putc ('maxrng', expr, lexpr)

            CALL CAT1_ERREP ('CAT_FNDW_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
