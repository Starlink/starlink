      SUBROUTINE CAT_SRNGB (CI, FI, MINRNG, MAXRNG, FIRSTR, LASTR,
     :  STATUS)
*+
*  Name:
*     CAT_SRNGB
*  Purpose:
*     Get the rows corresponding to a range for a sorted column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SRNGB (CI, FI, MINRNG, MAXRNG; FIRSTR, LASTR;
*       STATUS)
*  Description:
*     Get the first and last rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be performed on either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*
*     If there are no rows in the specified range then FIRSTR and LASTR
*     are returned both set zero.
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
*     FIRSTR  =  INTEGER (Returned)
*        First row in the specified range.
*     LASTR  =  INTEGER (Returned)
*        Last row in the specified range.
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
*               else
*                 Set both rows to zero.
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
*     9/2/95  (ACD): Original version (based on CAT_SFNDB).
*     17/2/95 (ACD): Fixed bug in error reporting.
*     1/5/95  (ACD): Fixed bug in the 'Invocation' section of the
*       prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI
      BYTE
     :  MINRNG,
     :  MAXRNG
*  Arguments Returned:
      INTEGER
     :  FIRSTR,
     :  LASTR
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
     :  MAXROW   !  "        "       "  MAXRNG
      INTEGER
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP)  ! Name of the column being selected.
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

C                    print4000, minrow, maxrow
C4000                format(1x, 'minrow, maxrow: ', i6, i6 )

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        IF (MINROW .LE. ROWS  .OR.  MAXROW .GE. 1) THEN

*
*                         Select the first and last rows of the
*                         selection and force them to lie in the
*                         range of the catalogue.

                           FIRSTR = MIN(MINROW, MAXROW)
                           LASTR = MAX(MINROW, MAXROW)

                           FIRSTR = MAX(FIRSTR, 1)
                           LASTR = MIN(LASTR, ROWS)

                        ELSE
                           FIRSTR = 0
                           LASTR = 0

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

                     CALL CHR_PUTC ('Invalid range determination ',
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

                     CALL CAT1_ERREP ('CAT_SRNGB_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SRNGB_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SRNGB_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SRNGB_INCT', 'The given '/
     :        /'catalogue identifier does not correspond to a '/
     :        /'catalogue or selection.', STATUS)
         END IF

*
*       If any error occurred then set the selections to null and
*       report the error.

         IF (STATUS .NE. CAT__OK) THEN
            FIRSTR = 0
            LASTR = 0

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDB: error determining range; ',
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
      SUBROUTINE CAT_SRNGC (CI, FI, MINRNG, MAXRNG, FIRSTR, LASTR,
     :  STATUS)
*+
*  Name:
*     CAT_SRNGC
*  Purpose:
*     Get the rows corresponding to a range for a sorted column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SRNGC (CI, FI, MINRNG, MAXRNG; FIRSTR, LASTR;
*       STATUS)
*  Description:
*     Get the first and last rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be performed on either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*
*     If there are no rows in the specified range then FIRSTR and LASTR
*     are returned both set zero.
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
*     FIRSTR  =  INTEGER (Returned)
*        First row in the specified range.
*     LASTR  =  INTEGER (Returned)
*        Last row in the specified range.
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
*               else
*                 Set both rows to zero.
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
*     9/2/95  (ACD): Original version (based on CAT_SFNDC).
*     17/2/95 (ACD): Fixed bug in error reporting.
*     1/5/95  (ACD): Fixed bug in the 'Invocation' section of the
*       prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI
      CHARACTER*(*)
     :  MINRNG,
     :  MAXRNG
*  Arguments Returned:
      INTEGER
     :  FIRSTR,
     :  LASTR
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
     :  MAXROW   !  "        "       "  MAXRNG
      INTEGER
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP)  ! Name of the column being selected.
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

C                    print4000, minrow, maxrow
C4000                format(1x, 'minrow, maxrow: ', i6, i6 )

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        IF (MINROW .LE. ROWS  .OR.  MAXROW .GE. 1) THEN

*
*                         Select the first and last rows of the
*                         selection and force them to lie in the
*                         range of the catalogue.

                           FIRSTR = MIN(MINROW, MAXROW)
                           LASTR = MAX(MINROW, MAXROW)

                           FIRSTR = MAX(FIRSTR, 1)
                           LASTR = MIN(LASTR, ROWS)

                        ELSE
                           FIRSTR = 0
                           LASTR = 0

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

                     CALL CHR_PUTC ('Invalid range determination ',
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

                     CALL CAT1_ERREP ('CAT_SRNGC_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SRNGC_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SRNGC_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SRNGC_INCT', 'The given '/
     :        /'catalogue identifier does not correspond to a '/
     :        /'catalogue or selection.', STATUS)
         END IF

*
*       If any error occurred then set the selections to null and
*       report the error.

         IF (STATUS .NE. CAT__OK) THEN
            FIRSTR = 0
            LASTR = 0

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDC: error determining range; ',
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
      SUBROUTINE CAT_SRNGD (CI, FI, MINRNG, MAXRNG, FIRSTR, LASTR,
     :  STATUS)
*+
*  Name:
*     CAT_SRNGD
*  Purpose:
*     Get the rows corresponding to a range for a sorted column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SRNGD (CI, FI, MINRNG, MAXRNG; FIRSTR, LASTR;
*       STATUS)
*  Description:
*     Get the first and last rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be performed on either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*
*     If there are no rows in the specified range then FIRSTR and LASTR
*     are returned both set zero.
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
*     FIRSTR  =  INTEGER (Returned)
*        First row in the specified range.
*     LASTR  =  INTEGER (Returned)
*        Last row in the specified range.
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
*               else
*                 Set both rows to zero.
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
*     9/2/95  (ACD): Original version (based on CAT_SFNDD).
*     17/2/95 (ACD): Fixed bug in error reporting.
*     1/5/95  (ACD): Fixed bug in the 'Invocation' section of the
*       prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI
      DOUBLE PRECISION
     :  MINRNG,
     :  MAXRNG
*  Arguments Returned:
      INTEGER
     :  FIRSTR,
     :  LASTR
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
     :  MAXROW   !  "        "       "  MAXRNG
      INTEGER
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP)  ! Name of the column being selected.
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

C                    print4000, minrow, maxrow
C4000                format(1x, 'minrow, maxrow: ', i6, i6 )

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        IF (MINROW .LE. ROWS  .OR.  MAXROW .GE. 1) THEN

*
*                         Select the first and last rows of the
*                         selection and force them to lie in the
*                         range of the catalogue.

                           FIRSTR = MIN(MINROW, MAXROW)
                           LASTR = MAX(MINROW, MAXROW)

                           FIRSTR = MAX(FIRSTR, 1)
                           LASTR = MIN(LASTR, ROWS)

                        ELSE
                           FIRSTR = 0
                           LASTR = 0

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

                     CALL CHR_PUTC ('Invalid range determination ',
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

                     CALL CAT1_ERREP ('CAT_SRNGD_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SRNGD_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SRNGD_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SRNGD_INCT', 'The given '/
     :        /'catalogue identifier does not correspond to a '/
     :        /'catalogue or selection.', STATUS)
         END IF

*
*       If any error occurred then set the selections to null and
*       report the error.

         IF (STATUS .NE. CAT__OK) THEN
            FIRSTR = 0
            LASTR = 0

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDD: error determining range; ',
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
      SUBROUTINE CAT_SRNGI (CI, FI, MINRNG, MAXRNG, FIRSTR, LASTR,
     :  STATUS)
*+
*  Name:
*     CAT_SRNGI
*  Purpose:
*     Get the rows corresponding to a range for a sorted column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SRNGI (CI, FI, MINRNG, MAXRNG; FIRSTR, LASTR;
*       STATUS)
*  Description:
*     Get the first and last rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be performed on either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*
*     If there are no rows in the specified range then FIRSTR and LASTR
*     are returned both set zero.
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
*     FIRSTR  =  INTEGER (Returned)
*        First row in the specified range.
*     LASTR  =  INTEGER (Returned)
*        Last row in the specified range.
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
*               else
*                 Set both rows to zero.
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
*     9/2/95  (ACD): Original version (based on CAT_SFNDI).
*     17/2/95 (ACD): Fixed bug in error reporting.
*     1/5/95  (ACD): Fixed bug in the 'Invocation' section of the
*       prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI
      INTEGER
     :  MINRNG,
     :  MAXRNG
*  Arguments Returned:
      INTEGER
     :  FIRSTR,
     :  LASTR
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
     :  MAXROW   !  "        "       "  MAXRNG
      INTEGER
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP)  ! Name of the column being selected.
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

C                    print4000, minrow, maxrow
C4000                format(1x, 'minrow, maxrow: ', i6, i6 )

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        IF (MINROW .LE. ROWS  .OR.  MAXROW .GE. 1) THEN

*
*                         Select the first and last rows of the
*                         selection and force them to lie in the
*                         range of the catalogue.

                           FIRSTR = MIN(MINROW, MAXROW)
                           LASTR = MAX(MINROW, MAXROW)

                           FIRSTR = MAX(FIRSTR, 1)
                           LASTR = MIN(LASTR, ROWS)

                        ELSE
                           FIRSTR = 0
                           LASTR = 0

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

                     CALL CHR_PUTC ('Invalid range determination ',
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

                     CALL CAT1_ERREP ('CAT_SRNGI_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SRNGI_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SRNGI_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SRNGI_INCT', 'The given '/
     :        /'catalogue identifier does not correspond to a '/
     :        /'catalogue or selection.', STATUS)
         END IF

*
*       If any error occurred then set the selections to null and
*       report the error.

         IF (STATUS .NE. CAT__OK) THEN
            FIRSTR = 0
            LASTR = 0

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDI: error determining range; ',
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
      SUBROUTINE CAT_SRNGL (CI, FI, MINRNG, MAXRNG, FIRSTR, LASTR,
     :  STATUS)
*+
*  Name:
*     CAT_SRNGL
*  Purpose:
*     Get the rows corresponding to a range for a sorted column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SRNGL (CI, FI, MINRNG, MAXRNG; FIRSTR, LASTR;
*       STATUS)
*  Description:
*     Get the first and last rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be performed on either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*
*     If there are no rows in the specified range then FIRSTR and LASTR
*     are returned both set zero.
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
*     FIRSTR  =  INTEGER (Returned)
*        First row in the specified range.
*     LASTR  =  INTEGER (Returned)
*        Last row in the specified range.
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
*               else
*                 Set both rows to zero.
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
*     9/2/95  (ACD): Original version (based on CAT_SFNDL).
*     17/2/95 (ACD): Fixed bug in error reporting.
*     1/5/95  (ACD): Fixed bug in the 'Invocation' section of the
*       prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI
      LOGICAL
     :  MINRNG,
     :  MAXRNG
*  Arguments Returned:
      INTEGER
     :  FIRSTR,
     :  LASTR
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
     :  MAXROW   !  "        "       "  MAXRNG
      INTEGER
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP)  ! Name of the column being selected.
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

C                    print4000, minrow, maxrow
C4000                format(1x, 'minrow, maxrow: ', i6, i6 )

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        IF (MINROW .LE. ROWS  .OR.  MAXROW .GE. 1) THEN

*
*                         Select the first and last rows of the
*                         selection and force them to lie in the
*                         range of the catalogue.

                           FIRSTR = MIN(MINROW, MAXROW)
                           LASTR = MAX(MINROW, MAXROW)

                           FIRSTR = MAX(FIRSTR, 1)
                           LASTR = MIN(LASTR, ROWS)

                        ELSE
                           FIRSTR = 0
                           LASTR = 0

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

                     CALL CHR_PUTC ('Invalid range determination ',
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

                     CALL CAT1_ERREP ('CAT_SRNGL_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SRNGL_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SRNGL_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SRNGL_INCT', 'The given '/
     :        /'catalogue identifier does not correspond to a '/
     :        /'catalogue or selection.', STATUS)
         END IF

*
*       If any error occurred then set the selections to null and
*       report the error.

         IF (STATUS .NE. CAT__OK) THEN
            FIRSTR = 0
            LASTR = 0

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDL: error determining range; ',
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
      SUBROUTINE CAT_SRNGR (CI, FI, MINRNG, MAXRNG, FIRSTR, LASTR,
     :  STATUS)
*+
*  Name:
*     CAT_SRNGR
*  Purpose:
*     Get the rows corresponding to a range for a sorted column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SRNGR (CI, FI, MINRNG, MAXRNG; FIRSTR, LASTR;
*       STATUS)
*  Description:
*     Get the first and last rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be performed on either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*
*     If there are no rows in the specified range then FIRSTR and LASTR
*     are returned both set zero.
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
*     FIRSTR  =  INTEGER (Returned)
*        First row in the specified range.
*     LASTR  =  INTEGER (Returned)
*        Last row in the specified range.
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
*               else
*                 Set both rows to zero.
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
*     9/2/95  (ACD): Original version (based on CAT_SFNDR).
*     17/2/95 (ACD): Fixed bug in error reporting.
*     1/5/95  (ACD): Fixed bug in the 'Invocation' section of the
*       prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI
      REAL
     :  MINRNG,
     :  MAXRNG
*  Arguments Returned:
      INTEGER
     :  FIRSTR,
     :  LASTR
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
     :  MAXROW   !  "        "       "  MAXRNG
      INTEGER
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP)  ! Name of the column being selected.
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

C                    print4000, minrow, maxrow
C4000                format(1x, 'minrow, maxrow: ', i6, i6 )

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        IF (MINROW .LE. ROWS  .OR.  MAXROW .GE. 1) THEN

*
*                         Select the first and last rows of the
*                         selection and force them to lie in the
*                         range of the catalogue.

                           FIRSTR = MIN(MINROW, MAXROW)
                           LASTR = MAX(MINROW, MAXROW)

                           FIRSTR = MAX(FIRSTR, 1)
                           LASTR = MIN(LASTR, ROWS)

                        ELSE
                           FIRSTR = 0
                           LASTR = 0

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

                     CALL CHR_PUTC ('Invalid range determination ',
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

                     CALL CAT1_ERREP ('CAT_SRNGR_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SRNGR_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SRNGR_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SRNGR_INCT', 'The given '/
     :        /'catalogue identifier does not correspond to a '/
     :        /'catalogue or selection.', STATUS)
         END IF

*
*       If any error occurred then set the selections to null and
*       report the error.

         IF (STATUS .NE. CAT__OK) THEN
            FIRSTR = 0
            LASTR = 0

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDR: error determining range; ',
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
      SUBROUTINE CAT_SRNGW (CI, FI, MINRNG, MAXRNG, FIRSTR, LASTR,
     :  STATUS)
*+
*  Name:
*     CAT_SRNGW
*  Purpose:
*     Get the rows corresponding to a range for a sorted column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SRNGW (CI, FI, MINRNG, MAXRNG; FIRSTR, LASTR;
*       STATUS)
*  Description:
*     Get the first and last rows in a catalogue for which the fields
*     for a specified column lie in a given range.
*
*     A field is selected if it lies in the range:
*
*       field value .GE. MINRNG  .AND.  field value .LE. MAXRNG
*
*     The selection may be performed on either a genuine catalogue or
*     some previous selection from a catalogue.
*
*     The specified column must be sorted in ascending or descending
*     order (eventually the routine will be enhanced to work on
*     indexed columns too).
*
*     If there are no rows in the specified range then FIRSTR and LASTR
*     are returned both set zero.
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
*     FIRSTR  =  INTEGER (Returned)
*        First row in the specified range.
*     LASTR  =  INTEGER (Returned)
*        Last row in the specified range.
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
*               else
*                 Set both rows to zero.
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
*     9/2/95  (ACD): Original version (based on CAT_SFNDW).
*     17/2/95 (ACD): Fixed bug in error reporting.
*     1/5/95  (ACD): Fixed bug in the 'Invocation' section of the
*       prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI
      INTEGER*2
     :  MINRNG,
     :  MAXRNG
*  Arguments Returned:
      INTEGER
     :  FIRSTR,
     :  LASTR
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
     :  MAXROW   !  "        "       "  MAXRNG
      INTEGER
     :  ERRPOS,  ! Length of ERRTXT (excl. trail. blanks).
     :  LCNAME,  !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LSTAT    ! Local status.
      CHARACTER
     :  ERRTXT*75,          ! Text for error message.
     :  CNAME*(CAT__SZCMP)  ! Name of the column being selected.
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

C                    print4000, minrow, maxrow
C4000                format(1x, 'minrow, maxrow: ', i6, i6 )

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Check whether any rows were selected.

                        IF (MINROW .LE. ROWS  .OR.  MAXROW .GE. 1) THEN

*
*                         Select the first and last rows of the
*                         selection and force them to lie in the
*                         range of the catalogue.

                           FIRSTR = MIN(MINROW, MAXROW)
                           LASTR = MAX(MINROW, MAXROW)

                           FIRSTR = MAX(FIRSTR, 1)
                           LASTR = MIN(LASTR, ROWS)

                        ELSE
                           FIRSTR = 0
                           LASTR = 0

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

                     CALL CHR_PUTC ('Invalid range determination ',
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

                     CALL CAT1_ERREP ('CAT_SRNGW_INSR',
     :                 ERRTXT(1 : ERRPOS), STATUS)
                  END IF

               ELSE

*
*                The input cataloge or selection and the column do
*                not correspond to the same catalogue.  Set the status
*                and report an error.

                  STATUS = CAT__INVID

                  CALL CAT1_ERREP ('CAT_SRNGW_DIFF', 'The input '/
     :              /'and column correspond to different '/
     :              /'catalogues.', STATUS)
               END IF

            ELSE

*
*             The given column identifier does not correspond to a
*             column.  Set the status and report an error.

               STATUS = CAT__INVID

               CALL CAT1_ERREP ('CAT_SRNGW_INXP', 'The given '/
     :           /'column identifier does not correspond to a '/
     :           /'column.', STATUS)
            END IF

         ELSE

*
*          The input catalogue identifier does not correspond to a
*          catalogue or a selection.  Set the status and report an
*          error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_SRNGW_INCT', 'The given '/
     :        /'catalogue identifier does not correspond to a '/
     :        /'catalogue or selection.', STATUS)
         END IF

*
*       If any error occurred then set the selections to null and
*       report the error.

         IF (STATUS .NE. CAT__OK) THEN
            FIRSTR = 0
            LASTR = 0

            ERRPOS = 0
            ERRTXT = ' '

            CALL CHR_PUTC ('CAT_FNDW: error determining range; ',
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
