      SUBROUTINE CAT_INEW (FI, DISP, ORDER, II, STATUS)
*+
*  Name:
*     CAT_INEW
*  Purpose:
*     Create an index on a column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_INEW (FI, DISP, ORDER; II; STATUS)
*  Description:
*     Create an index on a column.
*
*     Currently only temporary indices are created.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the index will be
*        created.  The identifier may refer to either a scalar column
*        or a vector column element.
*     DISP  =  CHARACTER*(*) (Given)
*        The disposition of the index.  The possibilities are:
*        'TEMP'  -  temporary,
*        'PERM'  -  permanent.
*        Currently only temporary indices are implemented.
*     ORDER  =  INTEGER (Given)
*        Order of the index, coded as follows:
*        CAT__ASCND  -  ascending,
*        CAT__DSCND  -  descending.
*     II  =  INTEGER (Returned)
*        Identifier to the new index.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of the column identifier.
*     If the identifier corresponds to a scalar column or a vector
*     column element then
*       If the required order is either ascending or descending then
*         If the dispostion is 'TEMP' (temporary) then
*           Determine the parent catalogue of the column.
*           Determine the number of rows in the catalogue.
*           Create work space for the list of sorted rows.
*           Create temporary work space required by the sorting routine.
*           Generate the list of sorted rows.
*           Release the temporary work space.
*           If ok then
*             Create a new identifier for the index.
*             If ok then
*               Set the attributes of the index identifier.
*             end if
*           end if
*         else
*           Set the status.
*           Report an error: attempt to create an index which is not
*           temporary.
*         end if
*       else
*         Set the status.
*         Report an error: the specified sort order corresponded to
*         neither ascending nor descending.
*       end if
*     else
*       set the status.
*       Report an error: the given identifier does not correspond to a
*       scalar column or a vector column element.
*     end if
*  Implementation Deficiencies:
*     Only temporary indices can be created, not permanent ones.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
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
*     24/11/94 (ACD): Original version.
*     20/4/95  (ACD): First stable version.
*     10/11/95 (ACD): Enhanced to work on vector column elements in
*        addition to scalar columns.
*     4/4/01   (ACD): Corrected a typing mistake in the prologue comments.
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
     :  FI,
     :  ORDER
      CHARACTER
     :  DISP*(*)
*  Arguments Returned:
      INTEGER
     :  II
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Type of the given column identifier.
     :  CI,      ! Identifier to the parent catalogue.
     :  ROWS,    ! Number of rows in the catalogue.
     :  SORPTR,  ! Pointer to the array of sorted row numbers.
     :  NULPTR,  ! Pointer to list of null rows numbers.
     :  VALPTR,  ! Pointer to list of field values.
     :  SEQPTR,  ! Pointer to list of sequence numbers.
     :  COLPTR,  ! Pointer to list of valid row numbers.
     :  ERRLEN,  ! Length of ERRTXT (excl. trail. blanks).
     :  LDISP    !   "    "  DISP   ( "  .   "  .   "   ).
      CHARACTER
     :  ERRTXT*75  ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the given identifier to check that it
*       does indeed correspond to a scalar column or a vector column
*       element.

         CALL CAT_TIDTP (FI, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP) THEN

*
*          Check that the specified sort order corresponds to either
*          ascending or descending.

            IF (ORDER .EQ. CAT__ASCND  .OR.  ORDER .EQ. CAT__DSCND) THEN

*
*             Check that the disposition of the index is 'TEMP'
*             (temporary).  Currently on temporary indices are supported.

               IF (DISP .EQ. 'TEMP') THEN

*
*                Determine the parent catalogue of the column.

                  CALL CAT_TIDPR (FI, CI, STATUS)

*
*                Determine the number of rows in the catalogue.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Create work space for the list of sorted rows.

C                 print3000, rows
C3000             format(1x, 'No. of rows (prior to mapping) ' /
C    :              1x, 'rows: ', i10 / )


                  CALL CAT1_CRTAR (ROWS, '_INTEGER', SORPTR, STATUS)

*
*                Create temporary work space required by the sorting
*                routine.

                  CALL CAT1_CRTAR (ROWS, '_INTEGER', NULPTR, STATUS)
                  CALL CAT1_CRTAR (ROWS, '_DOUBLE', VALPTR, STATUS)
                  CALL CAT1_CRTAR (ROWS, '_INTEGER', COLPTR, STATUS)
                  CALL CAT1_CRTAR (ROWS, '_INTEGER', SEQPTR, STATUS)

*
*                Generate the list of sorted rows.

C                 print2000, ci, fi, rows, status
C2000             format(1x, 'before call to CAT1_GNSRT ' /
C    :              1x, 'ci, fi, rows, status: ', i5, i5, i5, i20)

                  CALL CAT1_GNSRT (CI, FI, ROWS, ORDER,
     :                             %VAL(CNF_PVAL(NULPTR)),
     :              %VAL(CNF_PVAL(VALPTR)), %VAL(CNF_PVAL(COLPTR)),
     :              %VAL(CNF_PVAL(SEQPTR)),
     :              %VAL(CNF_PVAL(SORPTR)), STATUS)

C                 print2001, ci, fi, rows, status
C2001             format(1x, 'after call to CAT1_GNSRT ' /
C    :              1x, 'ci, fi, rows, status: ', i5, i5, i5, i20)

*
*                Release the temporary work space.

                  CALL CAT1_FREAR (NULPTR, STATUS)
                  CALL CAT1_FREAR (VALPTR, STATUS)
                  CALL CAT1_FREAR (COLPTR, STATUS)
                  CALL CAT1_FREAR (SEQPTR, STATUS)

*
*                If all is ok the attempt to create an index identifier,
*                and if this succeeds then proceed to set its attributes.

                  IF (STATUS .EQ. CAT__OK) THEN
                     CALL CAT1_CRTID (CAT__IITYP, CI, II, STATUS)

                     IF (STATUS .EQ. CAT__OK) THEN
                        CALL CAT1_ADDAI (II, 'COLID', .FALSE., FI,
     :                    STATUS)
                        CALL CAT1_ADDAI (II, 'ORDER', .FALSE., ORDER,
     :                    STATUS)
                        CALL CAT1_ADDAI (II, 'NUMSEL', .FALSE., ROWS,
     :                    STATUS)
                        CALL CAT1_ADDAC (II, 'COMM', .FALSE.,
     :                    'Temporary index.', STATUS)
                        CALL CAT1_ADDAI (II, 'PTR', .FALSE., SORPTR,
     :                    STATUS)
                        CALL CAT1_ADDAD (II, 'DATE', .FALSE., 0.0D0,
     :                    STATUS)
                     END IF
                  END IF

               ELSE

*
*                An attempt was made to create an index which was not
*                temporary.  Set the status and report an error.

                  STATUS = CAT__ERROR

                  ERRTXT = ' '
                  ERRLEN = 0

                  CALL CHR_PUTC ('Invalid disposition specified for '/
     :              /'new index: ', ERRTXT, ERRLEN)

                  IF (DISP .NE. ' ') THEN
                     LDISP = CHR_LEN(DISP)
                     CALL CHR_PUTC (DISP(1 : LDISP), ERRTXT, ERRLEN)
                  ELSE
                     CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
                  END IF

                  CALL CAT1_ERREP ('CAT_INEW_DSP', ERRTXT(1 : ERRLEN),
     :              STATUS)
               END IF

            ELSE

*
*             The code specified for the sort order is invalid; set
*             the status and report an error.

               STATUS = CAT__ERROR

               ERRTXT = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('Sort code (', ERRTXT, ERRLEN)
               CALL CHR_PUTI (ORDER, ERRTXT, ERRLEN)
               CALL CHR_PUTC (') corresponds to neither ascending '/
     :           /'nor descending order.', ERRTXT, ERRLEN)

               CALL CAT1_ERREP ('CAT_INEW_ORD', ERRTXT(1 : ERRLEN),
     :           STATUS)

            END IF

         ELSE

*
*          The given identifier does not correspond to a scalar column;
*          set the status and report an error.

            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT_INEW_COL', 'type of the given '/
     :        /'identifier is not scalar column or vector column '/
     :        /'element.', STATUS)

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_INEW_ERR', 'CAT_INEW_ERR: error '/
     :        /'creating an index.', STATUS)
         END IF

      END IF

      END
