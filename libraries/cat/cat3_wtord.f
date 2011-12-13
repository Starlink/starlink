      SUBROUTINE CAT3_WTORD (CI, FITUNT, STATUS)
*+
*  Name:
*     CAT3_WTORD
*  Purpose:
*     Write the FITS keyword specifying the order of the table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_WTORD (CI, FITUNT; STATUS)
*  Description:
*     Write the FITS keyword specifying the order of the table.
*
*     The value of the keyword is simply the name of the column on
*     which it is sorted, optionally preceded by a minus sign.
*
*     If the column is in ascending order there is no minus sign.  If
*     it is in descending order there is a preceding minus sign.
*
*     If the table is not sorted on any column then the keyword is not
*     written.  If it is sorted simultaneously on several columns then
*     first encountered is written as the value of the keyword.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Do while (there are more columns to be examined)
*       Attempt to get an identifier for the next column.
*       If ok and the identifier is not null then
*         Get the order of the column.
*         If the column is in ascending or descending order then
*           Get the name of the column.
*           If the order keyword has not previously been created then
*             Assemble the value of the order keyword.
*             Attempt to write the order keyword.
*           else
*             Report a message: the order of the column will be
*             ignored.
*           end if
*         end if
*       end if
*       If the status is not ok or the identifier is null then
*         Set the termination flag.
*       end if
*     end do
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
*     8/9/94  (ACD): Original version.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     20/3/96 (ACD): Fixed minor bug; variable FITSTT was being
*        initialised explicitly to 0 instead of FITOK.
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
     :  FITUNT
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FCOUNT,   ! Sequence number of the current column.
     :  FI,       ! Identifier for the current column.
     :  ORDER,    ! Order attribute for the current column.
     :  FITSTT,   ! FITSIO status.
     :  LFNAME,   ! Length of FNAME  (excl. trail. blanks).
     :  LCNAME,   !   "    "  CNAME  ( "  .   "  .   "   ).
     :  LONAME,   !   "    "  ONAME  ( "  .   "  .   "   ).
     :  KEYLEN,   !   "    "  KEYVAL ( "  .   "  .   "   ).
     :  ERRLEN    !   "    "  ERRTXT ( "  .   "  .   "   ).
      CHARACTER
     :  CNAME*(CAT__SZCNM),    ! Catalogue name.
     :  FNAME*(CAT__SZCMP),    ! Name of the current column.
     :  ONAME*(CAT__SZCMP),    ! Name of the sorted (ordered) column.
     :  KEYVAL*(CAT__SZCMP+1), ! Value of FITS keyword.
     :  ERRTXT*75              ! Text of warning message.
      LOGICAL
     :  FSORT,    ! Flag; has sorted column keyword been written?
     :  MORE      ! Flag; more columns to be processed.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Examine every column.

         FSORT = .FALSE.
         FCOUNT = 0
         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Attempt to get an identifier for the next column and proceed
*          if all is ok and the identifier is not null.

            FCOUNT = FCOUNT + 1
            CALL CAT_TNDNT (CI, CAT__FITYP, FCOUNT, FI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*             Get the order of the column and check if it is in
*             ascending or descending order.

               CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

               IF (ORDER .EQ. CAT__ASCND  .OR.  ORDER .EQ. CAT__DSCND)
     :           THEN

*
*                Get the name of the column.

                  CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)

*
*                Check whether the order keyword has already been
*                created.

                  IF (.NOT. FSORT) THEN

*
*                   The keyword has not been created.  Assemble its
*                   value.  Remember that for an ascending column the
*                   value is just the name of the column.  For a
*                   descending column it is the name of the column
*                   preceded by a minus sign.

                     KEYLEN = 0
                     KEYVAL = ' '

                     IF (ORDER .EQ. CAT__DSCND) THEN
                        CALL CHR_PUTC ('-', KEYVAL, KEYLEN)
                     END IF

                     IF (FNAME .NE. ' ') THEN
                        LFNAME = CHR_LEN(FNAME)
                        CALL CHR_PUTC (FNAME(1 : LFNAME), KEYVAL,
     :                    KEYLEN)
                     ELSE
                        CALL CHR_PUTC ('<unknown>', KEYVAL, KEYLEN)
                     END IF

*
*                   Attempt to write the FITS keyword.

                     FITSTT = FITOK
                     CALL FTPKYS (FITUNT, 'TSORTKEY',
     :                 KEYVAL(1 : KEYLEN), 'Column on which the '/
     :                 /'table is sorted.', FITSTT)

                     IF (FITSTT .NE. FITOK) THEN
                        STATUS = CAT__ERROR

                        CALL CAT3_FITER ('CAT3_WTORD_ERR',
     :                    'Failure creating keyword TSORTKEY.', FITSTT,
     :                    STATUS)
                     END IF

*
*                   Set the flag indicating that the keyword has been
*                   written and save the name of the column.

                     FSORT = .TRUE.
                     ONAME = FNAME

                  ELSE

*
*                   The keyword defining the sort order has already
*                   been written.  Display warning messages indicating
*                   that the current column, though sorted, will not
*                   be used to define the sort order.

                     ERRTXT = ' '
                     ERRLEN = 0

                     CALL CHR_PUTC ('*warning* Catalogue ',
     :                 ERRTXT, ERRLEN)

                     CALL CAT_TIQAC (CI, 'NAME', CNAME, STATUS)
                     IF (CNAME .NE. ' ') THEN
                        LCNAME = CHR_LEN(CNAME)
                        CALL CHR_PUTC (CNAME(1 : LCNAME), ERRTXT,
     :                    ERRLEN)
                     ELSE
                        CALL CHR_PUTC ('<unknown>', ERRTXT, ERRLEN)
                     END IF

                     CALL CHR_PUTC (' is sorted on column ',
     :                 ERRTXT, ERRLEN)

                     IF (ONAME .NE. ' ') THEN
                        LONAME = CHR_LEN(ONAME)
                        CALL CHR_PUTC (ONAME(1 : LONAME), ERRTXT,
     :                    ERRLEN)
                     ELSE
                        CALL CHR_PUTC ('<unknown>', ERRTXT, ERRLEN)
                     END IF

                     CALL CHR_PUTC (';', ERRTXT, ERRLEN)

                     CALL CAT1_MSG (' ', ERRTXT(1 :ERRLEN), STATUS)

                     ERRTXT = ' '
                     ERRLEN = 0

                     CALL CHR_PUTC ('   the additional sorting on '/
     :                 /'column ', ERRTXT, ERRLEN)

                     IF (FNAME .NE. ' ') THEN
                        LFNAME = CHR_LEN(FNAME)
                        CALL CHR_PUTC (FNAME(1 : LFNAME), ERRTXT,
     :                    ERRLEN)
                     ELSE
                        CALL CHR_PUTC ('<unknown>', ERRTXT, ERRLEN)
                     END IF

                     CALL CHR_PUTC (' will be ignored.', ERRTXT, ERRLEN)

                     CALL CAT1_MSG (' ', ERRTXT(1 :ERRLEN), STATUS)
                  END IF
               END IF
            END IF

*
*          If the status is not ok or the identifier is null then set
*          the termination flag.

            IF (STATUS .NE. CAT__OK  .OR.  FI .EQ. CAT__NOID) THEN
               MORE = .FALSE.
            END IF
         END DO

      END IF

      END
