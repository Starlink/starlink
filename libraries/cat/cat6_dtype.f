      SUBROUTINE CAT6_DTYPE (NCOL, FIELDS, NULFLG, DTYPE, CWIDTH,
     :  DECPL, EXPFMT, STATUS)
*+
*  Name:
*     CAT6_DTYPE
*  Purpose:
*     Accumulate the data types for the columns in a tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_DTYPE (NCOL, FIELDS, NULFLG; DTYPE, CWIDTH, DECPL, EXPFMT;
*       STATUS)
*  Description:
*     Accumulate the data types for the columns in a tab-separated table.
*
*     A data type is determined for all the fields in the current row.
*     The current data type is compared with the accumulating one and
*     if appropriate substituted (in descending order the preferences
*     are: INTEGER, DOUBLE PRECISION, CHARACTER).  The width of
*     character columns is also calculated; a wider field replaces the
*     existing column width.
*  Arguments:
*     NCOL  =  INTEGER (Given)
*        Number of columns in the table.
*     FIELDS(NCOL)  =  CHARACTER*(*) (Given)
*        Values for the fields in the current row.
*     NULFLG(NCOL)  =  LOGICAL (Given)
*        Null value flags for the fields in the current row.
*     DTYPE(NCOL)  =  INTEGER (Given and Returned)
*        Accumulating data types for columns.
*     CWIDTH(NCOL)  =  INTEGER (Given and Returned)
*        Accumulating widths for CHARACTER columns.
*     DECPL(NCOL)  =  INTEGER (Given and Returned)
*        Accumulating numbers of decimal places for DOUBLE PRECISION
*        columns.
*     EXPFMT(NCOL)  =  LOGICAL (Given and Returned)
*        Accumulating number of Exponential format flags or DOUBLE
*        PRECISION columns.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every field
*       If the field is not null then
*         Attempt to decode the field value as an INTEGER
*         If ok then
*           Set the current data type = INTEGER.
*         else
*           Attempt to decode the field value as a DOUBLE PRECISION
*           If ok then
*             Set the current data type = DOUBLE PRECISION.
*           else
*             Set the current data type = CHARACTER.
*           end if
*         end if
*         Modify the accumulating data types if necessary.
*         Determine the length of the field.
*         Modify the accumulating length if necessary.
*         Determine the number of decimal places.
*         Modify the accummulating number of decimal places if necessary.
*         Check for any exponent.
*         Modify the accummulating  exponent flag if necessary.
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
*     20/5/99 (ACD): Original version.
*     25/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  NCOL
      CHARACTER
     :  FIELDS(NCOL)*(*)
      LOGICAL
     :  NULFLG(NCOL)
*  Arguments Given and Returned:
      INTEGER
     :  DTYPE(NCOL),
     :  CWIDTH(NCOL),
     :  DECPL(NCOL)
      LOGICAL
     :  EXPFMT(NCOL)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
      INTEGER CHR_INDEX
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  LSTAT,  ! Local status.
     :  IVAL,   ! INTEGER value decoded from current field.
     :  CTYPE,  ! Data type determined for the current field.
     :  FLDLEN, ! Length of current field (excl. trail. blanks).
     :  DECPOS, ! Position of any decimal point.
     :  CURPL   ! Number of decimal places.
      INTEGER
     :  ELPOS,  ! Position of lower case 'e' (exponent).
     :  EUPOS,  !    "     "  upper  "   'E' (   "    ).
     :  DLPOS,  !    "     "  lower  "   'd' (exponent).
     :  DUPOS   !    "     "  upper  "   'D' (   "    ).
      DOUBLE PRECISION
     :  DVAL    ! DOUBLE PRECISION value decoded from current field.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Examine every field.

         DO LOOP = 1, NCOL

*
*          Check that the field is not null.

            IF (.NOT. NULFLG(LOOP)) THEN

*
*             Attempt to decode an INTEGER.

               LSTAT = CAT__OK
               CALL CHR_CTOI (FIELDS(LOOP), IVAL, LSTAT)
               IF (LSTAT .EQ. CAT__OK) THEN
                  CTYPE = CAT__TYPEI
               ELSE

*
*                Attempt to decode a DOUBLE PRECISION.

                  LSTAT = CAT__OK
                  CALL CHR_CTOD (FIELDS(LOOP), DVAL, LSTAT)
                  IF (LSTAT .EQ. CAT__OK) THEN
                     CTYPE = CAT__TYPED
                  ELSE
                     CTYPE = CAT__TYPEC
                  END IF
               END IF

*
*             Modify the accumulating data type if necessary.  The
*             results table for the accumulating and current data
*             types is:
*
*                                       Accumulating
*                             Current | I  | DB | C  |
*                             --------+----+----+----+
*                                  I  | I  | DB | C  |
*                             --------+----+----+----+
*                                  DB | DB | DB | C  |
*                             --------+----+----+----+
*                                  C  | C  | C  | C  |
*                             --------+----+----+----+
*

               IF (DTYPE(LOOP) .EQ. CAT__TYPED) THEN
                  IF (CTYPE .EQ. CAT__TYPEC) THEN
                     DTYPE(LOOP) = CAT__TYPEC
                  END IF
               ELSE IF (DTYPE(LOOP) .EQ. CAT__TYPEI) THEN
                  IF (CTYPE .EQ. CAT__TYPED) THEN
                     DTYPE(LOOP) = CAT__TYPED
                  ELSE IF (CTYPE .EQ. CAT__TYPEC) THEN
                     DTYPE(LOOP) = CAT__TYPEC
                  END IF
               END IF

*
*             Determine the length of the current field and modify the
*             accumulating length if necessary.

               IF (FIELDS(LOOP) .NE. ' ') THEN
                  FLDLEN = CHR_LEN(FIELDS(LOOP))
                  CWIDTH(LOOP) = MAX(CWIDTH(LOOP), FLDLEN)
               ELSE
                  FLDLEN = 0
               END IF

*
*             The following checks are made for all columns, but are
*             strictly only necessary for DOUBLE PRECISION ones.
*
*             Check the number of decimal places and modify the
*             accummulating number of decimal places if necessary.

               DECPOS = CHR_INDEX(FIELDS(LOOP), '.')

               IF (DECPOS .GT. 0) THEN
                  CURPL = FLDLEN - DECPOS
                  DECPL(LOOP) = MAX(DECPL(LOOP), CURPL)
               END IF

*
*             Check for any exponent and modify the accummulating
*             exponent flag if necessary.  Note that an exponent may
*             indicated by any one of 'e', 'E', 'd' or 'D'.

               ELPOS = CHR_INDEX(FIELDS(LOOP), 'e')
               EUPOS = CHR_INDEX(FIELDS(LOOP), 'E')
               DLPOS = CHR_INDEX(FIELDS(LOOP), 'd')
               DUPOS = CHR_INDEX(FIELDS(LOOP), 'D')

               IF (ELPOS .GT. 0  .OR.  EUPOS .GT. 0  .OR.
     :             DLPOS .GT. 0  .OR.  DUPOS .GT. 0) THEN
                  EXPFMT(LOOP) = .TRUE.
               END IF

            END IF
         END DO

      END IF

      END
