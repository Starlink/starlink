      SUBROUTINE CAT3_RDORD (FITUNT, ONAME, ORDER, STATUS)
*+
*  Name:
*     CAT3_RDORD
*  Purpose:
*     Read the name of the column on which the table is sorted.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_RDORD (FITUNT; ONAME, ORDER; STATUS)
*  Description:
*     Read the name of the column on which the table is sorted (or
*     ordered).
*
*     The name is read from the standard FITS tables keyword
*     TSORTKEY.  The absence of this keyword indicates that the table
*     is unsorted, and in this case ONAME is returned set to blank,
*     ORDER is set to CAT__NOORD (unordered) and the return status
*     remains ok.
*  Arguments:
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     ONAME  =  CHARACTER*(*) (Returned)
*        The name of the column on which the table is sorted (or
*        ordered).  If the table is not sorted on any column
*        ONAME is returned set to blank.
*     ORDER  =  INTEGER (Returned)
*        The way in which ONAME is sorted.  The possibilities are:
*        CAT__ASCND  -  ascending,
*        CAT__DSCND  -  descending.
*        If ONAME is returned as blank ORDER is returned as CAT__NOORD,
*        corresponding to un-ordered.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get the value of the sorted column keyword TSORTKEY.
*     If ok then
*       Determine whether the column is in ascending or descending
*       order.
*       Obtain the name of the column.
*     else
*       Set the name of the column to blank.
*       Set the order to unordered.
*     end if
*
*     Note that the two cases of the keyword being absent and an error
*     occurring are not differentiated.  In both cases the name of the
*     colunm is returned as blank and the status is ok.
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
*     12/9/94 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  FITUNT
*  Arguments Returned:
      CHARACTER
     :  ONAME*(*)
      INTEGER
     :  ORDER
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FITSTT,      ! FITSIO status.
     :  KEYLEN,      ! Length of KEYVAL (excl. trail. blanks).
     :  LOOP         ! Loop index.
      CHARACTER
     :  KEYVAL*70,   ! Value of FITS keyword.
     :  KEYCOM*70    ! Comments for FITS keyword.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to get the value of the standard FITS keyword TSORTKEY
*       which holds the name of the column on which the table is sorted.

         FITSTT = FITOK
         CALL FTGKEY (FITUNT, 'TSORTKEY', KEYVAL, KEYCOM, FITSTT)

*
*       Proceed if all is ok.

         IF (FITSTT .EQ. FITOK) THEN

*
*          Strip any enclosing quotes from the value.

            IF (KEYVAL .NE. ' ') THEN
               KEYLEN = CHR_LEN(KEYVAL)

               DO LOOP = 1, KEYLEN
                  IF (KEYVAL(LOOP : LOOP) .EQ. '''') THEN
                     KEYVAL(LOOP : LOOP) = ' '
                  END IF
               END DO
            END IF

*
*          Determine whether the column is in ascending or descending
*          order.  A minus sign before the column name indicates
*          descending order.  Otherwise the column is ascending.

            CALL CHR_LDBLK (KEYVAL)

            IF (KEYVAL(1 : 1) .EQ. '-') THEN
               ORDER = CAT__DSCND
               KEYVAL(1 : 1) = ' '
            ELSE
               ORDER = CAT__ASCND
            END IF

*
*          Obtain the name of the column.

            CALL CHR_LDBLK (KEYVAL)
            ONAME = KEYVAL

         ELSE

*
*          The keyword could not be obtained successfully.  Set the
*          name of the sorted column to blank and the order to
*          un-ordered.  Note that deliberately no status is set or
*          error reported.

            ONAME = ' '
            ORDER = CAT__NOORD

         END IF

      END IF

      END
