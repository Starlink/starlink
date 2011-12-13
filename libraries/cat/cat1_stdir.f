      SUBROUTINE CAT1_STDIR (NITEM, ITMNAM, ITMVAL, LINE, FIRST,
     :  STATUS)
*+
*  Name:
*     CAT1_STDIR
*  Purpose:
*     Set catalogue directives from a description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_STDIR (NITEM, ITMNAM, ITMVAL, LINE; FIRST; STATUS)
*  Description:
*     Set catalogue directives using values read from a description
*     file.
*
*     Note that default values for the variables in the description
*     directives common block should have been set previous.  The
*     defaults cannot be set in this routine because they are different
*     STL and DAB catalogues (and this routine is used by both).
*  Arguments:
*     NITEM  =  INTEGER (Given)
*        Number of items specifiying directives.
*     ITMNAM(NITEM)  =  CHARACTER*(*) (Given)
*        Names of the items.
*     ITMVAL(NITEM  =  CHARACTER*(*) (Given)
*        Values of the items.
*     LINE  =  INTEGER (Given)
*        Approximate line number of the directives in the description file.
*     FIRST  =  LOGICAL (Given and Returned)
*        Flag; has the first description file error been reported?
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each item in the list
*       Convert the item name to upper case.
*       If the item name is valid then
*         Construct the corresponding catalogue attiribute or details.
*       else
*         Set the status message - unknown item.
*       end if
*       Report any error.
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
*     10/7/96 (ACD): Original version.
*     11/7/96 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_DSDIR_CMN'    ! Description directives common block.
*  Arguments Given:
      INTEGER
     :  NITEM,
     :  LINE
      CHARACTER
     :  ITMNAM(NITEM)*(*),
     :  ITMVAL(NITEM)*(*)
*  Arguments Given and Returned:
      LOGICAL
     :  FIRST
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,       ! Loop index.
     :  IVALUE,     ! Value decoded for current item.
     :  LSTAT       ! Local status decoding numeric values.
      CHARACTER
     :  CURNAM*(CAT__SZANM),  ! Current name.
     :  CURVAL*(CAT__SZVAL),  !    "    value.
     :  PRSMSG*30   ! Message associated with error.
      LOGICAL
     :  PRSOK       ! Flag; any error interpretting the current item?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Process each item in the list, replacing the value for the
*       corresponding directive with the value of the item, as
*       appropriate.

         DO LOOP = 1, NITEM
            CURNAM = ITMNAM(LOOP)
            CURVAL = ITMVAL(LOOP)

            PRSOK = .TRUE.

*
*          Convert the item name to upper case.

            CALL CHR_UCASE (CURNAM)

*
*          Check for the file name directive.

            IF (CURNAM .EQ. 'FILE') THEN
               DFILE__CAT1 = CURVAL
               DFLFG__CAT1 = CAT1__SEPFL

*
*          Check for the position directive.

            ELSE IF (CURNAM .EQ. 'POSITION') THEN
               CALL CHR_LDBLK (CURVAL)

               IF (CURVAL(1 : 2) .EQ. 'CO') THEN
                  DPOSN__CAT1 = CAT1__PSCOL

               ELSE IF (CURVAL(1 : 2) .EQ. 'CH') THEN
                  DPOSN__CAT1 = CAT1__PSCHR

               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'illegal value'

               END IF

*
*          Check for the record size directive.

            ELSE IF (CURNAM .EQ. 'RECORDSIZE') THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (CURVAL, IVALUE, LSTAT)
               IF (LSTAT .EQ. CAT__OK) THEN
                  IF (IVALUE .GT. 0) THEN
                     DRESZ__CAT1 = IVALUE
                  ELSE
                     PRSOK = .FALSE.
                     PRSMSG = 'positive integer required'
                  END IF
               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'positive integer required'
               END IF

*
*          Check for the number of rows directive.

            ELSE IF (CURNAM .EQ. 'ROWS') THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (CURVAL, IVALUE, LSTAT)
               IF (LSTAT .EQ. CAT__OK) THEN
                  IF (IVALUE .GT. 0) THEN
                     DROWS__CAT1 = IVALUE
                  ELSE
                     PRSOK = .FALSE.
                     PRSMSG = 'positive integer required'
                  END IF
               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'positive integer required'
               END IF

*
*          Check for the number of header rows to skip directive.

            ELSE IF (CURNAM .EQ. 'SKIP') THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (CURVAL, IVALUE, LSTAT)
               IF (LSTAT .EQ. CAT__OK) THEN
                  IF (IVALUE .GE. 0) THEN
                     DSKIP__CAT1 = IVALUE
                  ELSE
                     PRSOK = .FALSE.
                     PRSMSG = 'illegal value'
                  END IF
               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'illegal value'
               END IF

            ELSE

*
*             Unrecognised item; set the error flag and message.

               PRSOK = .FALSE.
               PRSMSG = 'unknown item'

            END IF

*
*          Report any error.

            IF (.NOT. PRSOK) THEN
               CALL CAT1_ECSCR (LINE, 'column', ITMNAM(LOOP),
     :           ITMVAL(LOOP), PRSMSG, FIRST, STATUS)
            END IF
         END DO

C        print2000, dflfg__cat1, dfile__cat1(1 : 20), dposn__cat1,
C    :     dresz__cat1, drows__cat1,  dskip__cat1, status
C2000    format(
C    :     1x, 'dflfg__cat1: ', l5 /
C    :     1x, 'dfile__cat1: ', a20, '...' /
C    :     1x, 'dposn__cat1: ', i5 /
C    :     1x, 'dresz__cat1: ', i5 /
C    :     1x, 'drows__cat1: ', i6 /
C    :     1x, 'dskip__cat1: ', i5 /
C    :     1x, 'status: ', i20 / )

      END IF

      END
