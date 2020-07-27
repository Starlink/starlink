      SUBROUTINE CAT1_CRCOL (CI, NITEM, ITMNAM, ITMVAL, INCOMM, LINE,
     :  FIRST, STATUS)
*+
*  Name:
*     CAT1_CRCOL
*  Purpose:
*     Create a column from a description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CRCOL (CI, NITEM, ITMNAM, ITMVAL, INCOMM, LINE; FIRST;
*       STATUS)
*  Description:
*     Create a column from details supplied in a description file.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     NITEM  =  INTEGER (Given)
*        Number of items defining the column.
*     ITMNAM(NITEM)  =  CHARACTER*(*) (Given)
*        Names of the items.
*     ITMVAL(NITEM  =  CHARACTER*(*) (Given)
*        Values of the items.
*     INCOMM  =  CHARACTER*(*) (Given)
*        In-line comments for the item.
*     LINE  =  INTEGER (Given)
*        Approximate line number of the column in the description file.
*     FIRST  =  LOGICAL (Given and Returned)
*        Flag; has the first description file error been reported?
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Generate the default attributes for a column.
*     For each item in the list
*       Convert the item name to upper case.
*       Create an upper case copy of the item value.
*       If the item name is valid then
*         Construct the corresponding catalogue attiribute or details.
*       else
*         Set the status message - unknown item.
*       end if
*       Report any error.
*     end for
*     If all ok then
*       If the comments attribute has not been set then
*         Substitute any in-line comments.
*       end if
*       If the external display format is not suitable for the data type
*       then
*         Generate a suitable external display format.
*       end if
*       Attempt to create the column.
*       If the table format has not been set then
*         Adopt the external format as the table format.
*       end if
*       Increment the number of columns.
*       Store the non-attribute details.
*     end if
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
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
*     9/7/96  (ACD): Original version.
*     19/9/96 (ACD): First stable version.
*     28/3/97 (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     26/5/97 (ACD): Made appropriate keyword values case insensitive.
*     20/7/00  (ACD): Added checks that the column external formats
*       read from the table are valid.
*     27/7/20 (DSB): Force LOGICAL columns to use locum instead of null.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_DSDIR_CMN'    ! Description directives common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  NITEM,
     :  LINE
      CHARACTER
     :  ITMNAM(NITEM)*(*),
     :  ITMVAL(NITEM)*(*),
     :  INCOMM*(*)
*  Arguments Given and Returned:
      LOGICAL
     :  FIRST
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  FI,         ! Column identifier.
     :  LOOP,       ! Loop index.
     :  LSTAT,      ! Local status decoding numeric values.
     :  IVALUE,     ! Integer value decoded from CURVAL.
     :  POSN        ! Position for the current column.
      CHARACTER
     :  CURNAM*(CAT__SZANM),  ! Current name.
     :  CURVAL*(CAT__SZVAL),  !    "    value.
     :  CURVUP*(CAT__SZVAL),  ! Upper case copy of CURVAL.
     :  TBLFMT*(CAT__SZEXF),  ! Table format.
     :  UFUNIT*(CAT__SZUNI),  ! Upper case copy of FUNIT.
     :  PRSMSG*30   ! Message associated with error.
      LOGICAL
     :  PRSOK,      ! Flag; any error interpretting the current item?
     :  FMTOK       ! Flag; is external format ok?

*
*    Attributes for a single column.

      INTEGER
     :  FGENUS,  ! Genus attribute.
     :  FDTYPE,  ! Data type attribute.
     :  FCSIZE,  ! Character size attribute.
     :  FDIM,    ! Dimensionality attribute.
     :  FSIZE,   ! Size attribute.
     :  FNULL,   ! Null flag attribute.
     :  FORDER   ! Order attribute.
      CHARACTER
     :  FNAME*(CAT__SZCMP),  ! Name attribute.
     :  FEXP*(CAT__SZEXP),   ! Expression attribute.
     :  FXCEPT*(CAT__SZVAL), ! Exception value attribute.
     :  FUNIT*(CAT__SZUNI),  ! Units attribute.
     :  FXFMT*(CAT__SZEXF),  ! External format attribute.
     :  FCOMM*(CAT__SZCOM)   ! Comments attribute for the column.
      DOUBLE PRECISION
     :  FSCALE,  ! Scale factor attribute.
     :  FZERO,   ! Zero point attribute.
     :  FDATE    ! Modification date attribute.
      LOGICAL
     :  FPDISP   ! Preferential display flag attribute.

*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Generate the default attributes for a column.

         CALL CAT1_DFATT (FNAME, FGENUS, FEXP, FDTYPE, FCSIZE, FDIM,
     :     FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER, FDATE, FUNIT,
     :     FXFMT, FPDISP, FCOMM, STATUS)

         POSN = 0
         TBLFMT = ' '

*
*       Process each item in the list, replacing the value for the
*       corresponding attribute with the value of the item, as
*       appropriate.

         DO LOOP = 1, NITEM
            CURNAM = ITMNAM(LOOP)
            CURVAL = ITMVAL(LOOP)

            PRSOK = .TRUE.

*
*          Convert the item name to upper case.

            CALL CHR_UCASE (CURNAM)

*
*          Create an upper case copy of the item value.

            CURVUP = CURVAL
            CALL CHR_UCASE (CURVUP)

*
*          Check for the column name attribute.

            IF (CURNAM .EQ. 'NAME') THEN
               CALL CAT1_DCNME (CURVAL, FNAME, FDIM, FSIZE, PRSOK,
     :           PRSMSG, STATUS)

*
*          Check for the data type attribute.

            ELSE IF (CURNAM .EQ. 'DTYPE') THEN
               IF (CURVUP .EQ. 'BYTE') THEN
                  FDTYPE = CAT__TYPEB

               ELSE IF (CURVUP .EQ. 'WORD') THEN
                  FDTYPE = CAT__TYPEW

               ELSE IF (CURVUP .EQ. 'INTEGER') THEN
                  FDTYPE = CAT__TYPEI

               ELSE IF (CURVUP .EQ. 'INT64') THEN
                  FDTYPE = CAT__TYPEK

               ELSE IF (CURVUP .EQ. 'REAL') THEN
                  FDTYPE = CAT__TYPER

               ELSE IF (CURVUP .EQ. 'DOUBLE') THEN
                  FDTYPE = CAT__TYPED

               ELSE IF (CURVUP .EQ. 'LOGICAL') THEN
                  FDTYPE = CAT__TYPEL
                  FNULL = CAT__LOCUM

               ELSE IF (CURVUP(1 : 5) .EQ. 'CHAR*') THEN
                  FDTYPE = CAT__TYPEC

                  CURVUP(1 : 5) = '     '

                  LSTAT = CAT__OK
                  CALL CHR_CTOI (CURVUP, FCSIZE, LSTAT)
                  IF (LSTAT .NE. CAT__OK) THEN
                     PRSOK = .FALSE.
                     PRSMSG = 'missing character size'
                  END IF

               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'illegal value'

               END IF

*
*          Check for the position details.

            ELSE IF (CURNAM .EQ. 'VALPOS') THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (CURVAL, IVALUE, LSTAT)
               IF (LSTAT .EQ. CAT__OK) THEN
                  IF (IVALUE .GT. 0) THEN
                     POSN = IVALUE
                  ELSE
                     PRSOK = .FALSE.
                     PRSMSG = 'positive integer required'
                  END IF
               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'positive integer required'
               END IF

*
*          Check for the order attribute.  Note that only the first
*          letter of the value is checked.

            ELSE IF (CURNAM .EQ. 'ORDER') THEN
               IF (CURVUP(1 : 1) .EQ. 'A') THEN
                  FORDER = CAT__ASCND

               ELSE IF (CURVUP(1 : 1) .EQ. 'D') THEN
                  FORDER = CAT__DSCND

               ELSE IF (CURVUP(1 : 1) .EQ. 'U') THEN
                  FORDER = CAT__NOORD

               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'illegal value'

               END IF

*
*          Check for the units attribute.

            ELSE IF (CURNAM .EQ. 'UNITS') THEN
               FUNIT = CURVAL

*
*          Check for the external display format attribute.

            ELSE IF (CURNAM .EQ. 'EXFMT') THEN
               FXFMT = CURVAL

*
*          Check for the preferrential display flag attribute.

            ELSE IF (CURNAM .EQ. 'PREFDISP') THEN
               IF (CURVUP .EQ. 'TRUE') THEN
                  FPDISP = .TRUE.

               ELSE IF (CURVUP .EQ. 'FALSE') THEN
                  FPDISP = .FALSE.

               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'illegal value'

               END IF

*
*          Check for the comments attribute.

            ELSE IF (CURNAM .EQ. 'COMMENTS') THEN
               FCOMM = CURVAL

*
*          Check for the scale factor attribute.

            ELSE IF (CURNAM .EQ. 'SCALEF') THEN
               LSTAT = CAT__OK
               CALL CHR_CTOD (CURVAL, FSCALE, LSTAT)
               IF (LSTAT .NE. CAT__OK) THEN
                  PRSOK = .FALSE.
                  PRSMSG = 'numeric value required'
               END IF

*
*          Check for the zero point attribute.

            ELSE IF (CURNAM .EQ. 'ZEROP') THEN
               LSTAT = CAT__OK
               CALL CHR_CTOD (CURVAL, FZERO, LSTAT)
               IF (LSTAT .NE. CAT__OK) THEN
                  PRSOK = .FALSE.
                  PRSMSG = 'numeric value required'
               END IF

*
*          Check for the table format details.

            ELSE IF (CURNAM .EQ. 'TBLFMT') THEN
               TBLFMT = CURVAL

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

*
*       If all is ok then attempt to create the column.

         IF (STATUS .EQ. CAT__OK  .AND.  FIRST) THEN

*
*          If no comments attribute has been specified then any in-line
*          comments are substituted.

            IF (FCOMM .EQ. ' ') THEN
               FCOMM = INCOMM
            END IF

*
*          If the external format is blank then generate one (and
*          tweak it for columns of angles).  If it is not blank
*          then check it is ok and if not generate a substitute.

            IF (FXFMT .EQ. ' ') THEN
               CALL CAT1_DXFMT (FDTYPE, FCSIZE, FUNIT, FXFMT, STATUS)

               IF (FDTYPE .EQ. CAT__TYPED) THEN
                  UFUNIT = FUNIT
                  CALL CHR_UCASE (UFUNIT)
                  IF (UFUNIT(1 : 7) .EQ. 'RADIANS') THEN
                     FXFMT = 'D19.10'
                  END IF
               END IF

            ELSE
               CALL CAT1_CXFMT (FNAME, FXFMT, FDTYPE, FCSIZE,
     :          FMTOK, STATUS)

               IF (.NOT. FMTOK) THEN
                  CALL CAT1_DXFMT (FDTYPE, FCSIZE, FUNIT, FXFMT, STATUS)
               END IF
            END IF

*
*          Attempt to create the column.

            CALL CAT1_ADDCL (CI, FNAME, FGENUS, FEXP, FDTYPE, FCSIZE,
     :        FDIM, FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER,
     :        FDATE, FUNIT, FXFMT, FPDISP, FCOMM, FI, STATUS)

C           print2000, fname, fdtype, funit(1 : 10), fcomm(1 : 15),
C    :        status
C2000       format(1x, a15, 1x, i4, 1x, a10, 1x, a15, 1x, i15)

*
*          If the table format has not been set then adopt the external
*          format as the table format.

            IF (TBLFMT .EQ. ' ') THEN
               TBLFMT = FXFMT
            END IF

*
*          Increment the number of columns.

            DCOLS__CAT1 = DCOLS__CAT1 + 1

*
*          Store the non-attribute details.  Note that currently the
*          only non-attribute details are the position and a copy of the
*          identifier (to allow the position to be identified).

            DFID__CAT1(DCOLS__CAT1) = FI
            DPOS__CAT1(DCOLS__CAT1) = POSN
            DFMT__CAT1(DCOLS__CAT1) = TBLFMT

         END IF

      END IF

      END
