      SUBROUTINE CAT1_CRPAR (CI, NITEM, ITMNAM, ITMVAL, INCOMM, LINE,
     :  FIRST, STATUS)
*+
*  Name:
*     CAT1_CRPAR
*  Purpose:
*     Create a parameter from a description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CRPAR (CI, NITEM, ITMNAM, ITMVAL, INCOMM, LINE; FIRST;
*       STATUS)
*  Description:
*     Create a parameter from details supplied in a description file.
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
*     Generate the default attributes for a parameter.
*     For each item in the list
*       Convert the item name to upper case.
*       Create an upper case copy of the item value.
*       If the item name is valid then
*         Construct the corresponding catalogue attribute or details.
*       else
*         Set the status message - unknown item.
*       end if
*       Report any error.
*     end for
*     If all ok then
*       If the comments attribute has not been set then
*         Substitute any in-line comments.
*       end if
*       If the external display format has not been set then
*         Generate a suitable external display format.
*       end if
*       Attempt to create the parameter.
*       Increment the number of parameters.
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
*     17/7/96 (ACD): Original version (from CAT1_CRCOL).
*     24/7/96 (ACD): First stable version.
*     28/3/97 (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     26/5/97 (ACD): Made appropriate keyword values case insensitive.
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
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  QI,         ! Parameter identifier.
     :  LOOP,       ! Loop index.
     :  LSTAT,      ! Local status decoding numeric values.
     :  LQVAL,      ! Length of QVAL  (excl. trail. blanks).
     :  LQXFMT,     !   "    "  QXFMT ( "  .   "  .   "   ).
     :  SQXFMT      ! Size of external format for character column.
      CHARACTER
     :  CURNAM*(CAT__SZANM),  ! Current name.
     :  CURVAL*(CAT__SZVAL),  !    "    value.
     :  CURVUP*(CAT__SZVAL),  ! Upper case copy of CURVAL.
     :  PRSMSG*30   ! Message associated with error.
      LOGICAL
     :  PRSOK       ! Flag; any error interpretting the current item?

*
*    Attributes for a single parameter.

      CHARACTER
     :  QNAME*(CAT__SZCMP),  ! Name attribute.
     :  QUNIT*(CAT__SZUNI),  ! Units attribute.
     :  QXFMT*(CAT__SZEXF),  ! External format attribute.
     :  QCOMM*(CAT__SZCOM),  ! Comments attribute.
     :  QVALUE*(CAT__SZVAL)  ! Value attribute.
      INTEGER
     :  QDTYPE,  ! Data type attribute.
     :  QCSIZE,  ! Character size attribute.
     :  QDIM,    ! Dimensionality attribute.
     :  QSIZE    ! Size attribute.
      DOUBLE PRECISION
     :  QDATE    ! Modification date attribute.
      LOGICAL
     :  QPDISP   ! Preferential display flag attribute.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Generate the default attributes for a parameter.

         CALL CAT1_DPATT (QNAME, QDTYPE, QCSIZE, QDIM, QSIZE, QDATE,
     :     QUNIT, QXFMT, QPDISP, QCOMM, QVALUE, STATUS)

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
               CALL CAT1_DCNME (CURVAL, QNAME, QDIM, QSIZE, PRSOK,
     :           PRSMSG, STATUS)

               IF (QDIM .NE. CAT__SCALR) THEN
                  PRSOK = .FALSE.
                  PRSMSG = 'vector parameters not supported'
               END IF

*
*          Check for the data type attribute.

            ELSE IF (CURNAM .EQ. 'DTYPE') THEN
               IF (CURVUP .EQ. 'BYTE') THEN
                  QDTYPE = CAT__TYPEB

               ELSE IF (CURVUP .EQ. 'WORD') THEN
                  QDTYPE = CAT__TYPEW

               ELSE IF (CURVUP .EQ. 'INTEGER') THEN
                  QDTYPE = CAT__TYPEI

               ELSE IF (CURVUP .EQ. 'INT64') THEN
                  QDTYPE = CAT__TYPEI

               ELSE IF (CURVUP .EQ. 'REAL') THEN
                  QDTYPE = CAT__TYPER

               ELSE IF (CURVUP .EQ. 'DOUBLE') THEN
                  QDTYPE = CAT__TYPED

               ELSE IF (CURVUP .EQ. 'LOGICAL') THEN
                  QDTYPE = CAT__TYPEL

               ELSE IF (CURVUP(1 : 5) .EQ. 'CHAR*') THEN
                  QDTYPE = CAT__TYPEC

                  CURVUP(1 : 5) = '     '

                  LSTAT = CAT__OK
                  CALL CHR_CTOI (CURVUP, QCSIZE, LSTAT)
                  IF (LSTAT .NE. CAT__OK) THEN
                     PRSOK = .FALSE.
                     PRSMSG = 'missing character size'
                  END IF

               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'illegal value'

               END IF

*
*          Check for the value.  Any quotation marks are removed.
*          Note that a blank value for a parameter is acceptable.

            ELSE IF (CURNAM .EQ. 'VALPOS') THEN
               QVALUE = CURVAL

               IF (QVALUE(1 : 1) .EQ. ''''  .OR.
     :             QVALUE(1 : 1) .EQ. '"') THEN
                  QVALUE(1 : 1) = ' '

                  CALL CHR_LDBLK (QVALUE)

                  IF (QVALUE .NE. ' ') THEN
                     LQVAL = CHR_LEN(QVALUE)
                     QVALUE(LQVAL : LQVAL) = ' '
                  END IF
               END IF

*
*          Check for the units attribute.

            ELSE IF (CURNAM .EQ. 'UNITS') THEN
               QUNIT = CURVAL

*
*          Check for the external display format attribute.

            ELSE IF (CURNAM .EQ. 'EXFMT') THEN
               QXFMT = CURVAL

*
*          Check for the preferrential display flag attribute.

            ELSE IF (CURNAM .EQ. 'PREFDISP') THEN
               IF (CURVUP .EQ. 'TRUE') THEN
                  QPDISP = .TRUE.

               ELSE IF (CURVUP .EQ. 'FALSE') THEN
                  QPDISP = .FALSE.

               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'illegal value'

               END IF

*
*          Check for the comments attribute.

            ELSE IF (CURNAM .EQ. 'COMMENTS') THEN
               QCOMM = CURVAL

            ELSE

*
*             Unrecognised item; set the error flag and message.

               PRSOK = .FALSE.
               PRSMSG = 'unknown item'

            END IF

*
*          Report any error.

            IF (.NOT. PRSOK) THEN
               CALL CAT1_ECSCR (LINE, 'parameter', ITMNAM(LOOP),
     :           ITMVAL(LOOP), PRSMSG, FIRST, STATUS)
            END IF
         END DO

*
*       If all is ok then attempt to create the parameter.  If no
*       comments attribute has been specified then any in-line
*       comments are substituted.  Similarly, if the external
*       display format has not been set then a format suitable
*       for the data type is generated.  Note that the DOUBLE PRECISION
*       format should be able represent angles to an accuracy of 0.01
*       second of arc.

         IF (STATUS .EQ. CAT__OK  .AND.  FIRST) THEN
            IF (QCOMM .EQ. ' ') THEN
               QCOMM = INCOMM
            END IF

            IF (QXFMT .EQ. ' ') THEN
               IF (QDTYPE .EQ. CAT__TYPEB) THEN
                  QXFMT = 'I4'
               ELSE IF (QDTYPE .EQ. CAT__TYPEW) THEN
                  QXFMT = 'I6'
               ELSE IF (QDTYPE .EQ. CAT__TYPEI) THEN
                  QXFMT = 'I6'
               ELSE IF (QDTYPE .EQ. CAT__TYPER) THEN
                  QXFMT = 'E12.3'
               ELSE IF (QDTYPE .EQ. CAT__TYPED) THEN
                  QXFMT = 'D19.10'
               ELSE IF (QDTYPE .EQ. CAT__TYPEL) THEN
                  QXFMT = 'L5'
               ELSE IF (QDTYPE .EQ. CAT__TYPEC) THEN
                  SQXFMT = QCSIZE + 2

                  LQXFMT = 0
                  QXFMT = ' '

                  CALL CHR_PUTC ('A', QXFMT, LQXFMT)
                  CALL CHR_PUTI (SQXFMT, QXFMT, LQXFMT)
               END IF
            END IF

            CALL CAT1_ADDPR (CI, QNAME, QDTYPE, QCSIZE, QDIM, QSIZE,
     :        QDATE, QUNIT, QXFMT, QPDISP, QCOMM, QVALUE, QI, STATUS)
*
*          Increment the number of columns.

            DPARS__CAT1 = DPARS__CAT1 + 1

         END IF

      END IF

      END
