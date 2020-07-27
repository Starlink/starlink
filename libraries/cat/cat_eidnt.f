      SUBROUTINE CAT_EIDNT (CI, EXPR, EI, STATUS)
*+
*  Name:
*     CAT_EIDNT
*  Purpose:
*     Get an identifier for an expression.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EIDNT (CI, EXPR; EI; STATUS)
*  Description:
*     Get an identifier for an expression.  A parse of the the
*     expression is attempted, and if successful an identifier is
*     returned.  If the parse fails an error status is raised.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue to which the expression refers.
*     EXPR  =  CHARACTER*(*) (Given)
*        The expression.
*     EI  =  INTEGER (Returned)
*        Identifier to the expression.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of the identifier.
*     If the identifier corresponds to a catalogue then
*       Attempt to parse the expression.
*       If ok then
*         Convert the parser data type to the CAT data type.
*         Attempt to create an new expression identifier.
*         If ok then
*           Create the default attributes for a scalar column.
*           As apropriate, replace these default attributes with the
*           ones appropriate for the expression.
*           Add the attributes for the expression list to the list of
*           attributes.
*         end if
*       end if
*     else
*       Set the status.
*       Report error; the given identifier does not correspond to a
*       catalogue.
*     end if
*
*     Remember: the attributes for an expression are the same as those
*     for a scalar column.
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
*     3/5/93 (ACD):  Prologue only.
*     10/8/93 (ACD): Original version.
*     11/8/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     14/4/94 (ACD): Saved defining expression as an attribute.
*     29/4/94 (ACD): Added suitable external formats.
*     3/5/94  (ACD): Added check that identifier corresponds to a
*       catalogue.
*     31/3/95 (ACD): Revised which attributes are mutable.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     9/10/95 (ACD): Changed all occurrences of 'StarBase' to 'CAT'.
*     13/8/96 (ACD): Improved the error reporting.
*     28/3/97 (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     27/7/20 (DSB): Must use locom values, not null values, with
*                    _LOGICAL expressions.


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
     :  CI
      CHARACTER
     :  EXPR*(*)
*  Arguments Returned:
      INTEGER
     :  EI
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER PARSOK   ! Success status from the parser.
      PARAMETER (PARSOK = 0)
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Identifier type.
     :  PARSTT,  ! Parses status.
     :  PDTYPE,  ! Parser data type.
     :  SBTYPE,  ! CAT data type.
     :  XID,     ! Parser expression identifier.
     :  LEXPR,   ! Length of EXPR   (excl. trail. blanks).
     :  ERRLEN   !   "    "  ERRTXT ( "      "  .   "   ).
      CHARACTER
     :  EXTFMT*(CAT__SZEXF), ! External format.
     :  ERRTXT*75            ! Text of error message.

*
*    Attributes for a single scalar expression.

      INTEGER
     :  EGENUS,  ! Genus attribute.
     :  EDTYPE,  ! Data type attribute.
     :  ECSIZE,  ! Character size attribute.
     :  EDIM,    ! Dimensionality attribute.
     :  ESIZE,   ! Size attribute.
     :  ENULL,   ! Null flag attribute.
     :  EORDER   ! Order attribute.
      CHARACTER
     :  ENAME*(CAT__SZCMP),  ! Name attribute.
     :  EEXP*(CAT__SZEXP),   ! Expression attribute.
     :  EXCEPT*(10),         ! Exception value attribute.
     :  EUNIT*(CAT__SZUNI),  ! Units attribute.
     :  EXFMT*(CAT__SZEXF),  ! External format attribute.
     :  ECOMM*(CAT__SZCOM)   ! Comments attribute for the column.
      DOUBLE PRECISION
     :  ESCALE,  ! Scale factor attribute.
     :  EZERO,   ! Zero point attribute.
     :  EDATE    ! Modification date attribute.
      LOGICAL
     :  EPDISP   ! Preferential display flag attribute.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the given identifier corresponds to a catalogue.

         CALL CAT_TIDTP (CI, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__CITYP) THEN

*
*          Attempt to parse the expression.

            PARSTT = 0
            CALL ANT_XCOMP (CI, EXPR, XID, PDTYPE, PARSTT)

            IF (PARSTT .NE. PARSOK) THEN
               STATUS = CAT__INVEX
               EI = CAT__NOID
            END IF

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Convert the parser code for the data type into the equivalent
*             CAT data type.  Also, invent a suitable external format.

               IF (PDTYPE .EQ. -1) THEN
                  SBTYPE = CAT__TYPEC
                  EXTFMT = 'A1'

               ELSE IF (PDTYPE .EQ. 0) THEN
                  SBTYPE = CAT__TYPEL
                  EXTFMT = 'L5'

               ELSE IF (PDTYPE .EQ. 1) THEN
                  SBTYPE = CAT__TYPEB
                  EXTFMT = 'I10'

               ELSE IF (PDTYPE .EQ. 2) THEN
                  SBTYPE = CAT__TYPEW
                  EXTFMT = 'I10'

               ELSE IF (PDTYPE .EQ. 3) THEN
                  SBTYPE = CAT__TYPEI
                  EXTFMT = 'I10'

               ELSE IF (PDTYPE .EQ. 4) THEN
                  SBTYPE = CAT__TYPER
                  EXTFMT = 'E12.4'

               ELSE IF (PDTYPE .EQ. 5) THEN
                  SBTYPE = CAT__TYPED
                  EXTFMT = 'D16.8'

               END IF

*
*             Attempt to create a new identifier for this expression.

               CALL CAT1_CRTID (CAT__EITYP, CI, EI, STATUS)

               IF (STATUS .EQ. CAT__OK) THEN

*
*                Create the attributes for a scalar column.

                  CALL CAT1_DFATT (ENAME, EGENUS, EEXP, EDTYPE, ECSIZE,
     :              EDIM, ESIZE, ENULL, EXCEPT, ESCALE, EZERO, EORDER,
     :              EDATE, EUNIT, EXFMT, EPDISP, ECOMM, STATUS)

*
*                As appropriate, replace the default attributes with those
*                specific to the expression.

                  EGENUS = CAT__GVIRT
                  ENAME = ' '
                  EDTYPE = SBTYPE
                  ECSIZE = 1          ! ?????????????????????????????
                  EXFMT = EXTFMT
                  EEXP = EXPR

*
*                Must use locom values, not null values, with _LOGICAL
*                expressions.
                  IF( EDTYPE .EQ. CAT__TYPEL ) ENULL = CAT__LOCUM

*
*                Add these attributes to the list of attributes
*                Note that for an expression they are all mutable except:
*                expression, data type, character size, dimensionality and
*                size.

                  CALL CAT1_ADDAC (EI, 'NAME', .TRUE., ENAME, STATUS)
                  CALL CAT1_ADDAI (EI, 'GENUS', .FALSE., EGENUS, STATUS)
                  CALL CAT1_ADDAC (EI, 'EXPR', .FALSE., EEXP, STATUS)
                  CALL CAT1_ADDAI (EI, 'DTYPE', .FALSE., EDTYPE, STATUS)
                  CALL CAT1_ADDAI (EI, 'CSIZE', .FALSE., ECSIZE, STATUS)
                  CALL CAT1_ADDAI (EI, 'DIMS', .FALSE., EDIM, STATUS)
                  CALL CAT1_ADDAI (EI, 'SIZE', .FALSE., ESIZE, STATUS)
                  CALL CAT1_ADDAI (EI, 'NULL', .TRUE., ENULL, STATUS)
                  CALL CAT1_ADDAC (EI, 'EXCEPT', .FALSE., EXCEPT,
     :              STATUS)
                  CALL CAT1_ADDAD (EI, 'SCALEF', .FALSE., ESCALE,
     :              STATUS)
                  CALL CAT1_ADDAD (EI, 'ZEROP', .FALSE., EZERO, STATUS)
                  CALL CAT1_ADDAI (EI, 'ORDER', .FALSE., EORDER, STATUS)
                  CALL CAT1_ADDAD (EI, 'DATE', .TRUE., EDATE, STATUS)
                  CALL CAT1_ADDAC (EI, 'UNITS', .TRUE., EUNIT, STATUS)
                  CALL CAT1_ADDAC (EI, 'EXFMT', .TRUE., EXFMT, STATUS)
                  CALL CAT1_ADDAL (EI, 'PRFDSP', .TRUE., EPDISP, STATUS)
                  CALL CAT1_ADDAC (EI, 'COMM', .TRUE., ECOMM, STATUS)

                  CALL CAT1_ADDAI (EI, 'XID', .FALSE., XID, STATUS)

               END IF

            END IF

         ELSE

*
*          The identifier does not correspond to a catalogue.  Set the
*          status (if appropriate) and report an error.

            IF (STATUS .EQ. CAT__OK) THEN
               STATUS = CAT__INVID
            END IF

            CALL CAT1_ERREP ('CAT_EIDNT_INID', 'Parent identifier '/
     :        /'of expression does not correspond to a catalogue.',
     :        STATUS)
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_EIDNT: error getting identifier for '/
     :        /'expression ', ERRTXT, ERRLEN)

            IF (EXPR .NE. ' ') THEN
               LEXPR = CHR_LEN(EXPR)

               IF (LEXPR .LE. 20) THEN
                  CALL CHR_PUTC (EXPR(1 : LEXPR), ERRTXT, ERRLEN)
               ELSE
                  CALL CHR_PUTC (EXPR(1 : 17), ERRTXT, ERRLEN)
                  CALL CHR_PUTC ('...', ERRTXT, ERRLEN)
               END IF

            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CAT1_ERREP ('CAT_EIDNT_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
