      SUBROUTINE CAT_PNEW0 (CI, PTYPE, PNAME, DTYPE, PI, STATUS)
*+
*  Name:
*     CAT_PNEW0
*  Purpose:
*     Create a scalar part (column or parameter).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PNEW0 (CI, PTYPE, PNAME, DTYPE; PI; STATUS)
*  Description:
*     Create a scalar part (column or parameter).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue in which the part is to be created.
*     PTYPE  =  INTEGER (Given)
*        Type of part to be created, coded as follows:
*        CAT__FITYP - column (or field),
*        CAT__QITYP - parameter.
*     PNAME  =  CHARACTER*(*) (Given)
*        Name of the part to be created.
*     DTYPE  =  INTEGER (Given)
*        Integer code for the data type of the part to be created.
*     PI  =  INTEGER (Returned)
*        Identifier to the new part.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the code for the type of identifier corresponds to a part (ie.
*     a column or parameter) then
*       If the part name does not already exist in the catalogue then
*         Get the array element for the catalogue.
*         Attempt to get an identifier for the part.
*         If ok then
*           If the part is a column then
*             Create the default set of values for the attributes of a
*             column.
*             Set the values of the attributes corresponding to the name
*             and the data type (ie. the ones which are passed here as
*             arguments).
*             Create the attributes (they are all mutable at this stage
*             except those for the name and the data type).
*             If (all is ok) then
*               Increment the number of physical columns.
*             end if
*           else (the part is parameter)
*             Create the default set of values for the attributes of a
*             parameter.
*             Set the values of the attributes corresponding to the name
*             and the data type (ie. the ones which are passed here as
*             arguments).
*             Create the attributes (they are all mutable at this stage
*             except those for the name and the data type).
*             If (all is ok) then
*               Increment the number of parameters.
*             end if
*           end if
*         end if
*       else
*         Set the status: duplicate column name.
*       end if
*     else
*       Report error: invalide identifier type.
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     When a new column is created the number of physical columns is
*     incremented.  This is something of a kludge.  It is ok whilst
*     physical columns are the only sort supported, but will have to
*     be re-thought when virtual columns are implemented.
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
*     3/5/93   (ACD): Prologue only.
*     5/8/93   (ACD): Original version.
*     18/10/93 (ACD): First implementation.
*     24/1/94  (ACD): Modified error reporting.
*     28/3/97  (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     21/12/99 (ACD): Added check that the given column or parameter
*        name is unique, ie. does not alreay exist in the catalogue.
*     27/7/20 (DSB): Must use locom values, not null values, with
*                    _LOGICAL columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  PTYPE,
     :  DTYPE
      CHARACTER
     :  PNAME*(*)
*  Arguments Returned:
      INTEGER
     :  PI
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  EXIST  ! Flag; does the given name already exist in the catalogue?
      INTEGER
     :  CIELM,     ! Common block array element for the catalogue.
     :  LPNAME,    ! Length of PNAME  (excl. trail. blanks).
     :  ERRLEN     !   "    "  ERRTXT ( "  .   "  .   "   ).
      CHARACTER
     :  ERRTXT*75  ! Text of error message.

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
     :  FXCEPT*(10),         ! Exception value attribute.
     :  FUNIT*(CAT__SZUNI),  ! Units attribute.
     :  FXFMT*(CAT__SZEXF),  ! External format attribute.
     :  FCOMM*(CAT__SZCOM)   ! Comments attribute for the column.
      DOUBLE PRECISION
     :  FSCALE,  ! Scale factor attribute.
     :  FZERO,   ! Zero point attribute.
     :  FDATE    ! Modification date attribute.
      LOGICAL
     :  FPDISP   ! Preferential display flag attribute.

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
*       Check if the code for the type of identifier corresponds to a
*       part (ie. a column or parameter).

         IF (PTYPE .EQ. CAT__FITYP  .OR.  PTYPE .EQ. CAT__QITYP) THEN

*
*          Check whether the given name is unique, ie. whether a
*          column or parameter of this name already exists in the
*          catalogue, and proceed if the name is unique.

            CALL CAT1_NMCHK (CI, PNAME, EXIST, STATUS)
            IF (.NOT. EXIST) THEN

*
*             Attempt to create an identifier for the part.

               CALL CAT1_CRTID (PTYPE, CI, PI, STATUS)
               IF (STATUS .EQ. CAT__OK) THEN

*
*                Get the common block array element for the catalogue.

                  CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*                Handle the case where the part is a column.

                  IF (PTYPE .EQ. CAT__FITYP) THEN

*
*                   Create the default set of values for the attributes of
*                   a column.

                     CALL CAT1_DFATT (FNAME, FGENUS, FEXP, FDTYPE,
     :                 FCSIZE, FDIM, FSIZE, FNULL, FXCEPT, FSCALE,
     :                 FZERO, FORDER, FDATE, FUNIT, FXFMT, FPDISP,
     :                 FCOMM, STATUS)

*
*                   Set the values of the attributes corresponding to the
*                   name and the data type (ie. the ones which are passed
*                   here as arguments).

                     FNAME = PNAME
                     FDTYPE = DTYPE

*
*                   Must use locum values, not nulls, with _LOGICAL columns
                     IF( FDTYPE .EQ. CAT__TYPEL ) FNULL = CAT__LOCUM

*
*                   Create the attributes (they are all mutable at this
*                   stage except those for the name and the data type).

                     CALL CAT1_ADDAC (PI, 'NAME', .FALSE., FNAME,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'GENUS', .TRUE., FGENUS,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'EXPR', .TRUE., FEXP,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'DTYPE', .FALSE., FDTYPE,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'CSIZE', .TRUE., FCSIZE,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'DIMS', .TRUE., FDIM,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'SIZE', .TRUE., FSIZE,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'NULL', .TRUE., FNULL,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'EXCEPT', .TRUE., FXCEPT,
     :                 STATUS)
                     CALL CAT1_ADDAD (PI, 'SCALEF', .TRUE., FSCALE,
     :                 STATUS)
                     CALL CAT1_ADDAD (PI, 'ZEROP', .TRUE., FZERO,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'ORDER', .TRUE., FORDER,
     :                 STATUS)
                     CALL CAT1_ADDAD (PI, 'DATE', .TRUE., FDATE,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'UNITS', .TRUE., FUNIT,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'EXFMT', .TRUE., FXFMT,
     :                 STATUS)
                     CALL CAT1_ADDAL (PI, 'PRFDSP', .TRUE., FPDISP,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'COMM', .TRUE., FCOMM,
     :                 STATUS)

*
*                   If all is ok then increment the number of physical
*                   columns.

                     IF (STATUS .EQ. CAT__OK) THEN
                        NPCOL__CAT1(CIELM) = NPCOL__CAT1(CIELM) + 1
                     END IF

*
*                Handle the case where the part is a parameter.

                  ELSE

*
*                   Create the default set of values for the attributes of
*                   a parameter.

                     CALL CAT1_DPATT (QNAME, QDTYPE, QCSIZE, QDIM,
     :                 QSIZE, QDATE, QUNIT, QXFMT, QPDISP, QCOMM,
     :                 QVALUE, STATUS)

*
*                   Set the values of the attributes corresponding to the
*                   name and the data type (ie. the ones which are passed
*                   here as arguments).

                     QNAME = PNAME
                     QDTYPE = DTYPE

*
*                   Create the attributes (they are all mutable at this
*                   stage except those for the name and the data type).

                     CALL CAT1_ADDAC (PI, 'NAME', .FALSE., QNAME,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'DTYPE', .FALSE., QDTYPE,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'CSIZE', .TRUE., QCSIZE,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'DIMS', .TRUE., QDIM,
     :                 STATUS)
                     CALL CAT1_ADDAI (PI, 'SIZE', .TRUE., QSIZE,
     :                 STATUS)
                     CALL CAT1_ADDAD (PI, 'DATE', .TRUE., QDATE,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'UNITS', .TRUE., QUNIT,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'EXFMT', .TRUE., QXFMT,
     :                 STATUS)
                     CALL CAT1_ADDAL (PI, 'PRFDSP', .TRUE., QPDISP,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'COMM', .TRUE., QCOMM,
     :                 STATUS)
                     CALL CAT1_ADDAC (PI, 'VALUE', .TRUE., QVALUE,
     :                 STATUS)

*
*                   If all is ok then increment the number of parameters.

                     IF (STATUS .EQ. CAT__OK) THEN
                        NPAR__CAT1(CIELM) = NPAR__CAT1(CIELM) + 1
                     END IF

                  END IF
               END IF
            ELSE

*
*             A column or parameter with the given name already exists
*             in the catalogue; set the status.

               STATUS = CAT__DUPNM

            END IF

         ELSE
            STATUS = CAT__ERROR

            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Identifier type does not correspond to a '/
     :        /'column or parameter (code ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (PTYPE, ERRTXT, ERRLEN)
            CALL CHR_PUTC (').', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_PNEW0_NCP', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_PNEW0: failed to create column or '/
     :        /'parameter ', ERRTXT, ERRLEN)

            IF (PNAME .NE. ' ') THEN
               LPNAME = CHR_LEN (PNAME)
               CALL CHR_PUTC (PNAME(1 : LPNAME), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_PNEW0_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
