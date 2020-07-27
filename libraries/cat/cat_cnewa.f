      SUBROUTINE CAT_CNEWA (CI, FNAME, EXPR, DTYPE, CSIZE, DIMS, SIZEA,
     :  NULL, EXCEPT, SCALEF, ZEROP, ORDER, UNITS, EXTFMT, PRFDSP,
     :  COMM, FI, STATUS)
*+
*  Name:
*     CAT_CNEWA
*  Purpose:
*     Create a column, simultaneously setting all its attributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_CNEWA (CI, FNAME, EXPR, DTYPE, CSIZE, DIMS, SIZEA,
*       NULL, EXCEPT, SCALEF, ZEROP, ORDER, UNITS, EXTFMT, PRFDSP,
*       COMM; FI; STATUS)
*  Description:
*     Create a column, simultaneously setting all its attributes.
*
*     Note that the GENUS attribute is automatically set to CAT__GPHYS.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FNAME  =  CHARACTER*(*) (Given)
*        Name of the column (or field).
*     EXPR  =  CHARACTER*(*) (Given)
*        The expression defining a virtual column; blank for a physical
*        column.
*     DTYPE  =  INTEGER (Given)
*        Type of the column.
*     CSIZE  =  INTEGER (Given)
*        Size of a CHARACTER column.  If the column is not of type
*        CHARACTER CSIZE is irrelevant; it is conventional to set it
*        to zero.
*     DIMS  =  INTEGER (Given)
*        Dimensionality of the column.  The permitted values are:
*        CAT__SCALR  -  scalar,
*        CAT__VECTR  -  vector.
*     SIZEA  =  INTEGER (Given)
*        If the column is a vector this attribute should be set to the
*        number of elements in the vector.  For a scalar it should be
*        set to one.
*     NULL  =  INTEGER (Given)
*        The way that null values are handled for the column.  The
*        permitted values are:
*        CAT__NULLD - default, HDS null values used,
*        CAT__NULLS - null values explictly specified for the column,
*        CAT__LOCUM - null values not supported for the column and a
*                     locum value used instead.
*     EXCEPT  =  CHARACTER*(*) (Given)
*        In the cases where either the column supports explicitly
*        specified null values, or nulls are not supported and a locum
*        is used instead, EXCEPT contains the required value, written
*        into a character string.
*     SCALEF  =  DOUBLE PRECISION (Given)
*        Scale factor for scaled columns.  1.0D0 for columns which are
*        not scaled.
*     ZEROP  =  DOUBLE PRECISION (Given)
*        Zero point for scaled columns.  0.0D0 for columns which are
*        not scaled.
*     ORDER  =  INTEGER (Given)
*        The order in which values occur in the column, coded as
*        follows:
*        CAT__ASCND - ascending,
*        CAT__DSCND - descending,
*        CAT__NOORD - none.
*     UNITS  =  CHARACTER*(*) (Given)
*        The units of the column.
*     EXTFMT  =  CHARACTER*(*) (Given)
*        The external format for the column.
*     PRFDSP  =  LOGICAL (Given)
*        The preferential display flag for the column:
*        .TRUE.  - display the column by default,
*        .FALSE. - do not display the column by default.
*     COMM  =  CHARACTER*(*) (Given)
*        Comments about the column.
*     FI  =  INTEGER (Returned)
*        Identifier for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the column name does not already exist in the catalogue then
*       Attempt to get an identifier for the column.
*       If ok then
*         Get the array element for the catalogue.
*         Create the attributes (they are all mutable at this stage
*         except those for the name and the data type).
*         If (all is ok) then
*           Increment the number of physical columns.
*         end if
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
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
*     23/11/94(ACD): Original version.
*     21/12/99 (ACD): Added check that the given column or name is unique,
*        ie. does not alreay exist in the catalogue.
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
     :  DTYPE,
     :  CSIZE,
     :  DIMS,
     :  SIZEA,
     :  NULL,
     :  ORDER
      CHARACTER
     :  FNAME*(*),
     :  EXPR*(*),
     :  EXCEPT*(*),
     :  UNITS*(*),
     :  EXTFMT*(*),
     :  COMM*(*)
      DOUBLE PRECISION
     :  SCALEF,
     :  ZEROP
      LOGICAL
     :  PRFDSP
*  Arguments Returned:
      INTEGER
     :  FI
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  EXIST  ! Flag; does the given name already exist in the catalogue?
      INTEGER
     :  CIELM,     ! Common block array element for the catalogue.
     :  LFNAME,    ! Length of FNAME  (excl. trail. blanks).
     :  ERRLEN     !   "    "  ERRTXT ( "  .   "  .   "   ).
      DOUBLE PRECISION
     :  DATE       ! Modification date attribute.
      CHARACTER
     :  ERRTXT*75  ! Text of error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*       Report an error and return if an attempt it made to create a
*       _LOGICAL column with null values.
         IF( DTYPE .EQ. CAT__TYPEL .AND. NULL .NE. CAT__LOCUM ) THEN
            STATUS = CAT__INVNL
            CALL CAT1_ERREP (' ', 'Attempt to create a _LOGICAL '//
     :                       'column with null values.', STATUS )
            CALL CAT1_ERREP (' ', 'The CAT library does not support '//
     :                       '_LOGICAL columns with null values.',
     :                       STATUS )

            RETURN
         END IF

*       Check whether the given column name is unique, ie. whether a
*       column or parameter of this name already exists in the catalogue,
*       and proceed if the name is unique.

         CALL CAT1_NMCHK (CI, FNAME, EXIST, STATUS)
         IF (.NOT. EXIST) THEN

*
*          Attempt to create an identifier for the column.

            CALL CAT1_CRTID (CAT__FITYP, CI, FI, STATUS)
            IF (STATUS .EQ. CAT__OK) THEN

*
*             Get the common block array element for the catalogue.

               CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*             Get the creation date.

               CALL CAT1_GTDAT (DATE, STATUS)

*
*             Create the attributes (they are all mutable at this stage
*             except those for the name and the data type).

               CALL CAT1_ADDAC (FI, 'NAME', .FALSE., FNAME, STATUS)
               CALL CAT1_ADDAI (FI, 'GENUS', .TRUE., CAT__GPHYS, STATUS)
               CALL CAT1_ADDAC (FI, 'EXPR', .TRUE., EXPR, STATUS)
               CALL CAT1_ADDAI (FI, 'DTYPE', .FALSE., DTYPE, STATUS)
               CALL CAT1_ADDAI (FI, 'CSIZE', .TRUE., CSIZE, STATUS)
               CALL CAT1_ADDAI (FI, 'DIMS', .TRUE., DIMS, STATUS)
               CALL CAT1_ADDAI (FI, 'SIZE', .TRUE., SIZEA, STATUS)
               CALL CAT1_ADDAI (FI, 'NULL', .TRUE., NULL, STATUS)
               CALL CAT1_ADDAC (FI, 'EXCEPT', .TRUE., EXCEPT, STATUS)
               CALL CAT1_ADDAD (FI, 'SCALEF', .TRUE., SCALEF, STATUS)
               CALL CAT1_ADDAD (FI, 'ZEROP', .TRUE., ZEROP, STATUS)
               CALL CAT1_ADDAI (FI, 'ORDER', .TRUE., ORDER, STATUS)
               CALL CAT1_ADDAD (FI, 'DATE', .TRUE., DATE, STATUS)
               CALL CAT1_ADDAC (FI, 'UNITS', .TRUE., UNITS, STATUS)
               CALL CAT1_ADDAC (FI, 'EXFMT', .TRUE., EXTFMT, STATUS)
               CALL CAT1_ADDAL (FI, 'PRFDSP', .TRUE., PRFDSP, STATUS)
               CALL CAT1_ADDAC (FI, 'COMM', .TRUE., COMM, STATUS)

*
*             If all is ok then increment the number of physical columns.

               IF (STATUS .EQ. CAT__OK) THEN
                  NPCOL__CAT1(CIELM) = NPCOL__CAT1(CIELM) + 1
               END IF

            END IF
         ELSE

*
*          A column of the given name already exists; set the status.

            STATUS = CAT__DUPNM

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_CNEWA: failed to create column ',
     :         ERRTXT, ERRLEN)

            IF (FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN (FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_CNEWA_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
