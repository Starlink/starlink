      SUBROUTINE CAT_CINQ (FI, SZDIM, CI, FNAME, GENUS, EXPR, DTYPE,
     :  CSIZE,DIMS, SIZEA, NULL, EXCEPT, SCALEF, ZEROP, ORDER, UNITS,
     :  EXTFMT, PRFDSP, COMM, DATE, STATUS)
*+
*  Name:
*     CAT_CINQ
*  Purpose:
*     Inquire the values of all the attributes for a column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_CINQ (FI, SZDIM; CI, FNAME, GENUS, EXPR, DTYPE, CSIZE,
*       DIMS, SIZEA, NULL, EXCEPT, SCALEF, ZEROP, ORDER, UNITS, EXTFMT,
*       PRFDSP, COMM, DATE; STATUS)
*  Description:
*     Inquire the values of all the attributes for a column. Type
*     conversions are performed if necessary. If the conversion fails a
*     status is set (this is only likely to be important for null
*     values).  Note that the exception value is forced into type
*     character in order to avoid having a family of routines.  Note
*     also that the genus attribute is returned explicitly.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Identifier to the column.
*     SZDIM  =  INTEGER (Given)
*        Maximum permitted dimensionality for a column (defines the
*        size of array SIZEA).
*     CI  =  INTEGER (Returned)
*        Catalogue identifier.
*     FNAME  =  CHARACTER*(*) (Returned)
*        Name of the column (or field).
*     GENUS  =  INTEGER (Returned)
*        The genus of the column, coded as follows:
*        CAT__GVIRT - virtual column,
*        CAT__GPHYS - physical column.
*     EXPR  =  CHARACTER*(*) (Returned)
*        The expression defining a virtual column; blank for a physical
*        column.
*     DTYPE  =  INTEGER (Returned)
*        Type of the column.  The permitted types are identical to
*        HDS types.
*     CSIZE  =  INTEGER (Returned)
*        Size of a character column.  If the column is not of type
*        character CSIZE is set to zero.
*     DIMS  =  INTEGER (Returned)
*        Dimensionality of the column.  For a scalar DIMS = 0.
*     SIZEA(SZDIM)  =  INTEGER (Returned)
*        The size of the array in each of its dimensions.  The exception
*        is a scalar column, when SIZEA becomes a single-element,
*        one-dimensional array, set to 0.
*     NULL  =  INTEGER (Returned)
*        The way that null values are handled for the column.  The
*        permitted values are:
*        CAT__NULLD - default, HDS null values used,
*        CAT__NULLS - null values explicitly specified for the column,
*        CAT__LOCUM - null values not supported for the column and a
*                     locum value used instead.
*     EXCEPT  =  CHARACTER*(*) (Returned)
*        In the cases where either the column supports explicitly
*        specified null values, or nulls are not supported and a locum
*        is used instead, EXCEPT contains the required value, written
*        into a character string.
*     SCALEF  =  DOUBLE PRECISION (Returned)
*        Scale factor for scaled columns.  0.0D0 for columns which are
*        not scaled.
*     ZEROP  =  DOUBLE PRECISION (Returned)
*        Zero point for scaled columns.  0.0D0 for columns which are
*        not scaled.
*     ORDER  =  INTEGER (Returned)
*        The order in which values occur in the column, coded as
*        follows:
*        CAT__ASCND - ascending,
*        CAT__DSCND - descending,
*        CAT__NOORD - none.
*     UNITS  =  CHARACTER*(*) (Returned)
*        The units of the column.
*     EXTFMT  =  CHARACTER*(*) (Returned)
*        The external format for the column.
*     PRFDSP  =  LOGICAL (Returned)
*        The preferential display flag for the column:
*        .TRUE.  - display the column by default,
*        .FALSE. - do not display the column by default.
*     COMM  =  CHARACTER*(*) (Returned)
*        Comments about the column.
*     DATE  =  DOUBLE PRECISION (Returned)
*        The modification date of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the parent catalogue of the column.
*     For each attribute of the column.
*       Inquire the value of the attribute.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     13/7/93 (ACD): First implementation.
*     24/1/94 (ACD): Modified error reporting.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error constants.
*  Arguments Given:
      INTEGER
     :  FI,
     :  SZDIM
*  Arguments Returned:
      INTEGER
     :  CI,
     :  GENUS,
     :  DTYPE,
     :  CSIZE,
     :  DIMS,
     :  SIZEA(SZDIM),
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
     :  ZEROP,
     :  DATE
      LOGICAL
     :  PRFDSP
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Obtain the parent catalogue of the column.

         CALL CAT_TIDPR (FI, CI, STATUS)

*
*       Inquire the value of each attribute.

         CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)
         CALL CAT_TIQAI (FI, 'GENUS', GENUS, STATUS)
         CALL CAT_TIQAC (FI, 'EXPR', EXPR, STATUS)
         CALL CAT_TIQAI (FI, 'DTYPE', DTYPE, STATUS)
         CALL CAT_TIQAI (FI, 'CSIZE', CSIZE, STATUS)
         CALL CAT_TIQAI (FI, 'DIMS', DIMS, STATUS)
         CALL CAT_TIQAI (FI, 'SIZE', SIZEA(1), STATUS)
         CALL CAT_TIQAI (FI, 'NULL', NULL, STATUS)
         CALL CAT_TIQAC (FI, 'EXCEPT', EXCEPT, STATUS)
         CALL CAT_TIQAD (FI, 'SCALEF', SCALEF, STATUS)
         CALL CAT_TIQAD (FI, 'ZEROP', ZEROP, STATUS)
         CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)
         CALL CAT_TIQAD (FI, 'DATE', DATE, STATUS)
         CALL CAT_TIQAC (FI, 'UNITS', UNITS, STATUS)
         CALL CAT_TIQAC (FI, 'EXFMT', EXTFMT, STATUS)
         CALL CAT_TIQAL (FI, 'PRFDSP', PRFDSP, STATUS)
         CALL CAT_TIQAC (FI, 'COMM', COMM, STATUS)

*
*  Check the string size is not greater than the maximum allowed size.
         IF( CSIZE .GT. CAT__SZVAL .AND. STATUS .EQ. CAT__OK ) THEN
            STATUS = CAT__CSIZE
            CALL MSG_SETC( 'N', FNAME )
            CALL MSG_SETI( 'C', CSIZE )
            CALL MSG_SETI( 'M', CAT__SZVAL )
            CALL ERR_REP( 'CAT_CINQ_CSIZE', 'Size of character column'//
     :                    ' ^N is ^C, but the maximum allowed is ^M.',
     :                    STATUS )
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_CINQ_ERR', 'CAT_CINQ: Error '/
     :        /'inquiring all attribute values for a column.', STATUS)
         END IF

      END IF

      END
