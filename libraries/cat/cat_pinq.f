      SUBROUTINE CAT_PINQ (QI, SZDIM, CI, QNAME, DTYPE, CSIZE, DIMS,
     :  SIZEA, UNITS, EXTFMT, PRFDSP, COMM, VALUE, DATE, STATUS)
*+
*  Name:
*     CAT_PINQ
*  Purpose:
*     Inquire the values of all the attributes for a parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PINQ (QI, SZDIM; CI, QNAME, DTYPE, CSIZE, DIMS, SIZEA,
*       UNITS, EXTFMT, PRFDSP, COMM, VALUE, DATE; STATUS)
*  Description:
*     Inquire the values of all the attributes for a parameter. If the
*     parameter is an array, the value of the first element is returned.
*  Arguments:
*     QI  =  INTEGER (Given)
*        Parameter identifier.
*     SZDIM  =  INTEGER (Given)
*        Maximum permitted dimensionality for a column (defines the
*        size of array SIZEA).
*     CI  =  INTEGER (Returned)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Returned)
*        Name of the parameter.
*     DTYPE  =  INTEGER (Returned)
*        Type of the parameter.  The permitted types are identical to
*        HDS types.
*     CSIZE =  INTEGER (Given)
*        Size of a character parameter (set to zero if the column
*        is note of data type character).
*     DIMS  =  INTEGER (Returned)
*        Dimensionality of the parameter.  For a scalar DIMS = 0.
*     SIZEA(SZDIM)  =  INTEGER (Returned)
*        The size of the array in each of its dimensions.  The exception
*        is a scalar parameter, when SIZEA becomes a single-element,
*        one-dimensional array, set to 0.
*     UNITS  =  CHARACTER*(*) (Returned)
*        The units of the parameter.
*     EXTFMT  =  CHARACTER*(*) (Returned)
*        The external format for the parameter.
*     PRFDSP  =  LOGICAL (Returned)
*        The preferential display flag for the parameter:
*        .TRUE.  - display the parameter by default,
*        .FALSE. - do not display the parameter by default.
*     COMM  =  CHARACTER*(*) (Returned)
*        Comments about the parameter.
*     VALUE  =  CHARACTER*(*) (Returned)
*        The value of the parameter.  If the parameter is an array the
*        first element is returned.
*     DATE  =  DOUBLE PRECISION (Returned)
*        Modification date of the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the parent catalogue of the parameter.
*     For each attribute of the parameter.
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
*     3/5/93   (ACD): Prologue only.
*     17/7/93  (ACD): First implementation.
*     24/1/94  (ACD): Modified error reporting.
*     23/11/94 (ACD): Changed the variable holding the name of the
*       parameter from PNAME to QNAME for consistency.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  QI,
     :  SZDIM
*  Arguments Returned:
      INTEGER
     :  CI,
     :  DTYPE,
     :  CSIZE,
     :  DIMS,
     :  SIZEA(SZDIM)
      CHARACTER
     :  QNAME*(*),
     :  UNITS*(*),
     :  EXTFMT*(*),
     :  COMM*(*),
     :  VALUE*(*)
      LOGICAL
     :  PRFDSP
      DOUBLE PRECISION
     :  DATE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Obtain the parent catalogue of the parameter.

         CALL CAT_TIDPR (QI, CI, STATUS)

*
*       Inquire the value of each attribute.

         CALL CAT_TIQAC (QI, 'NAME', QNAME, STATUS)
         CALL CAT_TIQAI (QI, 'DTYPE', DTYPE, STATUS)
         CALL CAT_TIQAI (QI, 'CSIZE', CSIZE, STATUS)
         CALL CAT_TIQAI (QI, 'DIMS', DIMS, STATUS)
         CALL CAT_TIQAI (QI, 'SIZE', SIZEA(1), STATUS)
         CALL CAT_TIQAD (QI, 'DATE', DATE, STATUS)
         CALL CAT_TIQAC (QI, 'UNITS', UNITS, STATUS)
         CALL CAT_TIQAC (QI, 'EXFMT', EXTFMT, STATUS)
         CALL CAT_TIQAL (QI, 'PRFDSP', PRFDSP, STATUS)
         CALL CAT_TIQAC (QI, 'COMM', COMM, STATUS)
         CALL CAT_TIQAC (QI, 'VALUE', VALUE, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_PINQ_ERR', 'CAT_PINQ: error'/
     :        /'inquiring all parameter attributes.', STATUS)
         END IF

      END IF

      END
