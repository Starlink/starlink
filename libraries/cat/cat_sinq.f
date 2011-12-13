      SUBROUTINE CAT_SINQ (SI, CI, EXPR, NUMSEL, COMM, DATE, STATUS)
*+
*  Name:
*     CAT_SINQ
*  Purpose:
*     Inquire all the attributes of a selection.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SINQ (SI; CI, EXPR, NUMSEL, COMM, DATE; STATUS)
*  Description:
*     Inquire all the attributes of a selection.
*  Arguments:
*     SI  =  INTEGER (Given)
*        Selection identifier.
*     CI  =  INTEGER (Returned)
*        Catalogue identifier for the parent catalogue of the selection.
*     EXPR  =  CHARACTER*(*) (Returned)
*        Expression defining the selection.
*     NUMSEL  =  INTEGER (Returned)
*        Number of rows in the selection.
*     COMM  =  CHARACTER*(*) (Returned)
*        Comments for the selection.
*     DATE  =  DOUBLE PRECISION (Returned)
*        Date the selection was created.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the parent catalogue of the selection.
*     Inquire the value of each attribute.
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
*     14/4/94  (ACD): Original version.
*     23/11/94 (ACD): Fixed bug in prologue comments.
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
     :  SI
*  Arguments Returned:
      INTEGER
     :  CI,
     :  NUMSEL
      CHARACTER
     :  EXPR*(*),
     :  COMM*(*)
      DOUBLE PRECISION
     :  DATE
*  Status:
      INTEGER STATUS        ! Global status.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Obtain the parent catalogue of the selection.

         CALL CAT_TIDPR (SI, CI, STATUS)

*
*       Inquire the value of each attribute.

         CALL CAT_TIQAC (SI, 'EXPR', EXPR, STATUS)
         CALL CAT_TIQAI (SI, 'NUMSEL', NUMSEL, STATUS)
         CALL CAT_TIQAC (SI, 'COMM', COMM, STATUS)
         CALL CAT_TIQAD (SI, 'DATE', DATE, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_SINQ_ERR', 'CAT_SINQ: Error '/
     :        /'inquiring all attribute values for a selection.',
     :        STATUS)
         END IF

      END IF

      END
