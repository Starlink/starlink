      SUBROUTINE CAT_IINQ (II, CI, FI, ORDER, NUMSEL, COMM, DATE,
     :  STATUS)
*+
*  Name:
*     CAT_IINQ
*  Purpose:
*     Inquire all the attributes of an index.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_IINQ (II; CI, FI, ORDER, NUMSEL, COMM, DATE, STATUS)
*  Description:
*     Inquire all the attributes of an index.
*  Arguments:
*     II  =  INTEGER (Given)
*        Index identifier.
*     CI  =  INTEGER (Returned)
*        Catalogue identifier for the parent catalogue of the index.
*     FI  =  INTEGER (Returned)
*        Column identifier for the column from which the index was
*        created.
*     ORDER  =  INTEGER (Returned)
*        Order of the index, coded as follows:
*        CAT__ASCND  -  ascending,
*        CAT__DSCND  -  descending.
*     NUMSEL  =  INTEGER (Returned)
*        Number of rows in the index.
*     COMM  =  CHARACTER*(*) (Returned)
*        Comments for the index.
*     DATE  =  DOUBLE PRECISION (Returned)
*        Date the index was created.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the parent catalogue of the index.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     10/6/98 (ACD): Original version.
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
     :  II
*  Arguments Returned:
      INTEGER
     :  CI,
     :  FI,
     :  ORDER,
     :  NUMSEL
      CHARACTER
     :  COMM*(*)
      DOUBLE PRECISION
     :  DATE
*  Status:
      INTEGER STATUS        ! Global status.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Obtain the parent catalogue of the index.

         CALL CAT_TIDPR (II, CI, STATUS)

*
*       Inquire the value of each attribute.

         CALL CAT_TIQAI (II, 'COLID', FI, STATUS)
         CALL CAT_TIQAI (II, 'ORDER', ORDER, STATUS)
         CALL CAT_TIQAI (II, 'NUMSEL', NUMSEL, STATUS)
         CALL CAT_TIQAC (II, 'COMM', COMM, STATUS)
         CALL CAT_TIQAD (II, 'DATE', DATE, STATUS)

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_IINQ_ERR', 'CAT_IINQ: Error '/
     :        /'inquiring all attribute values for an index.',
     :        STATUS)
         END IF

      END IF

      END
