      SUBROUTINE CAT1_CRTSL (CI, EXPR, CRITL, NUMSEL, SELPTR, SI,
     :  STATUS)
*+
*  Name:
*     CAT1_CRTSL
*  Purpose:
*     Create a selection identifier.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CRTSL (CI, EXPR, CRITL, NUMSEL, SELPTR; SI; STATUS)
*  Description:
*     Create a selection identifier.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier for the parent catalogue of the selection.
*     EXPR  =  CHARACTER*(*) (Given)
*        Expression defining the selection.
*     CRITL  =  LOGICAL (Given)
*        Flag indicating whether or not the selection was created
*        by negating the expression or not, coded as follows:
*        .TRUE.  - selection corresponds to expression,
*        .FALSE. - selection corresponds to negation of expression.
*     NUMSEL  =  INTEGER (Given)
*        Number of selected rows.
*     SELPTR  =  INTEGER (Given)
*        Pointer to the list of selected rows.
*     SI  =  INTEGER (Returned)
*        Selection identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Construct the criterion for the selection.
*     Attempt to create a selection identifier.
*     If ok then
*       Create the attributes for the selection.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     14/4/94 (ACD): Original version.
*     6/9/94  (ACD): First stable version.
*     14/2/96 (ACD): Fixed bug: argument CRITL changed to data type
*        LOGICAL; it had erroneously been declared as INTEGER.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  NUMSEL,
     :  SELPTR
      LOGICAL
     :  CRITL
      CHARACTER
     :  EXPR*(*)
*  Arguments Returned:
      INTEGER
     :  SI
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  CRIT*(CAT__SZEXP)   ! Selection criterion.
      INTEGER
     :  LEXPR,   ! Length of EXPR (excl. trail. blanks).
     :  LCRIT    !   "    "  CRIT ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Construct the criterion for the selection.

         IF (CRITL) THEN
            CRIT = EXPR

         ELSE
            CRIT = ' '
            LCRIT = 0

            CALL CHR_PUTC ('.NOT. (', CRIT, LCRIT)

            IF (EXPR .NE. ' ') THEN
               LEXPR = CHR_LEN (EXPR)
               CALL CHR_PUTC (EXPR(1 : LEXPR), CRIT, LCRIT)
            END IF

            CALL CHR_PUTC (')', CRIT, LCRIT)
         END IF

*
*       Attempt to create a selection identifier and proceed if ok.

         CALL CAT1_CRTID (CAT__SITYP, CI, SI, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Create the attributes for the selection.

            CALL CAT1_ADDAC (SI, 'EXPR', .FALSE., CRIT, STATUS)
            CALL CAT1_ADDAD (SI, 'DATE', .FALSE., 0.0D0, STATUS)
            CALL CAT1_ADDAC (SI, 'COMM', .TRUE., ' ', STATUS)
            CALL CAT1_ADDAI (SI, 'NUMSEL', .FALSE., NUMSEL, STATUS)
            CALL CAT1_ADDAI (SI, 'PTR', .FALSE., SELPTR, STATUS)
         END IF

      END IF

      END
