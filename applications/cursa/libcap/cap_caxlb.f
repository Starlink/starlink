      SUBROUTINE CAP_CAXLB (CI, EXPR, LABEL, STATUS)
*+
*  Name:
*     CAP_CAXLB
*  Purpose:
*     Construct a scatterplot axis label.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CAXLB (CI, EXPR; LABEL; STATUS)
*  Description:
*     Construct a scatterplot axis label.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     EXPR  =  CHARACTER*(*)  (Given)
*        Expression which defines the axis to be labelled.
*     LABEL  =  CHARACTER*(*)  (Returned)
*        Label for the axis.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the expression.
*     Determine the type of the identifier.
*     If the expression is a simple column then
*       Get the units of the column.
*     else
*       Set the units to blank.
*     end if
*     If the status is bad then
*       Annull the status.
*     end if
*     Assemble the label.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     9/7/98  (ACD): Original version.
*     10/7/98 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  EXPR*(*)
*  Arguments Returned:
      CHARACTER
     :  LABEL*(*)
*  Status:
      INTEGER STATUS         ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  EID,      ! Expression identifier.
     :  IDTYPE,   ! Type of identifier.
     :  LEXPR,    ! Length of EXPR  (excl. trail. blanks).
     :  LUNITS,   !   "    "  UNITS ( "  .   "  .   "   ).
     :  LLABEL    !   "    "  LABEL ( "  .   "  .   "   ).
      CHARACTER
     :  UNITS*(CAT__SZUNI)   ! Column units.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to get an identifier for the expression.

         CALL CAT_TIDNT (CI, EXPR, EID, STATUS)

*
*       Determine the type of the identifier.

         CALL CAT_TIDTP (EID, IDTYPE, STATUS)

*
*       If the expression is a simple column then get the units.

         IF (STATUS .EQ. SAI__OK  .AND.  IDTYPE .EQ. CAT__FITYP) THEN
            CALL CAT_TIQAC (EID, 'UNITS', UNITS, STATUS)
         ELSE
            UNITS = ' '
         END IF

*
*       If the status is bad then annull the status and set the
*       units to blank.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)

            UNITS = ' '
         END IF

*
*       Assemble the label.

         LABEL = ' '
         LLABEL = 0

         IF (EXPR .NE. ' ') THEN
            LEXPR = CHR_LEN(EXPR)
            CALL CHR_PUTC (EXPR(1 : LEXPR), LABEL, LLABEL)
         END IF

         IF (UNITS .NE. ' ') THEN
            CALL CHR_PUTC (' (', LABEL, LLABEL)

            LUNITS = CHR_LEN(UNITS)
            CALL CHR_PUTC (UNITS(1 : LUNITS),LABEL, LLABEL)

            CALL CHR_PUTC (')', LABEL, LLABEL)
         END IF

         IF (LABEL .EQ. ' ') THEN
            LABEL = '<no label>'
         END IF

      END IF

      END
