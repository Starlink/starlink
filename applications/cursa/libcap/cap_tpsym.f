      SUBROUTINE CAP_TPSYM (PLTSYM, PGSYMB, STATUS)
*+
*  Name:
*     CAP_TPSYM
*  Purpose:
*     Translate a CURSA plotting symbol to a PGPLOT one.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_TPSYM (PLTSYM; PGSYMB; STATUS)
*  Description:
*     Translate a CURSA plotting symbol to the corresponding PGPLOT one.
*  Arguments:
*     PLTSYM  =  CHARACTER*(*) (Given)
*        CURSA plotting symbol.  The symbol is assumed to already
*        be in upper case.
*     PGSYMB  =  INTEGER (Returned)
*        PGPLOT plotting symbol.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the plotting symbol.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     9/7/98 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
C      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'SGZ_PAR'           ! catview parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! catview common block.
*  Arguments Given:
      CHARACTER
     :  PLTSYM*(*)
*  Arguments Returned:
      INTEGER
     :  PGSYMB
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Translate the plotting symbol.

         IF (PLTSYM .EQ. 'DOT') THEN
            PGSYMB = 1
         ELSE IF (PLTSYM .EQ. 'OPENCIRCLE') THEN
            PGSYMB = 4
         ELSE IF (PLTSYM .EQ. 'FILLEDCIRCLE') THEN
            PGSYMB = 17
         ELSE IF (PLTSYM .EQ. 'OPENSQUARE') THEN
            PGSYMB = 6
         ELSE IF (PLTSYM .EQ. 'FILLEDSQUARE') THEN
            PGSYMB = 16
         ELSE IF (PLTSYM .EQ. 'OPENTRIANGLE') THEN
            PGSYMB = 7
         ELSE IF (PLTSYM .EQ. 'FILLEDTRIANGLE') THEN
            PGSYMB = 13
         ELSE IF (PLTSYM .EQ. 'OPENSTAR') THEN
            PGSYMB = 12
         ELSE IF (PLTSYM .EQ. 'FILLEDSTAR') THEN
            PGSYMB = 18
         ELSE IF (PLTSYM .EQ. 'PLUS') THEN
            PGSYMB = 2
         ELSE IF (PLTSYM .EQ. 'MULT') THEN
            PGSYMB = 5
         ELSE IF (PLTSYM .EQ. 'ASTERISK') THEN
            PGSYMB = 3
         ELSE
            PGSYMB = 4

            CALL MSG_SETC ('PLTSYM', PLTSYM)
            CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid plotting symbol '/
     :        /'^PLTSYM was ignored.', STATUS)
         END IF

      END IF

      END
