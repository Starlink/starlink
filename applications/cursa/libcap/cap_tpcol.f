      SUBROUTINE CAP_TPCOL (COLOUR, PGCOL, STATUS)
*+
*  Name:
*     CAP_TPCOL
*  Purpose:
*     Translate a CURSA colour to the corresponding PGPLOT colour index.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_TPCOL (COLOUR; PGCOL; STATUS)
*  Description:
*     Translate a CURSA colour to the corresponding PGPLOT colour index.
*     Note that this routine assumes that the default PGPLOT colour
*     representations have not been altered.
*  Arguments:
*     COLOUR  =  CHARACTER*(*) (Given)
*        CURSA colour.  The symbol is assumed to already be in upper
*        case.
*     PGCOL  =  INTEGER (Returned)
*        PGPLOT colour index.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the colour.
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
     :  COLOUR*(*)
*  Arguments Returned:
      INTEGER
     :  PGCOL
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Translate the colour.

         IF (COLOUR .EQ. 'DEFAULT') THEN
            PGCOL = 1
         ELSE IF (COLOUR .EQ. 'RED') THEN
            PGCOL = 2
         ELSE IF (COLOUR .EQ. 'GREEN') THEN
            PGCOL = 3
         ELSE IF (COLOUR .EQ. 'BLUE') THEN
            PGCOL = 4
         ELSE IF (COLOUR .EQ. 'CYAN') THEN
            PGCOL = 5
         ELSE IF (COLOUR .EQ. 'MAGENTA') THEN
            PGCOL = 6
         ELSE IF (COLOUR .EQ. 'YELLOW') THEN
            PGCOL = 7
         ELSE
            PGCOL = 1

            CALL MSG_SETC ('COLOUR', COLOUR)
            CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid colour '/
     :        /'^COLOUR was ignored.', STATUS)
         END IF

      END IF

      END
