      SUBROUTINE CAP_RASCL (CIIN, STATUS)
*+
*  Name:
*     CAP_RASCL
*  Purpose:
*     Replace any 'ASCALE' functions with corrsponding 'SCALE'.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RASCL (CIIN; STATUS)
*  Description:
*     Replace any 'SCALE' expressions found in the graphics translation
*     file with the corresponding 'SCALE' expressions.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input target list.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check the default sizes.
*     For every IF block
*       For every clause
*         Check the criterion.
*         Check the sizes.
*       end for
*     end for
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/8/96 (ACD): Original version.
*     6/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
*  Global Variables:
      INCLUDE 'CIO_CMN'           ! CIO common block.
*  Arguments Given:
      INTEGER
     :  CIIN
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  NCLS,    ! No. of clauses in the current IF block.
     :  CURBLK,  ! Current IF block.
     :  CURCLS   ! Current clause.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check the default sizes.

         CALL CAP_RPSCL (CIIN, DSIZ1__CIO, STATUS)
         CALL CAP_RPSCL (CIIN, DSIZ2__CIO, STATUS)
         CALL CAP_RPSCL (CIIN, DSIZ3__CIO, STATUS)
         CALL CAP_RPSCL (CIIN, DSIZ4__CIO, STATUS)

*
*       Check the criteria and sizes for all of the clauses of all
*       of the IF blocks.

         DO CURBLK = 1, NIFB__CIO
            NCLS = NCLS__CIO(CURBLK)

            DO CURCLS = 1, NCLS
               CALL CAP_RPSCL (CIIN, CRIT__CIO(CURBLK, CURCLS), STATUS)

               CALL CAP_RPSCL (CIIN, SIZ1__CIO(CURBLK, CURCLS), STATUS)
               CALL CAP_RPSCL (CIIN, SIZ2__CIO(CURBLK, CURCLS), STATUS)
               CALL CAP_RPSCL (CIIN, SIZ3__CIO(CURBLK, CURCLS), STATUS)
               CALL CAP_RPSCL (CIIN, SIZ4__CIO(CURBLK, CURCLS), STATUS)
            END DO
         END DO

      END IF

      END
