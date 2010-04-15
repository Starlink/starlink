      SUBROUTINE SET_SEED(STATUS)
*+
* Name:
*    SET_SEED

* Invocation:
*    CALL SET_SEED(STATUS)

* Purpose:
*   Sets random-number seed for PDA routines.

* Description:
*   This routine uses PSX_TIME to obtain a starting INTEGER to feed to
*   PDA_RNSED to set the random number seed.

* Arguments:
*   STATUS = INTEGER (Given and returned)
* Authors:
*    J.W.Palmer, Manchester 27-02-97
* History:
*-
      IMPLICIT NONE
      INCLUDE 'status_inc'
      INCLUDE 'SAE_PAR'
      INTEGER TICKS, SEED
      INTEGER STATUS
      EXTERNAL PSX_TIME, PDA_RNSED

*


* Return if not ok on entry

      IF(STATUS .NE. SAI__OK) RETURN
      CALL PSX_TIME(TICKS, STATUS)
      IF(STATUS .NE. SAI__OK) RETURN
      SEED = ( TICKS / 4 ) * 4 + 1
      CALL PDA_RNSED( SEED, STATUS )

      END










