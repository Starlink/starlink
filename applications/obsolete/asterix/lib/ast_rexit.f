*+  AST_REXIT - Output a error message from a routine
      SUBROUTINE AST_REXIT( RTN, STATUS )
*    Description :
*    History :
*     27/4/89 : Original (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*   Import :
      CHARACTER*(*) RTN
*-

*    Check status is not a parameter abort
      IF ( (STATUS.NE.PAR__NULL) .OR.
     :     (STATUS.NE.PAR__ABORT) ) THEN

*      Tag the routine name on the end
        CALL ERR_REP( ' ', '...from '//RTN, STATUS )

      END IF

      END
