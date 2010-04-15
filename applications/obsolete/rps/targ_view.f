*+TARG_VIEW        Gets Target summary
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*---------------------------------------------------------------------------
      SUBROUTINE TARG_VIEW(IERR)
      IMPLICIT NONE

*  Calling Argument
      INTEGER IERR

*  Global Variables
      INCLUDE 'com_target.inc'
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_form_files.inc'

*  Functions
      INTEGER DBS_GETI

*  Local Variables
      INTEGER I

*  Executable Code

      NTARGET = MIN (DBS_GETI(REF_FORM,FLD_NTARGETS), 90 )
      DO I=1,NTARGET
         CALL TARG_READ(I,IERR)
         IF (IERR .NE. 0) GOTO 99
      END DO

99    CONTINUE
      END
