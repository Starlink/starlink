*+PROP_CHECK   Gets Proposal Checksum
      INTEGER FUNCTION   PROP_CHECK ()
      IMPLICIT NONE
 
*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_points.inc'
 
*  Functions
      INTEGER FORM_GECHEK
      INTEGER DBS_GETI

*  Local Variables
      INTEGER ISUM, ICHECK,I, NTARGET, IERR, BSUM
 
*  Executable Code
 
      NTARGET = DBS_GETI(REF_FORM,FLD_NTARGETS)
 
      ISUM = FORM_GECHEK(REF_FORM)
*	write(7,*) ' prop_check icheck cover ',isum
      IF (ISUM.LT.0) GOTO 10
 
      DO I=1, NTARGET
         CALL DBS_READ(REF_TARGET,I,IERR)
         IF (IERR.NE.0) THEN
            PROP_CHECK=-99
            GOTO 90
         END IF
         ICHECK = FORM_GECHEK(REF_TARGET)
*	write(7,*) ' prop_check icheck ',icheck
         IF (ICHECK .LT. 0) THEN
            ISUM = ICHECK
            GOTO 10
         END IF
         ISUM = ISUM + ICHECK
*	write(7,*) ' isum ',isum
      END DO

10    CONTINUE
      PROP_CHECK = ISUM
 
90    CONTINUE
      END
