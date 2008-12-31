*+TARG_DELETE      Deletes a target record
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*--------------------------------------------------------------------------
      SUBROUTINE TARG_DELETE(NDELETE)
      IMPLICIT NONE
 
*  Calling Arguments
      INTEGER NDELETE		! Record to delete
 
*  Global Variables
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_target.inc'		! To clear TARG_NO
 
*******************************************************************************
* History
*     1988 Dec 9th	MJR	First Version
*     1989 Jan          ::	Mods for split form
******************************************************************************
*-
*  Local Variables
      INTEGER KTARGET, IERR, RECN
      CHARACTER*3  CNUM
 
*  Executable Code
 
      write(9,*) 'targ_delete',ndelete
      WRITE( CNUM, '(I3)')NTARGET-1

      CALL DBS_PUTC(REF_FORM, FLD_NTARGETS, CNUM, IERR)				! Decrement No. targets in cover details
      RECN = 1
      CALL FORM_WRITE(REF_FORM,RECN,IERR)
      IF (IERR .NE.0) GOTO 99
 
      IF (NDELETE .LT. NTARGET) THEN
         DO KTARGET = NDELETE, NTARGET - 1					! For each rec except the one to be deleted
            CALL DBS_COPY ( REF_TARGET, KTARGET+1, KTARGET, IERR )		! Copy down one place in file
            IF (IERR.NE.0) GOTO 99
         END DO
      END IF
      TARG_NO(NTARGET) = 0
      NTARGET = NTARGET - 1
 
99    CONTINUE
 
      END
