*+FORM_READ        Reads record, displays any error message
*     1993 Jun          P. Brisco      Removed SMG junque.
*     1994 Feb          M Ricketts     Cut out error messages
*---------------------------------------------------------------------------
      SUBROUTINE FORM_READ(REF_NO, IREC, IERR)
      IMPLICIT NONE
 
*  Calling Arguments
      INTEGER REF_NO		! In	File ref.
      INTEGER IREC		!	Record number
      INTEGER IERR		! Out	Status, 0: OK
 
*  Functions
      CHARACTER*1 FORM_GETC
 
*  Local Variables
      CHARACTER*1 TEXT
      CHARACTER*43 
     &  ERROR_MESSAGE/'Fortran Read error      , <RET> to continue'/
* ________________________ Executable Code _______________________________
 
      CALL DBS_READ(REF_NO, IREC, IERR)

      IF (IERR.NE.0) THEN
         WRITE(ERROR_MESSAGE(19:24),'(I6)') IERR
         TEXT = FORM_GETC(ERROR_MESSAGE,' ')
      END IF

      END
