*+FORM_WRITE       Writes record, displays any error message
*     1993 June         P. Brisco       Delete SMG stuff.
*---------------------------------------------------------------------------
      SUBROUTINE FORM_WRITE(REF_NO, IREC, IERR)
      IMPLICIT NONE

*  Calling Arguments
      INTEGER REF_NO		! In	File ref.
      INTEGER IREC		!	Record number
      INTEGER IERR		! Out	Status, 0: OK

*  Functions
      CHARACTER*1 FORM_GETC

*  Local Variables
      CHARACTER*1 TEXT
      CHARACTER*44 ERR_MESSAGE
     &         /'Fortran Write error      , <ret> to continue'/

* ________________________ Executable Code _____________________________

      CALL FORM_CHECKSUM(REF_NO,IREC)		! Fill checksum bytes

      CALL DBS_WRITE(REF_NO, IREC, IERR)
      IF (IERR.NE.0) THEN
         WRITE(ERR_MESSAGE(20:25),'(I6)') IERR
         TEXT = FORM_GETC(ERR_MESSAGE,' ')
      END IF

      END
