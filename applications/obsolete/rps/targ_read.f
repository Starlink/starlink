*+TARG_READ        Reads Target record, updates Target summary
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*---------------------------------------------------------------------------
      SUBROUTINE TARG_READ(NTARG,IERR)
      IMPLICIT NONE
 
*  Calling Arguments
      INTEGER NTARG
      INTEGER IERR
 
*  Global Variables
      INCLUDE 'com_target.inc'
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_qual.inc'
 
*  Functions
      CHARACTER*20 DBS_GETC
      INTEGER DBS_GETI
      INTEGER DBS_FIELDNO
      INTEGER FORM_GECHEK		! Gets checksum
 
*  Local Variables
      INTEGER FIELD_NO
 
*  Executable Code
 
      CALL DBS_READ(REF_TARGET,NTARG,IERR)
      IF (IERR.NE.0) GOTO 99
 
      FIELD_NO = DBS_FIELDNO(REF_TARGET,'TARGET.NAME')
      TARG_NAME(NTARG) = DBS_GETC(REF_TARGET,FIELD_NO)
      FIELD_NO = DBS_FIELDNO(REF_TARGET,'TARGET.NUMBER')
      TARG_NO(NTARG) = DBS_GETI(REF_TARGET,FIELD_NO)
 
*  Get qual flags:
      IF ( FORM_GECHEK(REF_TARGET) .LT. 0) THEN
         QUAL_TARGET(NTARG) = .FALSE.
      ELSE 
         QUAL_TARGET(NTARG) = .TRUE.
      END IF

99    CONTINUE
      END
