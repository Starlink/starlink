C+
      CHARACTER*(*) FUNCTION ICH_CI (IVAL)
C
C                               I C H _ C I
C
C  Routine: ICH_CI
C
C  Function:
C     Given an integer value, returns a character string.
C
C  Description:
C     ICH_CI formats an integer into a character string, in as
C     few characters as possible.
C
C  Language:
C     FORTRAN
C
C  Call:
C     STRING = ICH_CI (IVAL)
C
C  Parameters:      (">" input, "<" output)
C     (>) IVAL      (Integer, ref) The number to be formatted.
C
C  Returns:
C     (<) STRING    (Fixed string, function value) The number formatted
C                   into a character string.  If the string is too short,
C                   (ie, if ICH_CI has been declared too short), a string of
C                   asterisks will be returned.  ICH_CI should be declared
C                   as CHARACTER*11 or larger.
C
C  External variables used: None
C
C  External routines used:  None.
C
C  Author:  K. Shortridge, AAO
C
C  Date: 5th June 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IVAL
C
C     Local variables
C
      INTEGER   I                         ! Loop variable
      INTEGER   IGNORE                    ! Dummy status value for write
      INTEGER   IST                       ! First non-blank character
      CHARACTER WORK*11                   ! Work string
C
C     Format the number into the work string.
C
      WRITE (WORK,'(I11)',IOSTAT=IGNORE) IVAL
C
C     Now try to tidy it up a bit.  Find the first non-blank
C     character.
C
      DO I=1,LEN(WORK)
         IF (WORK(I:I).NE.' ') THEN
            IST=I
            GO TO 320
         END IF
      END DO
      IST=LEN(WORK)
  320 CONTINUE
C
C     Now copy the result into the return function value,
C     assuming it will fit.
C
      IF (LEN(ICH_CI).LT.(11-IST+1)) THEN
         ICH_CI='**************'
      ELSE
         ICH_CI=WORK(IST:)
      END IF
C
      END
