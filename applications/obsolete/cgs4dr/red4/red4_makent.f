C+
      SUBROUTINE RED4_MAKENT (DATA, DIM, STATUS)
C
C     R E D 4 _ M A K E N T
C
C     Prompts user to make up a dummy NT set
C
C     Parameters -    (">" input, "<" output)
C
C     (>) DIM      (Integer) dimension of array
C     (<) DATA     (Integer array) Data array
C
C          1989?: Original version.                               (JFL ???)
C     3-Sep-1990: Code spaced out. Unused variables removed.      (SMB)
C     4-Sep-1990: At last, I have found out what the hard-wired
C                 64 was for! Parameter used and comment added.   (SMB)
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER DIM
      INTEGER DATA (DIM)
C
C     ADAM stuff
C
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
C
C     Local constants
C
      INTEGER STRPMULT             ! The NT normalisation factor.
      PARAMETER ( STRPMULT = 64 )  ! This is normally 64.
C
C     Local variables
C
      INTEGER I
      INTEGER STATUS
      INTEGER INTVAL
      INTEGER COORD
      LOGICAL LOOPING
C
C
      IF (STATUS .NE. ADAM__OK) RETURN

*   Initialise the array by filling it full of the NT normalisation
*   factor, STRPMULT

      DO I = 1, DIM

         DATA (I) = STRPMULT
      END DO

*   Now allow the user to define any other individual values.

      LOOPING = .TRUE.

      DO WHILE (LOOPING)

         CALL PAR_GET0I ('COORD', COORD, STATUS)
         CALL PAR_CANCL ('COORD', STATUS)

         IF ( (COORD .LE. 0) .OR. (COORD .GT. DIM) ) THEN

            LOOPING = .FALSE.
         ELSE

            CALL PAR_GET0I ('INTVAL', INTVAL, STATUS)
            CALL PAR_CANCL ('INTVAL', STATUS)

            DATA(COORD) = INTVAL
         ENDIF
      END DO

      END
