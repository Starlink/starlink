C+
      SUBROUTINE RED4_MAKEDATA (NX, NY, DATA, STATUS)
C
C     R E D 4 _ M A K E D A T A
C
C     Prompts user to make up a dummy data set
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NX       (Integer) X-dimension of array
C     (>) NY       (Integer) Y-dimension of array
C     (<) DATA     (Real array) Data array
C
C     1989:        Original version.                            (JFL)
C      9-Jul-1990: History added. Modified so that data with
C                  stripes may be generated for test purposes.
C                  Some obvious status checking deficiencies
C                  corrected.                                   (SMB)
C      3-Sep-1990: Code spaced out. Unused variables removed.   (SMB)
C     22-Feb-1993: Conform to error strategy                    (PND)
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY
      REAL DATA( NX, NY )
C
C     ADAM stuff
C
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
C
C     Local variables
C
      INTEGER I, J, K
      INTEGER STATUS
      REAL REALVAL, UP, DOWN
      INTEGER ACTVAL
      INTEGER COORDS(4)
      LOGICAL LOOPING
      LOGICAL SIMULATOR
      LOGICAL STRIPES
      INTEGER NITEM
      INTEGER WIDTH
C
C
      IF (STATUS .NE. ADAM__OK) RETURN

      CALL PAR_GET0L ('SIMULATOR', SIMULATOR, STATUS)
      CALL PAR_CANCL ('SIMULATOR', STATUS)

      IF ( .NOT. SIMULATOR ) THEN

         CALL PAR_GET0L ('STRIPES', STRIPES, STATUS)
         CALL PAR_CANCL ('STRIPES', STATUS)
      END IF

      IF (SIMULATOR) THEN

*    Want to read in a CGS4 simulator data array
         CALL DSA_INPUT ('SIMFILE', 'SIMFILE', STATUS)
         CALL PAR_CANCL ('SIMFILE', STATUS)

         NITEM = NX * NY

         CALL RED4_GET_DATA_OBJECT ('SIMFILE', '.DATA_ARRAY', 'FLOAT',
     :      NITEM, 0, DATA, 0, STATUS)

         CALL DSA_CLOSE_STRUCTURE ('SIMFILE', STATUS)

      ELSE IF ( STRIPES ) THEN

*      Artificial stripes required, for test purposes.

*      Obtain the values to write to the UP and DOWN parts of
*      the stripe.

         CALL PAR_GET0R ('UP', UP, STATUS)
         CALL PAR_GET0R ('DOWN', DOWN, STATUS)

*      Obtain the width of each stripe in pixels.

         CALL PAR_GET0I( 'WIDTH', WIDTH, STATUS )

         IF ( STATUS .EQ. ADAM__OK ) THEN

            DO J = 1, NY

               I = 1

               DO WHILE ( I .LE. NX )

*               Up stripe.

                  K = 1

                  DO WHILE ( (K .LE. WIDTH) .AND. (I .LE. NX) )

                     DATA(I,J) = UP
                     K = K + 1
                     I = I + 1
                  END DO

*               Down stripe.

                  K = 1

                  DO WHILE ( (K .LE. WIDTH) .AND. (I .LE. NX) )

                     DATA(I,J) = DOWN
                     K = K + 1
                     I = I + 1
                  END DO
               END DO
            END DO
         END IF

      ELSE

*    Make your own data

         CALL PAR_GET0R ('REALVAL', REALVAL, STATUS)
         CALL PAR_CANCL ('REALVAL', STATUS)

         DO J = 1, NY

            DO I = 1, NX

               DATA(I,J) = REALVAL
            END DO
         END DO
C
         LOOPING = .TRUE.

         DO WHILE (LOOPING)

            CALL PAR_GET1I ('COORDS', 4, COORDS, ACTVAL, STATUS)
            CALL PAR_CANCL ('COORDS', STATUS)

            IF (COORDS(1) .LE. 0) THEN

               LOOPING = .FALSE.
            ELSE

               CALL PAR_GET0R ('PIXVAL', REALVAL, STATUS)
               CALL PAR_CANCL ('PIXVAL', STATUS)

               IF ( STATUS .EQ. ADAM__OK ) THEN

                  DATA(COORDS(1), COORDS(2)) = REALVAL
               ELSE

                  LOOPING = .FALSE.
               END IF
            ENDIF
         END DO

      ENDIF
C
      END
