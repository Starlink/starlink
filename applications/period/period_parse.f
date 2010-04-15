
      FUNCTION PERIOD_PARSE(STRING, MASTER)

C=========================================================================
C Command parser. Compares all non-blank characters between two strings
C STRING and MASTER. PERIOD_PARSE = .TRUE. if they match. The strings
C will match if:
C
C	(1) Both STRING and MASTER are blank.
C	(2) STRING is shorter than MASTER and all letters matched.
C
C The strings will not match if:
C
C	(1) STRING is blank, MASTER is not.
C	(2) STRING has more non-blank characters than MASTER.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C=========================================================================

      IMPLICIT NONE

      INTEGER L1,M1,L2,M2,N
      LOGICAL PERIOD_PARSE
      CHARACTER*(*) STRING, MASTER

      PERIOD_PARSE = .TRUE.
      L1 = LEN(STRING)
      M1 = LEN(MASTER)
      L2 = 1
      M2 = 1
      N = 0
      DO WHILE ( L2.LE.L1 .AND. M2.LE.M1 )

C--------------------------------------------------------------------------
C Find non-blank characters.
C--------------------------------------------------------------------------

         N = N + 1
         DO WHILE ( L2.LE.L1 .AND. STRING(L2:L2).EQ.' ' )
            L2 = L2 + 1
         END DO
         L2 = MIN(L1, L2)
         DO WHILE ( M2.LE.M1 .AND. MASTER(M2:M2).EQ.' ' )
            M2 = M2 + 1
         END DO
         M2 = MIN(M1, M2)
         IF ( STRING(L2:L2).NE.' ' .AND. MASTER(M2:M2).NE.' ' .AND.
     :        STRING(L2:L2).NE.MASTER(M2:M2) ) THEN
            PERIOD_PARSE = .FALSE.
            RETURN
         ELSE IF ( STRING(L2:L2).EQ.' ' ) THEN
            IF ( N.EQ.1 .AND. MASTER(M2:M2).NE.' ' )
     :           PERIOD_PARSE = .FALSE.
            RETURN
         ELSE IF ( MASTER(M2:M2).EQ.' ' .AND. STRING(L2:L2).NE.' ' )
     :             THEN
            PERIOD_PARSE = .FALSE.
            RETURN
         END IF

C--------------------------------------------------------------------------
C Move onto next character.
C--------------------------------------------------------------------------

         L2 = L2 + 1
         M2 = M2 + 1
      END DO

      IF ( M2.GT.M1 .AND. L2.LE.L1 ) THEN
         DO WHILE ( L2.LE.L1 .AND. STRING(L2:L2).EQ.' ' )
            L2 = L2 + 1
         END DO
         L2 = MIN(L2, L1)
         IF ( STRING(L2:L2).NE.' ' ) PERIOD_PARSE = .FALSE.
      END IF

      RETURN
      END
