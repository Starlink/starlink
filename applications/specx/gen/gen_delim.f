
*-----------------------------------------------------------------------

      INTEGER FUNCTION GEN_DELIM (STRING, J, LEVEL, ILDEL)

C  Routine to check if a given character is the start of one of the delimiters
C  listed in the preamble to GETIT.
C  Syntactical Rules:
C         Checks if substring starting at position J of string STRING is a
C         valid delimiter sequence at level LEVEL or higher.
C  On output:
C         ILDEL returns # of character positions occupied by substring
C               (1 for a delimiter at end of string)
C         DELIM returns 1 if delimiter sequence found, 0 otherwise

      INTEGER*4 GEN_ILEN
      CHARACTER STRING*(*)
      CHARACTER CHAR*1, BLANK*1

      INTEGER*4 GEN_RANK

      DATA BLANK/' '/

      GEN_DELIM = 0
      ILS       = GEN_ILEN (STRING)
      ILDEL     = 0

      IC = J
      DO WHILE(IC.LE.ILS)
       CHAR  = STRING(IC:IC)
       IRANK = GEN_RANK (CHAR)
       IF (IRANK.GE.LEVEL) THEN         !  Valid delimiter for this level
         IF (GEN_DELIM.NE.0) THEN
           ILDEL = IC - J
           RETURN
         ELSE
           GEN_DELIM = 1
         END IF
       ELSE IF (CHAR.NE.BLANK) THEN      !  Non-blank ( and not delim. ).. end.
         ILDEL = IC - J
         IF (LEVEL.EQ.1 .AND. ILDEL.NE.0)   GEN_DELIM=1    ! Special. At level 1
         RETURN                                            !  blank is valid.
       END IF

      IC = IC + 1
      IF (GEN_DELIM.EQ.1 .AND. IC.GT.ILS) ILDEL = ILDEL + 1
      END DO

      RETURN
      END
