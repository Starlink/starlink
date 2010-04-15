
C-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION GEN_GETI4A
     &                   (PROMPT, DEF, NSIZ, FDEF, I4ARR, JDEF)

C   Routine to put a prompt message to the terminal and receive an
C   integer*4 array of length NSIZ, returned in I4ARR. JDEF as in GETCH.
C   IDEF is the default array for INTEG, FDEF is a format qualifier
C   e.g.'2(I3)', describing how the default is to be presented by the
C   prompt. A null string for FDEF will suppress the presentation
C   of the default value.

      IMPLICIT  NONE

      INTEGER*4 NSIZ,       JDEF
      INTEGER*4 DEF(NSIZ),  I4ARR(NSIZ)
      CHARACTER PROMPT*(*), FDEF*(*)

      INTEGER*4 NVAL, I
      LOGICAL*4 GEN_GETI4A2

      GEN_GETI4A = GEN_GETI4A2 (PROMPT, DEF,   NSIZ,
     &                          FDEF,   I4ARR, NVAL, JDEF)

      IF (NVAL.LT.NSIZ .AND. JDEF.EQ.0) THEN
        DO I = NVAL+1, NSIZ
          I4ARR(I) = 0
        END DO
      END IF

      RETURN
      END
