
C-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION GEN_GETR4A
     &                   (PROMPT, DEF, NSIZ, FDEF, R4ARR, JDEF)

C   Routine to put a prompt message to the terminal and receive a
C   real*4 array of size NSIZ returned in R4ARR. JDEF as for GETCH.
C   DEF is the default array for R4ARR, FDEF is a format qualifier
C   e.g.'2(F10,3)', describing how the default is to be presented by the
C   prompt. A null string for FDEF will suppress the presentation
C   of the default value(s).

      IMPLICIT  NONE

      INTEGER*4 NSIZ,       JDEF
      REAL*4    DEF(NSIZ),  R4ARR(NSIZ)
      CHARACTER PROMPT*(*), FDEF*(*)

      INTEGER*4 NVAL, I
      LOGICAL*4 GEN_GETR4A2

      GEN_GETR4A = GEN_GETR4A2 (PROMPT, DEF,   NSIZ,
     &                          FDEF,   R4ARR, NVAL, JDEF)

      IF (NVAL.LT.NSIZ .AND. JDEF.EQ.0) THEN
        DO I = NVAL+1, NSIZ
          R4ARR(I) = 0.0
        END DO
      END IF

      RETURN
      END
