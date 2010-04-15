
C-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION GEN_GETR4 (PROMPT, DEF, FDEF, R4, JDEF)

*   Routine to put a prompt message to the terminal and receive a real*4
*   variable returned in R4. JDEF as for GETCH. DEF is the default for R4.
*   FDEF is a format qualifier e.g.'F10,3', describing how the default is to
*   be presented by the prompt. A null string for FDEF will suppress the
*   presentation of the default value(s).

      IMPLICIT  NONE

      INTEGER*4 JDEF
      REAL*8    DEF,        R4
      CHARACTER PROMPT*(*), FDEF*(*)

      INTEGER*4 NVAL
      LOGICAL*4 GEN_GETR4A2

      GEN_GETR4 = GEN_GETR4A2 (PROMPT, DEF, 1, FDEF, R4, NVAL, JDEF)

      RETURN
      END
