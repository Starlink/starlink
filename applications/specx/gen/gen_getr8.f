
*-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION GEN_GETR8 (PROMPT, DEF, FDEF, R8, JDEF)

*   Routine to put a prompt message to the terminal and receive a real*8
*   variable returned in R8. JDEF as for GETCH. DEF is the default for R8.
*   FDEF is a format qualifier e.g.'F10,3', describing how the default is to
*   be presented by the prompt. A null string for FDEF will suppress the
*   presentation of the default value(s).

      IMPLICIT  NONE

      INTEGER*4 JDEF
      REAL*8    DEF,        R8
      CHARACTER PROMPT*(*), FDEF*(*)

      INTEGER*4 NVAL
      LOGICAL*4 GEN_GETR8A2

      GEN_GETR8 = GEN_GETR8A2 (PROMPT, DEF, 1, FDEF, R8, NVAL, JDEF)

      RETURN
      END
