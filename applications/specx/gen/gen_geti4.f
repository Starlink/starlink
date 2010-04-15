
C-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION GEN_GETI4 (PROMPT, DEF, FDEF, I4, JDEF)

*   Routine to put a prompt message to the terminal and receive a integer*4
*   variable returned in I4. JDEF as for GETCH. DEF is the default for I4.
*   FDEF is a format qualifier e.g.'I8.3', describing how the default is to
*   be presented by the prompt. A null string for FDEF will suppress the
*   presentation of the default value(s).

      IMPLICIT  NONE

      INTEGER*4 JDEF
      INTEGER*4 DEF,        I4
      CHARACTER PROMPT*(*), FDEF*(*)

      INTEGER*4 NVAL
      LOGICAL*4 GEN_GETI4A2

      GEN_GETI4 = GEN_GETI4A2 (PROMPT, DEF, 1, FDEF, I4, NVAL, JDEF)

      RETURN
      END
