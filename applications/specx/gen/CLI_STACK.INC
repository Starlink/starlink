*   This is the 'stack' to allow nesting of command files.

*   ICLI(4*ISP+1) = last used location
*   ICLI(4*ISP+2) = length of CLI for level ISP
*   ICLI(4*ISP+3) = input logical unit(ISP)
*   ICLI(4*ISP+4) = last line read on this unit

      INTEGER*4 MAXSTACK
      PARAMETER (MAXSTACK=7)

      INTEGER*4 ISP
      INTEGER*4 ICLI (1:4, 0:MAXSTACK)
      CHARACTER CLILINE*2048

      COMMON /CLI1/ ICLI, ISP
      COMMON /CLI2/ CLILINE

      SAVE  /CLI1/
      SAVE  /CLI2/
