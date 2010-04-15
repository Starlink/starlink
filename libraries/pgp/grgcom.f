      INTEGER FUNCTION GRGCOM(STRING, PROMPT, L)
*+
*     - - - - - - - -
*       G R G C O M     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Read with prompt from user's terminal
*
*   Given
*      PROMPT   c     Prompt String
*
*   Returned
*      GRGCOM   i     1 if successful, 0 if an error occurs
*      STRING   c     String read from terminal
*      L        i     Length of string
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE

      INTEGER L
      CHARACTER*(*) STRING, PROMPT

      INTEGER ISTAT, I

*  Print prompt
      PRINT *, PROMPT

*  Read reply
      READ (*,'(A)',IOSTAT=ISTAT) STRING
      IF (ISTAT.NE.0) THEN
          GRGCOM = 0
          GO TO 999
      ENDIF

*  Compute length by removing trailing blanks
      DO 10 I=LEN(STRING),1,-1
          L = I
          IF (STRING(I:I).NE.' ') GOTO 20
   10 CONTINUE

*  Success
   20 CONTINUE
      GRGCOM = 1
  999 CONTINUE
      END
