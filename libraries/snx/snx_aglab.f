
      SUBROUTINE snx_AGLAB (LNAME, TEXT)

*+
*
*  - - - - - -
*   A G L A B
*  - - - - - -
*
*  In NCAR AUTOGRAPH, set up one of the predefined
*  informational labels.
*
*  See section 2.30 et seq in the AUTOGRAPH manual.
*
*  Given:
*     LNAME     c      label name - R,L,B, or T.
*     TEXT      c      text of label
*
*  Line numbers 100 for 'T' and 'L', and -100 for 'B' and 'R'
*  are used.
*
*  Trailing blanks in TEXT are ignored to achieve centring.
*
*  If an illegal LNAME is given, the top label is set
*  to '*** AGLAB LABEL ERROR ***'
*
*  Called:  AGGETI, AGSETC, AGSETI
*
*  P T Wallace   Starlink   24 June 1987
*
*+

      IMPLICIT NONE

      CHARACTER*(*) LNAME,TEXT

      CHARACTER LN*1
      INTEGER NL,I,LM
      LOGICAL OK



*  Find out where the trailing blanks start
      DO 100 I=LEN(TEXT),2,-1
         IF (TEXT(I:I).NE.' ') GO TO 200
 100     CONTINUE
      I=1
 200  CONTINUE

*  Validate the label name and select line number
      OK=.TRUE.
      LN=LNAME
      IF (LN.EQ.'L'.OR.LN.EQ.'T') THEN
         NL=100
      ELSE IF (LN.EQ.'R'.OR.LN.EQ.'B') THEN
         NL=-100
      ELSE
         OK=.FALSE.
         LN='T'
         NL=100
         I=40
      END IF

*  Set up the label
      CALL AGGETI('LINE/MAXIMUM.',LM)
      CALL AGSETI('LINE/MAXIMUM.',I)
      IF (OK) THEN
         CALL AGSETC('LABEL/NAME.',LN)
         CALL AGSETI('LINE/NUMBER.',NL)
         CALL AGSETC('LINE/TEXT.',TEXT(:I))
      ELSE
         CALL AGSETC('LABEL/NAME.',LN)
         CALL AGSETI('LINE/NUMBER.',NL)
         CALL AGSETC('LINE/TEXT.','*** AGLAB LABEL ERROR ***')
      END IF
      CALL AGSETI('LINE/MAXIMUM.',LM)

      END
