
      SUBROUTINE snx_AGLAB (LNAME, TEXT)

*+
*  Name:
*     AGLAB

*  Purpose:
*     In NCAR AUTOGRAPH, set up one of the predefined
*     informational labels.

*  Language:
*     Starlink Fortran 77

*  Description:
*     See section 2.30 et seq in the AUTOGRAPH manual.

*  Arguments:
*     LNAME = CHAR (Given)
*         Label name - R,L,B, or T.
*     TEXT = CHAR (Given)
*         Text of label

*  Notes:
*     Line numbers 100 for 'T' and 'L', and -100 for 'B' and 'R'
*     are used.
*
*     Trailing blanks in TEXT are ignored to achieve centring.
*
*     If an illegal LNAME is given, the top label is set
*     to '*** AGLAB LABEL ERROR ***'

*  Externals:
*     AGGETI, AGSETC, AGSETI

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     {enter_new_authors_here}

*  History:
*     24-JUN-1987 (PTW):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

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
