      SUBROUTINE GRMCUR (ICH, ICX, ICY)
      INTEGER ICH, ICX, ICY
C
C Cursor movement:
C Input: ICH character code
C In/Out: ICX, ICY cursor position
C-----------------------------------------------------------------------
      INTEGER STEP
      SAVE STEP
      DATA STEP /4/
C
C     Up arrow or keypad 8:
      IF (ICH.EQ.-1 .OR. ICH.EQ.-28) THEN
          ICY = ICY+STEP
C     Down arrow or keypad 2:
      ELSE IF (ICH.EQ.-2 .OR. ICH.EQ.-22) THEN
          ICY = ICY-STEP
C     Right arrow or keypad 6:
      ELSE IF (ICH.EQ.-3 .OR. ICH.EQ.-26) THEN
          ICX = ICX+STEP
C     Left arrow or keypad 4:
      ELSE IF (ICH.EQ.-4 .OR. ICH.EQ.-24) THEN
          ICX = ICX-STEP
C     Keypad 7 (left and up):
      ELSE IF (ICH.EQ.-27) THEN
          ICX = ICX-STEP
          ICY = ICY+STEP
C     Keypad 9 (right and up):
      ELSE IF (ICH.EQ.-29) THEN
          ICX = ICX+STEP
          ICY = ICY+STEP
C     Keypad 3 (right and down):
      ELSE IF (ICH.EQ.-23) THEN
          ICX = ICX+STEP
          ICY = ICY-STEP
C     Keypad 1 (left and down):
      ELSE IF (ICH.EQ.-21) THEN
          ICX = ICX-STEP
          ICY = ICY-STEP
C     PF1:
      ELSE IF (ICH.EQ.-11) THEN
          STEP = 1
C     PF2:
      ELSE IF (ICH.EQ.-12) THEN
          STEP = 4
C     PF3:
      ELSE IF (ICH.EQ.-13) THEN
          STEP = 16
C     PF4:
      ELSE IF (ICH.EQ.-14) THEN
          STEP = 64
      END IF
      END
