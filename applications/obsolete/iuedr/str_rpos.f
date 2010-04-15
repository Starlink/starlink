      SUBROUTINE str_RPOS(FORMAT, LINE, BUFPOS)

*+
*
*   Name:
*      SUBROUTINE str_RPOS
*
*   Description:
*      Set current position in line parameter under format control.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      The FORMAT string is interpretted and used to set the current
*      position in the line parameter. The %p format is processed.
*      The position can be set to beyond the last character in the line.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE FORMAT(256)     ! format token
      BYTE LINE(100)       ! line to be modified

*   Import/Export:
      INTEGER BUFPOS       ! character position

*   External references:
      INTEGER str_LEN      ! string length

*   Local variables:
      LOGICAL REL          ! whether relative move
      LOGICAL TERM         ! not used here

      BYTE OFFSET          ! position offset for relative moves

      INTEGER BUFLEN       ! line length
      INTEGER NEWPOS       ! temporary newposition
      INTEGER POS          ! position or relative position

*   Implicit initialise
      IF (BUFPOS.LT.1) BUFPOS = 1
      BUFLEN = str_LEN(LINE)

*   Decode format
      CALL str_DECP(FORMAT, OFFSET, REL, POS, TERM)

      IF (REL) THEN

         IF (OFFSET.EQ.36) THEN

            NEWPOS = BUFLEN + POS

         ELSE IF (OFFSET.EQ.46) THEN

            NEWPOS = BUFPOS + POS

         ELSE

            NEWPOS = POS

         END IF

      ELSE IF (OFFSET.EQ.36) THEN

         NEWPOS = BUFLEN + 1

      ELSE

         NEWPOS = POS

      END IF

*   Modify position
      BUFPOS = MAX(1, MIN(BUFLEN + 1, NEWPOS))

      END
