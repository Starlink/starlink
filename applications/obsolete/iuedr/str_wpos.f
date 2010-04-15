      SUBROUTINE str_WPOS(FORMAT, MAXC, LINE, BUFPOS)

*+
*
*   Name:
*      SUBROUTINE str_WPOS
*
*   Description:
*      Set current position in lineparameter under format control.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          21-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          22-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The FORMAT string is interpretted and used to set the current
*      position in the line parameter. The %p format is processed.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE FORMAT(256)     ! format token

      INTEGER MAXC         ! maximum size of line

*   Import/Export:
      BYTE LINE(MAXC)      ! line to be modified

      INTEGER BUFPOS       ! character position

*   External references:
      INTEGER str_LEN      ! string length

*   Local variables:
      LOGICAL REL          ! whether relative move
      LOGICAL TERM         ! whether buffer is terminated at position

      BYTE OFFSET          ! position offset for relative moves

      INTEGER BUFLEN       ! string length
      INTEGER I            ! loop index
      INTEGER NEWPOS       ! temporary new position
      INTEGER POS          ! position or relative position

*   Implicit initialise
      IF (BUFPOS.LT.1) THEN

         BUFPOS = 1
         BUFLEN = 0
         CALL str_TERM(0, MAXC, LINE)

      ELSE

         BUFLEN = str_LEN(LINE)

      END IF

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

*   Keep new position within buffer limits
      NEWPOS = MAX(1, MIN(MAXC - 1, NEWPOS))

*   Modify buffer
      IF (NEWPOS.GT.BUFLEN + 1) THEN

         DO 50 I = BUFLEN + 1, NEWPOS - 1

            LINE(I) = 32

 50      CONTINUE

         BUFPOS = NEWPOS
         CALL str_TERM(BUFPOS - 1, MAXC, LINE)
         BUFLEN = str_LEN(LINE)

      ELSE IF (TERM) THEN

         BUFPOS = NEWPOS
         CALL str_TERM(BUFPOS - 1, MAXC, LINE)
         BUFLEN = str_LEN(LINE)

      ELSE

         BUFPOS = NEWPOS

      END IF

      END
