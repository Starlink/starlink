      SUBROUTINE STR_PRQ(STR, MAXC, FIRST, SVALUE, STATUS)

*+
*
*   Name:
*      SUBROUTINE STR_PRQ
*
*   Description:
*      Grab quoted string from string parameter.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      The input string is scanned for a quoted string.
*      The FIRST parameter is left pointing to the next character
*      after the present field.
*      This value is totally free-field.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE STR(100)         ! string to be scanned

      INTEGER MAXC          ! maximum size of returned string

*   Import/Export:
      BYTE FIRST            ! first character position containing float

*   Export:
      BYTE SVALUE(MAXC)     ! returned string

      INTEGER STATUS        ! status return

*   External references:
      INTEGER STR_LEN       ! string length

*   Local variables:
      LOGICAL NOUSED        ! no characters used yet

      BYTE DELIM            ! quote delimiter character

      INTEGER FUSED         ! position of first used character
      INTEGER LAST          ! last character position in string
      INTEGER POS           ! character position

*   Length of string
      LAST = STR_LEN(STR)
      NOUSED = .TRUE.
      STATUS = -3
      FUSED = 0
      POS = FIRST

 100  CONTINUE

      IF (POS.LE.LAST) THEN

         IF (NOUSED) THEN

            IF (STR(POS).NE.32 .AND. STR(POS).NE.9) THEN

               IF (STR(POS).NE.39 .AND. STR(POS).NE.34) GO TO 200
               NOUSED = .FALSE.
               FUSED = POS
               DELIM = STR(POS)

            END IF

         ELSE IF (STR(POS).EQ.DELIM) THEN

            POS = POS + 1
            STATUS = 0
            GO TO 200

         END IF

         POS = POS + 1
         GO TO 100

      END IF

 200  CONTINUE

*   Update FIRST based of number of characters used, field size and
*   justification
      IF (STATUS.EQ.0) THEN

         FIRST = POS
         CALL STR_TERM(0, MAXC, SVALUE)
         CALL STR_COPY(STR, FUSED, FIRST - 1, 1, MAXC, SVALUE)

      ELSE

         CALL STR_TERM(0, MAXC, SVALUE)

      END IF

      END
