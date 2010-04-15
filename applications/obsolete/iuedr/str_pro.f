      SUBROUTINE STR_PRO(RIGHT, FIELD, STR, FIRST, IVALUE, STATUS)

*+
*
*   Name:
*      SUBROUTINE STR_PRO
*
*   Description:
*      Grab octal value from parameter string using edit parameters.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      The string is scanned for an octal value specified by the edit
*      parameters.
*      The FIRST parameter is left pointing to the next character
*      after the present field.
*      The actual decoding of the substring into a numeric value is done
*      by a seperate routine.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      LOGICAL RIGHT         ! whether right justified

      INTEGER FIELD         ! field size

      BYTE STR(100)         ! string to be scanned

*   Import/Export:
      BYTE FIRST            ! first character position containing float

*   Export:
      INTEGER IVALUE        ! decoded value
      INTEGER STATUS        ! status return

*   External references:
      INTEGER STR_LEN       ! string length
      INTEGER STR_INDEX     ! index of character in string

*   Local variables:
      LOGICAL NOUSED        ! no characters used yet
      LOGICAL NOVAL         ! no numeric value yet

      BYTE VALUE(256)       ! local copy of field

      INTEGER DIG           ! character index
      INTEGER FUSED         ! position of first used character
      INTEGER LAST          ! last character position in string
      INTEGER POS           ! character position
      INTEGER USED          ! number of characters used

*   Length of string
      LAST = STR_LEN(STR)

*   FIELD specified
      IF (FIELD.LE.0) THEN

*      FIELD unspecified means must search for valid format
         NOUSED = .TRUE.
         NOVAL = .TRUE.
         STATUS = -3
         USED = 0
         POS = FIRST

 50      CONTINUE

         IF (POS.LE.LAST) THEN

            DIG = STR_INDEX('01234567 \\', STR(POS))
            IF (DIG.LE.0) GO TO 200

            IF (NOUSED .AND. STR(POS).NE.32) THEN

               NOUSED = .FALSE.
               FUSED = POS

            END IF

            IF (DIG.LE.8) THEN

               STATUS = 0
               NOVAL = .FALSE.

            ELSE IF (.NOT.NOVAL) THEN

               GO TO 200

            END IF

            IF (.NOT.NOUSED) THEN

               USED = USED + 1

            ELSE IF (RIGHT) THEN

               USED = USED + 1

            END IF

            POS = POS + 1
            GO TO 50

         END IF

*   Right justified means grab next FIELD characters
      ELSE IF (RIGHT) THEN

         FUSED = FIRST
         POS = FUSED + FIELD - 1

         IF (POS.LE.LAST + 1) THEN

            STATUS = 0

         ELSE

            STATUS = -3

         END IF

*   Left justified means find first valid character
      ELSE

         POS = FIRST

 100     CONTINUE

         IF (POS.LE.LAST) THEN

            IF (STR(POS).NE.32 .AND. STR(POS).NE.9) THEN

               IF (STR_INDEX('01234567\\', STR(POS)).EQ.0) THEN

                  STATUS = -3

               ELSE

                  FUSED = POS
                  STATUS = 0

               END IF

               GO TO 200

            END IF

            POS = POS + 1
            GO TO 100

         END IF

      END IF

 200  CONTINUE

*   Update FIRST based of number of characters used, field size and
*   justification
      IF (STATUS.EQ.0) THEN

         IF (FIELD.GT.0) THEN

            FIRST = FUSED + FIELD

         ELSE

            FIRST = POS

         END IF

         CALL STR_TERM(0, 256, VALUE)
         CALL STR_COPY(STR, FUSED, FIRST - 1, 1, 256, VALUE)
         CALL GEN_OTOI(VALUE, IVALUE, STATUS)

      ELSE

         IVALUE = 0

      END IF

      END
