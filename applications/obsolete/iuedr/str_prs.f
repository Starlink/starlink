      SUBROUTINE STR_PRS( RIGHT, FIELD, STR, MAXC, FIRST, SVALUE,
     :                    STATUS )

*+
*
*   Name:
*      SUBROUTINE STR_PRS
*
*   Description:
*      Grab string value from parameter string using edit parameters.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      The string is scanned for an string specified by the edit
*      parameters.
*      The FIRST parameter is left pointing to the next character
*      after the present field.
*
*   Bugs and deficiencies:
*      The logic is not right. It ends a string on the first white
*      character even when FIELD is specified.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      LOGICAL RIGHT         ! whether right justified

      INTEGER FIELD         ! field size

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
         STATUS = -3
         USED = 0
         POS = FIRST

 50      CONTINUE

         IF (POS.LE.LAST) THEN

            IF (STR(POS).NE.32 .AND. STR(POS).NE.9) THEN

               IF (NOUSED) THEN

                  NOUSED = .FALSE.
                  FUSED = POS
                  STATUS = 0

               END IF

            ELSE IF (.NOT.NOUSED) THEN

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

               FUSED = POS
               STATUS = 0
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

         CALL STR_TERM(0, MAXC, SVALUE)
         CALL STR_COPY(STR, FUSED, FIRST - 1, 1, MAXC, SVALUE)

      ELSE

         CALL STR_TERM(0, MAXC, SVALUE)

      END IF

      END
