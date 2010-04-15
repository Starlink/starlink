      SUBROUTINE str_DESC(FORMAT, RIGHT, FIELD, SUPR, JUMPLN, EDIT)

*+
*
*   Name:
*      SUBROUTINE str_DESC
*
*   Description:
*      The TOKEN string is broken up into edit description details.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      The first character is assumed to be a PERCENT and is ignored.
*      The form "%<right><field>.<prec><edit>" is required.
*      The <right> is a MINUS character and is optional; it indicates
*      that the value is not to be right justified in the field.
*      The <field> is an unsigned integer representing the minimum
*      field width; it is optional.
*      The PERIOD or COLON characters are just a seperator; they are only
*      needed if the <prec> field exists.
*      PERIOD is used to indicate that <prec> refers to a fixed
*      number of decimals after the point.
*      COLON is used to indicate that <prec> is the total number
*      of significant figures wanted.
*      The <prec> field is an unsigned integer representing the
*      required numeric precision for floating point formats;
*      it is optional. It can appear even when <field> does not.
*      The <edit> is a letter specifying the form of the format.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE FORMAT(256)      ! edit format

*   Export:
      LOGICAL JUMPLN        ! jump over line end
      LOGICAL RIGHT         ! whether need to right justify in field
      LOGICAL SUPR          ! assignment suppression

      BYTE EDIT             ! edit type character

      INTEGER FIELD         ! minimum field size

*   External references:
      INTEGER str_INDEX     ! index of character in string
      INTEGER str_LEN       ! string length
      INTEGER str_TYPE      ! character lexical type

*   Local variables:
      INTEGER DIGIT         ! decoded digit
      INTEGER FIRST         ! first character position
      INTEGER LAST          ! last chararcter position
      INTEGER NUM           ! decoded number

*   String length
      FIRST = 1
      LAST = str_LEN(FORMAT)

*   Default values
      RIGHT = .TRUE.
      FIELD = 0
      SUPR = .FALSE.
      JUMPLN = .TRUE.
      EDIT = 32

*   Work through format end on the <edit> character
 100  CONTINUE

      IF (FIRST.LE.LAST) THEN

         IF (FORMAT(FIRST).EQ.45) THEN

            RIGHT = .FALSE.
            FIRST = FIRST + 1

         ELSE IF (FORMAT(FIRST).EQ.36) THEN

            JUMPLN = .FALSE.
            FIRST = FIRST + 1

         ELSE IF (FORMAT(FIRST).EQ.42) THEN

            SUPR = .TRUE.
            FIRST = FIRST + 1

         ELSE IF (str_TYPE(FORMAT(FIRST)).EQ.1) THEN

            EDIT = FORMAT(FIRST)
            GO TO 200

         ELSE IF (str_TYPE(FORMAT(FIRST)).EQ.2) THEN

            NUM = 0
 120        CONTINUE

            IF (.NOT.(FIRST.LE.LAST)) THEN

               FIELD = NUM

            ELSE

               DIGIT = str_INDEX('0123456789\\', FORMAT(FIRST))

               IF (DIGIT.GT.0) THEN

                  NUM = NUM*10 + DIGIT - 1

               ELSE

                  FIELD = NUM
                  GO TO 100

               END IF

               FIRST = FIRST + 1
               GO TO 120

            END IF

         ELSE

            FIRST = FIRST + 1

         END IF

         GO TO 100

      END IF

 200  CONTINUE

      END
