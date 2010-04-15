      SUBROUTINE str_DECF(FORMAT, RIGHT, FIELD, FIXED, PREC, EDIT)

*+
*
*   Name:
*      SUBROUTINE str_DECF
*
*   Description:
*      The TOKEN string is broken up into edit description details.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          21-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          20-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The first character is assumed to be a PERCENT and is ignored.
*      The form "%<right><field>.<prec><edit>" is required.
*      The <right> is a MINUS character and is optional;  it indicates
*      that the value is not to be right justified in the field.
*      The <field> is an unsigned integer representing the minimum
*      field width;  it is optional.
*      The PERIOD or COLON character is just a seperator;  either is only
*      needed if the <prec> field exists.
*      PERIOD is used to indicate that <prec> refers to a fixed
*      number of decimals after the point.
*      COLON is used to indicate that <prec> is the total number
*      of significant figures wanted.
*      The <prec> field is an unsigned integer representing the
*      required numeric precision for floating point formats;
*      it is optional.   It can appear even when <field> does not.
*      The <edit> is a letter specifying the form of the format.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      LOGICAL FALSE         ! .FALSE.
      LOGICAL TRUE          ! .TRUE.

      PARAMETER (FALSE=.FALSE., TRUE=.TRUE.)

      BYTE BLANK            ! ASCII " "
      BYTE COLON            ! ASCII ":"
      BYTE MINUS            ! ASCII "-"
      BYTE PERIOD           ! ASCII "."

      PARAMETER (BLANK=32, COLON=58, MINUS=45, PERIOD=46)

      INTEGER DIGIT         ! DIGIT index
      INTEGER LETTER        ! LETTER index
      INTEGER MAXTOK        ! maximum length of token string

      PARAMETER (DIGIT=2, LETTER=1, MAXTOK=256)

*   Import:
      BYTE FORMAT(MAXTOK)   ! edit format

*   Export:
      LOGICAL RIGHT         ! whether need to right justify in field

      INTEGER FIELD         ! minimum field size

      LOGICAL FIXED         ! whether fixed point

      INTEGER PREC          ! required precision

      BYTE EDIT             ! edit type character

*   External references:
      INTEGER str_INDEX     ! index of character in string
      INTEGER str_LEN       ! string length
      INTEGER str_TYPE      ! character lexical type

*   Local variables:
      LOGICAL TERM          ! whether terminator found

      INTEGER LDIGIT        ! decoded digit
      INTEGER FIRST         ! first character position
      INTEGER LAST          ! last chararcter position
      INTEGER NUM           ! decoded number

*   String length
      FIRST = 1
      LAST = str_LEN(FORMAT)

*   Default values
      RIGHT = TRUE
      FIXED = FALSE
      FIELD = 0
      TERM = FALSE
      PREC = -1
      EDIT = BLANK

*   Work through format end on the <edit> character
      DO WHILE (FIRST.LE.LAST)

         IF (FORMAT(FIRST).EQ.MINUS) THEN
            RIGHT = FALSE
            FIRST = FIRST + 1
         ELSE IF (FORMAT(FIRST).EQ.PERIOD) THEN
            FIXED = TRUE
            TERM = TRUE
            FIRST = FIRST + 1
         ELSE IF (FORMAT(FIRST).EQ.COLON) THEN
            TERM = TRUE
            FIXED = FALSE
            FIRST = FIRST + 1
         ELSE IF (str_TYPE(FORMAT(FIRST)).EQ.LETTER) THEN
            EDIT = FORMAT(FIRST)
            GO TO 200
         ELSE IF (str_TYPE(FORMAT(FIRST)).EQ.DIGIT) THEN
            NUM = 0

            DO WHILE (FIRST.LE.LAST)
               LDIGIT = str_INDEX('0123456789\\', FORMAT(FIRST))

               IF (LDIGIT.GT.0) THEN
                  NUM = NUM*10 + LDIGIT - 1
               ELSE
                  GO TO 140
               END IF

               FIRST = FIRST + 1
            END DO

 140        CONTINUE

            IF (TERM) THEN
               PREC = MIN(NUM, MAXTOK-1)
            ELSE
               FIELD = MIN(NUM, MAXTOK-1)
            END IF

         ELSE
            FIRST = FIRST + 1
         END IF
      END DO

 200  CONTINUE

      END
