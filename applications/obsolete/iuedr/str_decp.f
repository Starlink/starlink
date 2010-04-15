      SUBROUTINE str_DECP(FORMAT, OFFSET, REL, POS, TERM)

*+
*
*   Name:
*      SUBROUTINE str_DECP
*
*   Description:
*      Decode position format into components.
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
*      The FORMAT string is decoded into the position elements.
*      The form %[$.][+-]<n>[*]p is allowed where:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE FORMAT(256)      ! edit format

*   Export:
      BYTE OFFSET           ! offset character

      LOGICAL REL           ! whether relative position

      INTEGER POS           ! position

      LOGICAL TERM          ! whether terminator

*   External references:
      INTEGER str_INDEX     ! index of character in string
      INTEGER str_LEN       ! string length
      INTEGER str_TYPE      ! character lexical type

*   Local variables:
      INTEGER DIGIT         ! decoded digit
      INTEGER DIR           ! sign for relative offset
      INTEGER FIRST         ! first character position
      INTEGER LAST          ! last chararcter position
      INTEGER NUM           ! decoded number

*   String length
      FIRST = 1
      LAST = str_LEN(FORMAT)

*   Default values
      REL = .FALSE.
      OFFSET = 46
      POS = 0
      TERM = .TRUE.
      DIR = 0

*   Work through format end on the <edit> character
 100  CONTINUE

      IF (FIRST.LE.LAST) THEN

         IF (FORMAT(FIRST).EQ.45) THEN

            REL = .TRUE.
            DIR = -1
            FIRST = FIRST + 1

         ELSE IF (FORMAT(FIRST).EQ.43) THEN

            REL = .TRUE.
            DIR = +1
            FIRST = FIRST + 1

         ELSE IF (FORMAT(FIRST).EQ.42) THEN

            TERM = .FALSE.
            FIRST = FIRST + 1

         ELSE IF (FORMAT(FIRST).EQ.46) THEN

            OFFSET = 46
            FIRST = FIRST + 1

         ELSE IF (FORMAT(FIRST).EQ.36) THEN

            OFFSET = 36
            FIRST = FIRST + 1

         ELSE IF (str_TYPE(FORMAT(FIRST)).EQ.1) THEN

            GO TO 200

         ELSE IF (str_TYPE(FORMAT(FIRST)).EQ.2) THEN

            NUM = 0

 120        CONTINUE

            IF (.NOT.(FIRST.LE.LAST)) THEN

               POS = NUM

            ELSE

               DIGIT = str_INDEX('0123456789\\', FORMAT(FIRST))

               IF (DIGIT.GT.0) THEN

                  NUM = NUM*10 + DIGIT - 1

               ELSE

                  POS = NUM
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

*   Sort out defaults
      IF (REL) POS = DIR*POS

      END
