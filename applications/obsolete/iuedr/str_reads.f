      SUBROUTINE str_READS(CNTRL, LINE, MAXC, POS, SVALUE, STATUS)

*+
*
*   Name:
*      SUBROUTINE str_READS
*
*   Description:
*      Decode string value from string parameter.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          27-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      An integer value is decoded from the supplied line string as
*      specified by the CNTRL format control string.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE CNTRL(256)       ! control string
      BYTE LINE(100)        ! string to be scanned

      INTEGER MAXC          ! maximum size of string

*   Import/Export:
      INTEGER POS           ! current character position in line

*   Export:
      BYTE SVALUE(MAXC)     ! string value

      INTEGER STATUS        ! status return

*   External references:
      INTEGER str_INDEX     ! character index in string
      INTEGER str_LEN       ! string length

*   Local variables:
      LOGICAL FOUND         ! whether value found
      LOGICAL JUMPLN        ! whether jump over newlines (ignored)
      LOGICAL RIGHT         ! whether right justified
      LOGICAL SUPR          ! whether value is used or not

      BYTE EDIT             ! edit character
      BYTE FORMAT(256)      ! format from CNTRL
      BYTE STEMP(256)       ! temporary string value

      INTEGER FIELD         ! field size
      INTEGER FIRST         ! first position in CNTRL
      INTEGER LAST          ! last position in CNTRL
      INTEGER TYPE          ! format type index

      STATUS = 0
      FOUND = .FALSE.
      FIRST = 1
      LAST = str_LEN(CNTRL)

 100  CONTINUE

      IF ((FIRST.LE.LAST) .AND. (STATUS.EQ.0)) THEN

         CALL str_GTOK(CNTRL, LAST, FIRST, TYPE, FORMAT)

         IF (TYPE.EQ.3) THEN

            CALL str_RPOS(FORMAT, LINE, POS)

         ELSE IF (TYPE.EQ.1) THEN

            CALL str_DESC(FORMAT, RIGHT, FIELD, SUPR, JUMPLN, EDIT)
            IF (str_INDEX('sqc\\', EDIT).EQ.0) EDIT = 115

            IF (EDIT.EQ.115) THEN

               CALL str_PRS(RIGHT, FIELD, LINE, 256, POS, STEMP, STATUS)

            ELSE IF (EDIT.EQ.113) THEN

               CALL str_PRQ(LINE, 256, POS, STEMP, STATUS)

            ELSE IF (EDIT.EQ.99) THEN

               IF (POS.LE.str_LEN(LINE)) THEN

                  STEMP(1) = LINE(POS)
                  CALL str_TERM(1, 256, STEMP)
                  STATUS = 0

               ELSE

                  STATUS = -3

               END IF

            ELSE

               STATUS = -3
               GO TO 200

            END IF

            IF (.NOT.SUPR .AND. STATUS.EQ.0) THEN

               CALL str_MOVE(STEMP, MAXC, SVALUE)
               FOUND = .TRUE.

            END IF

*           call str_mtch(format, line, pos, status)

         END IF

         GO TO 100

      END IF

 200  CONTINUE

*   Check value found
      IF (.NOT.FOUND) STATUS = -3

      END
