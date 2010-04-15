      SUBROUTINE str_READI(CNTRL, LINE, POS, IVALUE, STATUS)

*+
*
*   Name:
*      SUBROUTINE str_READI
*
*   Description:
*      Decode integer value from string parameter.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          27-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      A integer value is decoded from the supplied line string as
*      specified by the CNTRL format control string.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE CNTRL(256)       ! control string

      BYTE LINE(100)        ! string to be scanned

*   Import/Export:
      INTEGER POS           ! current character position in line

*   Export:
      INTEGER IVALUE        ! integer value
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

      INTEGER ITEMP         ! temporary integer value
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

            IF (str_INDEX('ioh\\', EDIT).EQ.0) EDIT = 105

            IF (EDIT.EQ.105) THEN
               CALL str_PRI(RIGHT, FIELD, LINE, POS, ITEMP, STATUS)

            ELSE IF (EDIT.EQ.111) THEN

               CALL str_PRO(RIGHT, FIELD, LINE, POS, ITEMP, STATUS)

            ELSE IF (EDIT.EQ.104) THEN

               CALL str_PRH(RIGHT, FIELD, LINE, POS, ITEMP, STATUS)

            ELSE

               STATUS = -3
               GO TO 200

            END IF

            IF (.NOT.SUPR .AND. STATUS.EQ.0) THEN

               IVALUE = ITEMP
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
