      SUBROUTINE str_READF(CNTRL, LINE, POS, FVALUE, STATUS)

*+
*
*   Name:
*      SUBROUTINE str_READF
*
*   Description:
*      Decode float value into parameter string under format control.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          27-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      A float value is decoded from the supplied line string as specified
*      by the CNTRL format control string.
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
      REAL*8 FVALUE           ! float value

      INTEGER STATUS        ! status return

*   External references:
      INTEGER str_INDEX     ! character index in string
      INTEGER str_LEN       ! string length

*   Local variables:
      LOGICAL FOUND         ! whether a value has been found
      LOGICAL JUMPLN        ! whether jump over newlines (ignored)
      LOGICAL RIGHT         ! whether right justified
      LOGICAL SUPR          ! whether value is used or not

      BYTE EDIT             ! edit character
      BYTE FORMAT(256)      ! format from CNTRL

      INTEGER FIELD         ! field size
      INTEGER FIRST         ! first position in CNTRL
      INTEGER ITEMP         ! temporary integer value
      INTEGER LAST          ! last position in CNTRL
      INTEGER TYPE          ! format type index

      REAL*8 FTEMP            ! temporary float value

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
            IF (str_INDEX('if\\', EDIT).EQ.0) EDIT = 102

            IF (EDIT.EQ.105) THEN

               CALL str_PRI(RIGHT, FIELD, LINE, POS, ITEMP, STATUS)
               FTEMP = ITEMP

            ELSE IF (EDIT.EQ.102) THEN

               CALL str_PRF(RIGHT, FIELD, LINE, POS, FTEMP, STATUS)

            ELSE

               STATUS = -3
               GO TO 200

            END IF

            IF (.NOT.SUPR .AND. STATUS.EQ.0) THEN

               FVALUE = FTEMP
               FOUND = .TRUE.

            END IF

*           call str_mtch(format, line, pos, status)

         END IF
         GO TO 100

      END IF

 200  CONTINUE

*   Check that a value is available
      IF (.NOT.FOUND) STATUS = -3

      END
