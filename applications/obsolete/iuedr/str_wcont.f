      SUBROUTINE str_WCONT(CNTRL, MAXC, LINE, POS)

*+
*
*   Name:
*      SUBROUTINE str_WCONT
*
*   Description:
*      Write control string into line parameter.
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
*      The CNTRL format control string is processed and the line
*      parameter modified.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE CNTRL(256)      ! control string

      INTEGER MAXC         ! size of value

*   Import/Export:
      BYTE LINE(MAXC)      ! line to be modified

      INTEGER POS          ! character position

*   External references:
      INTEGER str_LEN      ! string length

*   Local variables:
      BYTE FORMAT(256)     ! format from CNTRL
      BYTE VALUE(256)      ! value coded into string

      INTEGER FIRST        ! first position in CNTRL
      INTEGER LAST         ! last position in CNTRL
      INTEGER TYPE         ! format type index

      FIRST = 1
      LAST = str_LEN(CNTRL)

 100  CONTINUE

      IF (.NOT.(FIRST.LE.LAST)) THEN

         RETURN

      ELSE

         CALL str_GTOK(CNTRL, LAST, FIRST, TYPE, FORMAT)

         IF (TYPE.EQ.3) THEN

            CALL str_WPOS(FORMAT, MAXC, LINE, POS)

         ELSE IF (TYPE.EQ.2) THEN

            CALL str_WIT1(FORMAT, 256, VALUE)
            CALL str_ADD(VALUE, MAXC, LINE, POS)

         ELSE

            CALL str_ADD(FORMAT, MAXC, LINE, POS)

         END IF

         GO TO 100

      END IF

      END
