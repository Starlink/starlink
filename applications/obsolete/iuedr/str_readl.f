      SUBROUTINE STR_READL(CNTRL, LINE, POS, LVALUE, STATUS)

*+
*
*   Name:
*      SUBROUTINE STR_READL
*
*   Description:
*      Decode logical value from string parameter.
*
*   History:
*      Jack Giddings      03-JAN-82     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      A logical value is decoded from the supplied line string as
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
      LOGICAL LVALUE        ! string value

      INTEGER STATUS        ! status return

*   External references:
      LOGICAL STR_SIMLR     ! caseless string equality

*   Local variables:
      BYTE SVALUE(3)        ! temporary string

      CALL STR_READS(CNTRL, LINE, 3, POS, SVALUE, STATUS)

      IF (STATUS.EQ.0) THEN

         IF (STR_SIMLR(SVALUE, 'T\\')) THEN

            LVALUE = .TRUE.

         ELSE IF (STR_SIMLR(SVALUE, 'F\\')) THEN

            LVALUE = .FALSE.

         ELSE

            STATUS = -3

         END IF

      END IF

      END
