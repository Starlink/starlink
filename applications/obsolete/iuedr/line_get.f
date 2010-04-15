      SUBROUTINE line_GET(MAXC, VALUE)

*+
*
*   Name:
*      SUBROUTINE line_GET
*
*   Description:
*      Get the contents of the internal buffer.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          13-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      The internal buffer contents are copied into the VALUE
*      parameter string.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER MAXBUF       ! maximum length of buffer

      PARAMETER (MAXBUF=512)

*   Import:
      INTEGER MAXC         ! maximum size of VALUE string

*   Export:
      BYTE VALUE(MAXC)     ! value string

*   CMLINE:
      INCLUDE 'CMLINE'

      IF (BUFPOS.LT.1) THEN
         BUFPOS = 1
         CALL str_TERM(0, MAXBUF, BUF)
      END IF

      CALL str_MOVE(BUF, MAXC, VALUE)

      END
