      SUBROUTINE line_WRITI(CNTRL, IVALUE)

*+
*
*   Name:
*      SUBROUTINE line_WRITI
*
*   Description:
*      Encode integer value into internal buffer under format control.
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
*      IVALUE is planted in the internal buffer as specified by the
*      CNTRL format control string.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER MAXBUF       ! maximum length of buffer
      INTEGER MAXTOK       ! maximum length of control string token

      PARAMETER (MAXBUF=512, MAXTOK=256)

*   Import:
      BYTE CNTRL(MAXTOK)   ! control string

      INTEGER IVALUE       ! integer value

*   CMLINE:
      INCLUDE 'CMLINE'

      CALL str_WRITI(CNTRL, IVALUE, MAXBUF, BUF, BUFPOS)

      END
