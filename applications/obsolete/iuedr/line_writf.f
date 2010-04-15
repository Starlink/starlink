      SUBROUTINE line_WRITF(CNTRL, FVALUE)

*+
*
*   Name:
*      SUBROUTINE line_WRITF
*
*   Description:
*      Encode float value into internal buffer under format control.
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
*      FVALUE is planted in the internal buffer as specified by the
*      CNTRL format control string.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER MAXBUF       ! maximum size of buffer
      INTEGER MAXTOK       ! maximum length of control string token

      PARAMETER (MAXBUF=512, MAXTOK=256)

*   Import:
      BYTE CNTRL(MAXTOK)   ! control string
      REAL*8 FVALUE        ! float value

*   CMLINE:
      INCLUDE 'CMLINE'

      CALL str_WRITF(CNTRL, FVALUE, MAXBUF, BUF, BUFPOS)
      END
