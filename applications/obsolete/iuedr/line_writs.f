      SUBROUTINE line_WRITS(CNTRL, SVALUE)

*+
*
*   Name:
*      SUBROUTINE line_WRITS
*
*   Descritpion
*      Encode string value into internal buffer under format control.
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
*      SVALUE is planted in the internal buffer as specified by the
*      CNTRL format control string.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER MAXBUF        ! maximum length of buffer
      INTEGER MAXTOK        ! maximum length of control string token
      PARAMETER (MAXBUF=512, MAXTOK=256)

*   Import:
      CHARACTER*(*)  CNTRL  ! control string
      CHARACTER*(*)  SVALUE ! string value

*   CMLINE:
      INCLUDE 'CMLINE'

      CALL str_WRITS(CNTRL, SVALUE, MAXBUF, BUF, BUFPOS)

      END
