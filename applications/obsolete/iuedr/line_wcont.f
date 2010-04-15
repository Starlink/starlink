      SUBROUTINE line_WCONT( CNTRL )
*+
*
*   Name:
*      SUBROUTINE line_WCONT
*
*   Description:
*      Write into internal line buffer using control format.
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
*      The internal buffer is modified according to the control string.
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

*   CMLINE:
      INCLUDE 'CMLINE'

      CALL str_WCONT(CNTRL, MAXBUF, BUF, BUFPOS)

      END
