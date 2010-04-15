      SUBROUTINE ERRSTR(STR)

*+
*
*   Name:
*      SUBROUTINE ERRSTR
*
*   Description:
*      Add a terminated string to the current error message.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      01-SEP-81
*         AT4 version.
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          23-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      For now just direct the text to STDERR.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB       ! arbitrary string length
      INTEGER MAXLINE   ! maximum length of text string

      PARAMETER (ARB=100, MAXLINE=400)

*   Import:
      BYTE STR(ARB)     ! string to be written

*   CMERR:
      INCLUDE 'CMERR'

      CALL str_WRITS('%s\\', STR, MAXLINE, EMESS, EPOS)

      END
