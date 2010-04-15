      SUBROUTINE ERRINT(INTR)

*+
*
*   Name:
*      SUBROUTINE ERRINT
*
*   Description:
*      Code an integer and append it to the current error message.
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
*      The integer is coded in concise format. Just direct the text
*      to STDERR.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER MAXLINE   ! maximum length of text string

      PARAMETER (MAXLINE=400)

*   Import:
      INTEGER INTR     ! integer to be written

*   CMERR:
      INCLUDE 'CMERR'

      CALL str_WRITI('%i\\', INTR, MAXLINE, EMESS, EPOS)

      END
