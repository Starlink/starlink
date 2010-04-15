      SUBROUTINE PRTBUF( STATUS )

*+
*
*   Name:
*      SUBROUTINE PRTBUF
*
*   Description:
*      Get line of text from LINELIB and print.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      01-SEP-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          13-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      Get text using LINE_GET and print using PRTLIN.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Local constants:
      INTEGER MAXREC      ! maximum length of printed record
      PARAMETER (MAXREC=133)

*   Export:
      INTEGER STATUS      ! status return

*   Local variables:
      BYTE LINE(MAXREC)   ! copy of text line

*.

      CALL line_GET( MAXREC, LINE )
      CALL PRTLIN( LINE, STATUS )

      END
