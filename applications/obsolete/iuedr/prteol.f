      SUBROUTINE PRTEOL( STATUS )

*+
*
*   Name:
*      SUBROUTINE PRTEOL
*
*   Description:
*      Finish a line and print, then terminate it.
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
*      Martin Clayton     06-DEC-94     IUEDR Vn. 3.2
*         Removed I/O unit number.
*
*   Method:
*      Print a NULL line.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Export:
      INTEGER STATUS     ! status return

*.

      CALL PRTLIN( '\\', STATUS )

      END
