      SUBROUTINE OPW (LU, FILE, IOS)
*+
*  - - - -
*   O P W
*  - - - -
*
*  !!! Platform-independent version, also suitable for !!!
*  !!! VAX, DECstation and Sun SPARCstation            !!!
*
*  Open an output file.
*
*  Given:
*     LU     i      I/O unit number
*     FILE   c*(*)  file name
*
*  Returned:
*     IOS    i      I/O status (0=OK)
*
*  P T Wallace   Starlink   2 June 1992
*-

      IMPLICIT NONE

      INTEGER LU
      CHARACTER*(*) FILE
      INTEGER IOS



      OPEN (LU,FILE=FILE,STATUS='UNKNOWN',IOSTAT=IOS)

      END
