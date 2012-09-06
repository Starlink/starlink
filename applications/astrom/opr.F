#if HAVE_CONFIG_H
#include "config.h"
#endif
      SUBROUTINE OPR (LU, FILE, IOS)
*+
*  - - - -
*   O P R
*  - - - -
*
*  Open an input file, avoiding any platform-specific need for write
*  access.
*
*  Given:
*     LU     i      I/O unit number
*     FILE   c*(*)  file name
*
*  Returned:
*     IOS    i      I/O status (0=OK)
*
*  P T Wallace   Starlink   19 May 1992
*-

      IMPLICIT NONE

      INTEGER LU
      CHARACTER*(*) FILE
      INTEGER IOS



      OPEN (LU,FILE=FILE,STATUS='OLD',
#if HAVE_FC_OPEN_ACTIONREAD
     :     ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :     READONLY,
#endif
     :     IOSTAT=IOS)

      END
