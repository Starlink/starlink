      SUBROUTINE hlp_HINIT (LU, FNAME, EOS)
*+
*  - - - - - -
*   H I N I T
*  - - - - - -
*
*  Initialize HLP system.
*
*  Given (arguments):
*     LU      i        I/O unit number for help library
*     FNAME   c*(*)    help library file name
*     EOS     c*1      character to use as end-of-string
*
*  Returned (in COMMON):
*     JHELP   i        state of HLP system:  -1=closed
*     LUHL    i        I/O unit number for help library
*     LOFFNU  i        logical level number for new help library
*     HLOPEN  c*(*)    name of open help library (reset to ' ')
*     HLNEXT  c*(*)    name of help library for next access
*     HEOS    c*1      end-of-string character
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      INTEGER LU
      CHARACTER*(*) FNAME,EOS

      INCLUDE 'helpic'



      LUHL=LU
      HLOPEN=' '
      HLNEXT=FNAME
      LOFFNU=0
      HEOS=EOS
      JHELP=-1

      END
