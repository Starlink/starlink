      SUBROUTINE hlp_HSEEKX (FNAME, IADRX, LOGLEV)
*+
*  - - - - - - -
*   H S E E K X
*  - - - - - - -
*
*  Position help library index for a sequential access using the
*  hlp_HREADX routine.
*
*  Given (arguments):
*     FNAME     c*()    name of help library
*     IADRX     i       index address within help library file (1st = 0)
*     LOGLEV    i       logical level of help library
*
*  Returned (in COMMON):
*     HLNEXT    c*()    name of next help library to be accessed
*     NEXTX     i       address for next sequential access of index
*     LOFFNU    i       logical level of next help library
*
*  Note that no checks are made that the file is open, or that the IADRX
*  value points to the start of a record or lies within the file.  If
*  any of these conditions are violated, appropriate errors will be
*  reported when attempts to access the file are made.
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      CHARACTER*(*) FNAME
      INTEGER IADRX,LOGLEV

      INCLUDE 'helpic'



      HLNEXT=FNAME
      NEXTX=IADRX
      LOFFNU=LOGLEV

      END
