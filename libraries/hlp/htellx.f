      SUBROUTINE hlp_HTELLX (FNAME, IADRX, LOGLEV)
*+
*  - - - - - - -
*   H T E L L X
*  - - - - - - -
*
*  Inquire the help library's current name, index address and logical
*  level.
*
*  Given (in COMMON):
*     HLNEXT    c*()    name of next help library to be accessed
*     NEXTX     i       index address for next hlp_HREADX access
*     LOFFNU    i       logical level of next help library
*
*  Returned (arguments):
*     FNAME     c*()    name of help library
*     IADRX     i       index address within help library file (1st = 0)
*     LOGLEV    i       logical level of help library
*
*  Note that no checks are made that the file is open, or that the IADRX
*  value points to the start of a record or lies within the file.  If
*  any of these conditions are violated, appropriate errors will be
*  reported when attempts to access the file are made based on the
*  result of calling the present routine.
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      CHARACTER*(*) FNAME
      INTEGER IADRX,LOGLEV

      INCLUDE 'helpic'


      FNAME=HLNEXT
      IADRX=NEXTX
      LOGLEV=LOFFNU

      END
