C+
***********************************************************************
*                                                                     *
*  PGPLOT Fortran Graphics Subroutine Library                         *
*                                                                     *
*  T. J. Pearson, California Institute of Technology,                 *
*  Pasadena, California 91125.                                        *
*                                                                     *
*  Routines for handling the obsolete character set                   *
*  ------------------------------------------------                   *
*  These routines are not called by PGPLOT but are called by some     *
*  old user-written programs.                                         *
***********************************************************************

******* Index of Modules **********************************************

* GRCHAR -- draw a string of characters
* GRCHR0 -- support routine for GRCHAR and GRMARK
* GRDAT2 -- character set definition (block data)
* GRGTC0 -- obtain character digitization
* GRMARK -- mark points with specified symbol

***********************************************************************
C--

C*GRCHAR -- draw a string of characters
C+
      SUBROUTINE GRCHAR (IDENT,CENTER,ORIENT,ABSXY,X0,Y0,STRING)
C
C GRPCKG: Draw a string of characters. The plot is not windowed
C in the current subarea, but in the full plotting area.
C
C Arguments:
C
C IDENT (input, integer): plot identifier, as returned by GROPEN.
C CENTER (input, logical): if .TRUE., the first character of the string
C      is centered at (X0,Y0); otherwise the bottom left corner of the
C      first character is placed at (X0,Y0).
C ORIENT (input, real): the angle in degrees that the string is to make
C      with the horizontal, increasing anticlockwise.
C ABSXY (input, logical): if .TRUE., (X0,Y0) are absolute device
C      coordinates; otherwise they are world coordinates (the scaling
C      transformation is applied).
C X0, Y0 (input, real): position of first character (see CENTER).
C STRING (input, character): the string of ASCII characters; control
C      characters 0-20 have special representations; all other
C      non-graphic characters are plotted as blank spaces.
C
C (1-Feb-1983)
C-----------------------------------------------------------------------
      CHARACTER*(*) STRING
      INTEGER  IDENT
      LOGICAL  ABSXY, CENTER
      REAL     ORIENT, X0, Y0
C
      CALL GRSLCT(IDENT)
      CALL GRCHR0(.FALSE., CENTER, ORIENT, ABSXY, X0, Y0, STRING)
      RETURN
      END
