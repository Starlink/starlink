      SUBROUTINE snx_WRTST (XG, YG, STRING, HG, IOR, ICTR)

*+
*  Name:
*     WRTST

*  Purpose:
*     Write a character string at a given position in grid coordinates

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     XG,YG = REAL (Given)
*         Grid coordinates
*     STRING = CHAR (Given)
*         String to be plotted
*     HG = REAL (Given)
*         Nominal character height in grid Y units
*     IOR = INTEGER (Given)
*         Orientation (deg anticlockwise from horizontal)
*     ICTR = INTEGER (Given)
*         Justification (see below)

*  Externals:
*     snx_AGGUX, snx_AGGUY, AGPWRT, KUPY, PLOTIT

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     {enter_new_authors_here}

*  History:
*     09-JUN-1987 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  ICTR Argument:
*     ICTR is passed directly to the AGPWRT routine.  The default
*     version of AGPWRT supports three ICTR options:  with ICTR=0,
*     the string is centred on the given position; with ICTR=-1 the
*     centre of the left edge of the string is at the given position
*     (i.e. the string is left justified); with ICTR=+1 the centre of
*     the right edge of the string is at the given position (i.e. right
*     justified.  The AGPWRITX version of the AGPWRT routine offers
*     five ICTR options:
*
*         ICTR = -2   left justified, proportionally spaced
*         ICTR = -1   left justified, monospaced
*         ICTR =  0   centred, proportionally spaced
*         ICTR = +1   right justified, monospaced
*         ICTR = +2   right justified, proportionally spaced
*
*     The proportionally spaced options also support PWRITX control
*     sequences for Greek letters, special characters etc.

*-

      IMPLICIT NONE

      REAL XG,YG
      CHARACTER*(*) STRING
      REAL HG
      INTEGER IOR,ICTR

      REAL XU,YU
      INTEGER NH

      REAL snx_AGGUX,snx_AGGUY
      INTEGER KUPY


*  X,Y in user coordinates
      XU = snx_AGGUX(XG)
      YU = snx_AGGUY(YG)

*  Character nominal height in plotter units
      NH = MAX(4,KUPY(snx_AGGUY(HG))-KUPY(snx_AGGUY(0.0)))

*  Draw the string
      CALL AGPWRT(XU,YU,STRING,LEN(STRING),NH,IOR,ICTR)
      CALL PLOTIT(0,0,2)

      END
