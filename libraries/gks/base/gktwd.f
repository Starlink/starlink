      SUBROUTINE GKTWD (NPTS,XW,YW,XD,YD)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation Utility
*  Author:             RCS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The subroutine is designed to transform an array of points from world
*     coordinates into device coordinates. Code varies between
*     level 0 and other levels. At level 0, two factors are zero and
*     since an array of points is transformed per call, some noticable
*     saving can be achieved at level 0 by preprocessing out the
*     redundant terms.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/83  RCS  Original version stabilized
*     11/08/83  JRG  Change of name from GKTRN to GKTWD for
*                    consistency with other transformation routines
*     10/11/83  JRG  Level 1 code and rearrangement of comments
*
*  ARGUMENTS
*  ---------
*    INP   NPTS         the number of coordinate pairs
*    INP   XW (NPTS)    the real array of world x-coordinates
*    INP   YW (NPTS)     .   .    .     .   .   y-coordinates
*    OUT   XD (NPTS)    the real array of device (transformed) x-coordinates
*    OUT   YD (NPTS)     .    .    .    .    .        .        y-coordinates
*
      INTEGER NPTS
      REAL  XW(NPTS), YW(NPTS), XD(NPTS), YD(NPTS)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    Workstation index
*     Read   /WKD/    Transformation matrix
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     A,B,C,D,E,F  Working copies of transformation matrix entries
*     I        Loop variable
*
      INTEGER I
      REAL    A,B,C,D,E,F
*
*  ALGORITHM
*  ---------
*      A general transformation must embody the normalization transformation,
*      segment transformation, and workstation transformation.
*
*      It requires the (ith) world point  ( XW(I),YW(I) )  to be transformed
*      into the (ith) device point  ( XD(I),YD(I) ) according to :
*
*                              _          _     _     _
*                             |            |   |       |
*                             | A   B   C  |   | XW(I) |
*      [ XD(I) , YD(I) ]  =   |            |   |       |
*                             |            |   | YW(I) |
*                             | D   E   F  |   |       |
*                             |            |   |   1   |
*                              -          -     -     -
*
*      where A,B,C,D,E and F are transformation parameters.
*
*
*      The workstation-derived data, contains a workstation-dependent,
*      2-dimensional array "QWTOTT" which corresponds to the
*      transformation variables A,B,C,D,E and F as follows
*
*          A <-> 1      B <-> 2      C <-> 3
*          D <-> 4      E <-> 5      F <-> 6
*
*      In output level 0, only 4 transformation parameters are needed as the
*      segment transformation is not used. Therefore B and D are
*      zero at this level.
*
*-------------------------------------------------------------------

      A = QWTOTT(1,KWKIX)
      C = QWTOTT(3,KWKIX)
      E = QWTOTT(5,KWKIX)
      F = QWTOTT(6,KWKIX)
      B = QWTOTT(2,KWKIX)
      D = QWTOTT(4,KWKIX)
*
*   The local variables are now the transformation variables  A,B,C,D,E and F.
*
      DO 10 I=1,NPTS
         XD(I) =  A * XW(I)  +   B * YW(I)  +   C
         YD(I) =  D * XW(I)  +   E * YW(I)  +   F
   10 CONTINUE
*
      END
