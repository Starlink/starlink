C# IL>=a, OL>=0
      SUBROUTINE GKMTXF (XFORM,NPTS,XW,YW,XD,YD)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The subroutine is designed to transform an array of points using the
*     supplied matrix
*
*  MAINTENANCE LOG
*  ---------------
*     19/12/83  JGW  Original version stabilized
*     20/12/83  AS   Tidy up
*
*  ARGUMENTS
*  ---------
*    INP   XFORM        transform matrix
*    INP   NPTS         the number of coordinate pairs
*    INP   XW (NPTS)    the real array of world x-coordinates
*    INP   YW (NPTS)     .   .    .     .   .   y-coordinates
*    OUT   XD (NPTS)    the real array of device (transformed) x-coordinates
*    OUT   YD (NPTS)     .    .    .    .    .        .        y-coordinates
*
      INTEGER NPTS
      REAL    XFORM(3,2), XW(NPTS), YW(NPTS), XD(NPTS), YD(NPTS)
*
*  LOCALS
*  ------
*     A,C,E,F  Working copies of transformation matrix entries
*     I        Loop variable
*
      INTEGER I
      REAL    A,B,C,D,E,F,XINP
*
*  ALGORITHM
*  ---------
*                              -          -     -     -
*                             |            |   |       |
*                             | A   B  C   |   | XW(I) |
*      [ XD(I) , YD(I) ]  =   |            |   |       |
*                             |            |   | YW(I) |
*                             | D   E   F  |   |       |
*                             |            |   |   1   |
*                              -          -     -     -
*      [where "I" runs from  1 to the number of points (NPTS)].
*
*
*     DO 10 I=1,NPTS
*        XD(I) =  A * XW(I)  +   B * YW(I)  +   C
*        YD(I) =  D * XW(I)  +   E * YW(I)  +   F
*     10    CONTINUE
*
*
*---------------------------------------------------------------------

      A = XFORM(1,1)
      B = XFORM(2,1)
      C = XFORM(3,1)
      D = XFORM(1,2)
      E = XFORM(2,2)
      F = XFORM(3,2)


*  Now we have the calculation:

      DO 10 I=1,NPTS
         XINP  = XW(I)
         XD(I) =  A*XINP + B*YW(I) + C
         YD(I) =  D*XINP + E*YW(I) + F
   10 CONTINUE


      END
