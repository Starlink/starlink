C# IL>=a, OL>=0
      SUBROUTINE GKTWDV (XW,YW,XD,YD)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             RCS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      The subroutine transforms a vector in world
*      coordinates into device coordinates.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/83  RCS  Original version stabilized
*     11/08/83  JRG  Change of name from GKTRNV to GKTWDV for
*                    consistency with other transformation routines
*     02/10/85  JRG  Removed blank line at end (bug S96)
*
*  ARGUMENTS
*  ---------
*     INP   XW    the real vector in world x-coordinates
*     INP   YW     .   .    .      .   .   y-coordinates
*     OUT   XD    the real vector in device (transformed) x-coordinates
*     OUT   YD     .    .    .    .    .         .        y-coordinates
*
      REAL   XW, YW, XD, YD
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    Workstation index
*     Modify /WKD/    QWTOTT
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  ALGORITHM
*  ---------
*      We require the world vector ( XW, YW )
*      to be transformed into the device vector  ( XD, YD )
*      according to :               _           _   _    _
*                                  |             | |      |
*                                  | A    B    0 | |  XW  |
*        [   XD  ,   YD  ]    =    |             | |      |
*                                  |             | |  YW  |
*                                  | D    E    0 | |      |
*                                  |             | |   1  |
*                                   -           -   -    -
*
*      where A,B,D and E are  transformation parameters.
*
*      The normalization transformation, segment transformation
*      and workstation transformation are embodied in these variables.
*
*      Since no displacement is involved, the constant terms are zero.
*      [The constant terms C and F are zero : c.f. Subroutine GKTWD.]
*
*      Thus we have:
*         A = QWTOTT(1,KWKIX)
*         B = QWTOTT(2,KWKIX)
*         D = QWTOTT(4,KWKIX)
*         E = QWTOTT(5,KWKIX)
*---------------------------------------------------------------------


      XD  =  QWTOTT(1,KWKIX)* XW   +   QWTOTT(2,KWKIX)* YW
      YD  =  QWTOTT(4,KWKIX)* XW   +   QWTOTT(5,KWKIX)* YW



      RETURN


      END
