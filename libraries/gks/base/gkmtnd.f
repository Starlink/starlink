C# IL>=a, OL>=0
      SUBROUTINE GKMTND (XFORM,TDCX,TDCY)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The subroutine is designed to transform a matrix,
*     which transforms within NDC (eg a segment transformation)
*     to the corresonding matrix for DC.
*
*     As only the translation vector is effected, only it is output.
*
*  MAINTENANCE LOG
*  ---------------
*     23/03/88  KEVP  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   XFORM        transform matrix  w.r.t. NDC
*     OUT   TDCX,TDCY    translation vector in DC
*
      REAL XFORM(3,2), TDCX,TDCY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     WVX,WVY, WWX,WWY     Dimension of workstation viewport and window
*     SF                   Scale factor for conversion of NDC to DC
*     TX,TY                Translation vector for conversion of NDC to DC
*     VX,VY                Transformed DC origin
*
      REAL    VX,VY, WVX,WVY, WWX,WWY, SF, TX,TY
*
*  ALGORITHM
*  ---------
*     The transformation of NDC to DC is equivalent to multiplying
*     the vector ( with an extra 1 on the end) by a matrix A
*
*                -            -
*               |              |
*               | SF   0   TX  |
*        A  =   |              |
*               |              |
*               | 0    SF  TY  |
*               |              |
*                -            -    ,
*
*     where SF is the scale factor and (TX,TY) is the translation
*     vector of the NDC to DC transformation.
*     The the DC to NDC matrix B is given by
*
*                -                  -
*               |                    |
*               | 1/SF   0   -TX/SF  |
*        B  =   |                    |
*               |                    |
*               | 0    1/SF  -TY/SF  |
*               |                    |
*                -                  -    .
*
*     The new translation vector is obtained by taking the DC origin
*     converting it to NDC using B applying the input matrix XFORM
*     and converting back to DC using A.
*
*---------------------------------------------------------------------
*
*     Obtain NDC to DC constants.
      WVX = QCWVXR(KWKIX)-QCWVXL(KWKIX)
      WVY = QCWVYT(KWKIX)-QCWVYB(KWKIX)
      WWX = QCWWXR(KWKIX)-QCWWXL(KWKIX)
      WWY = QCWWYT(KWKIX)-QCWWYB(KWKIX)
      IF (WWX * WVY .GT. WWY * WVX) THEN
         SF = WVX / WWX
      ELSE
         SF = WVY / WWY
      ENDIF
      TX = QCWVXL(KWKIX) - SF*QCWWXL(KWKIX)
      TY = QCWVYB(KWKIX) - SF*QCWWYB(KWKIX)

*     convert DC origin to NDC and apply input matrix XFORM
      VX = XFORM(1,1)*(-TX/SF) + XFORM(2,1)*(-TY/SF) + XFORM(3,1)
      VY = XFORM(1,2)*(-TX/SF) + XFORM(2,2)*(-TY/SF) + XFORM(3,2)

*     convert back to DC
      TDCX = SF*VX + TX
      TDCY = SF*VY + TY

      END
