      SUBROUTINE GK2MTR
*--------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine derives the workstation total transformation.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/ Transformation parameters from Workstation
*                     Communication Area
*     Read   /GKYWSL/ Workstation Window & Workstation Viewport
*     Modify /GKYWKD/ Derived transformation and clipping rectangle
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkmc.par'

      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  ALGORITHM
*  ---------
*     (a) Accepts transformation in Workstation Communication Area
*         (QWR11 ... QWR16) and
*         combines it with Workstation Transformation.
*     (b) Accepts clipping rectangle in QWR7 ... QWR10
*
*---------------------------------------------------------------------


*     Data expected:
*     QWR1 ... QWR6: transformation C2
*     QWR7-10 : clipping rectangle (NDC) XL, XR, YB, YT
*     QWR11 ...QWR16: transformation C3

*     Translation C3
      QWTOTT(1,KWKIX) = QWR11
      QWTOTT(2,KWKIX) = QWR12
      QWTOTT(3,KWKIX) = QWR13
      QWTOTT(4,KWKIX) = QWR14
      QWTOTT(5,KWKIX) = QWR15
      QWTOTT(6,KWKIX) = QWR16

*     Clipping rectangle
      QWCLXL(KWKIX) = QWR7
      QWCLXR(KWKIX) = QWR8
      QWCLYB(KWKIX) = QWR9
      QWCLYT(KWKIX) = QWR10

      END
