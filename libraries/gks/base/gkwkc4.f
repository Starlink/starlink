C# IL>=a, OL>=0
      SUBROUTINE GKWKC4
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation Utility
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To rederive the workstation total transformation and the workstation
*     total clipping rectangle.
*
*  MAINTENANCE LOG
*  ---------------
*    13/04/83 JRG   Original version stabilized
*    10/12/83 JRG   Comment change
*    30/01/84 JRG   Use QWR11...16 instead of QWR1...6
*      8/3/84 JRG   Put in code to make boundaries visible
*     30/3/84 JRG   New method for making boundaries visible. Adjust
*                   total transformation so that area within the
*                   combined clipping rectangle is slightly reduced
*                   about its centre.
*    06/08/90 KEVP  Removed unused local variables (S342).
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
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     ADJ      Size of adjustment.
*     WWX,WWY  Extent of Workstation Window in NDC
*     WVX,WVY  Extent of Workstation Viewport in DC
*     SF       Scale Factor to convert from NDC to DC
*     TINY     Small factor used in making image slightly smaller
*     CX,CY    Centre of combined clipping rectangle
*
      REAL  ADJ, SF, WWX,WWY, WVX,WVY,
     :      TINY, CX,CY
*
*  ALGORITHM
*  ---------
*     (a) Accepts transformation in Workstation Communication Area
*         (QWR11 ... QWR16) and
*         combines it with Workstation Transformation.
*     (b) Accepts clipping rectangle in QWR7 ... QWR10  and combines it
*         with the trimmed Workstation Viewport.
*
*---------------------------------------------------------------------


*     Data expected:
*     QWR1 ... QWR6: transformation C2
*     QWR7-10 : clipping rectangle (NDC) XL, XR, YB, YT
*     QWR11 ...QWR16: transformation C3

*     Find the extent of the wkstn viewport and wkstn window
      WVX = QCWVXR(KWKIX)-QCWVXL(KWKIX)
      WVY = QCWVYT(KWKIX)-QCWVYB(KWKIX)
      WWX = QCWWXR(KWKIX)-QCWWXL(KWKIX)
      WWY = QCWWYT(KWKIX)-QCWWYB(KWKIX)


*     Window to viewport scale factor gives largest rectangle that will
*     fit into bottom left corner of viewport (as aspect ratio of
*     window & viewport are the same).
      IF (WWX * WVY .GT. WWY * WVX) THEN
         SF = WVX / WWX
      ELSE
         SF = WVY / WWY
      ENDIF

*     C4 = C3 * scale factor + viewport translation
      QWTOTT(1,KWKIX) = QWR11 * SF
      QWTOTT(2,KWKIX) = QWR12 * SF
      QWTOTT(3,KWKIX) = (QWR13 - QCWWXL(KWKIX)) * SF + QCWVXL(KWKIX)
      QWTOTT(4,KWKIX) = QWR14 * SF
      QWTOTT(5,KWKIX) = QWR15 * SF
      QWTOTT(6,KWKIX) = (QWR16 - QCWWYB(KWKIX)) * SF + QCWVYB(KWKIX)

*     New clipping rectangle = transformed intersection of window
*     and NDC clipping rectangle.
*     If intersection is empty, XL will be > XR or YB will be > YT.
*     Clipping routines in GKS and drivers need to cope with this.
      QWCLXL(KWKIX) = (MAX(QCWWXL(KWKIX),QWR7) - QCWWXL(KWKIX)) * SF
     :                + QCWVXL(KWKIX)
      QWCLXR(KWKIX) = (MIN(QCWWXR(KWKIX),QWR8) - QCWWXL(KWKIX)) * SF
     :                + QCWVXL(KWKIX)
      QWCLYB(KWKIX) = (MAX(QCWWYB(KWKIX),QWR9) - QCWWYB(KWKIX)) * SF
     :                + QCWVYB(KWKIX)
      QWCLYT(KWKIX) = (MIN(QCWWYT(KWKIX),QWR10) - QCWWYB(KWKIX)) * SF
     :                + QCWVYB(KWKIX)

*     Now we have the clipping rectangle, we can adjust the
*     transformation to reduce the image slightly about the centre
*     of the clip rectangle.
      CX=(QWCLXL(KWKIX)+QWCLXR(KWKIX))*0.5
      CY=(QWCLYB(KWKIX)+QWCLYT(KWKIX))*0.5
      TINY=QTOL
      ADJ=1-TINY
      QWTOTT(1,KWKIX)=QWTOTT(1,KWKIX)*ADJ
      QWTOTT(2,KWKIX)=QWTOTT(2,KWKIX)*ADJ
      QWTOTT(3,KWKIX)=QWTOTT(3,KWKIX)*ADJ+CX*TINY
      QWTOTT(4,KWKIX)=QWTOTT(4,KWKIX)*ADJ
      QWTOTT(5,KWKIX)=QWTOTT(5,KWKIX)*ADJ
      QWTOTT(6,KWKIX)=QWTOTT(6,KWKIX)*ADJ+CY*TINY

      END
