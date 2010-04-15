
*---------------------------------------------------------------------
      SUBROUTINE GK1ABG
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set up device background colour
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     None
*
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  IREM    - Dummy integer, required by the buffering routine.
*  DUMMY   - Dummy character, required by the buffering routine.
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*
      CHARACTER S*70, DUMMY
      INTEGER IREM
*
*  ALGORITHM
*  ---------
*     Get colour components for colour index 0 from the Heap, use those for
*     setting the current colour on the device, set the current path to current
*     clipping rectangle and fill this path with current colour.
*
*---------------------------------------------------------------------
*

*     Start from a new line in the external file
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*     Fill the whole of the clipping area in a background colour.
      WRITE(S, 50) QHP(KHPXR(KCTBPT(1,KWKIX))+0),
     :             QHP(KHPXR(KCTBPT(2,KWKIX))+0),
     :             QHP(KHPXR(KCTBPT(3,KWKIX))+0)
   50 FORMAT(3F8.5, ' setrgbcolor clippath fill newpath')
      CALL GKFOCO(KIOPB, S(1:58), IREM)

      END
