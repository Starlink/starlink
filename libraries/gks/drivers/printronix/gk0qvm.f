


      SUBROUTINE GK0QVM
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           PLP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*     Get "virtual memory" for the bitmap.
*
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'

*
*     Intrinsic functions declaration
*
      INTRINSIC NINT
*
*     External functions declaration
*
      CHARACTER GKAN1
      INTEGER GK0QLC
*
*  LOCALS
*  ------
*
*     Offsets in KWKDAT
*
      INTEGER NVADR,NCOL,NROW
      PARAMETER (NVADR=1,NCOL=2,NROW=3)
*
      INTEGER NWSTYP,LPORT
      PARAMETER (NWSTYP=6,LPORT=0)
*
*     Offsets in QWKDAT
*
      INTEGER NXSC, NYSC
      PARAMETER (NXSC=1,NYSC=2)
*
      INTEGER IX,IY,LCOL,LROW,ICLEAR
      PARAMETER  (ICLEAR = 64)
*
*
*  ALGORITHM
*  ---------
*     Calculate number of bytes needed from the workstation viewport
*     size and allocate heap. Then reset the bitmap.
*
*--------------------------------------------------------------------

*
*     Each row is rounded up to an integral number of bytes. The
*     lines parallel to the longer side of the frame are "rows",
*     regardless of the WS orientation.
*
      IF(KWKDAT(NWSTYP,KWKIX).EQ.LPORT)THEN
         LROW = (NINT(QCWVYT(KWKIX)*QWKDAT(NXSC,KWKIX))+6)/6
         LCOL = (NINT(QCWVXR(KWKIX)*QWKDAT(NYSC,KWKIX))+1)
      ELSE
         LROW = (NINT(QCWVXR(KWKIX)*QWKDAT(NXSC,KWKIX))+6)/6
         LCOL = (NINT(QCWVYT(KWKIX)*QWKDAT(NYSC,KWKIX))+1)
      ENDIF
*
      KWKDAT(NROW,KWKIX) = LROW
      KWKDAT(NCOL,KWKIX) = LCOL
*
*     Allocate Heap Character Space
*
      CALL GKHPAL(LCOL*LROW,KCHARS,KWKDAT(NVADR,KWKIX))
      IF(KERROR.EQ.0)THEN
*
*        Clear the bitmap
*
         DO 100 IY=LCOL-1,0,-1
            DO 200 IX=0,LROW-1
               CHP(GK0QLC(IX*6,IY))=GKAN1(ICLEAR)
  200       CONTINUE
  100    CONTINUE
      ELSE
         KERROR=26
      ENDIF
*
      END
