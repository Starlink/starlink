


      SUBROUTINE GK0QSB(IX,IY)
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           DLT
*                    AJC  Modification for Printronix P300 Lineprinter
*                    PTW  Cosmetics
*                    PLP  Modifications for PRIME
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*     Sets bit IX,IY in bitmap
*
*
*  ARGUMENTS
*  ---------
*     INP IX,IY    - Point to be set
*
      INTEGER IX,IY
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
      INTRINSIC MOD
*
*     External functions declaration
*
      CHARACTER GKAN1
      INTEGER   GKNA1,GK0QLC,GKLOR
*
*  LOCALS
*  ------
*
      INTEGER INTBIT,INTMOD,IPOS
      INTEGER MASK(0:5)
*
      DATA MASK /1,2,4,8,16,32/
*
*--------------------------------------------------------------------

*
*     Get ASCII representation of the character in the bitmap
*
      INTBIT=GKNA1(CHP(GK0QLC(IX,IY)))
      IPOS=MOD(IX,6)
*
*     Modify the existing character
*
      INTMOD=GKLOR(INTBIT,MASK(IPOS))
*
*     Return the modified value to bitmap
*
      CHP(GK0QLC(IX,IY))=GKAN1(INTMOD)

      END
