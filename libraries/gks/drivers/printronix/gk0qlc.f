


      INTEGER FUNCTION GK0QLC(IX,IY)
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           PLP
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*  Calculates Character Heap target address
*
*
*  ARGUMENTS
*  ---------
*
*  INP  IX, IY - bitmap point coordinates
*
      INTEGER IX,IY
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'

*
*
*  LOCALS
*  ------
*
*     Offsets in KWKDAT
*
      INTEGER NVADR,NCOL,NROW
      PARAMETER (NVADR=1,NCOL=2,NROW=3)
*
      INTEGER I
*
*
*  ALGORITHM
*  ---------
*     Target address for J-th variable in any heap allocation is:
*     INDEX + J - 1. Here, INDEX is stored in KHPXC(KWKDAT(NVADR,KWKIX))
*     and because INDEX points to the first character of the last
*     row in the bitmap, J-th element is counted "backwards".
*
*--------------------------------------------------------------------
*
      I=IX/6+1
*
      GK0QLC = KHPXC(KWKDAT(NVADR,KWKIX)) +
     :        ((KWKDAT(NCOL,KWKIX)-1-IY)*KWKDAT(NROW,KWKIX) + I) -
     :        1
*
      END
