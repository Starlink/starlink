      SUBROUTINE GK3CCL
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*     Clear display surface
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
*
*  ARGUMENTS
*  ---------
*
*  ERRORS
*  ------
*
*  ALGORITHM
*  ---------
*      End picture
*      If (TeX) Return to text mode
*
*  LOCALS
*  ------
      INTEGER NLEFT
      CHARACTER*1 IS2, VDM
      INTRINSIC CHAR
*      PARAMETER (IS2=CHAR(30), VDM=CHAR(125))
*  Offsets into KWKDAT workspace
      INTEGER CT, ROTATE, TEX
      PARAMETER (CT=1, ROTATE=2, TEX=3)
*  End of offsets
      IS2 = CHAR(30)
      VDM = CHAR(125)

      CALL GKFOCO(KIOPB,'%'//IS2,NLEFT)
      IF (KWKDAT(TEX,KWKIX).EQ.1)
     :               CALL GKFOCO(KIOPB,VDM//'p00'//IS2,NLEFT)

      END
