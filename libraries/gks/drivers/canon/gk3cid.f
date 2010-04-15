      SUBROUTINE GK3CID
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
*     Initialize plotter
*
*  ARGUMENTS
*  ---------
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  ERRORS
*  ------
*
*  ALGORITHM
*  ---------
*     If (not Tex) then
*        Enter ISO mode (harmless in ISO mode)
*        Reset
*        Enter ISO mode (harmless in ISO mode)
*        Select full paint mode
*     else
*        Memorize current position
*     endif
*     Enter VDM mode
*     Begin picture
*     Scaling mode mm x 0.01
*     If (tex) then
*        Return to text mode
*        Restore current position
*        Enter VDM mode
*     endif
*     Begin picture body
*     If (not tex) then
*        If (rotated) then
*           Shift X & Y origin
*        else
*           Shift Y origin
*        endif
*     endif
*
*
*
*  LOCALS
*  ------
*
      INTEGER NLEFT, NC1, NC2
      CHARACTER*1 ESC, IS2, CSI, VDM
      CHARACTER*10 C1, C2
*      PARAMETER (ESC=CHAR(27), IS2=CHAR(30), CSI=CHAR(155),
*     :           VDM=CHAR(125))
      REAL FX
      PARAMETER (FX=8.46668)
*  Offsets into KWKDAT workspace
      INTEGER CT, ROTATE, TEX
      PARAMETER (CT=1, ROTATE=2, TEX=3)
*  End of offsets

* Linux can not use CHAR in a PARAMETER statement
      ESC=CHAR(27)
      IS2=CHAR(30)
      CSI=CHAR(155)
      VDM=CHAR(125)


      IF (KWKDAT(TEX,KWKIX).EQ.0) THEN
         CALL GKFOCO(KIOPB,ESC//';',NLEFT)
         CALL GKFOCO(KIOPB,ESC//'c',NLEFT)
         CALL GKFOCO(KIOPB,ESC//';',NLEFT)
         CALL GKFOCO(KIOPB,CSI//'2&z',NLEFT)
         CALL GKFOCO(KIOPB,CSI//'14p',NLEFT)
      END IF
      IF (KWKDAT(TEX,KWKIX).EQ.1) THEN
         CALL GKFOCO(KIOPB,CSI//'1;49x',NLEFT)
      END IF
      CALL GKFOCO(KIOPB,CSI//'0&}',NLEFT)
      CALL GKFOCO(KIOPB,'#GKS-UK'//IS2,NLEFT)
      CALL GKFOCO(KIOPB,'!0"1'//IS2,NLEFT)
      IF (KWKDAT(TEX,KWKIX).EQ.1) THEN
         CALL GKFOCO(KIOPB,VDM//'p00'//IS2,NLEFT)
         CALL GKFOCO(KIOPB,CSI//'0;49x',NLEFT)
         CALL GKFOCO(KIOPB,CSI//'1&}',NLEFT)
      END IF
      CALL GKFOCO(KIOPB,'$'//IS2,NLEFT)
      IF (KWKDAT(TEX,KWKIX).EQ.0) THEN
         IF (KWKDAT(ROTATE,KWKIX).EQ.1) THEN
            CALL GK3CEI(NINT(REAL(KDSRY(KWKIX)-1.0)*FX),C1,NC1)
            CALL GK3CEI(NINT(REAL(KDSRX(KWKIX)-1.0)*FX),C2,NC2)
         ELSE
            C1 = '0'
            NC1 = 1
            CALL GK3CEI(NINT(REAL(KDSRY(KWKIX)-1.0)*FX),C2,NC2)
         END IF
      END IF
      CALL GKFOCO(KIOPB,VDM//'"'//C1(:NC1)//C2(:NC2)//IS2,NLEFT)
      END
