


*----------------------------------------------------------------
      SUBROUTINE GK0CPA(IFUNC,NINT,INTA)
*--------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             RMK
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*     CC81  Depending on the value of IFUNC, either outputs bytes to buffer
*     or sends buffer to plotter with handshaking.
*
*  MAINTENANCE LOG
*  ---------------
*     09/07/84  RMK   Original version stabilized
*     30/05/85  RMK   If sending buffer, set current point to unknown, to
*                     force complete address to be sent at start of next buffer.
*
*  ARGUMENTS
*  ---------
*
*  INP IFUNC  Either KIOSN or KIOPB
*  INP NINT  Number of values in array INTA
*  INP INTA  Array containing bytes to be sent to device
*
      INTEGER NINT
      INTEGER INTA(NINT)
      INTEGER IFUNC
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IPR(2),IDAT(4)
      INTEGER NOUT,NLEFT
      INTEGER IETX(1)
      INTEGER ISOHP(2),IDC1CR(2)
      INTEGER IXPEN, IYPEN
      PARAMETER(IXPEN=2, IYPEN=3)
      DATA ISOHP/1,80/
      DATA IDC1CR/17,13/
      DATA IETX/3/
*
*  COMMENTS
*  --------
*    If KIOPB and enough space in buffer, then add bytes to buffer.
*    Otherwise, add Request Buffer Status (<DC1><CR>) and send buffer.
*    Await response from plotter and then switch it off. Then put
*    plotter on at start of next buffer and (if KIOPB) add bytes
*    to buffer.
*
* -----------------------------------------------------
*
      CALL GKIOBO(KIOQS,1,KDAT,NLEFT)
      IF(IFUNC.EQ.KIOPB.AND.NLEFT.GE.NINT+2) THEN
         CALL GKIOBO(KIOPB,NINT,INTA,NLEFT)
      ELSE
         CALL GKIOBO(KIOPB,2,IDC1CR,NLEFT)
         CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
         CALL GKIOBI(3,0,IPR,4,IDAT,NOUT)
         CALL GKIOBO(KIOPB,1,IETX,NLEFT)
         CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
         CALL GKIOBO(KIOPB,2,ISOHP,NLEFT)
         IF(IFUNC.EQ.KIOPB)CALL GKIOBO(KIOPB,NINT,INTA,NLEFT)
         KWKDAT(IXPEN,KWKIX)=KNIL
         KWKDAT(IYPEN,KWKIX)=KNIL
      ENDIF
      RETURN
      END
