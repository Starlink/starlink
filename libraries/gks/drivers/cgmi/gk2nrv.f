      SUBROUTINE GK2NRV
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*    To reset the GKS state list at each new picture.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*     None

*  COMMON BLOCKS
*  -------------
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/GKSE_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gksl.par'
      INCLUDE '../../include/gaspct.par'
      INCLUDE '../../include/gkpid.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwcb.cmn'
      INCLUDE '../../include/gksl.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkcgn.cmn'
*
*  LOCALS
*  ------
*     J       Loop counter
*
      INTEGER J

*---------------------------------------------------------------------

*   Attributes.
      DO 305 J=1,3
        KCPLAF(J)=GINDIV
        KCPMAF(J)=GINDIV
  305   KCFAAF(J)=GINDIV

      DO 315 J=1,4
  315   KCTXAF(J)=GINDIV

*  line and marker modes are scaled
      KLWSMD(KWKIX)=1
      KEWSMD(KWKIX)=1
      KMSSMD(KWKIX)=1

* Line attributes
      KCPLI=1
      KCLNTY=1
      QCLNWD=1.0
      KCPLCI=1

* Marker attributes
      KCPMI=1
      KCMKTY=3
      QCMKSZ=1.0
      KCPMCI=1

* Text attributes
      KCTXI=1
      KCTXFN=1
      KCTXPR=GSTRP
      QCCHXP=1.0
      QCCHSP=0.0
      KCTXCI=1

      DEFVDC(KWKIX)=.TRUE.
      DEFCLP(KWKIX)=.TRUE.
      DEFPAT(KWKIX)=.TRUE.
      DEFREF(KWKIX)=.TRUE.

      DEFCHH(KWKIX)=.TRUE.

      KCCHP=GRIGHT
      KCHZCH=GAHNOR
      KCVTCH=GAVNOR

* Fill area attributes
      KCFAI=1
      KCFAIS=GHOLLO
      KCFASI=1
      KCFACI=1

*  Pick id
      KCPCID=1

*  VDC Origin
      QVDCOX(KWKIX) = 0.0
      QVDCOY(KWKIX) = 0.0

*  Defaults dependent on VDC type
      IF (KVDCTY(KWKIX).EQ.0) THEN
*  Integer VDC Scaling
         QVDCSC(KWKIX) = 1.0/32767.0
      ELSE
*  Real VDC scaling
         QVDCSC(KWKIX) = 1.0
      ENDIF

*  VDC X and Y scaling
      QVDCSX(KWKIX) = QVDCSC(KWKIX)
      QVDCSY(KWKIX) = QVDCSC(KWKIX)

*   Current clipping rectangle
      QCCLXL=0.0
      QCCLXR=1.0
      QCCLYB=0.0
      QCCLYT=1.0

*  Character Height
      QCCHH=0.01
      QCCHW=QCCHH
*  Character Vectors
      QCCHUX=0.0
      QCCHUY=1.0
      QCCHBX=1.0
      QCCHBY=0.0

*  Pattern size
      QCPAWX=1.0
      QCPAWY=0.0
      QCPAHX=0.0
      QCPAHY=1.0
*  Fill reference point
      QCPAX=0.0
      QCPAY=0.0

*   Transformations and clipping
      KCNTN=0
*                              (maxm norm trans is KT)
      DO 350 J=0,KT
        KTNOVP(J)=J
        QLWXL(J)=0.0
        QLWXR(J)=1.0
        QLWYB(J)=0.0
        QLWYT(J)=1.0
        QLVPXL(J)=0.0
        QLVPXR(J)=1.0
        QLVPYB(J)=0.0
        QLVPYT(J)=1.0
  350 CONTINUE

      KCLIN=GCLIP

*   Initialise source flags for transformations and attributes
      KSTRWK = KHANGE
      KSPLWK = KHANGE
      KSPMWK = KHANGE
      KSTXWK = KHANGE
      KSFAWK = KHANGE

*   Initialise hatch & pattern indexes
      KHTCHI(KWKIX)=1
      KPATTI(KWKIX)=1
      KHTPAT(KWKIX)=1

      RETURN
      END
