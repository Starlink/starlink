C# IL>=a, OL>=0
      SUBROUTINE GKPWRQ(XWKLOC,XWKTOL,XWKHFD)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Emulate request pick on incapable workstation
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilised
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP XWKLOC  - Routine to obtain locator position and trigger
*     INP XWKTOL  - Routine to obtain hit tolerances for primitives
*     INP XWKHFD  - Routine to obtain hardware text details
*
      EXTERNAL XWKLOC,XWKTOL,XWKHFD
*
*  COMMON BLOCK USAGE
*  ------------------
*
*     /WCA/  Read and modified as indicated
* --> Data expected (1st entry)
*     KWI1   : device number
*     KWI2   : KNIL indicating 1st entry
*
* --> Data expected (echo/unecho actioned)
*     KWI1   : device number
*     KWI2   : actioned request (ECHO,UNECHO)
*
* --> Data expected (scan actioned)
*     KWI1   : device number
*     KWI2   : actioned request (SCAN)
*     KWI3   : segment name
*     KWI4   : pickable (GUNDET,GDETEC)
*     QWR11..16 : segment transformation
*
* <-- Data returned (pick complete)
*     KWI1   : KNIL indicating pick completed
*     KWI2   : segment name
*     KWI3   : pick identifier
*     KWI4   : status
*
* <-- Data returned (echo/unecho request)
*     KWI1   : echoplay request (ECHO,UNECHO)
*     KWI2   : segment name
*     KWI3   : pick identifier
*     KWI4   : primitive identifier
*     KWI5   : echo specification (SEGMENT,PICKID,PRIMITIVE)
*     KWI6   : primitive type (KPL,KPM,KTX,KFA,KCA,KGDP)
*
* <-- Data returned (scan request)
*     KWI1   : echoplay request (SCAN)
*     KWI2   : segment name
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkpca.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwpc.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkcon.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkxfd.cmn'
*
*  LOCALS
*  ------
*     I       Loop count
*     IDNR    Device number
*     IACT    Actioned request
*     IREQ    Echoplay request
*     ICSG    Name of current segment
*     ISG     Segment name
*     DIST    Hit distance
*     PICKID  Pick identifier
*     PRIMID  Primitive identifier
*     PTYPE   Primitive type
*     ISTATE  Device state integers
*     RSTATE  Device state reals
*     STRAN   Segment transformation
*

      INTEGER I,IDNR,IACT,IREQ,ICSG,ISG
      INTEGER PICKID,PRIMID,PTYPE
      INTEGER ISTATE(KPCNWI)
      REAL    DIST,STRAN(6)
      REAL    RSTATE(KPCNWR)
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------

* Store device number and actioned request
      IDNR=KWI1
      IACT=KWI2

* Deduce state of echoplay from actioned request
      IF(IACT.EQ.KNIL) THEN
*       first time in select device and read initial state
        CALL GKPWSL(IDNR,ISTATE,RSTATE)
        IF(KERROR.NE.0) GOTO 999
      ELSE
*       not first time - read stored state
        CALL GKPWGS(IDNR,ISTATE,RSTATE)
      ENDIF

* Do next pass unless a scan is in progress
      IF(IACT.NE.KPSCAN) GOTO 100

* Scan segment if segment is pickable
      ICSG=KWI3
      IF(KWI4.EQ.GDETEC) THEN
        DO 10 I=1,6
          STRAN(I)=QWRA(10+I)
 10     CONTINUE
        CALL GKPWCN(RSTATE(KPWDCX),RSTATE(KPWDCY),STRAN,
     :                 XWKTOL,XWKHFD,DIST,RSTATE(KPWBOX),
     :                 PICKID,PRIMID,PTYPE)
        IF(DIST.LT.1.0 .AND. DIST.LT.RSTATE(KPWD)) THEN
*         exact hit or closer hit than previous - update state
          RSTATE(KPWD)=DIST
          ISTATE(KPWSTA)=GOK
          ISTATE(KPWSG)=ICSG
          ISTATE(KPWID)=PICKID
          ISTATE(KPWPI)=PRIMID
          ISTATE(KPWPT)=PTYPE

        ENDIF
        IF(DIST.GT.0.0) THEN
*         miss or close - store bounding box obtained during scan
          CALL GKPWSB(RSTATE(KPWBOX))
        ELSE
*         exact hit - start next pass
          GOTO 100
        ENDIF
      ENDIF

* Obtain name of next candidate segment
      CALL GKPWSS(RSTATE(KPWDCX),RSTATE(KPWDCY),ICSG,ISG)
      IF(ISG.NE.KNIL) THEN
        KWI1=KPSCAN
        KWI2=ISG
*       return to workstation
        GOTO 888
      ENDIF


* Main control loop - start next pass
 100  CONTINUE
      IF(KERROR.EQ.0) THEN
        CALL GKPWRD(XWKLOC,IACT,ISTATE,RSTATE,IREQ)
        IF(IREQ.EQ.KNIL) THEN
*         pick completed
          KWI1=KNIL
          KWI2=ISTATE(KPWSG)
          KWI3=ISTATE(KPWID)
          KWI4=ISTATE(KPWSTA)
          CALL GKPWRL(IDNR)
          GOTO 999
        ELSEIF(IREQ.EQ.KPSCAN) THEN
          ISTATE(KPWSTA)=GNPICK
*         obtain name of first potential candidate segment
          CALL GKPWSS(RSTATE(KPWDCX),RSTATE(KPWDCY),KNIL,ISG)
          IF(ISG.NE.KNIL) THEN
            RSTATE(KPWD)=1.0
            KWI1=KPSCAN
            KWI2=ISG
          ELSE
*           start next pass because no candidate found
            IACT=IREQ
            GOTO 100
          ENDIF
        ELSEIF(IREQ.EQ.KPECHO .OR. IREQ.EQ.KPUNEC) THEN
          IF(ISTATE(KPWESW).EQ.GNECHO) THEN
*           start next pass because echoing is off
            IACT=IREQ
            GOTO 100
          ENDIF
*         echoplay parameters
          KWI1=IREQ
          KWI2=ISTATE(KPWSG)
          KWI3=ISTATE(KPWID)
          KWI4=ISTATE(KPWPI)
          KWI5=ISTATE(KPWPET)
          KWI6=ISTATE(KPWPT)
        ENDIF
      ENDIF

*
 888  CONTINUE
*     preserve state until return
      CALL GKPWPS(IDNR,ISTATE,RSTATE)

*
 999  CONTINUE

      END
