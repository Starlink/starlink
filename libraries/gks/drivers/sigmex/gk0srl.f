      SUBROUTINE GK0SRL(NRD,XNDC,YNDC)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Request locator.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP NRD  Number of points = 1
*     I/O XNDC Initial locator position -> output locator position
*     I/O YNDC Initial locator position -> output locator position
*
      INTEGER NRD
      REAL XNDC(NRD), YNDC(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     INUL    Array used to pass a NUL to GKIOBO
*     BUFFER  Accumulates orders to go to GKIOCO
*     IX,IY   Point
*     NLEFT   Returned from GKIOCO, not used here
*     INTA,REALS Arrays to hold info returned by GKRQIP
*     XDC,YDC Initial locator in DC
*
      INTEGER INUL(1),INARR(11),IDUMMY(1),NOUT,ICODE
      CHARACTER*15 BUFFER
      INTEGER IX, IY, NLEFT, INTA(10), I, INN, IHIRES, IJOY, IHF(2)
      REAL XDC(1), YDC(1), REALA(10)
      PARAMETER (IHIRES=2, IJOY=6)
      DATA INUL(1)/0/
      DATA IHF/72,70/
*
*---------------------------------------------------------------------


* Get locator device information
      CALL GKILC(XNDC,YNDC,INTA,REALA,XDC,YDC)
      IF (KERROR.NE.0) GOTO 9999
* Attach graphics cursor to graphics pen and move to initial position
      CALL GKIOCO(KIOPB,'CE',NLEFT)
      CALL GK0SLN(1,XDC,YDC)
* Use of tablet and joystick are mutually exclusive, so select one
* depending on flag set at Open Workstation
      IF (KWKDAT(IJOY,KWKIX).EQ.1) THEN
* Joystick
        CALL GKIOCO(KIOPB,'CF',NLEFT)
      ELSE
* Tablet
        CALL GKIOCO(KIOPB,'GM3',NLEFT)
      ENDIF
*
* Pad with nuls to ensure that an end of frame is reached.
* Using 80 will ensure a sufficient delay for any line speed, and
* avoids needing to discover the baud rate from the system/user.
      DO 5 I=1,80
        CALL GKIOBO(KIOPB,1,INUL,NLEFT)
    5 CONTINUE

* Turn cursor on if echo on
      IF (INTA(KIPE).EQ.GECHO) CALL GKIOCO(KIOPB,'CA',NLEFT)

* If have high resolution model, need to read 10 characters rather than 8
      IF (KWKDAT(IHIRES,KWKIX).EQ.1) THEN
        INN = 10
      ELSE
        INN = 8
      ENDIF
* First, flush buffer
      CALL GKIOBO(KIOSO,0,IDUMMY,NLEFT)
* Start loop here
   10 CONTINUE
* Prompt, then read cursor position
      CALL GKIOBI(KIONP,2,IHF,INN,INARR,NOUT)
      IF (KERROR.NE.0) GOTO 9999
* Inspect input buffer INARR, number of bytes returned is NOUT
      IF (NOUT.EQ.INN) THEN
*    Key hit was not terminator
        ICODE = INARR(1)
      ELSE IF (NOUT.EQ.0) THEN
*    Key hit was control code, so read again (nopurge this time) to get rest
*    of buffer. Record the key hit as being <carriage return>
        ICODE = 13
        CALL GKIOBI(KIONN,1,INUL,INN-1,INARR(2),NOUT)
        IF (KERROR.NE.0.OR.NOUT.NE.INN-1) GOTO 9999
      ENDIF
*     Control-Z is the break character
      IF (ICODE.EQ.26) THEN
*    Break
        KWI1 = GNONE
        GOTO 9999
      ELSE
*    Valid character, decode to get coordinates and transform to NDC
        CALL GKATON(INN-1,INARR(2),BUFFER)
        IF (KWKDAT(IHIRES,KWKIX).EQ.1) THEN
          READ(BUFFER(1:INN-1),'(I4,1X,I4)') IX,IY
        ELSE
          READ(BUFFER(1:INN-1),'(I3,1X,I3)') IX,IY
        ENDIF
        XDC(1) = FLOAT(IX)
        YDC(1) = FLOAT(IY)
        CALL GKTDN(1,XDC,YDC,XNDC,YNDC)
* If point is not within workstation window, try again (didn't have a break)
        IF (KERROR.NE.0) THEN
          KERROR = 0
          GOTO 10
        ELSE
          KWI1 = GOK
          GOTO 9999
        ENDIF

      ENDIF

 9999 CONTINUE
* Turn cursor off
      CALL GKIOCO(KIOPB,'CB',NLEFT)
      CALL GKIOCO(KIOSN,' ',NLEFT)

      END
