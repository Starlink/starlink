      SUBROUTINE GK0SRK(NRD,XNDC,YNDC)

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
*     Request stroke.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP NRD    Length of arrays XNDC,YDC
*     OUT XNDC   Coordinates of points
*     OUT YNDC   Coordinates of points
*
      INTEGER NRD
      REAL XNDC(NRD), YNDC(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
      CHARACTER*1 GKAN1
*
*  LOCALS
*  ------
*
      CHARACTER*15 BUFFER
      INTEGER I, N, IC, IX, IY, NLEFT, INTA(10), INN, IHIRES, IJOY
      INTEGER INUL(1), NIPTS2, IOFFX, IOFFY
      REAL XDC(2), YDC(2), REALA(10)
      PARAMETER (IHIRES=2, IJOY=6)
      DATA INUL(1)/0/
*
*---------------------------------------------------------------------


* Limit stroke to 100 points
      IF (NRD.GT.100) NRD = 100

* First, get stroke device information
      CALL GKISK(NRD, KNRR, XNDC, YNDC, INTA, REALA,
     :           NIPTS2, IOFFX, IOFFY)
      IF (KERROR.EQ.0) THEN
* Draw initial stroke if there are any points and echo is set
        IF (NIPTS2.GE.2 .AND. INTA(KIPE).EQ.GECHO)
     :  CALL GK0SLN(NIPTS2, QSTACK(IOFFX), QSTACK(IOFFY))
* Save first point. After that we have no further need of the stack.
        IF (NIPTS2.GE.1) THEN
          XDC(1) = QSTACK(IOFFX+NIPTS2-1)
          YDC(1) = QSTACK(IOFFY+NIPTS2-1)
        ENDIF
        CALL GKSTDA(KREALS, IOFFX)
* If no points in initial stroke, set value up for later
        IF (KNRR.EQ.0) KNRR = 1

* Attach graphics cursor to graphics pen
        CALL GKIOCO(KIOPB,'CE',NLEFT)
* Use of tablet and joystick are mutually exclusive, so select one depending
* on the flag set in Open Workstation
        IF (KWKDAT(IJOY,KWKIX).EQ.1) THEN
* Joystick
          CALL GKIOCO(KIOPB,'CF',NLEFT)
        ELSE
* Tablet
          CALL GKIOCO(KIOPB,'GM3',NLEFT)
        ENDIF

* Pad with nuls to ensure that an end of frame is raeched.
* Using 80 will ensure a sufficient delay for any line speed, and
* avoids needing to discover the baud rate from the system/user.
        DO 7 I=1,80
          CALL GKIOBO(KIOPB,1,INUL,NLEFT)
    7   CONTINUE

* Turn cursor on if echo on
        IF (INTA(KIPE).EQ.GECHO) CALL GKIOCO(KIOPB,'CA',NLEFT)

* If have high resolution model, need to read 10 characters rather than 8
        IF (KWKDAT(IHIRES,KWKIX).EQ.1) THEN
          INN = 10
        ELSE
          INN = 8
        ENDIF
* Get stroke
        DO 10 I=KNRR,NRD
    9     CONTINUE
          CALL GKIOCO(KIOSN,' ',NLEFT)

* Prompt, then read cursor position
          CALL GKIOCI(KIOEP,6,'+-*/HF',BUFFER(1:INN),N)
* If length is zero, ie carriage return on some Sigmas, read typahead
* buffer.
          IF (N.EQ.0) THEN
            CALL GKIOCI(KIONN,0,' ',BUFFER(1:INN),N)
            GOTO 20
          ENDIF
* Check for BREAK CTRL/Z
          IF (INDEX(BUFFER(1:N),GKAN1(26)) .NE. 0) GOTO 30
* Check for trigger
          IF (N.LE.INN-1) GOTO 20
* Decode to get coordinates
          IC = INDEX(BUFFER(1:N),',')
          IF (IC.EQ.0) GOTO 10
          IF (KWKDAT(IHIRES,KWKIX).EQ.1) THEN
            READ(BUFFER(IC-4:IC+4),'(I4,1X,I4)') IX,IY
          ELSE
            READ(BUFFER(IC-3:IC+3),'(I3,1X,I3)')IX,IY
          ENDIF
          XDC(2) = FLOAT(IX)
          YDC(2) = FLOAT(IY)
          CALL GKTDN(1,XDC(2),YDC(2),XNDC(I),YNDC(I))
          IF (KERROR.NE.0) THEN
            KERROR = 0
            GOTO 9
          ENDIF
* Echo, ie draw line from previous point
          IF (I.GT.1) CALL GK0SLN(2,XDC,YDC)
          XDC(1) = XDC(2)
          YDC(1) = YDC(2)
   10   CONTINUE
        KNRR = I+1

   20   CONTINUE
        KWI1 = GOK
        KNRR = I-1
        GOTO 40
* Handle break
   30   KWI1=GNONE
   40   CONTINUE
* Turn cursor off
        CALL GKIOCO(KIOPB,'CB',NLEFT)
        CALL GKIOCO(KIOSN,' ',NLEFT)
      ENDIF

      END





