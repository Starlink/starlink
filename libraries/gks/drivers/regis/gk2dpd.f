      SUBROUTINE GK2DPD (REALA, ST)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) workstation driver
*  Author:             JRG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To display a prompt ST in echo area (specified in REALA).
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP  REALA   Contains real part of device state .... used for echo
*                  (that part that is independent of device class is
*                  declared here)
*     INP  ST      String to be used for prompt (trailing spaces removed
*
      REAL REALA(4)
      CHARACTER*(*) ST
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  Offsets in KWKDAT workspace
*
      INTEGER MODE, CHARHI,CHARWI,ICX,ICY, ICPEN
      PARAMETER (MODE = 1, CHARHI = 2, CHARWI = 3, ICX = 4, ICY = 5,
     :           ICPEN = 6)
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*
*     LOCALS
      INTEGER NSIGS,ITOP,ILEFT
*
*
*  ALGORITHM
*  ---------
*     Provides both a prompt and echo if required
*
*
*   Find last significant character (position will be NSIGS) in ST
      DO 100 NSIGS=LEN(ST),1,-1
        IF( ST(NSIGS:NSIGS).NE.' ' ) GOTO 105
  100 CONTINUE

*   Here, all characters are spaces
      NSIGS=0

*   Here, NSIGS has been set
  105 CONTINUE

*
*  Simulate a text window by setting the text cursor to ILEFT,ITOP
*  and setting the terminal into NOWRAP mode
*
      ILEFT =INT(REALA(KIPEXL)/KWKDAT(CHARWI,KWKIX))
      ITOP  =INT(REALA(KIPEYB)/KWKDAT(CHARHI,KWKIX))
      WRITE(*,200)CHAR(27),ITOP,ILEFT,CHAR(27)
  200 FORMAT(1H+,A,'[',I2.2,';',I3.3,'H',A,'[?7l',$)
*
*   Output prompt if required
*
      IF (NSIGS.GT.0) WRITE(*,'(1H+,A$)') ST(1:NSIGS)
*
      RETURN
      END
