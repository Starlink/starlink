C# IL>=b, OL>=0
      SUBROUTINE GINLC(IWKID,IDNR,ITNR,PX,PY,IPET,XMIN,XMAX,
     :                    YMIN,YMAX,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INITIALISE LOCATOR
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialise locator device
*
*  MAINTENANCE LOG
*  ---------------
*     07/10/83  AS    Original version stabilized
*     01/12/83  AS    Report error - initial position invalid
*     23/02/84  JL    Check NTN - report 50 if not OK  (I140)
*     21/01/87  SHS   IS conversion. Changed error numbers. Added NOTE.
*     17/04/89  KEVP  Allowed ITNR to differ from top priority
*                     normalization transformation (superseded).
*     14/05/90  RMK   Added check that echo area is inside display
*                     space (S384).  Removed unused local variables.
*     05/07/90  KEVP  Check NTN - report 152 if and only if not OK.
*                     NB: Error 50 not in GKS standard.
*                     Allow point to occur outside windows and
*                     viewports as these can change (S369).
*     09/07/90  KEVP  Restored initial locator (lost in above change)
*     22/07/90  PLP   Merged KEVP's and RMK's versions of this routine
*                     and removed unused locals XNDC and YNDC.
*     07/11/90  KEVP  Check for error 145 against the DISPLAY SPACE,
*                     not the workstation viewport (C59).
*     14/01/91  KEVP  Put checking of echo area into new routine GKIEA
*                     and ensured display surface is inquired properly (S414).
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Workstation identifier
*     INP  IDNR    Locator device number
*     INP  ITNR    Initial normalization transformation number
*     INP  PX,PY   Initial locator position - in WC
*     INP  IPET    Prompt and echo type
*     INP  XMIN    Echo area  }
*     INP  XMAX    Echo area  }
*     INP  YMIN    Echo area  } - in DC
*     INP  YMAX    Echo area  }
*     INP  LDR     Length of data record
*     INP  DATREC  Data record
*
      INTEGER IWKID, IDNR, ITNR, IPET, LDR
      REAL PX, PY, XMIN, XMAX, YMIN, YMAX
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     String length, error indicator, locator point in WC
      INTEGER LSTR, IER
      REAL XWC(1), YWC(1)
*
*  ERRORS
*  ------
*    152  Initial value is invalid
*   2003  Invalid data record
*
*   Detected by GKIEA:
*     38  Workstation not of category OUTIN or INPUT
*     51  Rectangle definition is invalid
*    145  Echo area is outside display space
*
*  NOTE
*  ----
*  The declaration of DATREC is different to that in the language
*  binding because it allows the size of DATREC to be checked.
*
*---------------------------------------------------------------------



      CALL GKPRLG(EINLC,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

* Check array size valid
        IF (LDR.GT.0) THEN

* Check length of string data record is 80
          LSTR = LEN(DATREC(1))
          IF (LSTR.EQ.80) THEN

* Check echo area
            CALL GKIEA (IWKID,XMIN,XMAX,YMIN,YMAX,IER)
            IF(IER .EQ. 0)THEN

* Check that initial normalisation transformation number is valid
              IF ((ITNR.LT.0).OR.(ITNR.GT.KT)) THEN
                 CALL GKERR(152)
                 GOTO 99
              ENDIF

* Set workstation
              KWI1 = IDNR
              KWI2 = IPET
              KWI3 = ITNR
              QWR1 = XMIN
              QWR2 = XMAX
              QWR3 = YMIN
              QWR4 = YMAX
              XWC(1) = PX
              YWC(1) = PY
              CALL GKSONW(IWKID,KINLC,1,KDAT,1,XWC,YWC,LDR,
     :                         DATREC)
              IF (KERROR.NE.0) CALL GKERR(KERROR)
            ELSE
* Invalid Echo area
              CALL GKERR(IER)
            ENDIF
          ELSE
* Invalid Data record string length
            CALL GKERR(2003)
          ENDIF
        ELSE
* Invalid Data record length
          CALL GKERR(2003)
        ENDIF
      ELSE
* Other errors
        CALL GKERR(KERROR)
      ENDIF

   99 CONTINUE
      END
