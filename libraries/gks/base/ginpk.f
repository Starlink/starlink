C# IL>=b, OL>=1
      SUBROUTINE GINPK(IWKID,IDNR,ISTAT,ISGNA,IPKID,IPET,
     :                    XMIN,XMAX,YMIN,YMAX,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INITIALISE PICK
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialise pick device
*
*  MAINTENANCE LOG
*  ---------------
*     07/10/83  AS    Original version stabilized
*     21/01/87  SHS   IS conversion. Changed error numbers. Added NOTE.
*     09/02/90  RMK   Added checking of echo area, prompt/echo type,
*                     pick identifier and segment name.
*     16/05/90  RMK   Removed prompt/echo type check as this is now
*                     done by the drivers.
*     07/11/90  KEVP  Check for error 145 against the DISPLAY SPACE,
*                     not the workstation viewport (C59).
*     14/01/91  KEVP  Put checking of echo area into new routine GKIEA
*                     and ensured display surface is inquired properly (S414).
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Workstation identifier
*     INP  IDNR    Pick device number
*     INP  ISTAT   Initial status
*     INP  ISGNA   Initial segment name
*     INP  IPKID   Initial pick identifier
*     INP  IPET    Prompt and echo type
*     INP  XMIN    Echo area  }
*     INP  XMAX    Echo area  } in DC
*     INP  YMIN    Echo area  }
*     INP  YMAX    Echo area  }
*     INP  LDR     Length of data record
*     INP  DATREC  Data record
*
      INTEGER IWKID, IDNR, ISTAT, ISGNA, IPKID, IPET, LDR
      REAL XMIN, XMAX, YMIN, YMAX
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdt.cmn'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwdt.cmn'
*
*  LOCALS
*  ------
*     Length of string and error indicator
      INTEGER LSTR, IER
*
*  ERRORS
*  ------
*    152  Initial value is invalid
*   2003  Invalid data record
*
*   Detected by GKIEA:
*     38  Workstation categgory not OUTIN or INPUT
*     51  Rectangle definition is invalid
*    145  Echo area is outside display space
*
*  NOTE
*  ----
*     The declaration of DATREC is different to that in the language
*     binding because it allows the size of DATREC to be checked.
*
*---------------------------------------------------------------------



      CALL GKPRLG(EINPK,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

* Check array size valid
        IF (LDR.LE.0) THEN
          CALL GKERR(2003)
          GOTO 99
        ENDIF

* Check length of string data record is 80
        LSTR = LEN(DATREC(1))
        IF (LSTR.NE.80) THEN
          CALL GKERR(2003)
          GOTO 99
        ENDIF

* Check echo area
        CALL GKIEA (IWKID,XMIN,XMAX,YMIN,YMAX,IER)
        IF(IER .NE. 0)THEN
          CALL GKERR(IER)
          GOTO 99
        ENDIF

* Check initial values of pick id and segment name
* (pick ids must be >= 0, and segment names > 0 )
        IF (IPKID.LT.0 .OR. ISGNA.LE.0) THEN
          CALL GKERR(152)
          GOTO 99
        ENDIF

* N.B. The workstation initialise input utility GKINIP checks that the
* input device number is supported on the workstation (and generates
* error 140 if it isn't).
* The driver initialise pick entry checks that the PET number is
* supported on the workstation  (and generates error 144 if it isn't)

* Values OK, now pass down to workstation
        KWI1 = IDNR
        KWI2 = IPET
        KWI3 = ISTAT
        KWI4 = ISGNA
        KWI5 = IPKID
        QWR1 = XMIN
        QWR2 = XMAX
        QWR3 = YMIN
        QWR4 = YMAX
        CALL GKSONW(IWKID,KINPK,1,KDAT,1,QDAT,QDAT,LDR,DATREC)
        IF (KERROR.NE.0) CALL GKERR(KERROR)

      ELSE
        CALL GKERR(KERROR)
      ENDIF
   99 CONTINUE
      END
