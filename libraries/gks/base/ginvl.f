C# IL>=b, OL>=0
      SUBROUTINE GINVL(IWKID,IDNR,VINIT,IPET,XMIN,XMAX,YMIN,YMAX,
     :                    VLOW,VHIGH,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INITIALISE VALUATOR
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialise valuator device
*
*  MAINTENANCE LOG
*  ---------------
*     07/10/83  AS    Original version stabilized
*     29/03/84  JRG   Check initial value
*     21/01/87  SHS   IS conversion. Changed error numbers. Added NOTE.
*     17/05/90  RMK   Added check that echo area is inside display
*                     space (S384).
*     07/11/90  KEVP  Check for error 145 against the DISPLAY SPACE,
*                     not the workstation viewport (C59).
*     14/01/91  KEVP  Put checking of echo area into new routine GKIEA
*                     and ensured display surface is inquired properly (S414).
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Workstation identifier
*     INP  IDNR    Valuator device number
*     INP  VINIT   Initial value
*     INP  IPET    Prompt and echo type
*     INP  XMIN    Echo area  }
*     INP  XMAX    Echo area  }
*     INP  YMIN    Echo area  } - in DC
*     INP  YMAX    Echo area  }
*     INP  VLOW    Minimum value
*     INP  VHIGH   Maximum value
*     INP  LDR     Length of data record
*     INP  DATREC  Data record
*
      INTEGER IWKID, IDNR, IPET, LDR
      REAL VINIT, XMIN, XMAX, YMIN, YMAX, VLOW, VHIGH
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
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
*     38  Workstation not of category OUTIN or INPUT
*     51  Rectangle definition is invalid
*    145  Echo area is outside display space
*
*  NOTE
*  ----
*     The declaration of DATREC is different to that in the language
*     binding because it allows the size of DATREC to be checked.
*
*---------------------------------------------------------------------



      CALL GKPRLG(EINVL,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

* Check array size valid
        IF (LDR.GT.0) THEN

* Check length of string data record is 80
          LSTR = LEN(DATREC(1))
          IF (LSTR.EQ.80) THEN

* Check echo area.
            CALL GKIEA (IWKID,XMIN,XMAX,YMIN,YMAX,IER)
            IF(IER .EQ. 0)THEN

* Check that initial value is within range
              IF( VLOW.LE.VINIT .AND. VINIT.LE.VHIGH ) THEN
                KWI1 = IDNR
                KWI2 = IPET
                QWR1 = XMIN
                QWR2 = XMAX
                QWR3 = YMIN
                QWR4 = YMAX
                QWR5 = VINIT
                QWR6 = VLOW
                QWR7 = VHIGH
                CALL GKSONW(IWKID,KINVL,1,KDAT,1,QDAT,QDAT,LDR,
     :                         DATREC)
                IF (KERROR.NE.0) CALL GKERR(KERROR)
              ELSE
                CALL GKERR(152)
              ENDIF
            ELSE
              CALL GKERR(IER)
            ENDIF
          ELSE
            CALL GKERR(2003)
          ENDIF
        ELSE
          CALL GKERR(2003)
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF

   99 CONTINUE

      END
