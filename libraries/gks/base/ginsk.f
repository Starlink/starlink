C# IL>=b, OL>=0
      SUBROUTINE GINSK(IWKID,IDNR,ITNR,N,PX,PY,IPET,XMIN,XMAX,
     :                    YMIN,YMAX,IBFLEN,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INITIALISE STROKE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialise stroke device
*
*  MAINTENANCE LOG
*  ---------------
*     07/10/83  AS    Original version stabilized
*     01/02/84  JL    Checking of initial stroke changed
*                     Error number from invalid initial stroke
*                     changed to 152
*                     Error number from invalid length of
*                     data record changed to 903
*                     (Fix for bug number I83)
*     23/02/84  JL    Check NTN - report 50 if not OK  (I140)
*     21/01/87  SHS   IS conversion. Changed error checking. Added NOTE.
*     22/01/87  JCS   IS conversion. Error number changes.
*     17/05/90  RMK   Added check that echo area is inside display
*                     space (S384). Removed unused local variable.
*     05/07/90  KEVP  Check NTN - report 152 if not OK.
*                     NB: Error 50 not in GKS standard.
*                     Allow stroke points to occur outside windows and
*                     viewports as these can change (S369).
*     22/07/90  PLP   Merged KEVP's and RMK's versions of this routine
*                     and removed unused locals I, XNDC and YNDC.
*     07/11/90  KEVP  Check for error 145 against the DISPLAY SPACE,
*                     not the workstation viewport (C59).
*     14/01/91  KEVP  Put checking of echo area into new routine GKIEA
*                     and ensured display surface is inquired properly (S414).
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Workstation identifier
*     INP  IDNR    Stroke device number
*     INP  ITNR    Initial normalization transformation number
*     INP  N       Number of points in initial stroke
*     INP  PX,PY   Initial stroke
*     INP  IPET    Prompt and echo type
*     INP  XMIN    Echo area  }
*     INP  XMAX    Echo area  } - in DC
*     INP  YMIN    Echo area  }
*     INP  YMAX    Echo area  }
*     INP  IBFLEN  Buffer length for stroke
*     INP  LDR     Length of data record
*     INP  DATREC  Data record
*
      INTEGER IWKID, IDNR, ITNR, N, IPET, IBFLEN, LDR
      REAL PX(*), PY(*), XMIN, XMAX, YMIN, YMAX
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
*     Length of string and error indicator
      INTEGER LSTR, IER
*
*  ERRORS
*  ------
*     152  Initial value is invalid
*     153  Number of points > buffer size
*    2003  Invalid data record
*
*     Dectected by GKIEA:
*      38  Workstation not of category OUTIN or INPUT
*      51  Rectangle definition is invalid
*     145  Echo area is outside display space
*
*  NOTE
*  ----
*     The declaration of DATREC is different to that in the language
*     binding because it allows the size of DATREC to be checked.
*
*---------------------------------------------------------------------



      CALL GKPRLG (EINSK,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

* Check initial stroke size valid
        IF (N.LE.0) THEN
          CALL GKERR(152)
          GOTO 99
        ELSE IF (N.GT.IBFLEN) THEN
          CALL GKERR(153)
          GOTO 99
        ENDIF

        IF (LDR.GT.0) THEN

* Check length of strings of data record is 80
          LSTR = LEN(DATREC(1))
          IF (LSTR.EQ.80) THEN

* Check echo area.
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
              KWI4 = IBFLEN
              QWR1 = XMIN
              QWR2 = XMAX
              QWR3 = YMIN
              QWR4 = YMAX
              CALL GKSONW(IWKID,KINSK,1,KDAT,N,PX,PY,LDR,DATREC)
              IF (KERROR.NE.0) CALL GKERR(KERROR)
            ELSE
* Echo area in error
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
