
C# IL>=b, OL>=0
      SUBROUTINE GINST(IWKID,IDNR,INLSTR,INSTR,IPET,XMIN,XMAX,YMIN,YMAX,
     :                    IBFLEN,INIPOS,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INITIALISE STRING
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialise string device
*
*  MAINTENANCE LOG
*  ---------------
*     17/10/83  AS    Original version stabilized
*     21/12/83  AS    Change KINCH to KINST
*     01/02/84  JL    Check length of initial string against buffer length
*                     Set error=152 if initial editing position incorrect
*                     Change error 901 to 903 for invalid data record
*                     (Fix Bug I84)
*     24/02/83  NB    Restructured (now calls GKERR if W/S set KERROR)
*     26/06/84  JRG   Remove multiple declaration of ISTAT (fix bug S63)
*     21/01/87  SHS   IS conversion. Added INLSTR to argument list.
*                     Changed error checking. Added NOTE below.
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
*     INP  IDNR    String device number
*     INP  INLSTR  Length of the initial string (.GE.0). The number of
*                  chars actually used is the min of INLSTR and the
*                  length of INSTR.
*     INP  INSTR   Initial string
*     INP  IPET    Prompt and echo type
*     INP  XMIN    Echo area  }
*     INP  XMAX    Echo area  }
*     INP  YMIN    Echo area  } - in DC
*     INP  YMAX    Echo area  }
*     INP  IBFLEN  Buffer length of string
*     INP  INIPOS  Initial cursor position
*     INP  LDR     Length of data record
*     INP  DATREC  Data record
*
      INTEGER IWKID, IDNR, INLSTR, IPET, IBFLEN, INIPOS, LDR
      REAL XMIN, XMAX, YMIN, YMAX
      CHARACTER*(*) INSTR
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
*
*  LOCALS
*  ------
*
       INTEGER LSTR, IOFF, ISTAT
*
*  ERRORS
*  ------
*    152  Initial value is invalid
*    154  Length of initial string greater than the buffer size
*   2003  Invalid data record
*
*   Detected by GKIEA:
*     38  Workstation category not OUTIN or INPUT
*     51  Rectangle definition is invalid
*    145  Echo area is outside display space
*
*  NOTE
*  ----
*     The declaration of DATREC is different to that in the language
*     binding because it allows the size of DATREC to be checked.
*
*---------------------------------------------------------------------



      IOFF = KNIL
      CALL GKPRLG(EINST,GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 99

* Check array size valid
      ISTAT = 2003
      IF (LDR.LE.0) GOTO 88

* Check length of string data record is 80
      LSTR = LEN(DATREC(1))
      IF (LSTR.NE.80) GOTO 88

* Check echo area
      CALL GKIEA (IWKID,XMIN,XMAX,YMIN,YMAX,ISTAT)
      IF(ISTAT .NE. 0) GOTO 88

* Check Initial string (plus one char) will fit into buffer
      ISTAT = 154
      LSTR = MIN(INLSTR,LEN(INSTR))
      IF (IBFLEN.LT.LSTR+1) GOTO 88

* Check initial cursor position is within
*  (or just beyond) initial string
      ISTAT = 152
      IF ((INIPOS.LT.1) .OR. (INIPOS.GT.LSTR+1)) GOTO 88


* Convert initial string to integer array
      CALL GKSTAL(KINTGS,LSTR,IOFF)
      IF (KERROR.NE.0) GOTO 99

      CALL GKNTOA(LSTR,INSTR,KSTACK(IOFF))
      IF (KERROR.NE.0) GOTO 99

      KWI1 = IDNR
      KWI2 = IPET
      KWI3 = IBFLEN
      KWI4 = INIPOS
      QWR1 = XMIN
      QWR2 = XMAX
      QWR3 = YMIN
      QWR4 = YMAX
      CALL GKSONW(IWKID,KINST,LSTR,KSTACK(IOFF),1,QDAT,
     :               QDAT,LDR,DATREC)
      IF (KERROR.NE.0) GOTO 99
      GOTO 100

   88 CONTINUE
      KERROR=ISTAT
   99 CONTINUE
      CALL GKERR(KERROR)
  100 CONTINUE
      CALL GKSTDA(KINTGS,IOFF)
      END
