C# IL>=b, OL>=0
      SUBROUTINE GINCH(IWKID,IDNR,ISTAT,ICHNR,IPET,XMIN,XMAX,YMIN,YMAX,
     :                    LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INITIALISE CHOICE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialise choice device
*
*  MAINTENANCE LOG
*  ---------------
*     07/10/83  AS    Original version stabilized
*     21/01/87  RMK   IS conversion. Added ISTAT to argument list, and
*                     passed this down to driver. Changed error numbers.
*     17/05/90  RMK   Added checks that echo area is inside display
*                     space and that initial choice number is
*                     valid (S384).
*     07/11/90  KEVP  Check for error 145 against the DISPLAY SPACE,
*                     not the workstation viewport (C59).
*     14/01/91  KEVP  Put checking of echo area into new routine GKIEA
*                     and ensured display surface is inquired properly (S414).
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Workstation identifier
*     INP  IDNR    Choice device number
*     INP  ISTAT   Initial status
*     INP  ICHNR   Initial choice number
*     INP  IPET    Prompt and echo type
*     INP  XMIN    Echo area  }
*     INP  XMAX    Echo area  }
*     INP  YMIN    Echo area  } in DC
*     INP  YMAX    Echo area  }
*     INP  LDR     Length of data record
*     INP  DATREC  Data record
*
      INTEGER IWKID, IDNR, ISTAT, ICHNR, IPET, LDR
      REAL XMIN, XMAX, YMIN, YMAX
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
*     38  Workstation category not OUTIN or INPUT
*     51  Rectangle definition is invalid
*    145  Echo area is outside display space
*
*---------------------------------------------------------------------



      CALL GKPRLG(EINCH,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

* Check array size valid
        IF (LDR.GT.0) THEN

* Check length of string data record is 80
          LSTR = LEN(DATREC(1))
          IF (LSTR.EQ.80) THEN

* Check echo area.
            CALL GKIEA (IWKID,XMIN,XMAX,YMIN,YMAX,IER)
            IF(IER .EQ. 0)THEN

* Check initial choice is valid
              IF (ICHNR.LT.1) THEN
                CALL GKERR(152)
                GOTO 99
              ENDIF

              KWI1 = IDNR
              KWI2 = IPET
              KWI3 = ICHNR
              KWI4 = ISTAT
              QWR1 = XMIN
              QWR2 = XMAX
              QWR3 = YMIN
              QWR4 = YMAX
              CALL GKSONW(IWKID,KINCH,1,KDAT,1,QDAT,QDAT,LDR,DATREC)
              IF (KERROR.NE.0) CALL GKERR(KERROR)

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
