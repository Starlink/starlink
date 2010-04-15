C# IL>=b, OL>=0
      SUBROUTINE GKIEA(IWKID,XMIN,XMAX,YMIN,YMAX,IER)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine: Front-End
*  Author:          KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Check echo area for input device
*
*  MAINTENANCE LOG
*  ---------------
*     14/01/91  KEVP  Created to fix bug S414.
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Workstation identifier
*     INP  XMIN    Echo area  }
*     INP  XMAX    Echo area  }
*     INP  YMIN    Echo area  } - in DC
*     INP  YMAX    Echo area  }
*     OUT  IER     Error Indicator
*
      INTEGER IWKID, IER
      REAL XMIN, XMAX, YMIN, YMAX
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
*
*  ERRORS
*  ------
*     38  Workstation not of category OUTIN or INPUT
*     51  Rectangle definition is invalid
*    145  Echo area is outside display space
*
*---------------------------------------------------------------------

* Check echo area is valid
      IF (XMIN.LT.XMAX .AND. YMIN.LT.YMAX) THEN

* Check echo area is inside display space. First check left and base.
           IF(XMIN .LT. 0.0 .OR. YMIN .LT. 0.0)THEN
              IER = 145
           ELSE
*          Need to inquire display surface size for right and top.
*             First need to get workstion type
              CALL GKSONW(IWKID,KQWKC,1,KDAT,1,QDAT,QDAT,1,CH)
              IF(KERROR .NE. 0)THEN
                IER = KERROR
              ELSE
*             Workstation type available as KWI2 - Inquire its display size
                CALL GKSONW(KWI2,KQMDS,1,KDAT,1,QDAT,QDAT,1,CH)
                IF(KERROR .NE. 0)THEN
*                 Error 31,33 or 36 has occurred, implying error 38.
                  IER = 38
                ELSEIF(XMAX .GT. QWR1  .OR. YMAX .GT. QWR2)THEN
                  IER = 145
                ELSE
*                 No error - Echo area OK
                  IER = 0
                ENDIF
              ENDIF
           ENDIF
      ELSE
* Invalid Echo area
              IER = 51
      ENDIF
      END
