C# IL>=b, OL>=0
      SUBROUTINE GKISK(NRD,NIPTS1, XNDC,YNDC,
     :   INTA,REALA, NIPTS2,IOFFX,IOFFY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation utility
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To obtain the state of the specified stroke device
*
*  MAINTENANCE LOG
*  ---------------
*     29/06/84  JRG   First version
*     20/08/85  RMK   XNDC,YNDC changed from integer to real (S149 - Salford).
*
*  ARGUMENTS
*  ---------
*     INP  NRD     Length of XNDC and YNDC arrays
*     INP  NIPTS1  Number of points in XNDC and YNDC arrays on
*                  entry (0=<NIPTS1=<NRD)
*     INP  XNDC,YNDC  Arrays holding initial stroke data in NDC
*     OUT  INTA    Array to hold INTEGER part of device state (length 10)
*     OUT  REALA   Array to hold REAL part of device state (length 4)
*     OUT  NIPTS2  Number of points in possible altered initial data
*     OUT IOFFX,IOFFY Stack offsets marking space to hold initial
*                  stroke in DC. IOFFX must be deallocated by caller (IOFFY
*                  will then disappear automatically).
*
      INTEGER NRD,NIPTS1
      INTEGER INTA(10)
      INTEGER NIPTS2, IOFFX,IOFFY
      REAL REALA(4),XNDC(NRD),YNDC(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    For KWKIX to identify workstation and device number
*     Read   /WSL/    For workstation window
*     Modify /STK/    Use stack in subsidiary routine
*     Read   /ERR/    KERROR (in fact modified by subsidiary routine)
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     I       Loop counter
*
      INTEGER I
*
*  STACK USAGE
*  -----------
*     NIPTS1*2 REAL     coordinates of initial stroke in DC
*
*  ERRORS
*  ------
*     Error in retrieving data
*     Too little space to put data
*
*-----------------------------------------------------------------------


* Get stroke device information
      CALL GKRQIP(GSTROK,KWI1,10,4,INTA,REALA)
      IF( KERROR.NE.0 ) GOTO 999

* Get space for initial stroke in DC
      CALL GKSTAL(KREALS,NIPTS1*2,IOFFX)
      IF( KERROR.NE.0 ) GOTO 999
      IOFFY=IOFFX+NIPTS1

* Check initial stroke positions in NDC are within the workstation window
      NIPTS2=NIPTS1
      DO 5 I=1,NIPTS1
        IF(XNDC(I).LT.QCWWXL(KWKIX) .OR. XNDC(I).GT.QCWWXR(KWKIX) .OR.
     :     YNDC(I).LT.QCWWYB(KWKIX) .OR. YNDC(I).GT.QCWWYT(KWKIX))THEN
          NIPTS2=0
          GOTO 999
        ENDIF
    5 CONTINUE

* Transform initial stroke NDC to DC
      IF( NIPTS2.GE.1 ) CALL GKTND(NIPTS2,XNDC,YNDC,
     :        QSTACK(IOFFX),QSTACK(IOFFY))

  999 CONTINUE
      END
