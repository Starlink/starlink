C# IL>=a, OL>=1
      SUBROUTINE GKSOPS(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONTEND - W/S
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Workstation interface routine called by GKS front end to send
*     control to all open workstations for operations which may
*     need to use CSS replay.
*
*  MAINTENANCE LOG
*  ---------------
*     05/12/83  JGW   Original version stabilized
*     21/12/83  JRG   Set KRPCC & KINENT
*       3/1/84  JRG   Clear down request for playback if error in workstation
*                     & deal with IER correctly
*      20/3/84  JRG   CSS request dealt with here, not in caller (bug I163)
*     28/11/85  DSG   Fix for Bug I252 (info. from R.Koserew)
*     05/06/86  RMK   Additional code for ICL fix I252. Bug was due to KWI1
*                     being overwritten in the DO-loop. Fix introduced local
*                     ISEGNO to save and restore value of KWI1.
*
*  ARGUMENTS
*  ---------
*     INP IENT - entrypoint code
*     INP NID  - number of elements in array IDAT
*     I/O IDAT - integer data passed to or from workstation driver
*     INP NRD  - number of elements in array RX and RY
*     I/O RX   - real x-coordinates passed to or from workstation driver
*     I/O RY   - real y-coordinates passed to or from workstation driver
*     INP NCD  - length of character array
*     I/O STR  - character array
*
      INTEGER IENT,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD),RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKDT/    KLAWKT
*     Read   /GKSTK/   KIUSED
*     Read   /GKWCB/   KNACWK,KACPT,KWTYIX
*     Modify /GKWCA/   KWKIX,KWKTYP,KRPCC,KINENT
*     Modify /GKERR/   KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdt.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IWKTYP,ITEMP,I,IER,ISEGNO
      CHARACTER*9 NAME
      PARAMETER (NAME='GKSOPS')
*
*---------------------------------------------------------------------

      KRPCC=KRPNO
      KINENT=IENT
      KSGRQ=0
      IER = 0
      ISEGNO = KWI1
      DO 10 I=1,KNOPWK
         KWKIX = KOPPT(I)
         IWKTYP = KWTYIX(KWKIX)
         KWKTYP = KLAWKT(IWKTYP)
         KERROR = 0
         KWI1 = ISEGNO

*        -- send command to W/S
      INCLUDE 'cdrive.inc'

         IF( KRPCC.NE.KRPNO) THEN
           IF( KERROR.EQ.0 ) THEN
             CALL GKSGPB(KWKID(KWKIX))
           ELSE
             KRPCC=KRPNO
           ENDIF
         ENDIF
        IF (IER.EQ.0 .AND. KERROR.NE.0) IER = KERROR
   10 CONTINUE

      IF( IER.EQ.0 .AND. KSGRQ.GT.0 ) CALL GKCSWD(KINENT,
     :                       1,KDAT, 1,QDAT,QDAT, 1,CH)
      IF (IER.NE.0) KERROR = IER

      END
