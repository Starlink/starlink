C# IL>=a, OL>=1
      SUBROUTINE GKSONS(IWK,IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
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
*     control to one specific workstation,identified by workstation
*     identifier. This routine is only used by for those operations
*     which may require CSS replay
*
*  MAINTENANCE LOG
*  ---------------
*     05/12/83  JGW   Original version stabilized
*     21/12/83  JRG   Set KRPCC & KINENT
*       3/1/84  JRG   Clear down request for playback if error in workstation
*      20/3/84  JRG   CSS request here (consistent with GKSOPS .. bug I163)
*
*  ARGUMENTS
*  ---------
*     INP IWK  - identifier or type - depends on entrypoint code
*     INP IENT - entrypoint code
*     INP NID  - number of elements in array IDAT
*     I/O IDAT - integer data passed to or from workstation driver
*     INP NRD  - number of elements in array RX and RY
*     I/O RX   - real x-coordinates passed to or from workstation driver
*     I/O RY   - real y-coordinates passed to or from workstation driver
*     INP NCD  - length of character array
*     I/O STR  - character array
*
      INTEGER IWK,IENT,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD),RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKDT /    KLAWKT
*     Read   /GKWCB/    KWKID,KWTYIX
*     Read   /GKSTK/    KIUSED
*     Modify /GKWCA/    KWKTYP,KWKIX,KRPCC,KINENT
*     Modify /GKERR/    KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdt.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IWKTYP,ITEMP,I
      CHARACTER*9 NAME
      PARAMETER (NAME='GKSONS')
*
*  ERRORS
*  ------
*     20   Specified workstation identifier is invalid
*     25   Specified workstation is not open
*
*---------------------------------------------------------------------

* Workstation identifier

      IF (IWK.LE.0) THEN
         KERROR = 20
         GOTO 999
      ENDIF

*     Find workstation index
      DO 10 I=1,KWK
         IF (KWKID(I).EQ.IWK) GOTO 20
   10 CONTINUE
      KERROR = 25
      GOTO 999

   20 CONTINUE
      KWKIX = I
      IWKTYP = KWTYIX(KWKIX)
      KWKTYP = KLAWKT(IWKTYP)
      KRPCC=KRPNO
      KINENT=IENT
      KSGRQ=0
      KERROR = 0

*     -- send command to W/S
      INCLUDE 'cdrive.inc'

      IF( KRPCC.NE.KRPNO) THEN
        IF( KERROR.EQ.0 ) THEN
          CALL GKSGPB(KWKID(KWKIX))
        ELSE
          KRPCC=KRPNO
        ENDIF
      ENDIF

      IF( KERROR.EQ.0 .AND. KSGRQ.GT.0 ) CALL GKCSWD(KINENT,
     :                         1,KDAT, 1,QDAT,QDAT, 1,CH)
  999 CONTINUE
      END
