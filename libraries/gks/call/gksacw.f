C# IL>=a, OL>=0
      SUBROUTINE GKSACW(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONTEND - W/S
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Workstation interface routine called by GKS front end to send
*     control to all active workstations.
*
*  MAINTENANCE LOG
*  ---------------
*     18/02/83  AS    Original version stabilized
*     21/03/83  AS    Change logical name for include on CDRIVE.INC
*     16/08/83  AS    Add character array to argument list
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
*     Modify /GKWCA/   KWKIX,KWKTYP
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
      INTEGER IWKTYP,ITEMP,I,IER
      CHARACTER*9 NAME
      PARAMETER (NAME='GKSACW')
*
*---------------------------------------------------------------------


      IER = 0
      DO 10 I=1,KNACWK
        KWKIX = KACPT(I)
        IWKTYP = KWTYIX(KWKIX)
        KWKTYP = KLAWKT(IWKTYP)
        KERROR = 0
      INCLUDE 'cdrive.inc'
* Only save first error to pass back
        IF (IER.EQ.0 .AND. KERROR.NE.0) IER = KERROR
   10 CONTINUE

      IF (IER.NE.0) KERROR = IER
      END
