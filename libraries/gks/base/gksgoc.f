C# IL>=a, OL>=2
      SUBROUTINE GKSGOC(IWKID,IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End Utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Directs control to one specified open workstation, and then to CSS.
*     Entered on behalf of GKS Level 2 Functions during WISS playback.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID- workstation identifier
*     INP IENT - entrypoint code
*     INP NID  - number of elements in array IDAT
*     I/O IDAT - integer data passed to or from workstation driver
*     INP NRD  - number of elements in array RX and RY
*     I/O RX   - real x-coordinates passed to or from workstation driver
*     I/O RY   - real y-coordinates passed to or from workstation driver
*     INP NCD  - length of character array
*     I/O STR  - character array
*
      INTEGER IWKID,IENT,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD),RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /ERR/ KERROR
*
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     None
*
*---------------------------------------------------------------------


*     send to one specified workstation
      CALL GKSONW(IWKID,IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*     send to CSS
      CALL GKCSRD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

      RETURN
      END
