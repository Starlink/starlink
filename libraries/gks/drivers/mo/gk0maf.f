      SUBROUTINE GK0MAF(ITEM,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Update ASFs
*
*  MAINTENANCE LOG
*  ---------------
*     25/01/91  DSG  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   ITEM   Item identification number
*     INP   IENT   Entrypoint code
*     INP   NID    Size of array IDAT
*     INP   IDAT   Integer data passed to workstation
*     INP   NRD    Size of arrays RX and RY
*     INP   RX     Real X-coordinate data passed to workstation
*     INP   RY     Real Y-coordinate data passed to workstation
*
      INTEGER ITEM,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD), RY(NRD)
*
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index (KWKIX) and type
*     Read   /ASPCT/     KLNTYA,KPLCIA,KMKTYA,KPMCIA,KTXFNA,
*                        KTXCIA,KFAISA,KFACIA..(PAR)
*     Modify /GKYWKD/    Derive workstation 'total' transform
*                        In KWKDAT(n,KWKIX) keep local copy of AFSs in elements
*                        1 to 13 and store buffer pointer in 14.
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gaspct.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gksl.cmn'
*
*  LOCALS
*  ------
*     J        Loop  variable
*
      INTEGER J
*
*     IPL   offset to aspect list for polyline
*     IPM   offset to aspect list for polymarker
*     ITX   offset to aspect list for text
*     IFA   offset to aspect list for fill area
*
      INTEGER    IPL,  IPM,  ITX, IFA
      PARAMETER (IPL=0,IPM=3,ITX=6,IFA=10)
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      DO 184 J = KLNTYA,KPLCIA
         KWKDAT(J+IPL,KWKIX) = KIPLAF(J)
  184 CONTINUE
      DO 185 J = KMKTYA,KPMCIA
         KWKDAT(J+IPM,KWKIX) = KIPMAF(J)
  185 CONTINUE
      DO 186 J = KTXFNA,KTXCIA
         KWKDAT(J+ITX,KWKIX) = KITXAF(J)
  186 CONTINUE
      DO 187 J = KFAISA,KFACIA
         KWKDAT(J+IFA,KWKIX) = KIFAAF(J)
  187 CONTINUE
      ITEM=43
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END
