C# IL>=b, OL>=0
      SUBROUTINE GKRQIP(IPCLSS,IDNR,NINT,NREA,INTA,REALA)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Gets input device data from WSL, checks for errors 140, 141.

*     This version intercepts for PICK to assume ONE pick device
*     for each LOCATOR device, ignoring PICK section of WDT.

*
*  MAINTENANCE LOG
*  ---------------
*     18/07/83  AS    Original version stabilized
*     15/06/89  KEVP  Pick Frigged version stabilised
*
*  ARGUMENTS
*  ---------
*     INP   IPCLSS input class
*     INP   IDNR   device number
*     INP   NINT   number of integers for this class
*     INP   NREA   number of reals for this class
*     OUT   INTA   integer data
*     OUT   REALA  real data
*
      INTEGER IPCLSS, IDNR, NINT, NREA, INTA(NINT)
      REAL REALA(NREA)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCAL
*  -----
      REAL RLOCAT(6)
*
*  ERRORS
*  ------
*     140    Specified input device not present
*     141    Input device is not in request mode
*
*---------------------------------------------------------------------



      IF(IPCLSS .EQ. GPICK)THEN
        CALL GKDRGE(KIPDPT(GLOCAT,KWKIX),IDNR,NINT,6,INTA,RLOCAT)
        REALA(KIPEXL) = RLOCAT(1)
        REALA(KIPEXR) = RLOCAT(2)
        REALA(KIPEYB) = RLOCAT(3)
        REALA(KIPEYT) = RLOCAT(4)
        INTA(KPCINS) = GNPICK
        INTA(KPCISG) = KNIL
        INTA(KPCINI) = KNIL
        INTA(KPCIWI) = 0
        INTA(KPCIWR) = 0
      ELSE
        CALL GKDRGE(KIPDPT(IPCLSS,KWKIX),IDNR,NINT,NREA,INTA,REALA)
      ENDIF
      IF (KERROR.EQ.0) THEN
        IF (INTA(KIPOPM).NE.GREQU) KERROR = 141
      ELSE
        KERROR = 140
      ENDIF

      END
