      SUBROUTINE GK0ESC(IMODE)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of workstation driver
*  Author:             DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      To set the line drawing mode for Tektronix lookalikes.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*     INP IMODE      Mode (0-Erase, 1-Draw)
*
      INTEGER IMODE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    KWKTYP
*     Read   /WKD/    KWKDAT
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     IESC   ASCII code for ESC
*     NLEFT  Returned by GKIOBO (not used here)
*     IW800 Orders to switch Cifer 2634 lines to white
*     IB800 Orders to switch Cifer 2634 lines to black
*     IW801 Orders to switch Cifer T5 lines to white
*     IB801 Orders to switch Cifer T5 lines to black
*     IW82X Orders to switch Standard/RAL mods Pericom lines to white
*     IB82X Orders to switch Pericom lines to black
*     IW845 Orders to switch GraphOn 235 lines to white
*     IB845 Orders to switch GraphOn 235 lines to black
*
      INTEGER IESC
      PARAMETER (IESC=27)
      INTEGER NLEFT
      INTEGER IB800(2), IW800(2), IB801(2), IW801(2),
     :        IB82X(2), IW82X(2), IB845(2), IW845(2)
*
*  Cifers
      DATA IB800/IESC,16/
      DATA IW800/IESC,1/
      DATA IB801/IESC,16/
      DATA IW801/IESC,1/
*  Pericoms (Standard and Ral mods)
      DATA IB82X/IESC,120/
      DATA IW82X/IESC,96/
*  GraphOn 235
      DATA IB845/IESC,16/
      DATA IW845/IESC,1/
*
*-----------------------------------------------------------------------

*   Cifer 2634
      IF( KWKTYP.EQ.800 ) THEN
        IF( IMODE .EQ. 1) THEN
          CALL GKIOBO(KIOPB,2,IW800,NLEFT)
        ELSE
          CALL GKIOBO(KIOPB,2,IB800,NLEFT)
        ENDIF

*   Cifer T5
      ELSEIF( KWKTYP.EQ.801 ) THEN
        IF( IMODE .EQ. 1) THEN
          CALL GKIOBO(KIOPB,2,IW801,NLEFT)
        ELSE
          CALL GKIOBO(KIOPB,2,IB801,NLEFT)
        ENDIF

*   Pericoms (Standard and RAL mods)
      ELSEIF( KWKTYP.EQ.820.OR.KWKTYP.EQ.821.OR.KWKTYP.EQ.825 ) THEN
        IF( IMODE .EQ. 1) THEN
          CALL GKIOBO(KIOPB,2,IW82X,NLEFT)
        ELSE
          CALL GKIOBO(KIOPB,2,IB82X,NLEFT)
        ENDIF

*   GraphOn
      ELSEIF( KWKTYP.EQ.845 ) THEN
        IF( IMODE .EQ. 1) THEN
          CALL GKIOBO(KIOPB,2,IW845,NLEFT)
        ELSE
          CALL GKIOBO(KIOPB,2,IB845,NLEFT)
        ENDIF
      ENDIF

      END
