
*-----------------------------------------------------------------------



      SUBROUTINE GK0EGC(RIN,GIN,BIN,ROUT,GOUT,BOUT)
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
*      To return the realized colour for a given requested colour for
*      emulator+s
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*     INP RIN    Requested red intensity
*     INP GIN    Requested green intensity
*     INP BIN    Requested blue intensity
*     INP ROUT   Realized red intensity
*     INP GOUT   Realized green intensity
*     INP BOUT   Realized blue intensity
*
      REAL RIN, GIN, BIN, ROUT, GOUT, BOUT
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     None
*-----------------------------------------------------------------------

*   Cifer 2634
      IF( KWKTYP.EQ.800 ) THEN
        ROUT = ANINT((MIN(RIN,GIN,BIN)+MAX(RIN,GIN,BIN))*0.5)
        GOUT = ROUT
        BOUT = ROUT

*   Standard Pericom, RAL mods Pericom & Cifer T5
      ELSEIF( KWKTYP.EQ.801 .OR. KWKTYP.EQ.820 .OR.
     :        KWKTYP.EQ.821 ) THEN
        ROUT = 0.0
        GOUT = ANINT((MIN(RIN,GIN,BIN)+MAX(RIN,GIN,BIN))*0.5)
        BOUT = 0.0
      ENDIF

      END
