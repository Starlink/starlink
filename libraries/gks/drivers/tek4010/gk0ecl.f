*---------------------------------------------------------------------



      SUBROUTINE GK0ECL
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of workstation driver
*  Author:             JRG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To handle any orders needed by Tektronix lookalikes on
*     'Close Workstation'
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*     None
*
*  FUNCTION RETURN VALUE
*  ---------------------
*     Index into name table array
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
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace ....
*     IBAUD  Offset for baud rate for this terminal
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace .....
*     IESC,IUPAR,ICTRLC  ASCII codes for ESC, Up Arrow and <CTRL>+C
*     ICTRLX             ASCII code for <CTRL>+X
*     NLEFT  Returned by GKIOBO (not used here)
*     IC800A Orders to switch to Cifer alpha mode
*     IC801A Orders to switch to Cifer T5 alpha mode and
*            make both screens visible.
*     IC820A Orders to switch Standard Pericom  to alpha and
*            make both screens visible.
*     IC821A Orders to switch RAL mods Pericom to alpha and
*            make both screens visible.
*
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace ....
      INTEGER IBAUD
      PARAMETER (IBAUD=1)
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace .....
      INTEGER IESC,IUPARO,ISOH,ICTRLC,ICTRLX
      PARAMETER (IESC=27,IUPARO=94,ISOH=1,ICTRLC=3,ICTRLX=24)
      INTEGER NLEFT
      INTEGER IC800A(3), IC801A(7), IC820A(4), IC821A(5)
*
*  Cifer 2634 ...send on Close Workstation
*     select alpha screen       (ESC,Uparrow,T)
      DATA IC800A/IESC,IUPARO,84/
*  Cifer T5...send on Close Workstation
*     select  alpha screen (ESC,\)
*     unblank alpha screen (<CTRL>+C,[,5,/,b)
      DATA IC801A/IESC,92,ICTRLC,91,53,47,98/
*  Standard Pericom Monterey...send on Close Workstation
*     make both screens visible (ESC,\,5)
*     select alpha screen       (<CTRL>+X)
      DATA IC820A/IESC,92,53,ICTRLX/
*  RAL mods Pericom Monterey...send on Close Workstation
*     make both screens visible (ESC,\,5)
*     select alpha screen       (ESC,SOH)
      DATA IC821A/IESC,92,53,IESC,ISOH/
*
*-----------------------------------------------------------------------


*   Cifer 2634
      IF( KWKTYP.EQ.800 ) THEN
*       Select alpha
        CALL GKIOBO(KIOPB, 3,IC800A, NLEFT)
*       Compensate for the mode switching slowness
        CALL GKSYN( KWKDAT(IBAUD,KWKIX), 1.0)

*   Cifer T5
      ELSEIF( KWKTYP.EQ.801) THEN
        CALL GKIOBO(KIOPB, 7,IC801A, NLEFT)

*   Standard Pericom Monterey
      ELSEIF( KWKTYP.EQ.820) THEN
        CALL GKIOBO(KIOPB, 4,IC820A, NLEFT)

*   Pericom Monterey with RAL mods
      ELSEIF( KWKTYP.EQ.821) THEN
        CALL GKIOBO(KIOPB, 5,IC821A, NLEFT)

      ENDIF

      END
