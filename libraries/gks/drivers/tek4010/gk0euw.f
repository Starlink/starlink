*-----------------------------------------------------------------------



      SUBROUTINE GK0EUW

*  -----------------------------------------------------------
*
*  GKS UK
*
*  Type of routine:       (Part of)workstation driver
*  Author:                PLP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF ROUTINE
*  ------------------
*     To handle Tektronix 4010 lookalikes on Update Workstation entry
*     in a uniform manner:
*     (a) Send code needed to switch from graphics to alpha.
*     (b) Put but don't send code necessary to return to graphics.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read    /WCA/  KWKTYP
*     Read    /WKD/  KWKDAT
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCAL VARIABLES
*  ---------------
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace...
*     IBAUD  Offset for baud rate for this terminal
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace...
*    IESC,IUPARO,ICAN,IGS  ASCII codes for ESC, up arrow, CAN & GS
*    ISOH,ICTRLC,ICTRLX    ASCII codes for SOH, CTRL-C, CTRL-X
*    IAnnn   ASCII codes to switch workstation nnn to  alpha
*    IGnnn   ASCII codes to switch workstation nnn back to graphics
*    NLEFT   Returned by GKIOBO (and ignored)
*
      INTEGER IBAUD, IESC, IUPARO, IGS, ISOH, ICTRLC, ICTRLX
      PARAMETER (IBAUD=1,
     :           IESC=27, IUPARO=94, IGS=29, ISOH=1, ICTRLC=3,
     :           ICTRLX=24)
      INTEGER IA800(3),IG800(3),
     :        IA801(7),IG801(13),
     :        IA820(4),IG820(4),
     :        IA821(5),IG821(4)
      INTEGER NLEFT
*
*  Cifer 2634 .... send on Update Workstation
*  Switch destination to be alpha (esc, uparrow, 84). This automatically
*  blanks graphics and unblanks alpha and nothing can be done to overlay
*  the screens.
      DATA IA800/IESC, IUPARO, 84/
*  put code to switch to graphics(esc, uparrow, 18). Again, this will
*  blank alpha and unblank graphics....
      DATA IG800/IESC, IUPARO, 18/
*
*  Cifer T5 .... send on Update Workstation
*  Select  alpha mode        (ESC,\)
*  Unblank alpha screen      (<CTRL>+C,[,5,/,b)
      DATA IA801/IESC,92,
     :           ICTRLC,91,53,47,98/
*  Switch  to Tek mode     (ESC,P,t)
*  blank   alpha    screen (<CTRL>+C,[,0,/,b)
*  unblank graphics screen (<CTRL>+C,[,6,/,b)
      DATA IG801/IESC,80,116,
     :           ICTRLC,91,48,47,98,
     :           ICTRLC,91,54,47,98/
*
*  Standard Pericom Monterey MG200...send on Update Workstation
*  Switch to alpha mode      (<CTRL>+X)
*  make both screens visible (ESC,\,5)
      DATA IA820/ICTRLX,
     :           IESC,92,53/
*  switch to graphics   (GS)
*  select graphics only (ESC,\,4)
      DATA IG820/IGS,
     :           IESC,92,52/
*
*  Pericom Monterey MG200 with RAL mods...send on Update Workstation
*  Switch to alpha mode      (ESC,SOH)
*  make both screens visible (ESC,\,5)
      DATA IA821/IESC,ISOH,
     :           IESC,92,53/
*  switch to graphics   (GS)
*  select graphics only (ESC,\,4)
      DATA IG821/IGS,
     :           IESC,92,52/
*  -----------------------------------------------------------
*

*   Cifer 2634
      IF( KWKTYP.EQ.800) THEN
         CALL GKIOBO(KIOPB,3,IA800,NLEFT)
*        Compensate for the mode switching slowness
         CALL GKSYN(KWKDAT(IBAUD,KWKIX), 1.0)
*        Now put IG800, but don't send
         CALL GKIOBO(KIOPB,3,IG800,NLEFT)

*   Cifer T5
      ELSEIF( KWKTYP.EQ.801) THEN
        CALL GKIOBO(KIOPB, 7,IA801,NLEFT)
        CALL GKIOBO(KIOSN, 1,IA801,NLEFT)
        CALL GKIOBO(KIOPB,13,IG801,NLEFT)

*   VT100 with Selanar board (No policy on this one as yet)
      ELSEIF(KWKTYP.EQ.810)THEN
*        NEED SOMETHING HERE!
         CONTINUE

*   Standard Pericom Monterey
      ELSEIF(KWKTYP.EQ.820) THEN
        CALL GKIOBO(KIOPB, 4,IA820,NLEFT)
        CALL GKIOBO(KIOSN, 1,IA820,NLEFT)
        CALL GKIOBO(KIOPB, 4,IG820,NLEFT)

*   Pericom Monterey with RAL mods
      ELSEIF(KWKTYP.EQ.821) THEN
        CALL GKIOBO(KIOPB, 5,IA821,NLEFT)
        CALL GKIOBO(KIOSN, 1,IA821,NLEFT)
        CALL GKIOBO(KIOPB, 4,IG821,NLEFT)

      END IF
*
      END
