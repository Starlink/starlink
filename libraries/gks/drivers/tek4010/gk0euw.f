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
     :        IA801(2),IG801(13),
     :        IA820(1),IG820(4),
     :        IA821(2),IG821(4),
     :        IA825(1),IG825(1),
     :        IA845(1),IG845(1)
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
      DATA IA801/IESC,92/
*  Switch  to Tek mode     (ESC,P,t)
*  blank   alpha    screen (<CTRL>+C,[,0,/,b)
*  unblank graphics screen (<CTRL>+C,[,6,/,b)
      DATA IG801/IESC,80,116,
     :           ICTRLC,91,48,47,98,
     :           ICTRLC,91,54,47,98/
*
*  Standard Pericom Monterey MG200...send on Update Workstation
*  Switch to alpha mode      (<CTRL>+X)
      DATA IA820/ICTRLX/
*  switch to graphics   (GS)
*  select graphics only (ESC,\,4)
      DATA IG820/IGS,
     :           IESC,92,52/
*
*  Pericom Monterey MG200 with RAL mods...send on Update Workstation
*  Switch to alpha mode      (ESC,SOH)
      DATA IA821/IESC,ISOH/
*  switch to graphics   (GS)
*  select graphics only (ESC,\,4)
      DATA IG821/IGS,
     :           IESC,92,52/
*
*  Pericom 7800...send on Update workstation
*  Switch to alpha mode
      DATA IA825/ICTRLX/
*  Switch to graphics
      DATA IG825/IGS/
*
*  Graphon 235...send on update workstation
*  Switch to alpha mode
      DATA IA845/ICTRLX/
*  Switch to graphics mode
      DATA IG845/IGS/
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
        CALL GKIOBO(KIOPB, 2,IA801,NLEFT)
        CALL GKIOBO(KIOSN, 1,IA801,NLEFT)
        CALL GKIOBO(KIOPB,13,IG801,NLEFT)

*   VT100 with Selanar board (No policy on this one as yet)
      ELSEIF(KWKTYP.EQ.810)THEN
*        NEED SOMETHING HERE!
         CONTINUE

*   Standard Pericom Monterey
      ELSEIF(KWKTYP.EQ.820) THEN
        CALL GKIOBO(KIOPB, 1,IA820,NLEFT)
        CALL GKIOBO(KIOSN, 1,IA820,NLEFT)
        CALL GKIOBO(KIOPB, 4,IG820,NLEFT)

*   Pericom Monterey with RAL mods
      ELSEIF(KWKTYP.EQ.821) THEN
        CALL GKIOBO(KIOPB, 2,IA821,NLEFT)
        CALL GKIOBO(KIOSN, 1,IA821,NLEFT)
        CALL GKIOBO(KIOPB, 4,IG821,NLEFT)

*   Pericom 7800 with RAL mods
      ELSEIF(KWKTYP.EQ.825) THEN
        CALL GKIOBO(KIOPB, 1,IA825,NLEFT)
        CALL GKIOBO(KIOSN, 1,IA825,NLEFT)
        CALL GKIOBO(KIOPB, 1,IG825,NLEFT)

*   GraphOn 235
      ELSEIF(KWKTYP.EQ.845) THEN
        CALL GKIOBO(KIOPB, 1,IA845,NLEFT)
        CALL GKIOBO(KIOSN, 1,IA845,NLEFT)
        CALL GKIOBO(KIOPB, 1,IG845,NLEFT)

      END IF
*
      END
