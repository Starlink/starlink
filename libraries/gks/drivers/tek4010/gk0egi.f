*-----------------------------------------------------------------------



      SUBROUTINE GK0EGI(INTA, REALA, PROMPT, STR, NOUT)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) workstation driver
*  Author:             DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To display a prompt on scrolling alpha store on emulator+
*     terminals and read input.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*   INP    INTA   contains INT part of device state
*   INP    PROMPT prompt to be displayed, trailing spaces are removed
*   INP    REALA  Contains real part of device state...Not used here, but
*                 included for compatibility reasons.
*   OUT    STR    string typed on keyboard
*   OUT    NOUT   number of characters in string
*
      INTEGER NOUT, INTA(4)
      REAL REALA(1)
      CHARACTER*(*) STR, PROMPT
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace...
*
*     IBAUD  Offset for baud rate for this terminal
*
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace .....
*     NSIGS   Number of significant characters (i.e. without trailing
*             blanks) in PROMPT
*     NLEFT   Number of characters left in output buffer
*     IDUM    Dummy for calls to KIOBO
*     IOCISW  Function code for input from terminal
*     IB800A  Orders to switch Cifer 2634 to alpha
*             - switch to alpha (ESC,Uparrow,T). This also
*               unblanks   alpha, blanks graphics.
*     IB800B  Orders to switch Cifer 2634 to graphics
*             - switch to graphics (ESC,Uparrow,DC2). This also
*               unblanks graphics, blanks alpha.
*     IB801A  Orders to switch Cifer T5 to alpha
*             - switch to alpha (ESC,\)
*             - unblank   alpha (<CTRL>+C,[,5,/,b)
*     IB801B  Orders to switch Cifer T5 to graphics
*             - switch to Tek mode (ESC,P,t)
*             - blank     alpha    (<CTRL>+C,[,0,/,b)
*             - unblank graphics   (<CTRL>+C,[,6,/,b)
*     IB820A  Orders to switch Standard Pericom Monterey to alpha:
*             - make both screens visible   (esc,\,5)
*             - switch to alpha screen      (<CTRL>+X)
*     IB820B  Orders to switch Pericom Monterey to graphics:
*             - enter graphics mode         (gs)
*             - select graphics screen only (esc,\,4)
*     IB821A  Orders to switch RAL mods Pericom Monterey to alpha:
*             - make both screens visible   (esc,\,5)
*             - switch to alpha screen      (esc,soh)
*     IB821B  Orders to switch Pericom Monterey to graphics:
*             - enter graphics mode         (gs)
*             - select graphics screen only (esc,\,4)
*
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace...
      INTEGER IBAUD
      PARAMETER (IBAUD=1)
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace .....
      INTEGER NSIGS, NLEFT, IOCISW, IDUM(1)
      INTEGER IB800A(3), IB800B(3),
     :        IB801A(7), IB801B(13),
     :        IB820A(4), IB820B(4),
     :        IB821A(5), IB821B(4)
      INTEGER    IESC,    IGS,    ISOH,   ICTRLC, ICTRLX
      PARAMETER (IESC=27, IGS=29, ISOH=1, ICTRLC=3, ICTRLX=24)
*  Cifer 2634
      DATA IB800A/IESC,94,84/
      DATA IB800B/IESC,94,18/
*  Cifer T5
      DATA IB801A/IESC,92,ICTRLC,91,53,47,98/
      DATA IB801B/IESC,80,116,ICTRLC,91,48,47,98,
     :            ICTRLC,91,54,47,98/
*  Standard Pericom Monterey
      DATA IB820A/IESC,92,53,ICTRLX/
      DATA IB820B/IGS,IESC,92,52/
*  Pericom Monterey with RAL mods
      DATA IB821A/IESC,92,53,IESC,ISOH/
      DATA IB821B/IGS,IESC,92,52/
*
*  ALGORITHM
*  ---------
*   Select alpha store, output prompt, read input, select graphics screen.
*
*-----------------------------------------------------------------------


*   Find last significant character (position will be NSIGS) in PROMPT
      DO 100 NSIGS=LEN(PROMPT),1,-1
        IF( PROMPT(NSIGS:NSIGS).NE.' ' ) GOTO 105
  100 CONTINUE
      NSIGS=0

*   Here, NSIGS has been set (=0 if all spaces)
  105 CONTINUE

*   Select alpha screen
*   Put orders to switch between screens in the buffer
      IF (KWKTYP.EQ.800) THEN
*   Cifer 2634
         CALL GKIOBO(KIOPB,3,IB800A,NLEFT)
*        wait till modes actually switch
         CALL GKSYN( KWKDAT(IBAUD,KWKIX), 1.0)

      ELSEIF (KWKTYP.EQ.801) THEN
*   Cifer T5
         CALL GKIOBO(KIOPB,7,IB801A,NLEFT)

      ELSEIF (KWKTYP.EQ.820) THEN
*   Standard Pericom Monterey
         CALL GKIOBO(KIOPB,4,IB820A,NLEFT)

      ELSEIF (KWKTYP.EQ.821) THEN
*   Pericom Monterey with RAL mods
         CALL GKIOBO(KIOPB,5,IB821A,NLEFT)

      ENDIF


*    Effect the screen selection - flush the buffer
      CALL GKIOBO(KIOSN,0,IDUM,NLEFT)

*   Output prompt and read input
      IF (INTA(KIPE).EQ.GECHO) THEN
         IOCISW = KIOEN
      ELSE
         IOCISW = KIONN
      END IF
      CALL GKIOCI(IOCISW,NSIGS, PROMPT, STR, NOUT)

*   Select graphics screen
      IF (KWKTYP.EQ.800) THEN
*   Cifer 2634
         CALL GKIOBO(KIOPB,3,IB800B,NLEFT)

      ELSEIF (KWKTYP.EQ.801) THEN
*   Cifer T5
         CALL GKIOBO(KIOPB,13,IB801B,NLEFT)

      ELSEIF (KWKTYP.EQ.820) THEN
*   Standard Pericom Monterey
         CALL GKIOBO(KIOPB,4,IB820B,NLEFT)

      ELSEIF (KWKTYP.EQ.821) THEN
*   Pericom Monterey with RAL mods
         CALL GKIOBO(KIOPB,4,IB821B,NLEFT)

      ENDIF

      END
