      SUBROUTINE GK0EBF

*  -----------------------------------------------------------
*
*  GKS UK
*
*  Type of routine:       (Part of)workstation driver
*  Author:                KWB
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF ROUTINE
*  ------------------
*     To handle Tektronix 4010 lookalikes.
*     (a) Sends extra codes needed at beginning to switch terminal into
*     Tektronix mode (this is without head and tail set up at (b))
*     (b) Where necessary, puts strings of ASCII codes at the head and
*     tail of each buffer, to switch terminal into Tektronix 4010 mode
*     at start and out again at end.
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
*    IESC,IUPARO,ICAN,IGS  ASCII codes for ESC, up arrow, CAN & GS
*    ICTRLC                ASCII codes for CTRL-C
*    ITKnnn  Orders to initialise terminal with workstation type nnn
*            This takes place before the begin and end buffers are set up
*    ITK82X  As above, but shared by two workstation types:
*            Standard Pericom and Pericom with RAL mods.
*    IAnnn   ASCII codes to go at start of buffer for workstation nnn
*            For some workstations, this is to switch terminal to Tek mode
*    IBnnn   ASCII codes to go at end of buffer for workstation nnn.
*            This would be to switch back out of Tek mode.
*    NLEFT   Returned by GKIOBO (and ignored)
*
      INTEGER IESC, IUPARO, IGS, ICTRLC
      PARAMETER (IESC=27, IUPARO=94, IGS=29, ICTRLC=3)
      INTEGER ITK800(6),ITK801(21),ITK82X(4)
      INTEGER IA810(4),IB810(2)
      INTEGER NLEFT
*
*  Cifer 2634 .... send on Open Workstation
*    Switch destination to be graphics (esc, uparrow, 18). This also
*    blanks alpha and unblanks graphics.
*    Reset Graphics Board (esc, uparrow, U)
      DATA ITK800/IESC, IUPARO, 18,
     :            IESC, IUPARO, 85/
*
*  Cifer T5 .... send on Open Workstation
*    Define "SUPER-ESCAPE" to be <CTRL>+C (used in
*            target screen selection and handling)
*                            (ESC,P,Z,0,0,0,3,X)
*    Select 4010 mode        (ESC,P,t)
*    Unblank graphics screen (<CTRL>+C,[,6,/,b)
*    Blank   alpha    screen (<CTRL>+C,[,0,/,b)
      DATA ITK801/IESC,80,90,48,48,48,51,88,
     :            IESC,80,116,
     :            ICTRLC,91,54,47,98,
     :            ICTRLC,91,48,47,98/
*
*  VT100 with Selanar Board
*  In the begin buffer, switch graphics board to Tek mode and
*  in the end buffer, switch back.
      DATA IA810/IESC,49,IESC,42/,
     :     IB810/IESC,50/
*
*  Pericom Monterey (Standard/RAL mods)...send on Open Workstation
*    Switch to graphics mode     (GS)
*    Select graphics screen only (ESC,\,4)
      DATA ITK82X/IGS,IESC,92,52/
*  -----------------------------------------------------------
*

*   Cifer 2634
      IF( KWKTYP.EQ.800) THEN
        CALL GKIOBO(KIOPB, 6,ITK800, NLEFT)
        CALL GKIOBO(KIOSN,  1,ITK800, NLEFT)

*   Cifer T5
      ELSEIF( KWKTYP.EQ.801) THEN
        CALL GKIOBO(KIOPB,21, ITK801, NLEFT)
        CALL GKIOBO(KIOSN, 1, ITK801, NLEFT)

*   VT100 with Selanar board
      ELSEIF(KWKTYP.EQ.810)THEN
        CALL GKIOBO(KIOBB,4,IA810,NLEFT)
        CALL GKIOBO(KIOEB,2,IB810,NLEFT)

*   Standard/RAL mods Pericom Monterey shared entry
      ELSEIF(KWKTYP.EQ.820.OR.KWKTYP.EQ.821) THEN
        CALL GKIOBO(KIOPB, 4,ITK82X, NLEFT)
        CALL GKIOBO(KIOSN, 1,ITK82X, NLEFT)

      END IF
*
      END
