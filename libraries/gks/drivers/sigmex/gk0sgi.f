      SUBROUTINE GK0SGI(INTA, REALA, ST, RETSTR, NOUT)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) workstation driver
*  Author:             JRG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To display a prompt ST on alpha screen and return the
*     response in RETSTR
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP  INTA    Contains int part of device state .... used for echo
*                  switch (the part that is independent of device class is
*                  declared here)
*     INP  REALA   Contains real part of device state...Not used here,
*                  but included for compatibility reasons.
*     INP  ST      String to be used for prompt
*     OUT  RETSTR  String containing the reply
*     OUT  NOUT    Number of characters input (ie number returned in RETSTR)
*
      INTEGER INTA(4),NOUT
      REAL REALA(1)
      CHARACTER*(*) ST, RETSTR
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gks.par'
*
*  LOCALS
*  ------
*     NSIGS   Number of significant characters (i.e. without trailing
*             blanks) in ST
*     NLEFT   Number of characters left in output buffer
*     IOCISW  Function code for GKIOCI
*
      INTEGER NSIGS, NLEFT, IOCISW
*
*  ALGORITHM
*  ---------
*     Turns alpha screen on (AA) and puts prompt there (uses BH)
*     Performs a character read from the terminal
*
*-----------------------------------------------------------------------


*   Find last significant character (position will be NSIGS) in ST
      DO 100 NSIGS=LEN(ST),1,-1
        IF( ST(NSIGS:NSIGS).NE.' ' ) GOTO 105
  100 CONTINUE

*   Here, all characters are spaces
      NSIGS=0

*   Here, NSIGS has been set
  105 CONTINUE

*    Turn alpha screen on and empty buffer without appending end bytes
      CALL GKIOCO(KIOPB, 'AABH', NLEFT)
      CALL GKIOCO(KIOSO, ' ', NLEFT)

*   Set up prompt, send prompt and read input
      IF ( INTA(KIPE).EQ.GECHO .AND. NSIGS.LE.80 ) THEN
         IOCISW = KIOEN
      ELSE
         IOCISW = KIONN
      ENDIF
      CALL GKIOCI(IOCISW, NSIGS, ST(1:NSIGS), RETSTR, NOUT)

      END



