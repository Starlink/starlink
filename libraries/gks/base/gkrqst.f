      SUBROUTINE GKRQST(NSDS,LPS,IPS,NID,LST,IDAT,GISUB)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Handles Request String
*
*  MAINTENANCE LOG
*  ---------------
*     13/12/88  KEVP  Original version stabilized.
*     03/05/90  RMK   Added check on KERROR after GKRQIP call.
*     18/02/91  KEVP  Redefined string length so as to not include
*                     any characters of the initial string after the
*                     final cursor position (S466).
*
*  ARGUMENTS
*  ---------
*     I/O  NSDS    Contains string device number on input
*                  and string device exit status on output
*     INP  LPS     Length of prompt-start
*     INP  IPS     Integer array containing prompt start
*     INP  NID     Length of array containing the string
*     OUT  LST     Length of string obtained
*     OUT  IDAT    Integer array containing the string obtained
*     INP  GISUB   Routine to call and get string input
*
      INTEGER NSDS, LPS, IPS(LPS), NID, IDAT(NID), LST
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     INTA    Array of integer string device data
*     LECHO   Length of string echoed
*     NOUT    Number of bytes returned on input
*     PROMPT  String to contain the prompt
*     REALA   Array of real string device data
*     STRING  String input string

      REAL      REALA(4)
      INTEGER   INTA(9), LECHO, NOUT
      CHARACTER*120  PROMPT, STRING
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GISUB
*     Display prompt and get input
*
*  COMMENTS
*  --------
*     A prompt is displayed that consists of the prompt-start
*     followed by the initial string up to but not including
*     the character at the initial cursor position.
*
*     The string obtained from the string input device is typed over
*     the initial string from the initial cursor position.
*     If the end of the initial string is not reached by the cursor,
*     the characters after the final cursor position not included
*     from the string.
*
*---------------------------------------------------------------------
*
*   Get string device information
      CALL GKRQIP(GSTRIN,NSDS,9,4,INTA,REALA)

*   KERROR set to 140 if input device NCDS not present on workstation
      IF (KERROR.NE.0) GOTO 999

*   Get initial string from heap
      IF(INTA(KSTINS) .NE. KNIL)THEN
        CALL GKHPGI (INTA(KSTINS),0,INTA(KSTINL),IDAT)
      ENDIF

*   Translate from integers to characters
      CALL GKATON(LPS,IPS,PROMPT(1:LPS))
      CALL GKATON(INTA(KSTICP)-1,IDAT,PROMPT(LPS+1:LPS+INTA(KSTICP)))

*   Display prompt in echo area and find out how much room there is
*   for input
      CALL GISUB(INTA,REALA,PROMPT(1:INTA(KSTICP)+LPS-1),STRING,NOUT)

*   Interpret the results: if not recognisable string then status=none
      IF( NOUT.LT.1 ) THEN
        NSDS=GNONE
        LST = 0
      ELSE
        LECHO = NID - INTA(KSTICP) + 1
        IF(LECHO .GT. NOUT)LECHO=NOUT
        CALL GKNTOA (LECHO,STRING,IDAT(INTA(KSTICP)))
        IF(KERROR .EQ. 101)THEN
           NSDS=GNONE
           LST = 0
           KERROR = 0
        ELSE
           NSDS = GOK
           LST  = LECHO + INTA(KSTICP) - 1
        ENDIF
      ENDIF

  999 CONTINUE
      END
