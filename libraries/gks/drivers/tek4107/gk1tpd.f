

      SUBROUTINE GK1TPD(INTA,REALA, ST,NSEE)
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
*     To display a prompt ST in echo area (specified in REALA) and
*     return how much room remains for input. Finally position
*     the beam for input.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP  INTA    Contains int part of device state .... used for echo
*                  (that part that is independent of device class is
*                  declared here)
*     INP  REALA   Contains real part of device state .... used for echo
*                  (that part that is independent of device class is
*                  declared here)
*     INP  ST      String to be used for prompt (trailing spaces removed
*     OUT  NSEE    Number of characters that could be displayed in the
*                  echo area
*
      INTEGER INTA(4),NSEE
      REAL REALA(4)
      CHARACTER*(*) ST
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*     ICHWID  Width of character used for echo in D.C.
*     ICHHIT  Height of character used for echo in D.C.
*   NECHOX,NECHOY Number of characters that can appear in echo area
*             in each of x and y directions
*     NSIGS   Number of significant characters (i.e. without trailing
*             blanks) in ST
*     NLEFT   Number of characters left in output buffer
*     NLINES  Number of lines in echo area used
*
      INTEGER ICHWID,ICHHIT
      PARAMETER (ICHWID=14,ICHHIT=14)
      INTEGER NECHOX,NECHOY, NSIGS, NLEFT, NLINES
*
*  ALGORITHM
*  ---------
*     Provides both a prompt and echo if required and if room.
*     If no echo needed, then provides prompt if room.
*     If echo needed and only room for one of prompt or echo, then
*     provides prompt.
*


*   Number of characters in echo area
      NECHOX=INT(  (REALA(KIPEXR)-REALA(KIPEXL))/FLOAT(ICHWID)  )
      NECHOY=INT(  (REALA(KIPEYT)-REALA(KIPEYB))/FLOAT(ICHHIT)  )

*   Find last significant character (position will be NSIGS) in ST
      DO 100 NSIGS=LEN(ST),1,-1
        IF( ST(NSIGS:NSIGS).NE.' ' ) GOTO 105
  100 CONTINUE

*   Here, all characters are spaces
      NSIGS=0

*   Here, NSIGS has been set
  105 CONTINUE

*   Display string if we can (NECHOY is big enough) and if
*   there are any significant characters in ST. NLINES is the
*   number of lines we have occupied so far.
      NLINES=0
      IF(  (NECHOY.GE.2 .OR. (NECHOY.EQ.1.AND.INTA(KIPE).EQ.GNECHO))
     :   .AND. NSIGS.GT.0  ) THEN
          NLINES=1
          CALL GK1TMV(NINT(REALA(KIPEXL)),
     :                   NINT(REALA(KIPEYT))-ICHHIT-1)
          CALL GKIOCO(KIOPB,ST(1:NSIGS),NLEFT)
          CALL GKIOCO(KIOSN,ST,NLEFT)
      ENDIF

*   Set NSEE and move beam if we will be echoing
      IF( NECHOY.GE.1 ) THEN
          NSEE=NECHOX
          IF( INTA(KIPE).EQ.GECHO ) THEN
            CALL GK1TMV(NINT(REALA(KIPEXL)),
     :                     NINT(REALA(KIPEYT))-ICHHIT*(NLINES+1)-1)
          ENDIF
      ELSE
          NSEE=0
      ENDIF

      END
