      SUBROUTINE GK2DGI(INTA,REALA,PROMPT,RETSTR,NOUT)
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
*     To display a prompt in echo area and return the reply.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP  INTA    Contains int part of device state .... used for echo switch
*                  (that part that is independent of device class is
*                  declared here)
*     INP  REALA   Contains real part of device state .... used for echo area
*                  (that part that is independent of device class is
*                  declared here)
*     INP  PROMPT  String to be used for prompt (trailing spaces removed)
*     OUT  RETSTR  String to be returned
*     OUT  NOUT    Number of characters input
*
      INTEGER INTA(4),NOUT
      REAL REALA(4)
      CHARACTER*(*) PROMPT, RETSTR
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     NECHOX,NECHOY Number of characters that can appear in echo area
*             in each of x and y directions
*     NSIGS   Number of significant characters (i.e. without trailing
*             blanks) in PROMPT
*     NLEFT   Number of characters left in output buffer
*     NLINES  Number of lines in echo area used
*     NSEE    Number of characters we have room for in echo area
*     IOCISW  Function code for input from terminal
*     SIZEX,SIZEY  Hold character size
*
      INTEGER NECHOX,NECHOY, NSIGS, NLEFT, NLINES, NSEE, IOCISW
      REAL X(1), Y(1), SIZEX, SIZEY
*
*  Offsets in KWKDAT workspace
*
      INTEGER MODE, CHARHI,CHARWI,ICX,ICY, ICPEN
      PARAMETER (MODE = 1, CHARHI = 2, CHARWI = 3, ICX = 4, ICY = 5,
     :           ICPEN = 6)
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*
*  ALGORITHM
*  ---------
*     Provides both a prompt and echo if required and if room.
*     If no echo needed, then provides prompt if room.
*     If echo needed and only room for one of prompt or echo, then
*     provides prompt.
*
*-----------------------------------------------------------------------


*   Work out character size
      SIZEX = KWKDAT(CHARWI,KWKIX)
      SIZEY = KWKDAT(CHARHI,KWKIX)

*   Number of characters in echo area
      NECHOX=INT(  (REALA(KIPEXR)-REALA(KIPEXL))/SIZEX  )
      NECHOY=INT(  (REALA(KIPEYT)-REALA(KIPEYB))/SIZEY  )

*   Find last significant character (position will be NSIGS) in PROMPT
      DO 100 NSIGS=LEN(PROMPT),1,-1
        IF( PROMPT(NSIGS:NSIGS).NE.' ' ) GOTO 105
  100 CONTINUE
      NSIGS=0

*   Here, NSIGS has been set (=0 if all spaces)
  105 CONTINUE

*   Display string if we can (NECHOY is big enough) and if
*   there are any significant characters in PROMPT. NLINES is the
*   number of lines we have occupied so far.
      NLINES=0
      IF(  (NECHOY.GE.2 .OR. (NECHOY.EQ.1.AND.INTA(KIPE).EQ.GNECHO))
     :   .AND. NSIGS.GT.0  ) THEN
          NLINES=1
          X(1) = REALA(KIPEXL)
          Y(1) = REALA(KIPEYT)-SIZEY-1.0
          CALL GK2DLN(1, X, Y)
      ENDIF

*   Set NSEE and move beam if we will be echoing
      IF( NECHOY.GE.1 ) THEN
          NSEE=NECHOX
          IF( INTA(KIPE).EQ.GECHO ) THEN
            X(1) = REALA(KIPEXL)
            Y(1) = REALA(KIPEYT)-SIZEY*FLOAT(NLINES+1)-1.0
            CALL GK2DLN(1, X, Y)
            CALL GKIOCO(KIOSN,' ',NLEFT)
          ENDIF
      ELSE
          NSEE=0
      ENDIF

*   Decide whether echoing is needed, and get input
      IF (INTA(KIPE).EQ.GECHO.AND.NSEE.GE.72) THEN
         IOCISW = KIOEP
      ELSE
         IOCISW = KIONP
      ENDIF
      CALL GKIOCO(KIOSO, ' ', NLEFT)
      CALL GKIOCI(IOCISW, NSIGS, PROMPT, RETSTR, NOUT)

      END
