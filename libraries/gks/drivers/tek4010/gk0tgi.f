*-----------------------------------------------------------------------



      SUBROUTINE GK0TGI(INTA,REALA,PROMPT,RETSTR,NOUT)
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
*     ICHSIZ  Offset in KWKDAT for whether no of hardware character sizes is
*             1 (GLOWER) or greater (GHIGHR)
*     NECHOX,NECHOY Number of characters that can appear in echo area
*             in each of x and y directions
*     NSIGS   Number of significant characters (i.e. without trailing
*             blanks) in PROMPT
*     NLEFT   Number of characters left in output buffer
*     NSEE    Number of characters we have room for in echo area
*     IOCISW  Function code for input from terminal
*     SIZEX,SIZEY  Hold character size
*     HTSE,WDS  Heights (excluding spacing round character) and widths
*             (including spacing) on Tek 4014.
*
      INTEGER ICHSIZ
      PARAMETER (ICHSIZ=2)
      INTEGER NECHOX,NECHOY, NSIGS, NLEFT, NSEE, IOCISW
      REAL X(1), Y(1), SIZEX, SIZEY, HTSE(4), WDS(4)
      DATA HTSE/31.6, 32.8, 50.0, 56.0/,
     :     WDS /31.0, 34.0, 51.0, 56.0/
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
*     For TEK4010 and the like there is only one character size, 14
*     pixels in width(including spacing) and 22 pixels in height
*     (including spacing).
      SIZEX = 14.0
      SIZEY = 22.0
      IF (KWKDAT(ICHSIZ, KWKIX).EQ.GHIGHR) THEN
         SIZEX = WDS(1)
         SIZEY = HTSE(1)
      ENDIF

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

*   Set NSEE and move beam
      IF( NECHOY.GE.1 ) THEN
          NSEE=NECHOX
          X(1) = REALA(KIPEXL)
          Y(1) = REALA(KIPEYT)-SIZEY-1.0
          CALL GK0TLN(1, X, Y)
          IF(INTA(KIPE).EQ.GECHO) CALL GKIOCO(KIOSN,' ',NLEFT)
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
