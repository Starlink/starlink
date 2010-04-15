*---------------------------------------------------------------------


      SUBROUTINE GK0XGI(INTA,REALA,PROMPT,RETSTR,NOUT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To display a prompt and return the reply.
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
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
*
*  LOCALS
*  ------
      INTEGER I

*  Find the length of the prompt string minus any trailing spaces
      DO 10 I = LEN(PROMPT),2,-1
         IF (PROMPT(I:I).NE.' ') GO TO 20
   10 CONTINUE
   20 CONTINUE

      CALL GK0XSI( KWKIX, PROMPT, I, RETSTR, LEN(RETSTR), NOUT)

      END
