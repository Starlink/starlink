      SUBROUTINE GKRQCH(NCDS,NCH,ICH)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             PLP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Handles Request Choice for two types of Choice devices
*
*  MAINTENANCE LOG
*  ---------------
*     18/01/88  PLP   Original version stabilized.
*     25/01/88  PLP   Introduced computed GOTO so as to treat choice
*                     devices independently.
*     03/05/90  RMK   Generate error 140 if choice device number isn't
*                     1 or 2. Added KERROR checks after GKRQIP calls.
*
*  ARGUMENTS
*  ---------
*     I/O NCDS - Contains choice device number on input and
*                the request choice exit status on output
*     OUT NCH  - Choice number
*     INP ICH  - Routine to call to get choice input
*
      INTEGER NCDS, NCH
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  INTRINSIC FUNCTIONS
*  -------------------
*
*
*  LOCALS
*  ------
*
*     CHOSTR String of single character representing choice input
*     ICHOIC Choice returned
*     INTA   Local integer array, stores integ. part of choice device data
*     IUS    Temporary character*1 variable
*     MAXCHO Maximal choice number
*     MINCHO Minimal choice number
*     NOUT   Number of bytes returned on input
*     PROMPT The prompt preceding input implemented by keyboard
*     REALA  Local real array, stores real part of choice device data
*     STEMP  Temporary character*1 variable
*
      INTEGER   ICHOIC, INTA(19), MAXCHO, MINCHO, NOUT, IUS
      REAL REALA(12)
      CHARACTER CHOSTR*1, PROMPT*50, STEMP*1
      PARAMETER ( IUS=31 )
*
*  EXTERNALS
*  ---------
*
      EXTERNAL ICH
*
*  EXTERNAL FUNCTION DEFINITIONS
*  ----------------------------
*
      INTEGER   GKNA1
      CHARACTER GKAN1
*
*---------------------------------------------------------------------

*  Conditional GOTO on choice device number
      GOTO ( 10, 20 ) NCDS

*  Only choice devices 1 and 2 supported by this utility
      KERROR = 140

      GOTO 999

*
*   Choice device 1.
*   Alternative choices: keys '1' to '9'; No choice: The '0' key;
*   Break : any other key.
*
   10 CONTINUE
*  Set up PROMPT, STEMP, MAXCHO, MINCHO
         PROMPT = 'Choice 1 to 9 :'
         STEMP  = '0'
         MINCHO = 1
         MAXCHO = 9

*   Get choice device info
      CALL GKRQIP(GCHOIC,NCDS,6,4,INTA,REALA)

*   KERROR set to 140 if input device NCDS not present on workstation
      IF (KERROR.NE.0) GOTO 999

*   Get choice input
      CALL ICH(INTA,REALA,PROMPT,CHOSTR,NOUT)

*   Interpret the results. Deal with the <RETURN> key first.

      IF ( NOUT.EQ.0 ) THEN
*        Just the <RETURN> key was hit, status = none
         NCDS=GNONE
         NCH =KNIL
      ELSE
*        Get the choice value...
         ICHOIC=GKNA1(CHOSTR)-GKNA1(STEMP)
*        Test the choice value against the range allowed
         IF ( MINCHO.LE.ICHOIC .AND. ICHOIC.LE.MAXCHO ) THEN
*           Choice value within the range - status = ok
            NCDS=GOK
            NCH =ICHOIC
         ELSEIF ( ICHOIC.EQ.0 ) THEN
*           The '0' was hit - status = no choice
            NCDS=GNCHOI
            NCH=KNIL
         ELSE
*           Ch. v. outside the range, ie break key was hit - status = none
            NCDS=GNONE
            NCH =KNIL
         ENDIF
      ENDIF

      GOTO 999

*
*   Choice device 2.
*   Alternative choices: any printing ASCII; No choice: The <RETURN> key;
*   Break : any other key.
*
   20 CONTINUE
*  Set up PROMPT, STEMP, MAXCHO, MINCHO
         PROMPT = 'Hit any printing key :'
         STEMP  = GKAN1(IUS)
         MINCHO = 1
         MAXCHO = 95

*   Get choice device info
      CALL GKRQIP(GCHOIC,NCDS,6,4,INTA,REALA)

*   KERROR set to 140 if input device NCDS not present on workstation
      IF (KERROR.NE.0) GOTO 999

*   Get choice input
      CALL ICH(INTA,REALA,PROMPT,CHOSTR,NOUT)

*   Interpret the results. Deal with the <RETURN> key first.

      IF ( NOUT.EQ.0 ) THEN
*        Just the <RETURN> key was hit, status = no choice
         NCDS=GNCHOI
         NCH =KNIL
      ELSE
*        Get the choice value...
         ICHOIC=GKNA1(CHOSTR)-GKNA1(STEMP)
*        Test the choice value against the range allowed
         IF ( MINCHO.LE.ICHOIC .AND. ICHOIC.LE.MAXCHO ) THEN
*           Choice value within the range - status = ok
            NCDS=GOK
            NCH =ICHOIC
         ELSE
*           Ch. v. outside the range, ie break key was hit - status = none
            NCDS=GNONE
            NCH =KNIL
         ENDIF
      ENDIF
*
  999 CONTINUE
*
      END
*---------------------------------------------------------------------



