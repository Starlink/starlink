C# IL>=a, OL>=0
      SUBROUTINE GKSWKL(IWKID, XMIN,XMAX, YMIN,YMAX, NAME, IWKENT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front end utility
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To perform actions common to both Set Workstation Window and
*     Set Workstation Viewport (name is derived from Set WorKstation Limits).
*
*  MAINTENANCE LOG
*  ---------------
*      29/6/83   JRG  Original version stabilized
*      15/01/86  DRJF Bug Fix (Reports error 53 now instead of 52,
*                     when workstation window is not within the N.D.C
*                     unit square) S116.
*     20/01/87  ARG   IS conversion. Catch negative error numbers.
*
*  ARGUMENTS
*  ---------
*     Inp   IWKID  Workstation Identifier
*     Inp   XMIN,XMAX Limits in x of rectangle (window or viewport)
*     Inp   YMIN,YMAX Limits in y of rectangle (window or viewport)
*     Inp   NAME    Name of calling subroutine (ESWKWN or ESWKVP).
*     Inp   IWKENT  Integer identifying appropriate workstation entrypoint
*
      INTEGER IWKID, IWKENT, NAME
      REAL XMIN,XMAX, YMIN,YMAX
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYWCA/ Set up QWR1 ... QWR4 (rectangle limits)
*     Modify /GKYERR/ Modify KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     53  WS window is not within NDC unit square
*
*  COMMENTS
*  --------
*     Errors 7,51 and 54 may be returned via other front-end routines
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG(NAME, GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 999

*   Error checking
*   First check limits are right way round
      CALL GKRECT(XMIN,XMAX,YMIN,YMAX)
      IF (KERROR.NE.0) GOTO 999

*   Then check that specified limits are within the NDC unit square,
*   but only do this if calling routine was Set Workstation Window.
      IF( NAME.EQ.ESWKWN ) THEN
        IF( XMIN.LT.0.0 .OR. XMAX.GT.1.0 .OR.
     :      YMIN.LT.0.0 .OR. YMAX.GT.1.0 ) THEN
              KERROR=53
              GOTO 999
        ENDIF
      ENDIF
*   End of preliminary error checking
*----------------------------------------

*   Set up data for and call workstation driver
      QWR1=XMIN
      QWR2=XMAX
      QWR3=YMIN
      QWR4=YMAX
      CALL GKSONW(IWKID, IWKENT, 1,KDAT, 1,QDAT,QDAT,1,CH)
      IF (KERROR.NE.0) GOTO 999

*   Check regeneration
      IF( KRGN ) CALL GKRGN
      IF (KERROR.NE.0) GOTO 999

*   Set the relevant update flags to be 'out of date'
      CALL GKTOLD
      GOTO 999

*   Finish
  999 CONTINUE
      END
