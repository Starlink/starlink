C# IL>=b, OL>=0
      SUBROUTINE GSVPIP(ITN,IRTN,IREL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET VIEWPORT INPUT PRIORITY
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Sets the viewport input priority of transformation number ITN
*     to be immediately higher or lower than that of t.n. IRTN
*
*  MAINTENANCE LOG
*  ---------------
*     05/05/83  JRG   Original version stabilized
*     23/06/83  JRG   Changes to use KERROR for error reporting
*     28/09/83  AS    Change subroutine name
*     20/01/87  SHS   IS conversion. If transformation numbers are equal
*                     take no action.
*     20/01/87  ARG   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP   ITN    Transformation number which is to be moved in
*                  priority list
*     INP   IRTN   Reference transformation number next to which ITN
*                  is to be moved.
*     INP   IREL   Contains value 'higher' or 'lower' indicating
*                  desired new relationship between ITN and IRTN
*
      INTEGER ITN, IRTN, IREL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/  List of transf. numbers in priority order KTNOVP
*     Modify /GKYERR/ KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IXT    Index in KTNOVP of ITN
*     IXRT   Index in KTNOVP of IRTN
*     IXNEW  Index in KTNOVP of new position of ITN
*     IDIR   Encoding of relative priority:  higher -> 1, lower -> -1
*     ISTEP  Sign of (IXRT-IXT) ....  i.e. +1 or -1
*     J      Loop counter
*
      INTEGER IXT,IXRT,IXNEW, IDIR,ISTEP, J
*
*  ERRORS
*  ------
*        8   Wrong GKS state
*       50   One of the transformation numbers is out of range
*     2000   IREL is neither higher nor lower
*    -2007   Entry not found in list
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG (ESVPIP,GGKOP,GSGOP)
      IF( KERROR.GT.0 ) GOTO 990

*   Check Transformation Numbers
      IF( ITN.LT.0 .OR. ITN.GT.KT .OR.
     :    IRTN.LT.0 .OR. IRTN.GT.KT ) THEN
        KERROR=50
        GOTO 990
      ENDIF

*   If both transformation numbers are equal, take no action
      IF (ITN.EQ.IRTN) GOTO 999

*   Encode IREL as the sign of an integer (error 2000 if IREL invalid)
      IF    ( IREL.EQ.GHIGHR ) THEN
                                 IDIR=+1
      ELSEIF( IREL.EQ.GLOWER ) THEN
                                 IDIR=-1
      ELSE
                                 KERROR = 2000
                                 GOTO 990
      ENDIF

*   Change only takes place if transformation numbers are unequal
      IF( ITN.NE.IRTN ) THEN

*   Find indexes in KTNOVP of transformation numbers (-> IXT & IXRT)
        DO 50 IXT=0,KT
          IF( KTNOVP(IXT).EQ.ITN ) GOTO 60
   50   CONTINUE
        GOTO 980
   60   CONTINUE

        DO 100 IXRT=0,KT
          IF( KTNOVP(IXRT).EQ.IRTN ) GOTO 110
  100   CONTINUE
        GOTO 980
  110   CONTINUE

*   Indexes are now in IXT and IXRT and IXT\=IXRT.
*   Encode their relative position as +1 or -1. This is used as a
*   loop step later.
        ISTEP=SIGN(1,IXRT-IXT)

*   Find index where ITN is to go.
*   (a) If it has to move to the far side of IRTN,
*       new position of ITN <- old position of IRTN
*   (b) If it has to move to the near side of IRTN,
*       it moves similarly, but nearer by 1.
        IXNEW=IXRT
        IF( IDIR.EQ.ISTEP ) IXNEW=IXRT-ISTEP

*   Now shuffle. First move is into IXT; last is out of IXNEW.
        DO 200 J=IXT+ISTEP,IXNEW,ISTEP
  200     KTNOVP(J-ISTEP)=KTNOVP(J)

*   Move ITN into new place
        KTNOVP(IXNEW)=ITN
      ENDIF
      GOTO 999

*   Report GKS bug. Drop through to GKS error report afterwards.
  980 CALL GKBUG(-2007,'GSVPIP')

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
