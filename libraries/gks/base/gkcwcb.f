C# IL>=a, OL>=0
      SUBROUTINE GKCWCB(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front end
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*    Purpose is to remove a workstation from the lists of open
*    workstations in the Workstation Control Block.
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  JRG   First version
*     16/03/83  JRG   Further commenting
*     30/03/83  JRG   CHECK.INC included
*     23/06/83  JRG   Statement label changes
*     19/01/87  PKY   IS conversion. Error number changes.
*     18/05/87  DCS   Use COMMON value for absence of valid workstation
*                     (KWKINV) rather than local (IWKINV) so that work-
*                     stations are properly closed (S267).
*     06/04/90  KEVP  Error number for corrupt WCB is -2003, not -2004.
*
*  ARGUMENTS
*  ---------
*     Inp IWKID   Workstation Identifier. Assumed to be an open workstation.
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYWCB/ Remove open workstation from lists
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwcb.cmn'
*
*  LOCALS
*  ------
*     NAME is name of this routine
*     IX holds the Workstation Index locally
*     IOPIX holds the relevant index to the list of Workstation Indexes
*     J is a loop counter
*
      CHARACTER*9 NAME
      PARAMETER (NAME='GKCWCB')
      INTEGER IX,IOPIX,J
*
*  ERRORS
*  ------
*    -2003   Corrupt W.C.B.
*
*---------------------------------------------------------------------


*   Look for workstation identifier in list of them. If so keep index in IX.
      DO 100 IX=1,KWK
        IF( KWKID(IX).EQ.IWKID ) GOTO 150
  100 CONTINUE
*   Here, not found. GKS bug, since should have been checked before
*   entering this routine.
      GOTO 980

*   Here, IX contains Workstation Index. Ensure that it exists in
*   'list of open workstation indexes'. If so keep that index in IOPIX.
  150 DO 200 IOPIX=1,KNOPWK
        IF( KOPPT(IOPIX).EQ.IX ) GOTO 250
  200 CONTINUE
*   Here, not found. GKS bug.
      GOTO 980
*   End of error checking
*---------------------------

*   Here, we have valid values in IX and IOPIX. Now make changes.
*   Firstly: set invalid the entry in array of workstation identifiers.
  250 KWKID(IX)=KWKINV
*   Secondly: remove workstation index from 'list of open workstation
*   indexes'. This is done by shuffling part of the list towards its
*   beginning. The list must end up being contiguous.
*   IOPIX is the relevant index in this array. In the loop, J indexes the
*   element that is to overwrite its predecessor.
      DO 300 J=IOPIX+1,KNOPWK
        KOPPT(J-1)=KOPPT(J)
  300 CONTINUE
      KNOPWK=KNOPWK-1
      GOTO 999

*   Here, to report a corrupt W.C.B.
  980 CALL GKBUG(-2003,NAME)

*   Quit
  999 CONTINUE
      END
