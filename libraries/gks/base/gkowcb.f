C# IL>=a, OL>=0
      SUBROUTINE GKOWCB(IWKID,IWTIX, IWKIX)
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
*     Purpose is to add a new workstation to the list of open ones in
*     the Workstation Control Block (W.C.B.). Note that the
*     Metafile Input Index Table is not updated here; that can only be
*     done after the workstation driver is entered as otherwise we
*     don't know the workstation is Metafile Input.
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  JRG   First version
*     16/03/83  JRG   Further commenting
*     30/03/83  JRG   CHECK.INC included
*     23/06/83  JRG   Changes to use KERROR for error reporting.
*                     Use value for invalid workstation.
*     19/01/87  PKY   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*   Inp IWKID   Workstation identifier supplied by application program
*   Inp IWTIX   Workstation Type Index (assumed to be in range 1...KMXWKT)
*   Out IWKIX   Workstation Index
*
      INTEGER IWKID,IWTIX, IWKIX
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYWCB/ Update lists of open workstations in W.C.B.
*     Modify /GKYERR/ KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NAME   is name of this routine
*     IBUG   GKS bug number
*     IX     is Workstation Index held locally before putting into IWKIX
*
      CHARACTER*9 NAME
      PARAMETER (NAME='GKOWCB')
      INTEGER IBUG,IX
*
*  ERRORS
*  ------
*       42 Maximum number of open workstations would be exceeded
*    -2003 Corrupt W.C.B.
*    -2004 Parameter IWTIX out of range
*
*---------------------------------------------------------------------



*   Find a space for open workstation
      DO 100 IX=1,KWK
        IF( KWKID(IX).EQ.KWKINV ) GOTO 110
  100 CONTINUE
*   Here: no room for another workstation. Error 42
      KERROR=42
      GOTO 999

*   Here: IX is index for space for open workstation. Array
*   'list of open workstation indexes' should also have room. If not,
*    there's a bug (error -2003).
  110 IF( KNOPWK.GE.KWK ) THEN
        IBUG=-2003
        GOTO 980
      ENDIF
      IF( IWTIX.LT.1 .OR. IWTIX.GT.KMXWKT ) THEN
        IBUG=-2004
        GOTO 980
      ENDIF
*   End error checking
*-----------------------

*   Now we have correct indexes in IX and IWTIX
      KWKID(IX)=IWKID
      KNOPWK=KNOPWK+1
      KOPPT(KNOPWK)=IX
      KWTYIX(IX)=IWTIX

*   Set the workstation index for return
      IWKIX=IX
      GOTO 999

*   Here to report bug (number in IBUG)
  980 CALL GKBUG(IBUG,NAME)

*
  999 CONTINUE
      END
