C# IL>=a, OL>=0
      SUBROUTINE GKMPAR(NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             DSG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Metafile interpreter Pattern Representation routine
*
*  MAINTENANCE LOG
*  ---------------
*     24/01/84  DSG   Original version stabilized
*     29/03/84  DSG   Bug fix S38
*     06/03/84  DSG   Dimensions of ILA increased to 3 to allow the
*                     subroutine to compile (Bug S43).
*     19/04/84  DSG   Number of rows and columns (M and N) interchanged
*     18/03/86  DSG   I246 - Stack deallocated
*     20/01/87  DCS   IS conversion. Set IAT to 1 initially as metafile
*                     index removed.
*     20/01/87  CJC   IS conversion. Change data in W.C.A. following
*                     GSPAR interface change.
*     06/08/90  KEVP  Unused local variable removed (S342).
*     13/08/90  KEVP  Put in stack error check (S311).
*
*  ARGUMENTS
*  ---------
*     INP   NCD    Length of item data record
*     INP   STR    Item data record
*
      INTEGER NCD
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WCA/    Various
*     Modify /SL/     KSTRWK
*     Read   /ERR/    KERROR
*
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkpid.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
*  LOCALS
*  ------
*     N, M            Numbers of columns and rows in the array
      INTEGER N, M

* Local arrays
      INTEGER ILA(3)

      INTEGER ICHUNK,ISET,NK,J,NCELLS,IAT,IRSZ,IISZ,ICSZ

* Fraction of the stack in use when the workstation is called
      REAL FRAC
      PARAMETER (FRAC = 0.49)
*
*  ERRORS
*  ------
*     84   Dimensions of colour array are invalid
*
*  ALGORITHM
*  ---------
*     The array of colour indices is output to all active workstations
*
*---------------------------------------------------------------------

      IAT=1

* Pattern index and number of rows (M) and columns (N) in the colour array
      CALL GKUPSI(IAT,STR,3,ILA)
      IAT=IAT+3*KPDSSZ
      N=ILA(2)
      M=ILA(3)
      IF (( N.LT.1).OR.(M.LT.1)) THEN
        KERROR = 84
        GOTO 999
      ENDIF

* Array of colour indices stored row by row.
* Allow for the possibility that truncation occurred during metafile
* input:
      NCELLS=M*N
      IF(NCELLS.GT.(NCD*80-4*KPDSSZ)/KPDSSZ) THEN
        KERROR=165
        GOTO 999
      ENDIF

      ICHUNK=NCELLS
      CALL GKSTQ(IRSZ,IISZ,ICSZ)

* Send to all active workstations
      KWI1=ILA(1)
      KWI2=N
      KWI3=M
      KWI4=1
      KWI5=1
      KWI6=N
      KWI7=M


      ICHUNK=MIN(ICHUNK,INT(FRAC*IISZ))
      CALL GKSTAL(KINTGS,ICHUNK,ISET)
      IF(KERROR .NE. 0)GOTO 999
      NK=ICHUNK
      DO 20 J=1,NCELLS,ICHUNK
        IF(NCELLS-J.LT.ICHUNK) NK=NCELLS-J+1
        CALL GKUPSI(IAT,STR,NK,KSTACK(ISET))
        IAT=IAT+NK*KPDSSZ
        CALL GKSACW(KSPAR,NK,KSTACK,1,QDAT,QDAT,1,CH)
        IF (KERROR.NE.0) GOTO 990
   20 CONTINUE

* Deallocate stack space
  990 CALL GKSTDA (KREALS,ISET)

  999 RETURN

      END
