C# IL>=a, OL>=0
      SUBROUTINE GKMCA(NCD,STR)
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
*     Metafile interpreter Cell Array primitive routine
*
*  MAINTENANCE LOG
*  ---------------
*     22/07/83  DSG   Original version stabilized
*     30/11/83  DSG   Fortran binding changes
*     13/01/83  AS    Correct routine name
*     24/01/84  DSG   Call to GKERR(95) replaced by KERROR=84
*     20/03/84  DSG   Bug Fix S27
*     19/04/84  DSG   S48: Numbers of rows and columns interchanged
*     04/05/84  DSG   S56: Impose limit to size of interpreted cell array
*     21/02/86  DCS   Replace call to GKSCTM by its contents.
*                     Remove unused locals ICHUNK, NK, J.
*     18/03/86  DSG   I246 - Deallocate stack.
*     19/01/87  DCS   IS conversion. Remove metafile index.
*     20/01/87  CJC   IS conversion. Change data in W.C.A. following GCA
*                     interface change.
*     09/12/88  RMK   Changed error 301 to 300 (S338).
*     13/08/90  KEVP  Put in check for stack allocation error (S311).
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
      INCLUDE '../include/gks.par'
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
      INTEGER ILA(2)
      REAL RLA(6)

      INTEGER ISET,NCELLS,IAT,IRSZ,IISZ,ICSZ

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
*     The Cell Array is output between P and Q on all active W/S's
*
*---------------------------------------------------------------------

      IAT=1
* Coordinates of the corner points of the pixel array
      CALL GKUPR(IAT,STR,6,RLA)
      IAT=IAT+6*KPDRSZ

* Number of rows (M) and columns (N) in the array
      CALL GKUPSI(IAT,STR,2,ILA)
      IAT=IAT+2*KPDSSZ
      N=ILA(1)
      M=ILA(2)
      IF (( N.LT.1).OR.(M.LT.1)) THEN
        KERROR = 84
        GOTO 999
      ENDIF

* Bring tranformation and clip info. up to date
      IF (KSTRWK.NE.KMI) THEN
        CALL GKCCTM
        IF(KERROR.NE.0) GOTO 999
        CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
        IF(KERROR.NE.0) GOTO 999
* Set flag
        KSTRWK=KMI
      ENDIF

* Array of colour indices stored row by row.
* Allow for the possibility that truncation occurred during metafile
* input:
      NCELLS=M*N
      IF(NCELLS.GT.(NCD*80-3*KPDSSZ-6*KPDRSZ)/KPDSSZ) THEN
        KERROR=165
        GOTO 999
      ENDIF

      CALL GKSTQ(IRSZ,IISZ,ICSZ)

* Stack space available limits size of cell array that can be processed
      IF(NCELLS.GT.FRAC*IISZ) THEN
        KERROR=300
        GOTO 999
      ENDIF

* Send to all active workstations
      QWR1=RLA(1)
      QWR2=RLA(2)
      QWR3=RLA(3)
      QWR4=RLA(4)
* Third corner of the transformed pixel array.
      QWR5=RLA(5)
      QWR6=RLA(6)

      KWI1=N
      KWI2=M
      KWI3=1
      KWI4=1
      KWI5=N
      KWI6=M

      CALL GKSTAL(KINTGS,NCELLS,ISET)
      IF(KERROR .NE. 0)GOTO 999
      CALL GKUPSI(IAT,STR,NCELLS,KSTACK(ISET))
      CALL GKSACW(KCA,NCELLS,KSTACK,1,QDAT,QDAT,1,CH)
      IF (KERROR.NE.0) GOTO 990

      IF (KRGN) CALL GKRGN

* Deallocate stack
  990 CALL GKSTDA(KINTGS,ISET)

  999 RETURN

      END
