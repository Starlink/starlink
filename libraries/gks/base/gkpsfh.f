C# IL>=a, OL>=0
      SUBROUTINE GKPSFH(NPOLY,IPOLY,NW,WSX,WSY,NSECT,ISEQ)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*-------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Reorder the vertices of a Polygon set in accordance to the
*     section list.
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP  Original Version Stabilised
*     29/03/89  KEVP  Corrected DO loop to handle reverse sections
*     26/04/89  KEVP  Changed name from GKFACS and removed debug
*                     WRITE statements.
*     16/11/89  RMK   Removed unused local variables.
*     22/07/90  PLP   Commenting brought in line with standard format.
*     22/08/90  KEVP  Corrected stack error goto statements (Bug C23).
*
*  ARGUMENTS
*  ---------
*     INP  NPOLY   The actual number of polygons
*     I/O  IPOLY   The position of the polygons in ISEQ
*     INP  NW      Number of points in the workspace
*     I/O  WSX,WSY The workspace
*     INP  NSECT   Number of sections
*     I/O  ISEQ    The splicing sequence
*
      INTEGER NW, NSECT, ISEQ(2,NSECT),
     :        NPOLY, IPOLY(NPOLY+1)
      REAL    WSX(NW),WSY(NW)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'

*  LOCALS
*  ------
*     IWR     Read pointer in unsorted workspace
*     IQW     Write pointer in sorted workspace
*     ISORT   Stack pointer for sorted copy of workspace
*     IPOST   Stack pointer to polygon starts
*
*     JPOLY   Current polygon
*     JSECT1  First section in polygon
*     LSECT   Last section of polygon
*
*     LENSEC  Length of current section
*     LENTOT  Total length of sections
*     JSECT   Position in ISEQ of current section
*     IWSS    Start of section to be moved
*     IWSE    End of section to be moved
*     ISTEP   +1 for forward section, -1 for reverse section
*
      INTEGER JPOLY, JSECT1, JSECT ,LSECT
      INTEGER IWR, IQW, ISORT, IPOST
      INTEGER LENSEC, LENTOT, IWSS,IWSE, ISTEP
*
*  STACK USAGE
*  -----------
*     ISORT   Sorted copy of the workspace
*     IPOST   Polygon starts
*
*  ALGORITHM
*  ---------
*     1, The total length of all sections is calculated.
*
*     2, Enough stack space is allocated to contain all the sections
*        and also for polygon starts
*
*     3, Sections are copied into the stack, so sorting them out,
*        reversing any reverse sections.
*
*     4, Sorted sections are copied into the workspace
*        ISEQ and IPOLY are accordingly modified, with
*        one section per polygon.
*
*  ERRORS
*  ------
*     -2004  Badly defined arguments.
*      (1) Total length of sections greater than length of workspace.
*      (2) Polygon has non-existent section in it.
*      (3) Section not inside bounds of workspace.
*
*------------------------------------------------------------------------
*
      LENTOT = 0
*     Measure total length of sections
      DO 10 JSECT=1,IPOLY(NPOLY+1)-1
         LENSEC = IABS (ISEQ(1,JSECT)-ISEQ(2,JSECT))
         LENTOT = LENTOT + LENSEC
   10 CONTINUE
      IF(LENTOT .GE. NW)THEN
         CALL GKBUG(-2004,'GKPSFH')
         GOTO 999
      ENDIF

*     Set up stack for sorted sections
      CALL GKSTAL (KREALS,2*LENTOT,ISORT)
      IF(KERROR .NE. 0)GOTO 999
*     and polygon starts
      CALL GKSTAL (KINTGS,NPOLY,IPOST)
      IF(KERROR .NE. 0)GOTO 989
*
      IQW = ISORT
      JSECT1 = IPOLY(1)
*
*     Loop begins for polygon
      DO 50 JPOLY=1,NPOLY
        LSECT = IPOLY(JPOLY+1) - 1
        IF((LSECT .LT. 1) .OR. (LSECT .GT. NSECT))THEN
          CALL GKBUG(-2004,'GKPSFH')
          GOTO 900
        ENDIF

*       Loop begins for section
        DO 40 JSECT=JSECT1,LSECT

*         Copy section into stack
          IWSS = ISEQ(1,JSECT)
          IWSE = ISEQ(2,JSECT)
          IF(IWSS .LE. IWSE)THEN
*            Forward section - retract end
             IWSE = IWSE - 1
             ISTEP = 1
          ELSE
*            Reverse section - retract start
             IWSS = IWSS - 1
             ISTEP = -1
          ENDIF
          IF((IWSS .LT. 1) .OR. (IWSS .GT. NW))THEN
            CALL GKBUG(-2004,'GKPSFH')
            GOTO 900
          ENDIF
          IF((IWSE .LT. 1) .OR. (IWSE .GT. NW))THEN
            CALL GKBUG(-2004,'GKPSFH')
            GOTO 900
          ENDIF

          DO 20 IWR = IWSS,IWSE,ISTEP
             QSTACK(IQW) = WSX(IWR)
             QSTACK(IQW+1) = WSY(IWR)
             IQW = IQW + 2
   20     CONTINUE
   40   CONTINUE
*       Loop ends for section

        JSECT1 = LSECT + 1
        KSTACK(IPOST+JPOLY-1) = (IQW - ISORT)/2 + 1
   50 CONTINUE
*     Loop ends for polygon
*
*     Copy sorted sections into workspace update ISEQ and IPOLY
      IQW = ISORT
      DO 60 IWR = 1,LENTOT
         WSX(IWR) = QSTACK(IQW)
         WSY(IWR) = QSTACK(IQW+1)
         IQW = IQW + 2
   60 CONTINUE
      ISEQ(1,1) = 1
      IPOLY(1) = 1
      DO 70 JPOLY=2,NPOLY+1
         IPOLY(JPOLY) = JPOLY
         ISEQ(2,JPOLY-1) = KSTACK(IPOST+JPOLY-2)
         ISEQ(1,JPOLY) = ISEQ(2,JPOLY-1)
   70 CONTINUE


*     Release Stack
  900 CONTINUE
      CALL GKSTDA (KINTGS,IPOST)
  989 CONTINUE
      CALL GKSTDA (KREALS,ISORT)

  999 CONTINUE
      END
