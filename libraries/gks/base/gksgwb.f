C# IL>=a, OL>=0
      SUBROUTINE GKSGWB(IENT,ERASBL)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Handles segment workstation entrypoints for workstations
*     that use bounding boxes with segments.
*
*  MAINTENANCE LOG
*  ---------------
*     28/04/89  KEVP  Original version stabilized
*     16/11/89  RMK   Removed unused local variable.
*     09/12/91  KEVP  Checked that Segment exists on workstation,
*                     before setting bounding box to undefined (C96).
*
*  ARGUMENTS
*  ---------
*     INP IENT   - Entrypoint code
*     INP ERASBL - TRUE if workstation can attempt to erase a single segment
*
      INTEGER IENT
      LOGICAL ERASBL
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     BOUND   Bounding box of segment
*     IPOINT Pointer to Segement
*
*  Indices of BOUND
*     JLEFT  Minimum X
*     JRIGHT Maximum X

      INTEGER JLEFT,JRIGHT, IPOINT
      PARAMETER (JLEFT=1,JRIGHT=2)
      REAL    BOUND(4)
*
*  COMMENTS
*  --------
*    In any workstation driver that uses bounded boxes for segments
*    in connection with the PICK utilities GKRQPK and GKPPxx,
*    should call GKSGWB instead of GKSGWK.
*
*---------------------------------------------------------------------


*     Create segment list with bounding boxes, if necessary.
      IF (KSSGPT(KWKIX).EQ.KNIL) CALL GKSLCR(KBOX,KSSGPT(KWKIX))
      IF(KERROR .NE. 0)GOTO 9999

*     Handle segments
      CALL GKSGWK (IENT,ERASBL)

*     Set bounding box as undefined,
*     if segment is being created or transformed and
*     exists on this this workstation.
      IF((IENT .EQ. KCRSG) .OR. (IENT .EQ. KSSGT))THEN
        CALL GKSLFN(KSSGPT(KWKIX),KWI1,IPOINT)
        IF(IPOINT .NE. KNIL)THEN
           BOUND(JLEFT)  =  0.0
           BOUND(JRIGHT) = -1.0
           CALL GKSLBB (KSSGPT(KWKIX),KCURR,BOUND)
        ENDIF
      ENDIF

 9999 CONTINUE
      END
