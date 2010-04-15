C# IL>=a, OL>=0
      SUBROUTINE GKPSCO (NCROSS,IEDGCL,SIGCOO,IORDER,IPLACE,NTOP)
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
*     Find the order of the crossing points around the clipping rectangle
*     and give the number of such crossings on the top of the rectangle.
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP  Original Version Stabilised
*     26/04/89  KEVP  Name changed from GKFCXO.
*     22/07/90  PLP   Commenting brought in line with standard format.
*
*  ARGUMENTS
*  ---------
*     INP  NCROSS   Number of crossings
*     INP  IEDGCL   Clip status of the clip rectangle edge, that the
*                   crossing occurs (-10 base, -1 left, 1 right, 10 top)
*     INP  SIGCOO   The significant coordinate of the crossing point
*     OUT  IORDER   The order that the crossings occur on the
*                   clipping rectangle anticlockise
*     OUT  IPLACE   The placing of each crossing in IORDER
*     OUT  NTOP     Number of crossings on top-edge of clipping rectangle
*
      INTEGER NCROSS,IEDGCL(NCROSS),IORDER(NCROSS),IPLACE(NCROSS)
      REAL    SIGCOO(NCROSS)
      INTEGER NTOP
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
*
*  LOCALS
*  ------
*     ICROSS   Crossing identifier {1:NCROSS}
*     IEDGE    Current edge of clipping rectangle
*              =1 for base  (anticlockwise = left to right)
*              =2 for left  (anticlockwise = downwards)
*              =3 for right (anticlockwise = upwards)
*              =4 for top   (anticlockwise = right to left)
*     JEDGE    Index in IORDER of start of each clipping rectangle edge
*     NCEDGE   Number of crossings for each clipping rectangle edge
*     J1,J2    Do loop indices for ordering crossings
*     J1S,J1E  Search limits for ordering crossings
*              offsets in IORDER(JOFF)
*     JOFF     Offset in IORDER for start of current rectangle edge
*     ISTEP    The search step for the current edge IEDGE {-1 or 1}
*     ISWAP    Swapping buffer for ordering of crossings
*     ICRMIN   Index of crossing with minimum position
*     JMIN     Identifier of crossing with minimum position
*     XYCUR    Current position on edge
*     XYMIN    Minimum position on edge

      INTEGER ICROSS, IEDGE, JEDGE(4), NCEDGE(4), ISTEP, J1,J2,
     :        J1S,J1E, ISWAP, ICRMIN, JMIN, JOFF
      REAL    XYCUR, XYMIN
*
*---------------------------------------------------------------------
*     First count the number of crossings on each edge
      DO 500 IEDGE=1,4
         NCEDGE(IEDGE) = 0
  500 CONTINUE
      DO 510 ICROSS=1,NCROSS
         IEDGE = (IEDGCL(ICROSS)+18)/6
         NCEDGE(IEDGE) = NCEDGE(IEDGE) + 1
  510 CONTINUE

*     Set number of top crossings
      NTOP = NCEDGE(4)

*     Set Edge offsets in IORDER
*     (They are ordered so that all crossings will be ordered
*      in the whole allocation anticlockwise.)
      JEDGE(4) = 1
      JEDGE(2) = JEDGE(4) + NCEDGE(4)
      JEDGE(1) = JEDGE(2) + NCEDGE(2)
      JEDGE(3) = JEDGE(1) + NCEDGE(1)
*     Sort out Crossings according to the edge they cross
      DO 520 IEDGE=1,4
         NCEDGE(IEDGE) = 0
  520 CONTINUE
      DO 530 ICROSS=1,NCROSS
         IEDGE = (IEDGCL(ICROSS)+18)/6
         IPLACE(ICROSS) = JEDGE(IEDGE)+NCEDGE(IEDGE)
         IORDER(IPLACE(ICROSS)) = ICROSS
         NCEDGE(IEDGE) = NCEDGE(IEDGE) + 1
  530 CONTINUE

*     For each edge of clipping rectangle
*     sort the crossings into order (anticlockwise)
*     ie, BASE & RIGHT increasing coords (odd edge number)
*         TOP & LEFT   decreasing coords (even edge number)
      ISTEP = 1
      DO 555 IEDGE=1,4
         IF(NCEDGE(IEDGE) .GT. 0)THEN
           JOFF = JEDGE(IEDGE)
           J1S = ((1-ISTEP)/2)*(NCEDGE(IEDGE)-1)
           J1E = ((1+ISTEP)/2)*(NCEDGE(IEDGE)-1)
*          Zone to be ordered   (J1S:J1E)
*          Zone already ordered (J1S:J1-ISTEP)
*          Unordered Zone       (J1:J1E) must have at least two pts
           DO 552 J1=J1S,J1E-ISTEP,ISTEP
*             Search unordered zone for its minimum
              ICROSS = IORDER(JOFF+J1)
              XYMIN = SIGCOO(ICROSS)
              JMIN  = KNIL
              DO 551 J2=J1+ISTEP,J1E,ISTEP
                 XYCUR = SIGCOO(IORDER(JOFF+J2))
                 IF(XYCUR .LT. XYMIN)THEN
                    XYMIN = XYCUR
                    JMIN  = J2
                 ENDIF
  551         CONTINUE
*             Swap minimum with first in unordered zone
              IF(JMIN .NE. KNIL)THEN
                 ISWAP = IORDER(JOFF+JMIN)
                 IORDER(JOFF+JMIN) = IORDER(JOFF+J1)
                 IORDER(JOFF+J1)   = ISWAP
*                Swap ordinal placings
                 ICRMIN = ISWAP
                 ISWAP = IPLACE(ICRMIN)
                 IPLACE(ICRMIN) = IPLACE(ICROSS)
                 IPLACE(ICROSS) = ISWAP
              ENDIF
*             Unordered zone is reduced by one
  552      CONTINUE
         ENDIF
*        Change direction of ordering for next edge
         ISTEP = -ISTEP
  555 CONTINUE

      END
