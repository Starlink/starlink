C# IL>=a, OL>=0
      SUBROUTINE GKPPAF(NRD,RX,RY)
*
*---------------------------------------------------------------------
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
*     Front-End for GKPPFA which determines
*     the distance between the pick point and the edge of a
*     fill-area primitive and whether the pick point
*     is inside it.
*
*     This routine may be passed as an argument
*     in place of the device polyline routine.
*
*     The pick point and squared distance and whether
*     the pick point is inside are stored in the
*     CSS communication area (idea from MGC)
*
*  MAINTENANCE LOG
*  ---------------
*     29/03/88  KEVP  Created
*     05/01/89  KEVP  Changed from GKLPAF to GKPPAF, output
*                     whether pick point is inside or not and
*                     made distance, distance from edge.
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates DC
*
      INTEGER NRD
      REAL    RX(NRD),RY(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkcca.cmn'
*     For storing pick point and squared distance
*     I/O  QSS1,QSS2 = Pick Point
*     I/O  QSS3      = Squared Distance (nearest so far) from edge
*     INP  QSS4      = Minimum Squared Distance (tolerence)
*     OUT  KSS2      = 0 if pick point outside, 1 if inside
*     INP  KSS3      = 0 if no hit in any previous (higher) segment
*
*  LOCALS
*  ------
*     INSIDE  True if inside fill-area
*     SQDIST  Squared distance
*
      REAL    SQDIST
      LOGICAL INSIDE
*-------------------------------------------------------------

*     Do anything only if there are at least 3 points
*     and distance QSS3 is not already sufficienty small.
      IF((NRD .GE. 3) .AND. (QSS3 .GE. QSS4))THEN
        CALL GKPPPL (QSS1,QSS2,NRD,RX,RY,SQDIST)
*       If no previous segment hit and pick point not on edge,
*       scan interior
        IF((KSS3 .EQ. 0) .AND. (SQDIST .GT. QSS4))THEN
           CALL GKPPFA (QSS1,QSS2,NRD,RX,RY,INSIDE)
        ELSE
           INSIDE = .TRUE.
        ENDIF
        IF(INSIDE)THEN
           KSS2 = 1
        ELSE
           KSS2 = 0
        ENDIF
        QSS3 = SQDIST
      ENDIF
      END
