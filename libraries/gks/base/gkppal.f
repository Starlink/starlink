C# IL>=a, OL>=0
      SUBROUTINE GKPPAL(NRD,RX,RY)
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
*     Front-End for GKPPPL which determines
*     the distance between the pick point and a
*     polyline primitive.
*
*     This routine may be passed as an argument
*     in place of the device polyline routine.
*
*     The pick point and squared distance are stored
*     in the CSS communication area (idea from MGC)
*
*  MAINTENANCE LOG
*  ---------------
*     29/03/88  KEVP  Created
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
*     QSS1,QSS2 = Pick Point
*     QSS3      = Squared Distance (nearest so far)
*
*  LOCALS
*  ------
*     SQDIST  Squared Distance

      REAL   SQDIST
*-------------------------------------------------------------

      CALL GKPPPL (QSS1,QSS2,NRD,RX,RY,SQDIST)
      IF(SQDIST .LT. QSS3) QSS3 = SQDIST
      END
