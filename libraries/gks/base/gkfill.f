C# IL>=a, OL>=0
      SUBROUTINE GKFILL(NRD,RX,RY,LINSUB,ROSUB)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             DRJF
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Acts as an interface between the Workstation Driver and the
*     Fill Area Utility
*
*  MAINTENANCE LOG
*  ---------------
*     13/11/18   Introduction of a scale factor for PATTERN,
*                HATCH and SOLID FILL AREA styles. Old GKFILL
*                routine used to call new GKFILL routine
*                (GKFILS) so that the present interface is
*                preserved.
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates
*     INP LINSUB Device driver polyline output routine
*     INP ROSUB  Device driver raster output routine
*
      INTEGER NRD
      REAL    RX(NRD),RY(NRD)
      EXTERNAL LINSUB, ROSUB
*
*  LOCALS
*  ------
*     IFILSC     Fill Area spacing scale factor
      INTEGER    IFILSC
      PARAMETER (IFILSC=1)
*
*-------------------------------------------------------------


      CALL GKFILS(NRD,RX,RY,IFILSC,LINSUB,ROSUB)
      RETURN
*
      END
