C# IL>=a, OL>=0
      SUBROUTINE GKCIRC(IOPT,NRD,RX,RY,LSIMUL,SIMLEN,LINSUB,ROSUB)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  UTILITY
*  Author:             DRJF
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Acts as an interface between the Workstation Driver and the
*     Utility that generates an arc/circle for GDP
*
*  MAINTENANCE LOG
*  ---------------
*     03/02/86 DRJF  Original version stabilized
*                    Introduction of a scale factor for PATTERN,
*                    HATCH and SOLID FILL AREA styles. Old GKCIRC
*                    routine used to call new GKCIRC routine
*                    (GKCRCS) so that the present interface is
*                    preserved.
*
*  ARGUMENTS
*  ---------
*     INP   IOPT    Curve option
*                     1: arc unstyled
*                     2: arc (chord) with interior styled
*                     3: arc (pie  ) with interior styled
*                     4: circle with interior styled
*     INP   RX }    Co-ordinates of three points on arc.
*     INP   RY }
*     INP   LSIMUL
*     INP   SIMLEN
*     INP   LINSUB  Line drawing function.
*     INP   ROSUB   Raster-op function.
*
      INTEGER IOPT,NRD
      REAL     RX(3), RY(3), SIMLEN
      LOGICAL  LSIMUL
      EXTERNAL LINSUB,ROSUB
*
*  LOCALS
*  ------
*     IFILSC     Fill Area spacing scale factor
      INTEGER    IFILSC
      PARAMETER (IFILSC=1)
*
*----------------------------------------------------------------------


      CALL GKCRCS(IOPT,NRD,RX,RY,IFILSC,
     :            LSIMUL,SIMLEN,LINSUB,ROSUB)
      RETURN
*
      END
