C# IL>=a, OL>=1
      SUBROUTINE GQSGP (IWTYPE,IER,NSG)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE NUMBER OF SEGMENT PRIORITIES SUPPORTED
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns number of segment priorities supported.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT NSG    - number of segment priorities supported
*
      INTEGER IWTYPE, IER, NSG
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*  LOCALS
*  ------
*
      INTEGER I
*
*---------------------------------------------------------------------


      CALL GKQINT(IWTYPE,KQSGP,IER,NSG,I,I,I,I,I,I)

      END
