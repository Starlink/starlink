C# IL>=a, OL>=1
      SUBROUTINE GQDDS (IWTYPE,IER,IDFMOD,IRGMOD)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE DEFAULT DEFERRAL STATE VALUES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns default deferral state values
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     28/09/83  AS    Change subroutine name
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT IDFMOD - default value for deferral mode
*     OUT IRGMOD - default value for implicit regeneration mode
*
      INTEGER IWTYPE, IER, IDFMOD, IRGMOD
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


      CALL GKQINT(IWTYPE,KQDDS,IER,IDFMOD,IRGMOD,I,I,I,I,I)

      END
