C# IL>=a, OL>=0
      SUBROUTINE GQWKCA (IWTYPE,IER,IWKCAT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE WORKSTATION CATEGORY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns workstation category
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT IWKCAT - workstation category
*
      INTEGER IWTYPE, IER, IWKCAT
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


      CALL GKQINT(IWTYPE,KQWKCA,IER,IWKCAT,I,I,I,I,I,I)

      END
