C# IL>=a, OL>=1
      SUBROUTINE GQEFAI (IWKID,NTH,IER,IO,IFAIND)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF FILL AREA INDICES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth element of list of fill area indices.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP NTH     list element request
*     OUT IER     error indicator
*     OUT IO      number of fill area bundle table entries
*     OUT IFAIND  Nth element of list of defined fill area indices
*
      INTEGER IWKID, NTH, IER, IO, IFAIND
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*---------------------------------------------------------------------


      CALL GKQXXI(IWKID,KQEFAI,NTH,IER,IO,IFAIND)

      END
