C# IL>=a, OL>=1
      SUBROUTINE GQETXI (IWKID,NTH,IER,IO,ITXIND)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF TEXT INDICES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth element of list of text indices.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP NTH     list element requested
*     OUT IER     error indicator
*     OUT IO      number of text bundle table entries
*     OUT ITXIND  Nth element of list of defined text indices
*
      INTEGER IWKID, NTH, IER, IO, ITXIND
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*---------------------------------------------------------------------


      CALL GKQXXI(IWKID,KQETXI,NTH,IER,IO,ITXIND)

      END
