C# IL>=a, OL>=1
      SUBROUTINE GQEPAI (IWKID,NTH,IER,IO,IPAIND)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF PATTERN INDICES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth element of list of pattern indices.
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
*     OUT IO      number of pattern table entries
*     OUT IPAIND  list of pattern indices
*
      INTEGER IWKID, NTH, IER, IO, IPAIND
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*---------------------------------------------------------------------


      CALL GKQXXI(IWKID,KQEPAI,NTH,IER,IO,IPAIND)

      END
