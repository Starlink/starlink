C# IL>=a, OL>=1
      SUBROUTINE GQEPLI (IWKID,NTH,IER,IO,IPLIND)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF POLYLINE INDICES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth element of list of polyline indices.
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
*     OUT IO      number of polyline bundle table entries
*     OUT IPLIND  Nth element of list of defined polyline indices

      INTEGER IWKID, NTH, IER, IO, IPLIND
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*---------------------------------------------------------------------


      CALL GKQXXI(IWKID,KQEPLI,NTH,IER,IO,IPLIND)

      END
